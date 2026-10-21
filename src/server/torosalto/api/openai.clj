(ns torosalto.api.openai
  "Local ChatGPT-plan OAuth and independent Responses API questions."
  (:require [buddy.sign.jwk :as jwk]
            [buddy.sign.jwt :as jwt]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.browse :as browse]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hato.client :as http])
  (:import [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
           [java.io BufferedReader InputStream InputStreamReader]
           [java.net InetSocketAddress URLDecoder URLEncoder]
           [java.nio.charset StandardCharsets]
           [java.nio.file CopyOption Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute PosixFilePermissions]
           [java.security MessageDigest SecureRandom]
           [java.util Base64 Timer TimerTask]))

(def ^:private issuer "https://auth.openai.com")
(def ^:private resource "https://api.openai.com/v1")
(def ^:private scopes "openid profile email offline_access resource.invoke chatgpt.tokens.use.direct")

(def ^:private login-timeout-ms (* 10 60 1000))
(def ^:private request-timeout-ms 120000)

(def ^:private secrets-file (io/file "secrets.edn"))

(defonce ^:private http-client
  (http/build-http-client {:connect-timeout 15000 :redirect-policy :never}))

(defn- fail! [message data]
  (throw (ex-info message data)))

(defn- nonblank? [value]
  (and (string? value) (not (str/blank? value))))

(defn- random-string []
  (let [bytes (byte-array 32)]
    (.nextBytes (SecureRandom.) bytes)
    (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) bytes)))

(defn- pkce-challenge [verifier]
  (.encodeToString
   (.withoutPadding (Base64/getUrlEncoder))
   (.digest (MessageDigest/getInstance "SHA-256")
            (.getBytes ^String verifier StandardCharsets/UTF_8))))

(defn- form-encode [params]
  (str/join "&" (for [[key value] params]
                  (str (URLEncoder/encode (name key) "UTF-8") "="
                       (URLEncoder/encode (str value) "UTF-8")))))

(defn- query-params [query]
  (reduce (fn [params part]
            (let [[key value] (str/split part #"=" 2)
                  key (URLDecoder/decode key "UTF-8")]
              (when (contains? params key)
                (fail! "Duplicate callback parameter." {}))
              (assoc params key (URLDecoder/decode (or value "") "UTF-8"))))
          {} (if (str/blank? query) [] (str/split query #"&"))))

(defn- read-secrets []
  (let [contents (if (.exists secrets-file) (slurp secrets-file) "")
        saved (try
                (if (str/blank? contents) {} (edn/read-string contents))
                (catch Exception _ (fail! "Cannot read secrets.edn as EDN." {})))]
    (when-not (and (map? saved) (or (nil? (:openai saved)) (map? (:openai saved))))
      (fail! "secrets.edn and its :openai entry must be maps." {}))
    saved))

(defn- read-state []
  (or (:openai (read-secrets)) {}))

(defn- save-state! [state]
  (locking secrets-file
    (let [saved (assoc (read-secrets) :openai state)
          destination (.toPath (.getAbsoluteFile secrets-file))]
      (io/make-parents secrets-file)
      (let [temporary (Files/createTempFile
                       (.getParent destination) ".secrets-" ".edn"
                       (into-array FileAttribute
                                   [(PosixFilePermissions/asFileAttribute
                                     (PosixFilePermissions/fromString "rw-------"))]))]
        (try
          (spit (.toFile temporary) (pr-str saved))
          (Files/move temporary destination
                      (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                              StandardCopyOption/REPLACE_EXISTING]))
          (finally (Files/deleteIfExists temporary))))
      state)))

(defn- consume-with-deadline!
  "Read a UTF-8 body before deadline-ns, closing the input to interrupt a stall.
   The caller owns normal body cleanup."
  [^InputStream input deadline-ns consume-body!]
  (let [remaining-ms (quot (- deadline-ns (System/nanoTime)) 1000000)]
    (when-not (pos? remaining-ms)
      (fail! "OpenAI response timed out." {}))
    (let [timer (Timer. "torosalto-openai-response" true)
          timed-out? (atom false)]
      (try
        (.schedule timer
                   (proxy [TimerTask] []
                     (run []
                       (reset! timed-out? true)
                       (try (.close input) (catch Exception _))))
                   (long remaining-ms))
        (let [reader (BufferedReader. (InputStreamReader. input StandardCharsets/UTF_8))
              result (consume-body! reader)]
          (when (or @timed-out? (>= (System/nanoTime) deadline-ns))
            (fail! "OpenAI response timed out." {}))
          result)
        (catch Exception error
          (if @timed-out? (fail! "OpenAI response timed out." {}) (throw error)))
        (finally (.cancel timer))))))

(defn- request!
  "Request and consume a response within one deadline; always close its body.
   JSON is the default; a custom consumer receives a UTF-8 BufferedReader."
  ([options] (request! options #(json/parse-stream-strict % true)))
  ([options consume-body!]
   (let [timeout-ms request-timeout-ms
         deadline-ns (+ (System/nanoTime) (* timeout-ms 1000000))
         {:keys [status headers body]}
         (http/request
          (merge {:accept :json} options
                 {:http-client http-client :timeout timeout-ms :as :stream
                  :decompress-body? false
                  ;; Hato's status exceptions include the credential-bearing request.
                  :throw-exceptions? false}))]
     (with-open [input ^InputStream body]
       ;; HTTP header names are case-insensitive; Hato returns an ordinary map.
       (let [headers (update-keys headers str/lower-case)
             content-type (or (get headers "content-type") "")
             media-type (-> content-type (str/split #";" 2) first str/trim str/lower-case)]
         (when-not (<= 200 status 299)
           (fail! "OpenAI HTTP request failed."
                  {:status status :request-id (get headers "x-request-id")}))
         ;; Some plan-backed streams omit Content-Type. The event reader still
         ;; requires response.completed before it can succeed.
         (when (and (= "text/event-stream" (:accept options))
                    (not (str/blank? content-type))
                    (not= "text/event-stream" media-type))
           (fail! "Expected an OpenAI event stream." {:content-type content-type}))
         (consume-with-deadline! input deadline-ns consume-body!))))))

(defn- reply! [^HttpExchange exchange status message]
  (let [bytes (.getBytes ^String message StandardCharsets/UTF_8)]
    (try
      (.set (.getResponseHeaders exchange) "Content-Type" "text/plain; charset=utf-8")
      (.set (.getResponseHeaders exchange) "Cache-Control" "no-store")
      (.sendResponseHeaders exchange status (long (alength bytes)))
      (with-open [out (.getResponseBody exchange)] (.write out bytes))
      (finally (.close exchange)))))

(defn- validate-callback [params client-id]
  (when-let [error (get params "error")]
    (fail! "ChatGPT authorization failed." {:oauth-error error}))
  (let [issued-id (or (get params "client_id") client-id)]
    (when-not (and (nonblank? (get params "code")) (nonblank? issued-id)
                   (not= "dynamic_agent_client" issued-id))
      (fail! "Sign-in callback is missing its code or issued client ID." {}))
    (when (and client-id (not= client-id issued-id))
      (fail! "Sign-in callback changed the registered client ID." {}))
    {:code (get params "code") :client-id issued-id}))

(defn- callback-handler [state client-id callback]
  (reify HttpHandler
    (handle [_ exchange]
      (let [uri (.getRequestURI exchange)
            params (try (query-params (.getRawQuery uri)) (catch Exception _ nil))]
        (cond
          (not= "GET" (.getRequestMethod exchange)) (reply! exchange 405 "Use GET.")
          (not= "/auth/callback" (.getPath uri)) (reply! exchange 404 "Not found.")
          (not= state (get params "state")) (reply! exchange 400 "Invalid sign-in state.")
          (realized? callback) (reply! exchange 409 "Callback already received.")
          :else
          (let [outcome (try (validate-callback params client-id) (catch Exception error error))]
            ;; Deliver after the reply attempt, even if the browser disconnects.
            (try
              (reply! exchange 200 "Callback received. Return to your REPL.")
              (catch Exception _ nil)
              (finally (deliver callback outcome)))))))))

(defn- authorize! [{:keys [host-id client-id]}]
  (let [state (random-string)
        nonce (random-string)
        verifier (random-string)
        callback (promise)
        server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)
        redirect-uri (str "http://127.0.0.1:" (.getPort (.getAddress server))
                          "/auth/callback")]
    (try
      (.createContext server "/auth/callback" (callback-handler state client-id callback))
      (.start server)
      (let [params (cond-> {:client_id (or client-id "dynamic_agent_client")
                           :ext_agent_host_id host-id
                           :response_type "code" :redirect_uri redirect-uri
                           :scope scopes :resource resource
                           :state state :nonce nonce
                           :code_challenge_method "S256"
                           :code_challenge (pkce-challenge verifier)}
                     (nil? client-id) (assoc :agent_name_hint "ToroSalto"))
            url (str issuer "/api/accounts/authorize?" (form-encode params))]
        (println "Continue with ChatGPT in your browser. If it does not open, visit:")
        (println url)
        ;; The printed URL also works when this process cannot launch a browser.
        (try (browse/browse-url url) (catch Exception _ nil)))
      (let [outcome (deref callback login-timeout-ms ::timeout)]
        (when (= ::timeout outcome)
          (fail! "Sign-in timed out. Run login! again." {}))
        (when (instance? Exception outcome) (throw outcome))
        (assoc outcome :redirect-uri redirect-uri :verifier verifier :nonce nonce))
      (finally (.stop server 0)))))

(defn- signing-key! [{:keys [kid alg crit]}]
  ;; The unverified header can select a key, but cannot establish trust.
  (let [expected-key-type ({:rs256 "RSA" :es256 "EC"} alg)]
    (when-not (and expected-key-type (nonblank? kid) (empty? crit))
      (fail! "Unsupported ID-token signing header." {}))
    (let [expected-algorithm (str/upper-case (name alg))
          key-set (:keys (request! {:url (str issuer "/.well-known/jwks.json")}))
          key (some (fn [key]
                      (when (and (= kid (:kid key))
                                 (= expected-key-type (:kty key))
                                 (or (nil? (:use key)) (= "sig" (:use key)))
                                 (or (nil? (:alg key)) (= expected-algorithm (:alg key))))
                        key))
                    key-set)]
      (when-not key (fail! "No matching OpenAI signing key." {}))
      (jwk/public-key key))))

(defn- verify-id-token! [id-token client-id nonce]
  (when-not (nonblank? id-token)
    (fail! "OpenAI did not return an ID token." {}))
  (let [header (jwt/decode-header id-token)
        key (signing-key! header)
        {:keys [sub exp iat aud azp] :as claims}
        (jwt/unsign id-token key {:alg (:alg header) :iss issuer :aud client-id :leeway 5})
        now (quot (System/currentTimeMillis) 1000)
        audiences (if (string? aud) [aud] aud)
        authorized-party-required? (or (> (count audiences) 1) (some? azp))]
    (when-not (and (nonblank? sub)
                   (number? exp) (number? iat) (<= iat (+ now 5))
                   (= nonce (:nonce claims)))
      (fail! "ID token is missing valid identity, time, or nonce claims." {}))
    (when (and authorized-party-required? (not= client-id azp))
      (fail! "ID-token authorized-party mismatch." {}))
    claims))

(defn- exchange-code!
  "Exchange browser authorization for a verified session for the saved account."
  [{:keys [code client-id redirect-uri verifier nonce]} expected-subject]
  (let [started-at (System/currentTimeMillis)
        tokens (request! {:method :post :url (str issuer "/api/accounts/oauth/token")
                          :form-params {:grant_type "authorization_code" :client_id client-id
                                        :code code :code_verifier verifier
                                        :redirect_uri redirect-uri :resource resource}})
        claims (verify-id-token! (:id_token tokens) client-id nonce)
        subject (:sub claims)
        granted (set (str/split (or (:scope tokens) "") #"\s+"))]
    (when (and expected-subject (not= expected-subject subject))
      (fail! "Signed-in account differs from the saved registration." {}))
    (when-not (every? granted ["resource.invoke" "chatgpt.tokens.use.direct"])
      (fail! "ChatGPT plan usage was not granted. Run login! and authorize it." {}))
    (when-not (and (nonblank? (:access_token tokens))
                   (= "bearer" (str/lower-case (or (:token_type tokens) "")))
                   (number? (:expires_in tokens)) (pos? (:expires_in tokens)))
      (fail! "OpenAI did not return usable access credentials." {}))
    {:access-token (:access_token tokens)
     :expires-at (+ started-at (* 1000 (:expires_in tokens)))
     :subject subject}))

(defn- valid-session? [session]
  (and (nonblank? (:access-token session))
       (nonblank? (:subject session))
       (number? (:expires-at session))
       (> (:expires-at session) (+ (System/currentTimeMillis) 30000))))

(defn login!
  "Reuse a valid saved session, or sign in through the browser.
   State is stored under :openai in secrets.edn. Bind the result with def;
   it contains credentials. Use {:force? true} to request a fresh browser login."
  ([] (login! {}))
  ([{:keys [force?]}]
   (locking secrets-file
     (let [saved (read-state)
           cached-session (:session saved)]
       (if (and (not force?) (valid-session? cached-session)
                (= (:subject saved) (:subject cached-session)))
         cached-session
         (let [registration (if (:host-id saved)
                              saved
                              (save-state! (assoc saved :host-id (str "urn:uuid:" (random-uuid)))))
               authorization (authorize! registration)
               registered (save-state! (assoc registration :client-id (:client-id authorization)))
               session (exchange-code! authorization (:subject saved))]
           (save-state! (assoc registered :subject (:subject session) :session session))
           session))))))

(defn- auth-headers [session]
  (when-not (valid-session? session)
    (fail! "Missing or expired OpenAI session. Run login! again." {}))
  {"Authorization" (str "Bearer " (:access-token session))})

(defn models!
  "Return this session's visible models as [{:id slug :name display-name}]."
  [session]
  (let [catalog (request! {:url (str resource "/models")
                           :headers (auth-headers session)})]
    (->> (:models catalog)
         (filter #(= "list" (:visibility %)))
         (mapv (fn [{:keys [slug display_name]}] {:id slug :name display_name})))))

(defn- next-event! [^BufferedReader reader]
  (loop [data []]
    (if-let [line (.readLine reader)]
      (cond
        (= "" line) (if (seq data) (str/join "\n" data) (recur []))
        (str/starts-with? line "data:")
        (recur (conj data (str/replace-first (subs line 5) #"^ " "")))
        :else (recur data))
      (when (seq data) (str/join "\n" data)))))

(defn- await-response! [reader]
  (loop [items []]
    (let [data (next-event! reader)]
      (when (or (nil? data) (= "[DONE]" data))
        (fail! "OpenAI stream ended without response.completed." {}))
      (let [event (json/parse-string data true)]
        (case (:type event)
          "response.output_item.done" (recur (conj items (:item event)))
          "response.completed"
          (let [response (:response event)]
            (when-not (= "completed" (:status response))
              (fail! "OpenAI response did not complete." {:status (:status response)}))
            ;; Plan-backed streams can send final items separately and leave
            ;; the completed response's :output empty.
            (cond-> response (empty? (:output response)) (assoc :output items)))
          "response.failed" (fail! "OpenAI response failed."
                                   {:error (get-in event [:response :error])})
          "response.incomplete" (fail! "OpenAI response is incomplete."
                                       {:details (get-in event [:response :incomplete_details])})
          "error" (fail! "OpenAI stream reported an error."
                         (select-keys event [:code :message :param]))
          (recur items))))))

(defn- response-result [{:keys [output model id usage]}]
  (let [parts (->> output
                   (filter #(and (= "message" (:type %)) (= "assistant" (:role %))))
                   (mapcat :content))
        text (apply str (keep #(when (= "output_text" (:type %)) (:text %)) parts))
        refusal (apply str (keep #(when (= "refusal" (:type %)) (:refusal %)) parts))]
    (cond-> {:text text :model model :response-id id :usage usage}
      (seq refusal) (assoc :refusal refusal))))

(defn ask!
  "Ask one independent text question with a model ID returned by models!.
   Return {:text :model :response-id :usage} after completed inference.
   A model refusal is returned explicitly in :refusal."
  [session {:keys [model question]}]
  (when-not (and (nonblank? model) (nonblank? question))
    (fail! "Provide nonblank :model and :question strings." {}))
  (response-result
   (request! {:method :post :url (str resource "/responses")
              :headers (auth-headers session) :accept "text/event-stream" :content-type :json
              :form-params {:model model :input [{:role "user" :content question}]
                            :store false :stream true}}
             await-response!)))

(comment
  ;; Restart the REPL after dependency changes.
  (def session (login!))
  (models! session)
  ;; Select an ID returned by models! (the first is fine for a smoke test).
  (def model (:id (first (models! session))))
  (ask! session {:model model :question "What is 2 + 2? Reply with only the number."}))

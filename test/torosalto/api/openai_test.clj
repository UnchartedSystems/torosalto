(ns torosalto.api.openai-test
  (:require [buddy.core.keys :as keys]
            [buddy.sign.jwt :as jwt]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.browse :as browse]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hato.client :as http]
            [torosalto.api.openai :as openai])
  (:import [com.sun.net.httpserver HttpHandler HttpServer]
           [java.io ByteArrayInputStream IOException StringWriter]
           [java.net InetSocketAddress URI]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
           [java.nio.file Files LinkOption]
           [java.nio.file.attribute FileAttribute PosixFilePermissions]
           [java.security KeyPairGenerator]
           [java.time Duration]
           [java.util.concurrent Callable CountDownLatch Executors Future TimeUnit]))

(def client-id "oaiapp_test")
(def nonce "test-nonce")
(def grant "openid profile email offline_access resource.invoke chatgpt.tokens.use.direct")
(def key-pair
  (delay (.generateKeyPair (doto (KeyPairGenerator/getInstance "RSA") (.initialize 2048)))))

(defn- private-call [symbol & args]
  (apply (ns-resolve 'torosalto.api.openai symbol) args))

(defn- claims []
  (let [now (quot (System/currentTimeMillis) 1000)]
    {:iss "https://auth.openai.com" :aud client-id :sub "test-subject" :nonce nonce
     :iat now :exp (+ now 3600)}))

(defn- tokens [claims]
  {:id_token (jwt/sign claims (.getPrivate @key-pair) {:alg :rs256 :header {:kid "test-key"}})
   :access_token "test-access-token" :refresh_token "test-refresh-token" :token_type "Bearer" :expires_in 3600 :scope grant})

(defn- signing-key []
  (assoc (keys/public-key->jwk (.getPublic @key-pair)) :kid "test-key" :use "sig" :alg "RS256"))

(defn- stream [text] (ByteArrayInputStream. (.getBytes text "UTF-8")))
(defn- event [value] (str "data: " (json/generate-string value) "\n\n"))
(defn- json-response [value]
  {:status 200 :headers {"content-type" "application/json"}
   :body (stream (json/generate-string value))})
(defn- session []
  {:access-token "test-access-token" :subject "test-subject"
   :expires-at (+ (System/currentTimeMillis) 3600000)})
(defn- save-session! [session]
  (spit @#'openai/secrets-file
        (pr-str {:other-api {:token "other-secret"}
                 :openai {:host-id "urn:uuid:test" :client-id client-id
                          :subject "test-subject" :session session}})))

(defn- read-file [] (edn/read-string (slurp @#'openai/secrets-file)))

(defn- renewable-session []
  (assoc (session) :refresh-token "test-refresh-token" :scopes (set (str/split grant #"\s+"))))

(def renewed-tokens
  {:access_token "renewed-access" :refresh_token "renewed-refresh"
   :token_type "Bearer" :expires_in 3600})

(defn- expire-saved-session! []
  (private-call 'with-state-lock
                #(private-call 'save-state!
                               (assoc-in (private-call 'read-state) [:session :expires-at] 0))))

(def completed-response
  {:status "completed" :id "resp_test" :model "test-model"
   :output [{:type "message" :role "assistant" :content [{:type "output_text" :text "4"}]}]
   :usage {:input_tokens 10 :output_tokens 1}})
(def completed-stream
  (str ": keepalive\n\n" (event {:type "response.output_text.delta" :delta "ignored progress"})
       (event {:type "response.completed" :response completed-response})))

(defn- temporary-secrets [run-test]
  (let [directory (.toFile (Files/createTempDirectory "torosalto-openai-test-" (make-array FileAttribute 0)))
        file (io/file directory "secrets.edn")]
    (try
      (spit file "")
      (binding [*out* (StringWriter.)]
        (with-redefs-fn {#'openai/secrets-file file} run-test))
      (finally (doseq [file (reverse (file-seq directory))] (.delete file))))))
(use-fixtures :each temporary-secrets)

(deftest state-is-explicit-and-preserves-other-apis
  (is (= {} (private-call 'read-state)))
  (is (= "" (slurp @#'openai/secrets-file)))
  (spit @#'openai/secrets-file (pr-str {:other-api {:token "other-secret"}}))
  (let [state {:host-id "urn:uuid:test" :client-id client-id :subject "test-subject" :session (session)}]
    (private-call 'save-state! state)
    (is (= state (private-call 'read-state)))
    (is (= {:other-api {:token "other-secret"} :openai state} (read-file)))
    (is (= "rw-------" (PosixFilePermissions/toString
                           (Files/getPosixFilePermissions (.toPath @#'openai/secrets-file)
                                                         (make-array LinkOption 0)))))
    (private-call 'save-state! (dissoc state :session))
    (is (= {:token "other-secret"} (:other-api (read-file))))))

(deftest invalid-state-is-never-overwritten
  (doseq [contents ["{" "[]" "{:openai :invalid}"]]
    (spit @#'openai/secrets-file contents)
    (is (thrown? clojure.lang.ExceptionInfo (openai/login!)))
    (is (= contents (slurp @#'openai/secrets-file)))))

(deftest login-verifies-id-tokens
  (let [issued-tokens (atom (tokens (claims)))
        published-keys (atom [(signing-key)])]
    (with-redefs-fn
      {#'openai/authorize! (fn [_] {:code "code" :client-id client-id :nonce nonce})
       #'http/request (fn [request]
                        (json-response
                         (case (:url request)
                           "https://auth.openai.com/api/accounts/oauth/token" @issued-tokens
                           "https://auth.openai.com/.well-known/jwks.json" {:keys @published-keys})))}
      (fn []
        (is (= "test-subject" (:subject (openai/login!))))
        (doseq [[label change]
                [["issuer" #(assoc % :iss "https://example.com")]
                 ["audience" #(assoc % :aud "other")]
                 ["nonce" #(assoc % :nonce "other")]
                 ["expired" #(assoc % :exp 1)]
                 ["not yet valid" #(assoc % :nbf (+ (:iat %) 100))]
                 ["future issue time" #(update % :iat + 100)]
                 ["missing expiry" #(dissoc % :exp)]
                 ["missing issue time" #(dissoc % :iat)]
                 ["missing subject" #(dissoc % :sub)]
                 ["missing nonce" #(dissoc % :nonce)]
                 ["authorized party" #(assoc % :azp "other")]
                 ["multiple audiences without azp" #(assoc % :aud [client-id "other"])]]]
          (testing label
            (reset! issued-tokens (tokens (change (claims))))
            (is (thrown? Exception (openai/login! {:force? true})))))
        (doseq [token [(jwt/sign (claims) "secret" {:alg :hs256})
                       (jwt/sign (claims) (.getPrivate @key-pair) {:alg :rs256 :header {:kid "unknown"}})
                       (jwt/sign (claims) "forged" {:alg :hs256 :header {:alg :rs256 :kid "test-key"}})]]
          (reset! issued-tokens (assoc (tokens (claims)) :id_token token))
          (is (thrown? Exception (openai/login! {:force? true}))))
        (testing "multiple audiences are allowed with the matching authorized party"
          (reset! issued-tokens (tokens (assoc (claims) :aud [client-id "other"] :azp client-id)))
          (is (= "test-subject" (:subject (openai/login! {:force? true})))))
        (reset! issued-tokens (tokens (claims)))
        (doseq [[label change] [["encryption key" #(assoc % :use "enc")]
                                ["wrong key type" #(assoc % :kty "EC")]
                                ["wrong algorithm" #(assoc % :alg "RS512")]]]
          (testing label
            (reset! published-keys [(change (signing-key))])
            (is (thrown? Exception (openai/login! {:force? true})))))))))

(deftest login-cache-and-first-response
  (let [requests (atom []) registrations (atom [])]
    (spit @#'openai/secrets-file (pr-str {:other-api {:token "other-secret"}}))
    (with-redefs-fn
      {#'openai/authorize!
       (fn [registration]
         (swap! registrations conj registration)
         (is (= (:host-id registration) (get-in (read-file) [:openai :host-id])))
         {:code "test-code" :client-id client-id :nonce nonce :verifier "test-verifier"
          :redirect-uri "http://127.0.0.1:1234/auth/callback"})
       #'http/request
       (fn [request]
         (swap! requests conj request)
         (case (:url request)
           "https://auth.openai.com/api/accounts/oauth/token"
           (do (is (= client-id (get-in (read-file) [:openai :client-id])))
               (json-response (tokens (claims))))
           "https://auth.openai.com/.well-known/jwks.json" (json-response {:keys [(signing-key)]})
           "https://api.openai.com/v1/models"
           (json-response {:models [{:slug "test-model" :display_name "Test" :visibility "list"}
                                    {:slug "hidden" :visibility "hidden"}]})
           "https://api.openai.com/v1/responses"
           {:status 200 :headers {"content-type" "text/event-stream"} :body (stream completed-stream)}))}
      (fn []
        (let [logged-in (openai/login!)]
          (is (= {:client-id client-id :subject "test-subject"} logged-in))
          (is (= "test-access-token" (get-in (read-file) [:openai :session :access-token])))
          (is (= "test-refresh-token" (get-in (read-file) [:openai :session :refresh-token])))
          (is (= logged-in (openai/login!)))
          (is (= 1 (count @registrations)))
          (is (= [{:id "test-model" :name "Test"}] (openai/models!)))
          (is (= {:text "4" :model "test-model" :response-id "resp_test"
                  :usage {:input_tokens 10 :output_tokens 1}
                  :output (:output completed-response)
                  :history (into [{:role "user" :content "What is 2 + 2?"}]
                                 (:output completed-response))}
                 (openai/ask! {:model "test-model" :question "What is 2 + 2?"})))
          (is (= {:grant_type "authorization_code" :client_id client-id :code "test-code"
                  :code_verifier "test-verifier" :redirect_uri "http://127.0.0.1:1234/auth/callback"
                  :resource "https://api.openai.com/v1"} (:form-params (first @requests))))
          (is (= {:model "test-model" :input [{:role "user" :content "What is 2 + 2?"}]
                  :reasoning {:context "all_turns"}
                  :store false :stream true} (:form-params (last @requests))))
          (is (= {"Authorization" "Bearer test-access-token"} (:headers (last @requests))))
          (openai/login! {:force? true})
          (is (= 2 (count @registrations)))
          (is (apply = (map :host-id @registrations)))
          (is (= client-id (:client-id (second @registrations))))
          (is (= {:token "other-secret"} (:other-api (read-file)))))))))

(deftest failed-exchange-retains-registration
  (with-redefs-fn {#'openai/authorize! (fn [_] {:code "expired" :client-id client-id})
                  #'http/request (fn [_] {:status 400 :headers {} :body (stream "Expired code")})}
    #(is (thrown? clojure.lang.ExceptionInfo (openai/login!))))
  (is (= client-id (get-in (read-file) [:openai :client-id])))
  (is (nil? (get-in (read-file) [:openai :session]))))

(deftest login-checks-account-permission-and-credentials
  (doseq [[subject change message]
          [["original-subject" identity #"differs from the saved"]
           ["test-subject" #(assoc % :scope "openid") #"plan usage was not granted"]
           ["test-subject" #(dissoc % :access_token) #"usable renewable credentials"]
           ["test-subject" #(assoc % :access_token " ") #"usable renewable credentials"]
           ["test-subject" #(dissoc % :refresh_token) #"usable renewable credentials"]
           ["test-subject" #(assoc % :refresh_token " ") #"usable renewable credentials"]
           ["test-subject" #(assoc % :token_type "other") #"usable renewable credentials"]
           ["test-subject" #(dissoc % :expires_in) #"usable renewable credentials"]
           ["test-subject" #(assoc % :expires_in 0) #"usable renewable credentials"]]]
    (spit @#'openai/secrets-file
          (pr-str {:openai {:host-id "urn:uuid:test" :client-id client-id :subject subject}}))
    (with-redefs-fn
      {#'openai/authorize! (fn [_] {:code "code" :client-id client-id :nonce nonce})
       #'http/request (fn [request] (json-response (if (= :post (:method request))
                                                   (change (tokens (claims)))
                                                   {:keys [(signing-key)]})))}
      #(is (thrown-with-msg? clojure.lang.ExceptionInfo message (openai/login!))))
    (is (= subject (get-in (read-file) [:openai :subject])))
    (is (nil? (get-in (read-file) [:openai :session])))))

(deftest session-and-question-validation
  (doseq [invalid [nil {} (assoc (session) :expires-at 0) (dissoc (session) :subject)
                   (assoc (session) :subject "different-account")]]
    (save-session! invalid)
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Run login!" (openai/models!)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Run login!"
                          (openai/ask! {:model "test" :question "Hello"}))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"nonblank"
                        (openai/ask! {:model "test" :question " "}))))

(deftest renewal-is-persisted-and-reused-across-calls
  (save-session! (assoc (renewable-session) :expires-at 0))
  (let [refreshes (atom []) credentials (atom [])]
    (with-redefs-fn
      {#'openai/authorize! (fn [_] (throw (AssertionError. "API calls must not open a browser")))
       #'http/request
       (fn [request]
         (case (:url request)
           "https://auth.openai.com/api/accounts/oauth/token"
           (let [n (count (swap! refreshes conj (:form-params request)))]
             (json-response (assoc renewed-tokens :access_token (str "access-" n)
                                                  :refresh_token (str "refresh-" n))))
           "https://api.openai.com/v1/responses"
           (do (swap! credentials conj (:headers request))
               {:status 200 :headers {} :body (stream completed-stream)})
           "https://api.openai.com/v1/models" (json-response {:models []})))}
      (fn []
        (dotimes [_ 2]
          (is (= "4" (:text (openai/ask! {:model "test" :question "Hello"}))))
          (is (= {:client-id client-id :subject "test-subject"} (openai/login!))))
        (is (= 1 (count @refreshes)))
        (is (= (:scopes (renewable-session)) (get-in (read-file) [:openai :session :scopes])))
        (expire-saved-session!)
        (is (= [] (openai/models!)))
        (is (= "4" (:text (openai/ask! {:model "test" :question "Again"}))))
        (is (= [{:grant_type "refresh_token" :client_id client-id
                 :refresh_token "test-refresh-token" :resource "https://api.openai.com/v1"}
                {:grant_type "refresh_token" :client_id client-id
                 :refresh_token "refresh-1" :resource "https://api.openai.com/v1"}]
               @refreshes))
        (is (= [{"Authorization" "Bearer access-1"} {"Authorization" "Bearer access-1"}
                {"Authorization" "Bearer access-2"}] @credentials))
        (is (= "refresh-2" (get-in (read-file) [:openai :session :refresh-token])))
        (is (> (get-in (read-file) [:openai :session :expires-at]) (System/currentTimeMillis)))
        (is (= {:token "other-secret"} (:other-api (read-file))))))))

(deftest expiry-margin-controls-refresh
  (doseq [[remaining expected-refreshes] [[30001 0] [30000 1] [0 1] [-1 1]]]
    (save-session! (assoc (renewable-session) :expires-at (+ 1000000 remaining)))
    (let [refreshes (atom 0)]
      (with-redefs-fn
        {#'openai/now-ms (constantly 1000000)
         #'http/request (fn [request]
                          (if (= :post (:method request))
                            (do (swap! refreshes inc) (json-response renewed-tokens))
                            (json-response {:models []})))}
        #(do (is (= [] (openai/models!)))
             (is (= expected-refreshes @refreshes)))))))

(deftest terminal-refresh-errors-clear-only-the-session
  (doseq [code ["invalid_grant" "invalid_refresh_token" "token_expired" "refresh_token_expired"
                "refresh_token_invalidated" "refresh_token_reused"]
          error [code {:code code :message "test-refresh-token"}]]
    (save-session! (assoc (renewable-session) :expires-at 0))
    (let [before (:openai (read-file))]
      (with-redefs-fn
        {#'openai/authorize! (fn [_] (throw (AssertionError. "Unexpected browser")))
         #'http/request (fn [_] (assoc (json-response {:error error :description "test-access-token"})
                                      :status 400 :headers {"X-Request-ID" "refresh-request"}))}
        #(try
           (openai/models!)
           (is false "Rejected refresh must fail")
           (catch clojure.lang.ExceptionInfo error
             (is (= {:status 400 :request-id "refresh-request" :oauth-error code
                     :reauth-required? true} (ex-data error)))
             (is (not (str/includes? (pr-str error) "test-refresh-token")))
             (is (nil? (.getCause error))))))
      (is (= (dissoc before :session) (:openai (read-file))))
      (is (= {:token "other-secret"} (:other-api (read-file)))))))

(deftest temporary-and-malformed-refresh-failures-preserve-credentials
  (doseq [response [(assoc (json-response {:error "temporarily_unavailable"}) :status 503)
                    (assoc (json-response {:error "invalid_grant"}) :status 503)
                    (assoc (json-response {:error "invalid_client"}) :status 400)
                    (assoc (json-response {:detail "test-refresh-token"}) :status 401)
                    (json-response (dissoc renewed-tokens :refresh_token))
                    (json-response (assoc renewed-tokens :expires_in 0))
                    (json-response (assoc renewed-tokens :access_token " "))
                    (json-response (assoc renewed-tokens :scope "openid"))
                    {:status 200 :headers {} :body (stream "test-refresh-token {invalid-json")}]]
    (save-session! (assoc (renewable-session) :expires-at 0))
    (let [before (slurp @#'openai/secrets-file)]
      (with-redefs-fn
        {#'openai/authorize! (fn [_] (throw (AssertionError. "Transient errors must not open a browser")))
         #'http/request (constantly response)}
        #(try
           (openai/login!)
           (is false "Refresh must fail")
           (catch clojure.lang.ExceptionInfo error
             (is (not (str/includes? (pr-str error) "test-refresh-token")))
             (is (nil? (.getCause error))))))
      (is (= before (slurp @#'openai/secrets-file)))))
  (save-session! (assoc (renewable-session) :expires-at 0))
  (let [before (read-file)]
    (with-redefs [http/request (fn [request] (throw (ex-info "test-refresh-token" request)))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"transport failed" (openai/models!))))
    (is (= before (read-file)))))

(deftest login-upgrades-legacy-credentials-and-recovers-terminal-refresh-errors
  (doseq [saved-session [(session) (assoc (renewable-session) :expires-at 0)]]
    (save-session! saved-session)
    (let [authorizations (atom [])]
      (with-redefs-fn
        {#'openai/authorize! (fn [registration]
                              (swap! authorizations conj registration)
                              {:code "code" :client-id client-id :nonce nonce})
         #'http/request (fn [request]
                          (cond
                            (= "refresh_token" (get-in request [:form-params :grant_type]))
                            (assoc (json-response {:error "invalid_grant"}) :status 400)
                            (= :post (:method request)) (json-response (tokens (claims)))
                            :else (json-response {:keys [(signing-key)]})))}
        (fn []
          (is (= {:client-id client-id :subject "test-subject"} (openai/login!)))
          (is (= 1 (count @authorizations)))
          (is (= "urn:uuid:test" (:host-id (first @authorizations))))
          (is (= client-id (:client-id (first @authorizations))))
          (is (= "test-refresh-token" (get-in (read-file) [:openai :session :refresh-token])))
          (is (= {:client-id client-id :subject "test-subject"} (openai/login!)))
          (is (= 1 (count @authorizations))))))))

(deftest invalid-registration-never-refreshes
  (doseq [change [#(assoc % :client-id "dynamic_agent_client") #(dissoc % :client-id)
                  #(assoc % :subject "different-account")]]
    (save-session! (assoc (renewable-session) :expires-at 0))
    (spit @#'openai/secrets-file (pr-str (update (read-file) :openai change)))
    (with-redefs [http/request (fn [_] (throw (AssertionError. "Invalid registration must not refresh")))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Run login!" (openai/models!))))))

(deftest concurrent-calls-refresh-once-and-run-inference-in-parallel
  (save-session! (assoc (renewable-session) :expires-at 0))
  (let [refreshes (atom 0) inference-started (CountDownLatch. 2)]
    (with-redefs [http/request
                  (fn [request]
                    (if (= "refresh_token" (get-in request [:form-params :grant_type]))
                      (do (swap! refreshes inc) (json-response renewed-tokens))
                      (do
                        (is (= {"Authorization" "Bearer renewed-access"} (:headers request)))
                        (.countDown inference-started)
                        (when-not (.await inference-started 5 TimeUnit/SECONDS)
                          (throw (AssertionError. "Inference must not hold the credential lock")))
                        (let [question (get-in request [:form-params :input 0 :content])
                              response (assoc-in completed-response [:output 0 :content 0 :text] question)]
                          {:status 200 :headers {}
                           :body (stream (event {:type "response.completed" :response response}))}))))]
      (let [executor (Executors/newFixedThreadPool 2)
            calls (mapv (fn [question]
                          (.submit executor ^Callable
                                   (bound-fn [] (openai/ask! {:model "test" :question question}))))
                        ["first" "second"])]
        (try
          (is (= ["first" "second"] (mapv #(:text (.get ^Future % 10 TimeUnit/SECONDS)) calls)))
          (is (= 1 @refreshes))
          (finally (.shutdownNow executor)))))))

(deftest two-jvms-share-rotating-credentials
  (save-session! (assoc (renewable-session) :expires-at 0))
  (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)
        executor (Executors/newFixedThreadPool 4)
        root (str "http://127.0.0.1:" (.getPort (.getAddress server)))
        refreshes (atom 0) received (atom []) processes (atom [])
        directory (.getParentFile @#'openai/secrets-file)]
    (.setExecutor server executor)
    (.createContext
     server "/"
     (let [handler (fn [exchange]
                     (let [route (.getPath (.getRequestURI exchange))]
                       (if (= route "/api/accounts/oauth/token")
                         (let [form (private-call 'query-params (slurp (.getRequestBody exchange)))
                               n (swap! refreshes inc)]
                           (swap! received conj form)
                           (Thread/sleep 150)
                           (private-call 'reply! exchange (if (= n 1) 200 400)
                                         (json/generate-string (if (= n 1) renewed-tokens
                                                                  {:error "refresh_token_reused"}))))
                         (let [input (json/parse-string (slurp (.getRequestBody exchange)) true)
                               question (get-in input [:input 0 :content])
                               response (assoc-in completed-response [:output 0 :content 0 :text] question)
                               bytes (.getBytes (event {:type "response.completed" :response response}) "UTF-8")]
                           (swap! received conj (.getFirst (.getRequestHeaders exchange) "Authorization"))
                           (.set (.getResponseHeaders exchange) "Content-Type" "text/event-stream")
                           (.sendResponseHeaders exchange 200 (long (alength bytes)))
                           (with-open [out (.getResponseBody exchange)] (.write out bytes))))))]
       (reify HttpHandler
         (handle [_ exchange]
           (try (handler exchange) (finally (.close exchange)))))))
    (.start server)
    (try
      (doseq [tag ["first" "second"]]
        (let [process (.start
                       (doto (ProcessBuilder.
                              (into-array String
                                          [(str (io/file (System/getProperty "java.home") "bin" "java"))
                                           "-cp" (System/getProperty "java.class.path")
                                           "clojure.main" "-m" "torosalto.api.openai-worker"
                                           (.getAbsolutePath @#'openai/secrets-file) root tag]))
                         (.redirectErrorStream true)))]
          (swap! processes conj process)))
      (let [deadline (+ (System/nanoTime) 30000000000)]
        (loop []
          (when-not (every? #(.exists (io/file directory (str % ".ready"))) ["first" "second"])
            (when (> (System/nanoTime) deadline)
              (throw (ex-info "Child JVMs did not become ready." {})))
            (Thread/sleep 20)
            (recur))))
      (spit (io/file directory "start") "go")
      (doseq [[process tag] (map vector @processes ["first" "second"])]
        (is (.waitFor ^Process process 15 TimeUnit/SECONDS))
        (when (.isAlive ^Process process)
          (.destroyForcibly ^Process process)
          (.waitFor ^Process process 5 TimeUnit/SECONDS))
        (let [output (slurp (.getInputStream ^Process process))]
          (is (= 0 (.exitValue ^Process process)) output)
          (is (= tag (str/trim output)))))
      (is (= 1 @refreshes))
      (is (= [{"grant_type" "refresh_token" "client_id" client-id
               "refresh_token" "test-refresh-token" "resource" root}]
             (filter map? @received)))
      (is (= ["Bearer renewed-access" "Bearer renewed-access"] (filter string? @received)))
      (is (= "renewed-refresh" (get-in (read-file) [:openai :session :refresh-token])))
      (doseq [file [@#'openai/secrets-file (io/file (str @#'openai/secrets-file ".lock"))]]
        (is (= "rw-------" (PosixFilePermissions/toString
                            (Files/getPosixFilePermissions (.toPath file) (make-array LinkOption 0))))))
      (finally
        (doseq [process @processes] (.destroyForcibly ^Process process))
        (.stop server 0)
        (.shutdownNow executor)))))

(deftest multi-turn-history-preserves-completed-output
  (save-session! (session))
  ;; Exercise both ordinary Responses output and plan-backed streams whose
  ;; response.completed output is empty. Provisional items must not be replayed.
  (doseq [empty-output? [false true]]
    (let [requests (atom [])
          turn-output (fn [turn]
                        [{:type "reasoning" :id (str "rs_" turn) :summary []
                          :encrypted_content (str "encrypted-reasoning-" turn)}
                         {:type "message" :id (str "msg_commentary_" turn)
                          :role "assistant" :phase "commentary" :status "completed"
                          :content [{:type "output_text" :text "Considering move. "}]}
                         {:type "message" :id (str "msg_final_" turn)
                          :role "assistant" :phase "final_answer" :status "completed"
                          :content [{:type "output_text" :text (str "Move " turn)}]}])]
      (with-redefs [http/request
                    (fn [request]
                      (let [turn (count (swap! requests conj request))
                            output (turn-output turn)
                            response (assoc completed-response :id (str "resp_" turn)
                                            :output (if empty-output? [] output))]
                        {:status 200 :headers {"content-type" "text/event-stream"}
                         :body (stream
                                (str (event {:type "response.output_item.added" :output_index 0
                                             :item (assoc (first output) :encrypted_content "partial")})
                                     (apply str (map-indexed
                                                 (fn [index item]
                                                   (event {:type "response.output_item.done"
                                                           :output_index index :item item}))
                                                 output))
                                     (event {:type "response.completed" :response response})))}))]
        (let [first-turn (openai/ask! {:model "gpt-6.1-sol" :question "Choose a move."})
              second-turn (openai/ask! {:model "gpt-6.1-sol" :history (:history first-turn)
                                       :question "The opponent replied. Choose again."})
              third-turn (openai/ask! {:model "gpt-6.1-sol" :history (:history second-turn)
                                      :question "Continue."})
              first-history (into [{:role "user" :content "Choose a move."}] (turn-output 1))
              second-input (conj first-history
                                 {:role "user" :content "The opponent replied. Choose again."})
              second-history (into second-input (turn-output 2))
              third-input (conj second-history {:role "user" :content "Continue."})]
          (is (= first-history (:history first-turn)))
          (is (= second-history (:history second-turn)))
          (is (= (into third-input (turn-output 3)) (:history third-turn)))
          (is (= [[{:role "user" :content "Choose a move."}] second-input third-input]
                 (mapv #(get-in % [:form-params :input]) @requests)))
          (doseq [[turn result] (map vector [1 2 3] [first-turn second-turn third-turn])]
            (is (= (turn-output turn) (:output result)))
            (is (= (str "Considering move. Move " turn) (:text result))))
          (doseq [request @requests]
            (is (= "text/event-stream" (:accept request)))
            (is (= {:model "gpt-6.1-sol" :reasoning {:context "all_turns"}
                    :input (get-in request [:form-params :input]) :store false :stream true}
                   (:form-params request)))))))))

(deftest history-validation
  (save-session! (session))
  (with-redefs [http/request (fn [_] (throw (AssertionError. "Invalid history must not make a request")))]
    (doseq [history ["transcript" {} #{} 1]]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Provide :history as a sequence"
                            (openai/ask! {:model "test" :question "Hello" :history history})))))
  (with-redefs [http/request (fn [_] {:status 200 :headers {} :body (stream completed-stream)})]
    (doseq [history [nil [] '()]]
      (is (= (into [{:role "user" :content "Hello"}] (:output completed-response))
             (:history (openai/ask! {:model "test" :question "Hello" :history history})))))))

(deftest final-output-and-refusal
  (save-session! (session))
  (is (= "4" (:text (private-call 'response-result completed-response))))
  (let [response (assoc completed-response :output
                        [{:type "reasoning" :content [{:type "output_text" :text "ignore"}]}
                         {:type "message" :role "assistant"
                          :content [{:type "output_text" :text "Hello "} {:type "output_text" :text "🌎"}
                                    {:type "refusal" :refusal "Cannot do that."}]}])]
    (is (= "Hello 🌎" (:text (private-call 'response-result response))))
    (is (= "Cannot do that." (:refusal (private-call 'response-result response)))))
  (let [response (assoc completed-response :output [{:type "message" :role "assistant"
                                                    :content [{:type "refusal" :refusal "Cannot do that."}]}])]
    (with-redefs [http/request (fn [_] {:status 200 :headers {"content-type" "text/event-stream"}
                                      :body (stream (event {:type "response.completed" :response response}))})]
      (let [result (openai/ask! {:model "test" :question "Hello"})]
        (is (= "" (:text result)))
        (is (= "Cannot do that." (:refusal result)))
        (is (= (:output response) (:output result)))
        (is (= (into [{:role "user" :content "Hello"}] (:output response)) (:history result)))))))

(deftest response-content-type
  (save-session! (session))
  (doseq [headers [{} {"content-type" ""}
                   {"content-type" "text/event-stream; charset=utf-8"}
                   {"Content-Type" "text/event-stream; charset=utf-8"}
                   {"CONTENT-TYPE" "text/event-stream; charset=utf-8"}]]
    (with-redefs [http/request (fn [_] {:status 200 :headers headers
                                      :body (stream completed-stream)})]
      (is (= "4" (:text (openai/ask! {:model "test" :question "Hello"}))))))
  (with-redefs [http/request (fn [_] {:status 200 :headers {}
                                    :body (stream "{\"error\":\"not an event stream\"}")})]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"without response.completed"
                          (openai/ask! {:model "test" :question "Hello"}))))
  (with-redefs [http/request (fn [_] {:status 401 :headers {"X-Request-ID" "req_test"}
                                    :body (stream "Unauthorized")})]
    (try
      (openai/ask! {:model "test" :question "Hello"})
      (is false "HTTP 401 must fail")
      (catch clojure.lang.ExceptionInfo error
        (is (= {:status 401 :request-id "req_test"} (ex-data error)))))))

(deftest final-items-can-arrive-before-an-empty-completed-output
  (save-session! (session))
  (doseq [part [{:type "output_text" :text "4"}
                {:type "refusal" :refusal "Cannot do that."}]]
    (let [item {:type "message" :role "assistant" :content [part]}
          wire (str (event {:type "response.output_text.delta" :delta "partial"})
                    (event {:type "response.output_item.done" :item item})
                    (event {:type "response.completed" :response (assoc completed-response :output [])}))]
      (with-redefs [http/request (fn [_] {:status 200 :headers {} :body (stream wire)})]
        (let [result (openai/ask! {:model "test" :question "Hello"})]
          (if (= "output_text" (:type part))
            (is (= "4" (:text result)))
            (is (= "Cannot do that." (:refusal result)))))))))

(deftest terminal-events-and-body-ownership
  (save-session! (session))
  (doseq [wire [(event {:type "response.output_text.delta" :delta "partial"}) "data: [DONE]\n\n"
                (event {:type "response.failed" :response {:error {:code "usage_limit"}}})
                (event {:type "response.incomplete"}) (event {:type "error" :code "test-error"})
                (event {:type "response.completed" :response {:status "failed"}}) "data: {bad json}\n\n"]]
    (let [closed? (atom false)
          input (proxy [ByteArrayInputStream] [(.getBytes wire "UTF-8")]
                  (close [] (reset! closed? true) (proxy-super close)))]
      (with-redefs [http/request (fn [_] {:status 200 :headers {"content-type" "text/event-stream"} :body input})]
        (is (thrown? Exception (openai/ask! {:model "test" :question "Hello"})))
        (is @closed?))))
  (let [wire (str ": keepalive\r\n\r\n" "event: response.completed\r\n"
                  "data: {\"type\":\"response.completed\",\r\n"
                  "data: \"response\":" (json/generate-string completed-response) "}\r\n\r\n")]
    (with-redefs [http/request (fn [_] {:status 200 :headers {"content-type" "text/event-stream; charset=utf-8"}
                                      :body (stream wire)})]
      (is (= "4" (:text (openai/ask! {:model "test" :question "Hello"})))))))

(defn- callback-request [url]
  (.send (HttpClient/newHttpClient)
         (-> (HttpRequest/newBuilder (URI/create url)) (.timeout (Duration/ofSeconds 2)) (.GET) (.build))
         (HttpResponse$BodyHandlers/ofString)))

(deftest loopback-authorization
  (doseq [saved-id [nil client-id]]
    (let [opened (atom nil)
          result (with-redefs [browse/browse-url
                               (fn [url]
                                 (let [params (private-call 'query-params (.getRawQuery (URI/create url)))
                                       redirect (get params "redirect_uri")]
                                   (reset! opened params)
                                   (is (= 400 (.statusCode (callback-request (str redirect "?state=wrong&code=bad")))))
                                   (callback-request (str redirect "?"
                                                          (private-call 'form-encode
                                                                        (cond-> {:state (get params "state") :code "valid"}
                                                                          (nil? saved-id) (assoc :client_id client-id)))))))]
                   (private-call 'authorize! (cond-> {:host-id "urn:uuid:test"} saved-id (assoc :client-id saved-id))))]
      (is (= "valid" (:code result)))
      (is (= client-id (:client-id result)))
      (is (= (or saved-id "dynamic_agent_client") (get @opened "client_id")))
      (is (= (when-not saved-id "ToroSalto") (get @opened "agent_name_hint")))
      (is (= grant (get @opened "scope")))
      (is (= (private-call 'pkce-challenge (:verifier result)) (get @opened "code_challenge")))
      (is (thrown? IOException (callback-request (:redirect-uri result)))))))

(deftest callback-reply-failure-does-not-lose-authorization
  (with-redefs-fn
    {#'openai/reply! (fn [exchange _ _] (.close exchange) (throw (IOException. "Disconnected")))
     #'browse/browse-url (fn [url]
                          (let [params (private-call 'query-params (.getRawQuery (URI/create url)))]
                            (try (callback-request (str (get params "redirect_uri") "?"
                                                         (private-call 'form-encode {:state (get params "state")
                                                                                     :code "valid" :client_id client-id})))
                                 (catch IOException _ nil))))}
    #(is (= "valid" (:code (private-call 'authorize! {:host-id "urn:uuid:test"}))))))

(deftest declined-and-invalid-callbacks
  (doseq [callback [{:error "access_denied"} {:code "code"} {:code "code" :client_id "dynamic_agent_client"}]]
    (with-redefs [browse/browse-url (fn [url]
                                    (let [params (private-call 'query-params (.getRawQuery (URI/create url)))]
                                      (callback-request (str (get params "redirect_uri") "?"
                                                             (private-call 'form-encode (assoc callback :state (get params "state")))))))]
      (is (thrown? clojure.lang.ExceptionInfo (private-call 'authorize! {:host-id "urn:uuid:test"}))))))

(deftest authorization-timeout-cleans-up-listener
  (let [redirect (atom nil)]
    (with-redefs-fn {#'openai/login-timeout-ms 30
                    #'browse/browse-url (fn [url]
                                         (reset! redirect (get (private-call 'query-params (.getRawQuery (URI/create url))) "redirect_uri"))
                                         (throw (ex-info "No browser" {})))}
      #(is (thrown-with-msg? clojure.lang.ExceptionInfo #"Sign-in timed out"
                             (private-call 'authorize! {:host-id "urn:uuid:test"}))))
    (is (thrown? IOException (callback-request @redirect)))))

(deftest http-transport-and-both-body-deadlines
  (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)
        executor (Executors/newFixedThreadPool 2)
        root (str "http://127.0.0.1:" (.getPort (.getAddress server))) received (atom nil)]
    (.setExecutor server executor)
    (.createContext server "/"
                    (reify HttpHandler
                      (handle [_ exchange]
                        (let [route (.getPath (.getRequestURI exchange))]
                          (try
                            (case route
                              "/error" (do (.set (.getResponseHeaders exchange) "x-request-id" "req_test")
                                           (private-call 'reply! exchange 401 "test-secret-token"))
                              "/json" (do (reset! received {:body (slurp (.getRequestBody exchange))
                                                            :type (.getFirst (.getRequestHeaders exchange) "Content-Type")})
                                          (private-call 'reply! exchange 200 "{\"ok\":true}"))
                              "/events" (let [bytes (.getBytes completed-stream "UTF-8")]
                                          (.set (.getResponseHeaders exchange) "Content-Type" "text/event-stream")
                                          (.sendResponseHeaders exchange 200 (long (alength bytes)))
                                          (with-open [out (.getResponseBody exchange)] (.write out bytes)))
                              (if (#{"/slow-json" "/slow-sse"} route)
                                (do (.set (.getResponseHeaders exchange) "Content-Type"
                                          (if (= route "/slow-json") "application/json" "text/event-stream"))
                                    (.sendResponseHeaders exchange 200 0)
                                    (with-open [out (.getResponseBody exchange)]
                                      (.write out (.getBytes (if (= route "/slow-json") "{\"ok\":" ": waiting\n\n") "UTF-8"))
                                      (.flush out) (Thread/sleep 300)))
                                (private-call 'reply! exchange 200 "Not an event stream")))
                            (catch Exception _ nil)
                            (finally (.close exchange)))))))
    (.start server)
    (try
      (is (= {:ok true} (private-call 'request! {:method :post :url (str root "/json") :form-params {:a "b&c"}})))
      (is (= {:body "a=b%26c" :type "application/x-www-form-urlencoded"} @received))
      (let [payload {:input [{:role "user" :content "Hello 🌎"}] :store false :stream true}]
        (is (= {:ok true} (private-call 'request! {:method :post :url (str root "/json") :content-type :json :form-params payload})))
        (is (= payload (json/parse-string (:body @received) true)))
        (is (= "application/json" (:type @received))))
      (is (= completed-response (private-call 'request! {:url (str root "/events") :accept "text/event-stream"} @#'openai/await-response!)))
      (doseq [accept [:json "text/event-stream"]]
        (try (private-call 'request! {:url (str root "/error") :accept accept :headers {"Authorization" "Bearer test-secret-token"}})
             (is false "HTTP 401 must fail")
             (catch clojure.lang.ExceptionInfo error
               (is (= {:status 401 :request-id "req_test"} (ex-data error)))
               (is (nil? (.getCause error))))))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Expected an OpenAI event stream"
                            (private-call 'request! {:url root :accept "text/event-stream"} @#'openai/await-response!)))
      (doseq [[route accept consumer] [["/slow-json" :json #(json/parse-stream-strict % true)]
                                       ["/slow-sse" "text/event-stream" @#'openai/await-response!]]]
        (with-redefs-fn {#'openai/request-timeout-ms 100}
          #(is (thrown-with-msg? clojure.lang.ExceptionInfo #"timed out"
                                 (private-call 'request! {:url (str root route) :accept accept} consumer)))))
      (finally (.stop server 0) (.shutdownNow executor)))))

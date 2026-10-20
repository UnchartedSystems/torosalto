(ns torosalto.notation
  (:require [torosalto.rules :as rules]
            [torosalto.display :as display]
            [torosalto.schema :as schema]
            [malli.core :as m]
            [malli.error :as me]
            [clojure.string :as string]))

(defn- throw-error! [error]
  (throw (ex-info (str "Invalid Notation: " (:error error)) {::error error})))

(defn- attempt [operation f]
  (try
    (f)
    (catch clojure.lang.ExceptionInfo e
      (if-let [error (or (::error (ex-data e))
                         (::schema/error (ex-data e)))]
        (assoc error :operation operation)
        (throw e)))))

(def letter->x
  (zipmap (map str "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (range 26)))

(def number->y
  (zipmap (map str (range 1 27)) (range 26)))

(def dir->delta
  {"NW" [-1  1] "N" [0 1] "NE" [1 1]
   "W"  [-1  0]             "E"  [1 0]
   "SW" [-1 -1] "S" [0 -1] "SE" [1 -1]})

(def delta->dir
  (into {} (map (fn [[direction delta]] [delta direction])) dir->delta))

(def Square #"^[A-Z](?:[1-9]|1[0-9]|2[0-6])$")
(def Direction (into [:enum] (keys dir->delta)))

(defn- square->coords [square]
  [(get letter->x (subs square 0 1))
   (get number->y (subs square 1))])

(defn- coords->square [[x y]]
  (str (char (+ (int \A) x)) (inc y)))

(defn- uppercase-input [notation]
  (schema/throw-violation! :string notation)
  (string/upper-case notation))

(def TurnTokens
  [:altn
   [:place [:catn [:square Square]]]
   [:free [:catn [:first Square] [:separator [:= "+"]] [:second Square]]]
   [:hop [:catn
          [:start Square]
          [:hops [:+ [:catn [:separator [:= ">"]] [:direction Direction]]]]
          [:place [:? [:catn [:separator [:= "+"]] [:square Square]]]]]]])

(def parse-turn-tokens (m/parser TurnTokens))
(def unparse-turn-tokens (m/unparser TurnTokens))

(defn- tokenize [pattern text]
  (let [tokens (re-seq pattern text)]
    (when-not (= text (str/join tokens))
      (throw-error!
       {:error :notation/invalid-syntax
        :details {:pattern pattern
                  :text text}}))
    (vec (remove str/blank? tokens))))

(defn- tokens->turn [{:keys [key value]}]
  (let [{:keys [square first second start hops place]} (:values value)]
    {:action key
     :details
     (case key
       :place {:coords (square->coords square)}
       :free {:coords-1 (square->coords first)
              :coords-2 (square->coords second)}
       :hop {:coords (square->coords start)
             :directions (mapv #(get dir->delta (:direction (:values %))) hops)
             :place-coords (when place (square->coords (:square (:values place))))})}))

(defn- turn->tokens [{:keys [action details]}]
  (let [{:keys [coords coords-1 coords-2 directions place-coords]} details]
    (m/tag action
           (m/tags
            (case action
              :place {:square (coords->square coords)}
              :free {:first (coords->square coords-1)
                     :separator "+"
                     :second (coords->square coords-2)}
              :hop {:start (coords->square coords)
                    :hops (mapv #(m/tags {:separator ">" :direction (get delta->dir %)})
                                directions)
                    :place (when place-coords
                             (m/tags {:separator "+"
                                      :square (coords->square place-coords)}))})))))

(defn- parse-turn* [text]
  (let [tokens (tokenize #"[A-Z][0-9]+|[A-Z]+|[0-9]+|[>+/]|\s+" text)
        parsed (parse-turn-tokens tokens)]
    
    (when (= ::m/invalid parsed)
      (throw-error!
       {:error :notation/invalid-syntax
        :details {:text text
                  :tokens tokens}}))
    
    (let [turn (tokens->turn parsed)]
      (schema/throw-violation! schema/Turn turn)
      turn)))

(defn parse-turn
  "Parse exactly one turn, returning a turn map or an error map."
  [notation]
  (attempt :parse-turn #(parse-turn* (uppercase-input notation))))

(defn parse-turns
  "Parse slash-separated turns. Blank input represents an empty vector."
  [notation]
  (attempt :parse-turns
           #(let [text (uppercase-input notation)]
              (if (string/blank? text)
                []
                (mapv (fn [index turn] (parse-turn* turn {:turn-index index}))
                      (range) (string/split text #"/" -1))))))

(defn- format-turn* [turn]
  (string/join (unparse-turn-tokens (turn->tokens turn))))

(defn format-turn
  "Format one representable turn as canonical uppercase notation."
  [turn]
  (attempt :format-turn
           #(do (schema/throw-violation! schema/Turn turn)
                (format-turn* turn))))

(defn format-turns
  "Format a sequence of turns; an empty sequence becomes an empty string."
  [turns]
  (attempt :format-turns
           #(do (schema/throw-violation! [:sequential schema/Turn] turns)
                (string/join "/" (map format-turn* turns)))))

(def ^:private MetadataTokens
  [:catn
   [:turn #"^[0-9]+$"]
   [:player [:enum "R" "B"]]
   [:red-prison #"^[0-9]+$"]
   [:blue-prison #"^[0-9]+$"]
   [:blocked [:? Square]]])

(def ^:private parse-metadata-tokens (m/parser MetadataTokens))

(defn- parse-column [size index text]
  (let [tokens (tokenize #"[0-9]+|[RB]|\s+" text)
        runs (mapv #(case % "R" :red "B" :blue (parse-long %)) tokens)
        run-schema [:vector {:min 1}
                    [:or schema/Stone [:int {:min 1 :max size}]]]]
    ;; Check every run and the total width before allocating any empty cells.
    (schema/throw-violation! run-schema runs)
    (let [width (reduce + (map #(if (int? %) % 1) runs))]
      (schema/throw-violation! [:= size] width))
    (vec (mapcat #(if (int? %) (repeat % :empty) [%]) runs))))

(defn- parse-game* [notation]
  (let [text (uppercase-input notation)
        parts (string/split text #"@" -1)]
    (when-not (= 2 (count parts))
      (throw-error! {:error :notation/invalid-syntax
                     :details {:text text :expected "one @ separator"}}))
    (let [[board-text metadata-text] parts
          columns (string/split board-text #"/" -1)
          size (count columns)
          metadata-tokens (if (string/blank? metadata-text)
                            [] (string/split (string/trim metadata-text) #"\s+"))]
      (schema/throw-violation! (into [:enum] schema/game-sizes) size)
      (schema/throw-violation! MetadataTokens metadata-tokens)
      (let [{:keys [turn player red-prison blue-prison blocked]}
            (:values (parse-metadata-tokens metadata-tokens))
            game {:size size
                  :turn-number (parse-long turn)
                  :player ({"R" :red "B" :blue} player)
                  :prison {:red (parse-long red-prison) :blue (parse-long blue-prison)}
                  :blocked (when blocked (square->coords blocked))
                  :next-blocked nil
                  :version 0
                  :board (mapv (fn [index column] (parse-column size index column))
                               (range) columns)}]
        (schema/throw-violation! schema/Game game)
        game))))

(defn parse-game
  "Parse a board snapshot. Segments are columns A onward, each from rank 1 up.
  Metadata is: @ turn player red-prison blue-prison [blocked-square]."
  [notation]
  (attempt :parse-game #(parse-game* notation)))

(defn- format-column [column]
  (string/join
   (map (fn [run]
          (case (first run)
            :empty (str (count run))
            :red (string/join (repeat (count run) "R"))
            :blue (string/join (repeat (count run) "B"))))
        (partition-by identity column))))

(defn format-game
  "Format a representable board snapshot. Winners and extra fields are rejected.
  Missing blocked/next-blocked and false blocked normalize to nil."
  [game]
  (attempt :format-game
           #(do
              (schema/throw-violation! :map game)
              (let [normalized (-> game
                                   (update :blocked (fn [coords] (or coords nil)))
                                   (update :next-blocked identity))]
                (schema/throw-violation! schema/Game normalized)
                (str (string/join "/" (map format-column (:board normalized)))
                     " @ " (string/join " "
                                     (cond-> [(str (:turn-number normalized))
                                              ({:red "R" :blue "B"} (:player normalized))
                                              (str (get-in normalized [:prison :red]))
                                              (str (get-in normalized [:prison :blue]))]
                                       (:blocked normalized)
                                       (conj (coords->square (:blocked normalized))))))))))

(defn simulate-game
  ([game turns]
   (simulate-game game false turns))
  ([game show? turns]
   (loop [{:keys [error] :as mssg} {:game game}
          remaining turns]
     (or (when error mssg)
         (when show? (do (display/show-game (:game mssg)) nil))
         (when (empty? remaining) mssg)
         (recur (rules/process-message
                 (assoc mssg :turn (first remaining)))
                (rest remaining))))))

(comment
  (parse-turns "C4/D5/B3/F5/C3>NW>NE+C6/A5+F2")
  (format-turn (parse-turn "C10>NW>SW+E5"))
  (let [notation "3R2B3/10/8RR/3BBB1R2/5B3R/1B8/5RB3/10/4B5/10 @ 24 B 3 0 B5"]
    (display/show-game (parse-game notation))
    (format-game (parse-game notation))))

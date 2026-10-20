(ns torosalto.notation
  "Parse and format turns and board snapshots.
  Invalid input throws ExceptionInfo with ::error or ::schema/error in ex-data."
  (:require [torosalto.rules :as rules]
            [torosalto.display :as display]
            [torosalto.schema :as schema]
            [malli.core :as m]
            [clojure.string :as string]))

(comment ;; Turn Notation
  "C4/D5/B3/F5/C3>NW>NE+C6/A5+F2"
  ;; Turns are described by slash separated token fragments
  ;; eg: "C4", "D5", "B3", "F5", "C3>NW>NE+C6", "A5+F2"
  ;; A letter followed a number is a board coordinate (eg: "C4" -> Column C, Row 4)
  ;; A Place action: ".../D5/..."
  ;; Described by a single coordinate representing where a stone is placed
  ;; A Free Action: ".../A5+F2/..."
  ;; Described by two placement coordinates split by a "+" token
  ;; A Hop Action: ".../C3>NW>NE/...", optionally ".../C3>NW>NE+C6/..."
  ;; Described by a starting stone coordinate, followed by compass directions representing hop directions
  ;; Optionally followed by a placement coordinate represented by a "+" token.
  )

(comment ;; Board Notation
  "3R2B3/10/8RR/3BBB1R2/5B3R/1B8/5RB3/10/4B5/10 @ 24 B 3 0 B5"
  ;; The board is described before the signifier, metadata is described after
  ;; @ is active, = is drawn, #R/#B identifies the winner, !R/!B identifies who resigned
  ;; The Board:
  ;; Each slash separated token fragment (eg: "3R2B3", "10", "8RR", etc) represents a column's state
  ;; A number represents a sequence of contiguous empty squares
  ;; R represents a single Red stone
  ;; B represents a single Blue stone
  ;; "3BBB1R2" -> [:empty :empty :empty :blue :blue :blue :empty :red :empty :empty]
  ;; The Metadata:
  ;; After the signifier, spaces separate, in order:
  ;; - Turn Number: "24"
  ;; - Active Player: "B"
  ;; - Red Prisoners: "3"
  ;; - Blue Prisoners: "0"
  ;; - Blocked Coordinate: "B5", is optional!
  )

(defn- throw-error! [error]
  (throw (ex-info (str "Invalid Notation. \n" error) {::error error})))

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
    (when-not (= text (string/join tokens))
      (throw-error!
       {:error :notation/invalid-syntax
        :details {:pattern pattern
                  :text text}}))
    (vec (remove string/blank? tokens))))

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

(defn parse-turn [notation]
  (let [text   (uppercase-input notation)
        tokens (tokenize #"[A-Z][0-9]+|[A-Z]+|[0-9]+|[>+/]|\s+" text)
        parsed (parse-turn-tokens tokens)]
    (when (= ::m/invalid parsed)
      (throw-error!
       {:error :notation/invalid-syntax
        :details {:text text
                  :tokens tokens}}))
    (let [turn (tokens->turn parsed)]
      (schema/throw-violation! schema/Turn turn)
      turn)))

(defn parse-turns [notation]
  (schema/throw-violation! :string notation)
  (if (string/blank? notation)
    []
    (mapv parse-turn (string/split notation #"/"))))

(defn- format-turn* [turn]
  (string/join (unparse-turn-tokens (turn->tokens turn))))

(defn format-turn
  "Format one turn as canonical uppercase notation.
  Throws ExceptionInfo on invalid input. Extra fields are omitted."
  [turn]
  (schema/throw-violation! schema/Turn turn)
  (format-turn* turn))

(defn format-turns
  "Format a sequence of turns; an empty sequence becomes an empty string.
  Throws ExceptionInfo on invalid input. Extra fields are omitted."
  [turns]
  (schema/throw-violation! [:sequential schema/Turn] turns)
  (string/join "/" (map format-turn* turns)))

(def MetadataTokens
  [:catn
   [:turn #"^[0-9]+$"]
   [:player [:enum "R" "B"]]
   [:red-prison #"^[0-9]+$"]
   [:blue-prison #"^[0-9]+$"]
   [:blocked [:? Square]]])

(def parse-metadata-tokens (m/parser MetadataTokens))

(def ^:private signifier->result
  {"@" {}
   "=" {:winner :draw}
   "#R" {:winner :red}
   "#B" {:winner :blue}
   "!R" {:winner :blue :resigned :red}
   "!B" {:winner :red :resigned :blue}})

(def ^:private result->signifier
  (into {} (map (fn [[signifier result]] [result signifier])) signifier->result))

(defn- parse-column [size text]
  (let [tokens (tokenize #"[0-9]+|[RB]|\s+" text)
        runs (mapv #(case % "R" :red "B" :blue (parse-long %)) tokens)
        run-schema [:vector {:min 1}
                    [:or schema/Stone [:int {:min 1 :max size}]]]]
    ;; Check every run and the total width before allocating any empty cells.
    (schema/throw-violation! run-schema runs)
    (let [width (reduce + (map #(if (int? %) % 1) runs))]
      (schema/throw-violation! [:= size] width))
    (vec (mapcat #(if (int? %) (repeat % :empty) [%]) runs))))

(defn parse-game
  "Parse a board snapshot. Segments are columns A onward, each from rank 1 up.
  Metadata is: signifier turn player red-prison blue-prison [blocked-square].
  Signifiers: @ active, = draw, #R/#B winner, !R/!B resigned player.
  Resignations set :winner and :resigned without changing the turn player.
  Throws ExceptionInfo on invalid input."
  [notation]
  (let [text (uppercase-input notation)
        parts (re-matches #"([^@=#!]*)(@|=|[#!][RB])([^@=#!]*)" text)]
    (when-not parts
      (throw-error! {:error :notation/invalid-syntax
                     :details {:text text :expected "one @, =, #R, #B, !R, or !B signifier"}}))
    (let [[_ board-text signifier metadata-text] parts
          columns (string/split board-text #"/" -1)
          size (count columns)
          metadata-tokens (if (string/blank? metadata-text)
                            [] (string/split (string/trim metadata-text) #"\s+"))]
      (schema/throw-violation! (into [:enum] schema/game-sizes) size)
      (schema/throw-violation! MetadataTokens metadata-tokens)
      (let [{:keys [turn player red-prison blue-prison blocked]}
            (:values (parse-metadata-tokens metadata-tokens))
            game (merge (get signifier->result signifier)
                        {:size size
                         :turn-number (parse-long turn)
                         :player ({"R" :red "B" :blue} player)
                         :prison {:red (parse-long red-prison) :blue (parse-long blue-prison)}
                         :blocked (when blocked (square->coords blocked))
                         :next-blocked nil
                         :version 0
                         :board (mapv #(parse-column size %) columns)})]
        (schema/throw-violation! schema/Game game)
        game))))

(defn- format-column [column]
  (string/join
   (map (fn [run]
          (case (first run)
            :empty (str (count run))
            :red (string/join (repeat (count run) "R"))
            :blue (string/join (repeat (count run) "B"))))
        (partition-by identity column))))

(defn format-game
  "Format a board snapshot, preserving :winner and :resigned in the signifier.
  :resigned requires the opposing player as :winner. Other extra fields are omitted.
  Missing or false blocked coordinates normalize to nil.
  Throws ExceptionInfo on invalid input."
  [game]
  (schema/throw-violation! :map game)
  (let [{:keys [board turn-number player prison blocked] :as normalized}
        (update game :blocked #(or % nil))]
    (schema/throw-violation! schema/Game normalized)
    (let [result (select-keys normalized [:winner :resigned])
          signifier (get result->signifier result)]
      (when-not signifier
        (throw-error! {:error :notation/invalid-result
                       :details {:result result}}))
      (str (string/join "/" (map format-column board))
           " " signifier " "
           (string/join " "
                        (cond-> [turn-number
                                 ({:red "R" :blue "B"} player)
                                 (:red prison)
                                 (:blue prison)]
                          blocked (conj (coords->square blocked))))))))

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
  (format-turn (parse-turn "C10>NW>SW+E5"))
  (format-turns (parse-turns "C4/D5/B3/F5/C3>NW>NE+C6/A5+F2"))
  (format-game (parse-game "3R2B3/10/8RR/3BBB1R2/5B3R/1B8/5RB3/10/4B5/10 @ 24 B 3 0 B5"))
  (let [notation "3R2B3/10/8RR/3BBB1R2/5B3R/1B8/5RB3/10/4B5/10 @ 24 B 3 0 B5"]
    (display/show-game (parse-game notation))
    (format-game (parse-game notation))))

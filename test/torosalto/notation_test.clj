(ns torosalto.notation-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [malli.core :as m]
            [malli.generator :as mg]
            [torosalto.notation :as notation]
            [torosalto.rules :as rules]
            [torosalto.schema :as schema])
  (:import [clojure.lang ExceptionInfo]))

(defn- exception-data [f]
  (try
    (f)
    nil
    (catch ExceptionInfo e
      (ex-data e))))

(defn- place [coords]
  {:action :place :details {:coords coords}})

(defn- hop [coords directions place-coords]
  {:action :hop
   :details {:coords coords :directions directions :place-coords place-coords}})

(defn- normalize-turn [turn]
  (if (= :hop (:action turn))
    (update-in turn [:details :place-coords] #(or % nil))
    turn))

(defn- normalize-game [game]
  (-> game
      (update :blocked #(or % nil))
      (update :next-blocked identity)))

(defn- empty-board-note [size]
  (str/join "/" (repeat size size)))

(deftest turn-forms
  (doseq [[text expected]
          [["C10" (place [2 9])]
           ["C10+E5" {:action :free :details {:coords-1 [2 9] :coords-2 [4 4]}}]
           ["C10>NW" (hop [2 9] [[-1 1]] nil)]
           ["C10>NW>SW+E5" (hop [2 9] [[-1 1] [-1 -1]] [4 4])]
           ["A1" (place [0 0])]
           ["Z26" (place [25 25])]]]
    (testing text
      (is (= expected (notation/parse-turn text)))
      (is (= text (notation/format-turn expected)))))
  (doseq [[direction delta] notation/dir->delta]
    (let [text (str "A1>" direction)]
      (is (= (hop [0 0] [delta] nil) (notation/parse-turn text)))
      (is (= text (notation/format-turn (hop [0 0] [delta] nil)))))))

(deftest turn-sequences-and-whitespace
  (let [canonical "C4/D5/B3/F5/C3>NW>NE+C6/A5+F2"
        turns (notation/parse-turns canonical)]
    (is (= 6 (count turns)))
    (is (= canonical (notation/format-turns turns)))
    (is (= canonical (notation/format-turns (seq turns))))
    (is (= turns (notation/parse-turns
                  " c4 /\n d5 / b3 / f5 / c3 > nw\t> ne + c6 / a5 + f2 "))))
  (is (= [] (notation/parse-turns "")))
  (is (= [] (notation/parse-turns " \n\t")))
  (is (= "" (notation/format-turns [])))
  (is (= "" (notation/format-turns '())))
  (is (thrown? ExceptionInfo (notation/parse-turn "")))
  (is (thrown? ExceptionInfo (notation/parse-turn "A1/B2"))))

(deftest malformed-turns
  (doseq [text ["C10/" "C10//" "/C10" "C10//D5" "C10/ /D5"
                "C10>" "C10>>NW" "C10>NW>" "C10+" "C10++E5"
                "C10+E5+F6" "C10+E5>NW" "C10>NW+E5+F6"
                "C10>1793" "C10>NORTH" "A0" "A01" "A27" "AA1"
                "C 10" "!A1" "A1!>NW" "A1!" "A1@" "A1#" "A1B2" "A1\u0000"
                "A99999999999999999999999999999999999"]]
    (testing text
      (doseq [parse [notation/parse-turn notation/parse-turns]]
        (let [error (::notation/error (exception-data #(parse text)))]
          (is (= :notation/invalid-syntax (:error error)))
          (is (string? (get-in error [:details :text])))))))
  (let [error (::notation/error
               (exception-data #(notation/parse-turns "A1/B2!")))]
    (is (= :notation/invalid-syntax (:error error)))
    (is (= "B2!" (get-in error [:details :text])))))

(deftest optional-turn-coordinates
  (let [turn (hop [2 9] [[-1 1]] nil)]
    (doseq [input [turn (assoc-in turn [:details :place-coords] false)
                   (update turn :details dissoc :place-coords)]]
      (is (= "C10>NW" (notation/format-turn input)))
      (is (= turn (notation/parse-turn (notation/format-turn input)))))))

(deftest snapshot-forms-and-ordering
  (doseq [size [8 10]]
    (let [game (rules/make-game {:size size})
          text (str (empty-board-note size) " @ 1 R 0 0")]
      (is (= game (notation/parse-game text)))
      (is (= text (notation/format-game game)))))
  (let [text "1R6/8/8/8/8/8/8/5B2 @ 7 B 3 4 B2"
        game (rules/make-game {:size 8 :turn-number 7 :player :blue
                               :prison {:red 3 :blue 4} :blocked [1 1]
                               :cells [[[0 1] :red] [[7 5] :blue]]})]
    (is (= game (notation/parse-game text)))
    (is (= text (notation/format-game game)))
    (is (= :red (get-in (notation/parse-game text) [:board 0 1])))
    (is (= :blue (get-in (notation/parse-game text) [:board 7 5])))
    (is (= game (notation/parse-game
                " 1 r 6 / 8 /\n8 / 8 / 8 / 8 / 8 / 5 b 2\n@\t7  b\n3   4\t b2 "))))
  (let [text "3R2B3/10/8RR/3BBB1R2/5B3R/1B8/5RB3/10/4B5/10 @ 24 B 3 0 B5"]
    (is (= text (notation/format-game (notation/parse-game text)))))
  (testing "separate empty runs canonicalize into one run"
    (is (= (str (empty-board-note 8) " @ 1 R 0 0")
           (notation/format-game
            (notation/parse-game "4 4/8/8/8/8/8/8/8 @ 1 R 0 0"))))))

(deftest snapshot-result-signifiers
  (doseq [size [8 10]
          player [:red :blue]
          blocked [nil [1 1]]
          [signifier result] [["@" {}]
                              ["=" {:winner :draw}]
                              ["#R" {:winner :red}]
                              ["#B" {:winner :blue}]
                              ["!R" {:winner :blue :resigned :red}]
                              ["!B" {:winner :red :resigned :blue}]]]
    (let [game (merge (rules/make-game {:size size :turn-number 24 :player player
                                       :prison {:red 3 :blue 0} :blocked blocked})
                      result)
          metadata (str "24 " ({:red "R" :blue "B"} player) " 3 0"
                        (when blocked " B2"))
          text (str (empty-board-note size) " " signifier " " metadata)]
      (testing text
        (is (= game (notation/parse-game text)))
        (is (= text (notation/format-game game)))
        (is (= game (notation/parse-game
                     (str (empty-board-note size) signifier metadata))))
        (is (= game (notation/parse-game
                     (str/lower-case (str (empty-board-note size) "\n" signifier "\t" metadata)))))))))

(deftest malformed-result-signifiers
  (doseq [signifier ["#" "!" "#G" "!G" "# R" "! B" "@@" "==" "#R!B" "@#R" "!R="]]
    (let [text (str (empty-board-note 8) " " signifier " 24 B 3 0 B2")
          data (exception-data #(notation/parse-game text))]
      (testing text
        (is (= :notation/invalid-syntax (:error (::notation/error data)))))))
  (doseq [result [{:resigned :red}
                  {:winner :red :resigned :red}
                  {:winner :draw :resigned :blue}
                  {:winner :red :resigned :green}
                  {:winner :red :resigned nil}]]
    (let [data (exception-data
                #(notation/format-game (merge (rules/make-game) result)))]
      (testing (str result)
        (is (= :notation/invalid-result (:error (::notation/error data))))))))

(deftest malformed-snapshots
  (let [board (empty-board-note 8)]
    (doseq [metadata ["" "1 R 0" "1 Q 0 0" "0 R 0 0" "-1 R 0 0"
                      "1 R -1 0" "1 R abc 0" "1 R 0 0 Z26" "1 R 0 0 I1"
                      "1 R 0 0 A9" "1 R 0 0 A0" "1 R 0 0 A1 garbage"
                      "1 R 0 0 @ garbage" "24 B R3B0 XB5"
                      "10000000001 R 0 0" "1 R 10000000001 0"
                      "1 R 0 10000000001" "9999999999999999999999999999 R 0 0"
                      "1 R 9999999999999999999999999999 0"]]
      (testing metadata
        (is (thrown? ExceptionInfo
                     (notation/parse-game (str board " @ " metadata))))))
    (doseq [text [(str board " 1 R 0 0") (str board " @ 1 R 0 0 @")
                  (str board " #RR 1 R 0 0")
                  "1 @ 1 R 0 0" "8/8/8/8/8/8/8/8/ @ 1 R 0 0"
                  "8//8/8/8/8/8/8 @ 1 R 0 0"
                  "8x/8/8/8/8/8/8/8 @ 1 R 0 0"
                  "7/8/8/8/8/8/8/8 @ 1 R 0 0"
                  "9/8/8/8/8/8/8/8 @ 1 R 0 0"
                  "44/8/8/8/8/8/8/8 @ 1 R 0 0"
                  "R8/8/8/8/8/8/8/8 @ 1 R 0 0"
                  "0R8/8/8/8/8/8/8/8 @ 1 R 0 0"
                  "99999999999999999999999999999/8/8/8/8/8/8/8 @ 1 R 0 0"
                  "10000000000/8/8/8/8/8/8/8 @ 1 R 0 0"
                  "8#/8/8/8/8/8/8/8 @ 1 R 0 0"
                  "8\u0000/8/8/8/8/8/8/8 @ 1 R 0 0"]]
      (testing text
        (is (thrown? ExceptionInfo (notation/parse-game text)))))
    (let [error (::notation/error
                 (exception-data
                  #(notation/parse-game "8x/8/8/8/8/8/8/8 @ 1 R 0 0")))]
      (is (= :notation/invalid-syntax (:error error)))
      (is (= "8X" (get-in error [:details :text]))))))

(deftest optional-game-fields-and-limits
  (let [canonical (rules/make-game {:size 8})]
    (doseq [game [canonical (assoc canonical :blocked false)
                  (dissoc canonical :blocked) (dissoc canonical :next-blocked)
                  (dissoc canonical :blocked :next-blocked)]]
      (is (= canonical (notation/parse-game (notation/format-game game))))))
  (doseq [size [8 10]]
    (let [game (rules/make-game {:size size :blocked [(dec size) (dec size)]
                                :turn-number schema/safe-max :player :blue
                                :prison {:red schema/safe-max :blue schema/safe-max}})]
      (is (= game (notation/parse-game (notation/format-game game)))))))

(deftest reject-invalid-data
  (let [turn (place [0 0]) game (rules/make-game {:size 8})]
    (doseq [input [(place [26 0]) (place [0 26])
                   (hop [0 0] [[0 1]] [26 0])]]
      (is (= :schema/violation
             (get-in (exception-data #(notation/format-turn input))
                     [::schema/error :error]))))
    (doseq [input [(assoc game :next-blocked [0 0]) (assoc game :version 1)
                   (assoc game :blocked [8 0]) (assoc game :turn-number 0)
                   (assoc-in game [:board 0 0] :green)
                   (assoc-in game [:prison :red] -1)]]
      (is (= :schema/violation
             (get-in (exception-data #(notation/format-game input))
                     [::schema/error :error]))))
    (is (= :schema/violation
           (get-in (exception-data #(notation/format-turns [turn (place [26 0])]))
                   [::schema/error :error])))))

(deftest formatting-omits-unencoded-fields
  (doseq [turn [(place [0 0])
                {:action :free :details {:coords-1 [0 0] :coords-2 [2 2]}}
                (hop [0 0] [[0 1]] nil)]
          input [(assoc turn :extra true) (assoc-in turn [:details :extra] true)]]
    (is (m/validate schema/Turn input))
    (is (= (notation/format-turn turn) (notation/format-turn input)))
    (is (= turn (notation/parse-turn (notation/format-turn input))))
    (is (= [turn] (notation/parse-turns (notation/format-turns [input])))))
  (let [game (rules/make-game {:size 8})]
    (doseq [input [(assoc game :extra true)
                   (assoc-in game [:prison :extra] 1)]]
      (is (m/validate schema/Game input))
      (is (= (notation/format-game game) (notation/format-game input)))
      (is (= game (notation/parse-game (notation/format-game input)))))))

(deftest malformed-data-is-quiet-and-throws
  (is (= ""
         (with-out-str
           (doseq [parse [notation/parse-turn notation/parse-turns notation/parse-game]
                   input [nil false 12 {} [] :red]]
             (let [data (exception-data #(parse input))
                   error (::schema/error data)]
               (is (= :schema/violation (:error error)))
               (is (contains? error :details))))
           (doseq [format [notation/format-turn notation/format-turns notation/format-game]
                   input [nil false 12 {} :red "A1"]]
             (is (thrown? ExceptionInfo (format input))))
           (doseq [input [{:action :hop :details {:coords [0 0] :directions []}}
                         {:action :hop :details {:coords [0 0] :directions [[2 0]]}}
                         (place [-1 0]) (place [0]) (place ["A" 1])
                         {:action :unknown :details {}}]]
             (is (thrown? ExceptionInfo (notation/format-turn input))))))))

(deftest conversion-does-not-enforce-game-legality
  (let [turn (hop [0 0] [[0 1] [1 1]] [1 1])
        game (rules/make-game {:size 8})]
    (is (= turn (notation/parse-turn (notation/format-turn turn))))
    (is (= :hops/not-orthogonal-or-diagonal (:error (rules/process-turn game turn)))))
  (let [turn (place [25 25])]
    (is (= turn (notation/parse-turn (notation/format-turn turn))))
    (is (= :position/invalid (:error (rules/process-turn rules/new-game turn)))))
  (let [game (rules/make-game {:size 8})
        turns (notation/parse-turns "A1/C1")
        result (notation/simulate-game game turns)]
    (is (= :red (get-in result [:game :board 0 0])))
    (is (= :blue (get-in result [:game :board 2 0])))
    (is (= (:game result) (notation/parse-game (notation/format-game (:game result)))))))

(deftest generated-turn-round-trips
  (let [turns (vec (mg/sample schema/Turn {:seed 426 :size 150}))]
    (is (= #{:place :free :hop} (set (map :action turns))))
    (doseq [turn turns]
      (let [text (notation/format-turn turn) parsed (notation/parse-turn text)]
        (is (string? text))
        (is (= (normalize-turn turn) parsed))
        (is (m/validate schema/Turn parsed))
        (is (= text (notation/format-turn parsed)))))
    (is (= (mapv normalize-turn turns)
           (notation/parse-turns (notation/format-turns turns))))))

(deftest generated-game-round-trips
  (let [games (mg/sample schema/Game {:seed 781 :size 80})]
    (is (= #{8 10} (set (map :size games))))
    (doseq [game games]
      (let [text (notation/format-game game) parsed (notation/parse-game text)]
        (is (string? text))
        (is (= (normalize-game game) parsed))
        (is (m/validate schema/Game parsed))
        (is (= text (notation/format-game parsed)))))))

(deftest arbitrary-text-is-validated
  (doseq [text (mg/sample [:string {:max 100}] {:seed 932 :size 120})
          [parse result-schema] [[notation/parse-turn schema/Turn]
                                 [notation/parse-turns [:vector schema/Turn]]
                                 [notation/parse-game schema/Game]]]
    (try
      (let [parsed (parse text)]
        (is (m/validate result-schema parsed)))
      (catch ExceptionInfo e
        (let [data (ex-data e)
              error (or (::notation/error data) (::schema/error data))]
          (is (contains? #{:notation/invalid-syntax :schema/violation} (:error error)))
          (is (contains? error :details)))))))

(ns torosalto.notation
  (:require [torosalto.rules :as rules]
            [torosalto.display :as display]
            [clojure.string :as str]))

(defn- throw-error! [e]
  (when e
    (throw
      (ex-info (str "Invalid Notation: " (:error e))
               {::error e}))))

;;;; Notation

(comment
  ;; Place
  "C10"
  ;; Free
  "C10+E5"
  ;; Hop
  "C10>1793"
  ;; Hop & Place
  "C10>1793+E5"
  ;; Turn Sequence
  "C4/D5/B3/F5/C3>97+C6/A5+F2"
  ;; Optional Hop
  "C10>NW>SW>SE>NE+E5")

(def letter->x
  {"A" 0 "B" 1 "C" 2 "D" 3 "E" 4 "F" 5 "G" 6 "H" 7 "I" 8 "J" 9
   "K" 10 "L" 11 "M" 12 "N" 13 "O" 14 "P" 15 "Q" 16 "R" 17
   "S" 18 "T" 19 "U" 20 "V" 21 "W" 22 "X" 23 "Y" 24 "Z" 25})

(def number->y
  {"1" 0 "2" 1 "3" 2 "4" 3 "5" 4 "6" 5 "7" 6 "8" 7 "9" 8 "10" 9
   "11" 10 "12" 11 "13" 12 "14" 13 "15" 14 "16" 15 "17" 16 "18" 17
   "19" 18 "20" 19 "21" 20 "22" 21 "23" 22 "24" 23 "25" 24 "26" 25})

(def dir->delta
  {"1" [-1  1] "NW" [-1  1]
   "2" [ 0  1] "N"  [ 0  1]
   "3" [ 1  1] "NE" [ 1  1]
   "4" [-1  0] "W"  [-1  0]
   "6" [ 1  0] "E"  [ 1  0]
   "7" [-1 -1] "SW" [-1 -1]
   "8" [ 0 -1] "S"  [ 0 -1]
   "9" [ 1 -1], "SE" [ 1 -1]})

(defn interp-coords [coords]
  (throw-error!
   (or (when-not (string? coords)
         {:error :parse/input-is-not-string
          :details {:input coords}})

       (when-not (< 1 (count coords))
         {:error :parse/invalid-string-format
          :details {:input coords}})))
  
  (let [x (get letter->x (subs coords 0 1))
        y (get number->y (subs coords 1))]
    (throw-error!
     (or (when-not x
           {:error :parse/missing-letter
            :detail {:coords [x y]}})

         (when-not y
           {:error :parse/missing-number
            :detail {:coords [x y]}})))
    [x y]))

(defn- parse-place [line]
  (let [coords (interp-coords (first line))]
    {:action :place
     :details {:coords coords}}))

(defn- parse-free [line]
  (let [coords-1 (interp-coords (first line))
        coords-2 (interp-coords (peek line))]
    {:action :free
     :details {:coords-1 coords-1
               :coords-2 coords-2}}))

(defn- parse-hop [line]
  (let [coords (interp-coords (first line))
        [place-coords? place?] (take 2 (reverse line))
        place-coords (if (= place? "+") (interp-coords place-coords?) nil)
        line (drop 2 line)
        line (if place-coords (drop-last 2 line) line)]
    (loop [[n1 n2 & ns] line
           directions []]
      (if (empty? ns)
        {:action :hop
         :details {:coords coords
                   :place-coords place-coords
                   :directions (conj directions (get dir->delta n1))}}
        (recur ns (conj directions (get dir->delta n1))))))) 

(defn- split-turns [s]
  (mapv #(vec (re-seq #"[^>+\s]+|[>+]" %))
        (str/split s #"/")))

;; TODO: add untrusted input validation, checks, QOL impr.
(defn- parse-turns [notation]
  (throw-error! (when-not (string? notation)
                  {:error :parse/notation-not-string
                   :details {:notation notation}}))
  (let [moves (split-turns (str/upper-case notation))]
    
    (vec
     (for [move moves]
       (cond (= 1 (count line)) (parse-place move)
             (and (= 3 (count line)) (= "+" (second line))) (parse-free move)
             (= ">" (second line)) (parse-hop move)
             :else {:error :notation/no-match
                    :details {:move move
                              :moves moves}})))))

(defn interpret-turns [notation]
  (try
    (parse-turns notation)
    (catch clojure.lang.ExceptionInfo e
      (if-let [error (::error (ex-data e))]
        (do (println error) error)
        (throw e)))))

(comment
  ;; Board Notation
  "3R2B3/10/8RR/3BBB1R2/5B3R/1B8/5RB3/10/4B5/10 24 B R3B0 XB5"
  ;; Spaces signify metadata
  "24" ;; turn 24
  "B" ;; who's turn is it
  "S10" ;; What is the size
  "R3B0" ;; state of the prison
  "XB5" ;; If present: X means blocked, B5 are coordinates
  )


(defn- split-game [game]
  (-> game
      (str/upper-case)
      (str/trim)
      (str/split #" ")))

(defn- split-rows [board]
  (mapv #(vec (re-seq #"\d+|[RB]" %))
        (str/split (str/replace board " " "") #"/")))

(defn- convert-rows [rows]
  (mapv
   (fn [row]
     (reduce
      #(into %1
       (case %2 "B" [:blue] "R" [:red]
         (repeat (parse-long %2) :empty)))
      [] row))
   rows))

(defn- verify-size [board]
  (reduce
   (fn [size row] (if (= size (count row)) size (reduced false)))
   (count board)
   board))

(defn- parse-game [notation]
  (let [[board-notation t pl r-p b-p b] (split-game notation)
        rows (split-rows board-notation)
        board (convert-rows rows)
        turn-number (parse-long t)
        player (case pl "B" :blue "R" :red)
        red-prisoners (parse-long r-p)
        blue-prisoners (parse-long b-p)
        blocked (when b (interp-coords b))]
    (if-let [size (verify-size board)]
      {:size size
       :turn-number turn-number
       :blocked blocked
       :next-blocked nil
       :version 0
       :player player
       :prison {:red red-prisoners
                :blue blue-prisoners}
       :board board}
      {:error :parse/inconsistent-size
       :details {:board board}})))

(defn interpret-game [notation]
  (try
    (parse-game notation)
    (catch clojure.lang.ExceptionInfo e
      (if-let [error (::error (ex-data e))]
        (do (println error) error)
        (throw e)))))

(def board-note "3R2B3/10/8RR/3BBB1R2/5B3R/1B8/5RB3/10/4B5/10 24 B 3 0 B5")
(display/show-game (parse-game board-note))

(let [row (partition-by identity [:empty :empty :red :empty :empty :empty :blue :red :red])]
  (reduce
   (fn [result cell]
     (if (= cell :empty)
       ()
       ()))
   row))

(defn format-game [game]
  )

(defn simulate-game
  ([game notation]
   (simulate-game game false notation))
  
  ([game show? notation]
   (let [turns (parse-turns notation)]
     (loop [{:keys [error] :as mssg} {:game game}
            remaining turns]
       (or (when error mssg)
           (when show? (do (display/show-game (:game mssg)) nil))
           (when (empty? remaining) mssg)
           (recur (rules/evaluate
                   (assoc mssg :turn (first remaining)))
                  (rest remaining)))))))

#_(simulate-game (rules/make-game) true "C4/D5/B3/D4/H8/D4>W>S+J1/H2+B7/C4")


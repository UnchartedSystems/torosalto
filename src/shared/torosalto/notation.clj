(ns torosalto.notation
  (:require [torosalto.rules :as rules]
            [clojure.string :as str]))

;;;; Notation


;; Notation Reference:
;; These are the only accepted forms:

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
"C10>NW>SW>SE>NE+E5"

;; Down the road:
;; - notation for creating a game
;; - optional player-oriented notation for winning & blocking.

(def letter->x
  {"A" 0 "B" 1 "C" 2 "D" 3 "E" 4 "F" 5 "G" 6 "H" 7 "I" 8 "J" 9
   "K" 10 "L" 11 "M" 12 "N" 13 "O" 14 "P" 15 "Q" 16 "R" 17
   "S" 18 "T" 19 "U" 20 "V" 21 "W" 22 "X" 23 "Y" 24 "Z" 25})

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
  (let [[letter num] [(subs coords 0 1) (subs coords 1)]]
    [(get letter->x letter) (dec (parse-long num))]))

(defn parse-str [s]
  (map #(-> (str/escape % {\> " > " \+ " + "})
            (str/split #" "))
       (str/split s #"/")))

(defn convert-place [line]
  (and
   (= 1 (count line))
   (let [coords (interp-coords (first line))]
     {:action :place
      :details {:coords coords}})))

(defn convert-free [line]
  (and
   (= 3 (count line))
   (= "+" (second line))
   (let [coords-1 (interp-coords (first line))
         coords-2 (interp-coords (peek line))]
     {:action :free
      :details {:coords-1 coords-1
                :coords-2 coords-2}})))

(defn convert-hop [line]
  (and
   (= ">" (second line))
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
         (recur ns (conj directions (get dir->delta n1)))))))) 

;; TODO: add untrusted input validation, checks, QOL impr.
(defn interpret [notation]
  (let [moves (parse-str (str/upper-case notation))]
    (vec
     (for [move moves]
       (or (convert-place move)
           (convert-free move)
           (convert-hop move)
           {:error :notation/no-match})))))

(defn simulate-game
  ([game notation]
   (simulate-game game false notation))
  
  ([game show? notation]
   (let [turns (interpret notation)]
     (loop [{:keys [error] :as mssg} {:game game}
            remaining turns]
       (or (when error mssg)
           (when show? (do (rules/show-game (:game mssg)) nil))
           (when (empty? remaining) mssg)
           (recur (rules/evaluate
                   (assoc mssg :turn (first remaining)))
                  (rest remaining)))))))

(simulate-game (rules/make-game) true "C4/D5/B3/D4/H8/D4>W>S+J1/H2+B7/C4")


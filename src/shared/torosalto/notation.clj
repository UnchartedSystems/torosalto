(ns torosalto.notation
  (:require [torosalto.rules :as rules]))

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

(def direction->delta
  {"1" [-1  1] "NW" [-1  1]
   "2" [ 0  1] "N"  [ 0  1]
   "3" [ 1  1] "NE" [ 1  1]
   "4" [-1  0] "W"  [-1  0]
   "6" [ 1  0] "E"  [ 1  0]
   "7" [-1 -1] "SW" [-1 -1]
   "8" [ 0 -1] "S"  [ 0 -1]
   "9" [ 1 -1], "SE" [ 1 -1]})

;; TODO capitalize letter
(defn interp-coords [coords]
  (let [[letter num] [(subs coords 0 1) (subs coords 1)]]
    [(get letter->x letter) (dec (parse-long num))]))


(defn interpret [notation])


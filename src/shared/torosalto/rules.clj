(ns torosalto.rules)

(def tile-states #{:empty :red :blue :blocked})

(def letter->y
  {"A" 0 "B" 1 "C" 2 "D" 3 "E" 4 "F" 5 "G" 6 "H" 7 "I" 8 "J" 9})

(def num->x
  {10 0 9 1 8 2 7 3 6 4 5 5 4 6 3 7 2 8 1 9})

;; TODO capitalize letter
(defn interp-coords [coords]
  (let [[letter num] [(subs coords 0 1) (subs coords 1)]]
    [(get letter->y letter) (get num->x num)]))

;; Board Creation

(defn make-board [board & cells]
  (if (empty? cells)
    board
    (let [[coords state] (first cells)
          cells (rest cells)]
      (recur
       (assoc-in board coords state)
       cells))))

(def board
  (vec (repeat 10 (vec (repeat 10 :empty)))))

(def test-board
  (make-board
   board
   [[3 5] :red]
   [[3 3] :red]
   [[2 6] :blue]
   [[2 5] :red]
   [[2 3] :red]
   [[1 4] :red]
   [[5 5] :red]
   [[8 8] :blue]
   [[8 7] :red]
   [[7 8] :red]
   [[9 8] :blue]
   [[7 8] :blue]
   [[8 9] :red]
   [[6 9] :red]
   [[5 9] :blue]
   [[9 9] :red]
   [[1 1] :red]
   [[7 0] :red]
   [[9 2] :red]
   [[5 4] :blue]
   [[5 3] :blue]))

;; Legal Rules

(defn place? [x y]
  (= :empty (get-in board [x y])))

(defn hop? [])

;; Display

(defn display [board]
  (let [nums ["10" " 9" " 8" " 7" " 6" " 5" " 4" " 3" " 2" " 1"]]
    (doseq [x (range 10)]
      (print (get nums x))
      (doseq [y (range 10)]
        (print 
         (case (get-in board [x y])
           :empty   " ·"
           :red     "\u001b[31m ◉\u001b[0m"
           :blue    "\u001b[34m ◉\u001b[0m"
           :blocked "-")))
      (println))
    (println "   A B C D E F G H I J")))

(doall
 (println)
 (display test-board))

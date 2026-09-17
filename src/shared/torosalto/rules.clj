(ns torosalto.rules)

;; Cell States
(def cell-states #{:empty :red :blue :blocked})
(def stones #{:red :blue})

;; Movement Constraints
(def adjacencies
  #{[1 -1]  [1 0]  [1 1]
    [0 -1]         [0 1]
    [-1 -1] [-1 0] [-1 1]})

(def diagonal
  #{[1 -1]  [1 1]
    [-1 -1] [-1 1]})

(def orthogonal
  #{[1 0] [0 1] [-1 0] [0 -1]})

;;;; Board Creation

(def board-size 10)

(def empty-board
  (vec (repeat board-size (vec (repeat board-size :empty)))))

(defn make-board [board cells]
  (if (empty? cells)
    board
    (let [[coords state] (first cells)
          cells (rest cells)]
      (recur
       (assoc-in board coords state)
       cells))))

(def test-board
  (make-board
   empty-board
   [[[3 5] :red] [[3 3] :red] [[2 6] :blue] [[2 5] :red]
    [[2 3] :red] [[1 4] :red] [[5 5] :red] [[8 8] :blue]
    [[8 7] :red] [[7 8] :red] [[9 8] :blue] [[7 8] :blue]
    [[8 9] :red] [[6 9] :red] [[5 9] :blue] [[9 9] :red]
    [[1 1] :red] [[7 0] :red] [[9 2] :red] [[5 4] :blue]
    [[5 3] :blue] [[0 8] :blue] [[6 8] :blocked] [[4 0] :blocked]]))

(def win-test-board
  (make-board
   empty-board
   [[[4 4] :red] [[4 3] :red] [[4 2] :red] [[4 5] :blue] [[4 6] :red]
    [[9 7] :red] [[0 8] :red] [[8 6] :red] [[7 5] :red] [[6 4] :red]
    [[0 4] :blue] [[1 3] :blue] [[2 2] :blue] [[3 1] :blue] [[4 0] :blue]]))



;;;; Utilities
(defn v+ [a b] (mapv + a b))
(defn v* [n v] (mapv #(* n %) v))

(def letter->y
  {"A" 0 "B" 1 "C" 2 "D" 3 "E" 4 "F" 5 "G" 6 "H" 7 "I" 8 "J" 9})

;; TODO capitalize letter
(defn interp-coords [coords]
  (let [[letter num] [(subs coords 0 1) (subs coords 1)]]
    [(get letter->y letter) (dec num)]))

(defn wrap [[x y]]
  [(mod x board-size)
   (mod y board-size)])

(defn move [coords delta]
  (wrap (v+ coords delta)))

(defn adj-stones [board coords]
  (filterv #(stones (second %))
           (mapv #(vector % (get-in board (move coords %)))
                 adjacencies)))

;;;; Legal Rules

(defn legal-position? [[x y]]
  (and (<= 0 x) (< x board-size)
       (<= 0 y) (< y board-size)))

(defn place? [board coords]
  (and (legal-position? coords)
       (= :empty (get-in board coords))))

(defn place-open? [board coords]
  (and (place? board coords)
       (empty? (adj-stones board coords))))

(defn hop?
  ([board coords direction]
   (hop? board coords direction adjacencies))
  ([board coords direction constraint]
   (boolean
    (and (legal-position? coords)
         (constraint direction)
         (not (stones (get-in board (move coords (v* 2 direction)))))
         (stones (get-in board (move coords direction)))))))


(defn- scan-line [board coords direction team]
  (->> (mapv #(v* % direction) (range -4 5))
       (mapv #(v+ coords %))
       (filterv #(legal-position? %))
       (mapv #(get-in board %))
       (mapv #(= team %))
       (partition-by identity)
       (filterv first)
       (mapv count)
       (reduce max 0)))

;; Sloppy but works: adj-stones wraps.
;; Plenty of redundant work.
(defn win? [board coords team]
  (let [line-dirs (mapv first (filterv #(= (second %) team) (adj-stones board coords)))]
    (->> (mapv #(scan-line board coords % team) line-dirs)
         (reduce max 0)
         (<= 5))))

(win? test-board [9 8] :blue)

(win? test-board [7 9] :red)

;;;; Display

(defn- num-guides [rows]
  (mapv #(format "%2d" %) (range 1 (inc rows))))

(defn- letter-guides [columns]
  (subs " A B C D E F G H I J K L M N O P Q R S T U V W X Y Z"
        0 (* columns 2)))

(defn display [board]
  (let [nums (num-guides board-size)]
    (doseq [x (reverse (range board-size))]
      (print (get nums x))
      (doseq [y (range board-size)]
        (print 
         (case (get-in board [x y])
           :empty   " ·"
           :red     "\u001b[31m ◉\u001b[0m"
           :blue    "\u001b[34m ◉\u001b[0m"
           :blocked " ×")))
      (println)))
  (println " " (letter-guides board-size)))

(doall
 (println)
 (display test-board))

(doall
 (println)
 (display win-test-board))

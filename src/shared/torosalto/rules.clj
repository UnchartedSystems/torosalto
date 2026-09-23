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

(def new-game
  {:player :red
   :prison {:red 0 :blue 0}
   :board empty-board})

(def game
  {:player :red
   :prison {:red 0 :blue 0}
   :board test-board})

;;;; Utilities

(defn v+ [a b] (mapv + a b))
(defn v* [n v] (mapv #(* n %) v))

(def letter->y
  {"A" 0 "B" 1 "C" 2 "D" 3 "E" 4 "F" 5 "G" 6 "H" 7 "I" 8 "J" 9})

;; TODO capitalize letter
(defn interp-coords [coords]
  (let [[letter num] [(subs coords 0 1) (subs coords 1)]]
    [(get letter->y letter) (dec (parse-long num))]))

;; Will need to be changed to take in a board size
(defn wrap [[x y]]
  [(mod x board-size)
   (mod y board-size)])

(defn move [coords delta]
  (wrap (v+ coords delta)))

(defn adjacent? [coords-1 coords-2]
  (boolean
   (some #(= coords-1 %)
         (mapv #(move coords-2 %) adjacencies))))

(defn adj-stones [board coords]
  (filterv #(stones (second %))
           (mapv #(vector % (get-in board (move coords %)))
                 adjacencies)))

(defn get-constraint [direction]
  (cond (diagonal direction)   diagonal
        (orthogonal direction) orthogonal
        :else false))

;;;; Legality Checks

(defn legal-position? [[x y]]
  (and (int? x) (int? y)
       (<= 0 x) (< x board-size)
       (<= 0 y) (< y board-size)))

(defn place? [{:keys [board]} coords]
  (and (legal-position? coords)
       (= :empty (get-in board coords))))

(defn place [{:keys [player board] :as game} coords]
  (assoc game
         :board (assoc-in board coords player)))

(defn place-open? [{:keys [board] :as game} coords]
  (and (place? game coords)
       (empty? (adj-stones board coords))))

(defn free? [{:keys [player prison board] :as game} coords-1 coords-2]
  (boolean
   (and (<= 2 (get prison player))
        (not= coords-1 coords-2)
        (not (adjacent? coords-1 coords-2))
        (place-open? game coords-1)
        (place-open? game coords-2))))

(defn free [{:keys [player prison board] :as game} coords-1 coords-2]
  (-> game
      (update-in [:prison player] - 2)
      (place coords-1)
      (place coords-2)))

(defn hop?
  ([game coords direction]
   (hop? game coords direction adjacencies))
  ([{:keys [player board]} coords direction constraint]
   (boolean
    (and (= player (get-in board coords))
         (legal-position? coords)
         (constraint direction)
         (not (stones (get-in board (move coords (v* 2 direction)))))
         (stones (get-in board (move coords direction)))))))

;; Either here or hops, need to check player against coords stone
(defn hop
  ([game coords direction]
   (hop game coords direction false))
  ([{:keys [prison board] :as game} coords direction blocked?]
   (let [hopped-over (move coords direction)
         hopped-to (move coords (v* 2 direction))
         attacker (get-in board coords)
         captured (get-in board hopped-over)]
     {:coords hopped-to
      :game (assoc game
                   :prison (update prison captured inc)
                   :board (-> board
                              (assoc-in coords :empty)
                              (assoc-in hopped-over (if blocked? :blocked :empty))
                              (assoc-in hopped-to attacker)))})))

(defn hops? [{:keys [prison board] :as game} coords directions]
  (let [constraint (get-constraint (first directions))]
    (boolean
     (when (and constraint (not (empty? directions)))
       (reduce
        (fn [{:keys [coords game]} direction]
          (if (hop? game coords direction constraint)
            (hop game coords direction)
            (reduced false)))
        {:coords coords
         :game game}
        directions)))))

;; hops & hops? replicate work traversing series of hops.
(defn hops [{:keys [prison board] :as game} coords directions]
  (let [blocked? (= 1 (count directions))]
    (:game
     (reduce (fn [{:keys [game coords]} direction]
               (hop game coords direction blocked?))
             {:coords coords
              :game game}
             directions))))



;;;; Direction Refactor?
;; Compass Headings:
#_[:n :ne :e :se :s :sw :w :nw]
;; Deltas:
#_[[1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1] [0 -1] [1 -1]]

;;;; Full Hops
;; Sequence of Directions as input?
;; This allows:
;; - Checking all directions match constraint
;; - Checking single vs multihops.

;;;; Turn structure
;; Where should turn be managed and change?
;; What am I actually building? Validation for CLJ & CLJS? CLJ CLI interaction? CLJS interaction? All?

;;;; Display Backgrounds
;; Good background text colors?
;; Should I compare turns to last turns and highlight differences?
;; Iteration over all options of a turn move and highlighting them?




;;;; Win Condition

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
;; When will win be checked? A player can win by hopping (specially on an odd board)
(defn win? [board coords team]
  (let [line-dirs (mapv first (filterv #(= (second %) team) (adj-stones board coords)))]
    (->> (mapv #(scan-line board coords % team) line-dirs)
         (reduce max 0)
         (<= 5))))

;;;; Display

(defn- num-guides [rows]
  (mapv #(format "%2d" %) (range 1 (inc rows))))

(defn- letter-guides [columns]
  (subs " A B C D E F G H I J K L M N O P Q R S T U V W X Y Z"
        0 (* columns 2)))

(defn show-board [board]
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

(defn show-game [{:keys [player prison board] :as game}]
  (let [red (format "%2d" (:red prison))
        blue (format "%2d" (:blue prison))]
    (println)
    (println
     (str
      "Turn:"
      (case player
        :red  "\u001b[31m Red \u001b[0m"
        :blue "\u001b[34m Blue\u001b[0m")
      "     "
      "\u001b[31m " red "\u001b[0m |\u001b[34m" blue "\u001b[0m")))
  (show-board board)
  game)

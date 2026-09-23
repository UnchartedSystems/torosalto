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

(defn- populate-board [{:keys [board] :as game} cells]
  (if (empty? cells)
    game
    (let [[[y x] state] (first cells)]
      (recur
       (assoc-in game [:board y x] state)
       (rest cells)))))

(defn make-game
  ([] (make-game {}))
  ([{:keys [size player prison board]}]
   (let [size (or size 10)
         empty-board (vec (repeat size (vec (repeat size :empty))))
         game        {:size size
                      :player (or player :red)
                      :prison (or prison {:red 0 :blue 0})
                      :board empty-board}]
     (if board
       (populate-board game board)
       game))))

(make-game)

(def new-game
  (make-game))

(def small-game
  (make-game
   {:size 8}))

(def test-game
  (make-game
   {:board
    [[[3 5] :red] [[3 3] :red] [[2 6] :blue] [[2 5] :red]
     [[2 3] :red] [[1 4] :red] [[5 5] :red] [[8 8] :blue]
     [[8 7] :red] [[7 8] :red] [[9 8] :blue] [[7 8] :blue]
     [[8 9] :red] [[6 9] :red] [[5 9] :blue] [[9 9] :red]
     [[1 1] :red] [[7 0] :red] [[9 2] :red] [[5 4] :blue]
     [[5 3] :blue] [[0 8] :blue] [[6 8] :blocked] [[4 0] :blocked]]}))

(def win-game
  (make-game
   {:board
    [[[4 4] :red] [[4 3] :red] [[4 2] :red] [[4 5] :blue] [[4 6] :red]
     [[9 7] :red] [[0 8] :red] [[8 6] :red] [[7 5] :red] [[6 4] :red]
     [[0 4] :blue] [[1 3] :blue] [[2 2] :blue] [[3 1] :blue] [[4 0] :blue]]}))

;;;; Utilities

(defn v+ [a b] (mapv + a b))
(defn v* [n v] (mapv #(* n %) v))

(def letter->y
  {"A" 0 "B" 1 "C" 2 "D" 3 "E" 4 "F" 5 "G" 6 "H" 7 "I" 8 "J" 9
   "K" 10 "L" 11 "M" 12 "N" 13 "O" 14 "P" 15 "Q" 16 "R" 17
   "S" 18 "T" 19 "U" 20 "V" 21 "W" 22 "X" 23 "Y" 24 "Z" 25})

;; TODO capitalize letter
(defn interp-coords [coords]
  (let [[letter num] [(subs coords 0 1) (subs coords 1)]]
    [(get letter->y letter) (dec (parse-long num))]))

;; Will need to be changed to take in a board size
(defn wrap [[x y] size]
  [(mod x size)
   (mod y size)])

;; MARK
(defn move [coords delta size]
  (wrap (v+ coords delta) size))

;; MARK
(defn adjacent? [coords-1 coords-2 size]
  (boolean
   (some #(= coords-1 %)
         (mapv #(move coords-2 % size) adjacencies))))

;; MARK
(defn adj-stones [board coords size]
  (filterv #(stones (second %))
           (mapv #(vector % (get-in board (move coords % size)))
                 adjacencies)))

(defn get-constraint [direction]
  (cond (diagonal direction)   diagonal
        (orthogonal direction) orthogonal
        :else false))

;;;; Legality Checks

(defn legal-position? [[x y] size]
  (and (int? x) (int? y)
       (<= 0 x) (< x size)
       (<= 0 y) (< y size)))

(defn place? [{:keys [size board]} coords]
  (and (legal-position? coords size)
       (= :empty (get-in board coords))))

(defn place [{:keys [player board] :as game} coords]
  (assoc game
         :board (assoc-in board coords player)))

(defn place-open? [{:keys [size board] :as game} coords]
  (and (place? game coords)
       (empty? (adj-stones board coords size))))

(defn free? [{:keys [size player prison board] :as game} coords-1 coords-2]
  (boolean
   (and (<= 2 (get prison player))
        (not= coords-1 coords-2)
        (not (adjacent? coords-1 coords-2 size))
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
  ([{:keys [size player board]} coords direction constraint]
   (boolean
    (and (= player (get-in board coords))
         (legal-position? coords size)
         (constraint direction)
         (not (stones (get-in board (move coords (v* 2 direction) size))))
         (stones (get-in board (move coords direction size)))))))

;; Either here or hops, need to check player against coords stone
(defn hop
  ([game coords direction]
   (hop game coords direction false))
  ([{:keys [size prison board] :as game} coords direction blocked?]
   (let [hopped-over (move coords direction size)
         hopped-to (move coords (v* 2 direction) size)
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

;;;; Win Condition

(defn- scan-line [size board coords direction team]
  (->> (mapv #(v* % direction) (range -4 5))
       (mapv #(v+ coords %))
       (filterv #(legal-position? % size))
       (mapv #(get-in board %))
       (mapv #(= team %))
       (partition-by identity)
       (filterv first)
       (mapv count)
       (reduce max 0)))

;; Sloppy but works: adj-stones wraps.
;; Plenty of redundant work.
;; When will win be checked? A player can win by hopping (specially on an odd board)
(defn win? [size board coords team]
  (let [line-dirs (mapv first (filterv #(= (second %) team) (adj-stones board coords size)))]
    (->> (mapv #(scan-line board coords % team) line-dirs)
         (reduce max 0)
         (<= 5))))

;;;; Turn structure
;; Where should turn be managed and change?
;; What am I actually building? Validation for CLJ & CLJS? CLJ CLI interaction? CLJS interaction? All?

;;;; Display Backgrounds
;; Good background text colors?
;; Should I compare turns to last turns and highlight differences?
;; Iteration over all options of a turn move and highlighting them?

;;;; Turns

;; Turn instructions should be encoded as data from the website & CLI
;; Each chrome will use helpers to create interactive turn systems
;; The output from these interactive systems will be data that gets processed here.
;; Interactive turn systems should send a hash along with the initial game and turns
;; At the end after processing moves, hashes will be compared.
;; It's an API: down the road I'll add error feedback.


;;;; Display

(defn- num-guides [rows]
  (mapv #(format "%2d" %) (range 1 (inc rows))))

(defn- letter-guides [columns]
  (subs " A B C D E F G H I J K L M N O P Q R S T U V W X Y Z"
        0 (* columns 2)))

(defn show-board [board size]
  (let [nums (num-guides size)]
    (doseq [x (reverse (range size))]
      (print (get nums x))
      (doseq [y (range size)]
        (print 
         (case (get-in board [x y])
           :empty   " ·"
           :red     "\u001b[31m ◉\u001b[0m"
           :blue    "\u001b[34m ◉\u001b[0m"
           :blocked " ×")))
      (println)))
  (println " " (letter-guides size)))

(defn show-game [{:keys [size player prison board] :as game}]
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
  (show-board board size)
  game)


(ns torosalto.rules)

;; Cell States
(def cell-states #{:empty :red :blue})
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

(defn- populate-board [game cells]
  (reduce (fn [game [[x y] state]]
            (assoc-in game [:board x y] state))
          game
          cells))

;; Inputs need to be validated!
(defn make-game
  ([] (make-game {}))
  
  ([{:keys [size turn-number player blocked next-blocked prison cells]}]
   (let [size (or size 10)
         empty-board (vec (repeat size (vec (repeat size :empty))))
         game {:size size
               :turn-number (or turn-number 1)
               :blocked (or blocked nil)
               :next-blocked (or next-blocked nil)
               :version 0
               :player (or player :red)
               :prison (or prison {:red 0 :blue 0})
               :board empty-board}]
     (if cells
       (populate-board game cells)
       game))))

(def new-game
  (make-game))

(def small-game
  (make-game
   {:size 8}))

(def test-game
  (make-game
   {:prison {:red 3 :blue 1}
    :cells
    [[[3 5] :red] [[3 3] :red] [[2 6] :blue]
     [[2 5] :red] [[2 3] :red] [[1 4] :red]
     [[5 5] :red] [[8 8] :blue] [[8 7] :red]
     [[7 8] :red] [[9 8] :blue] [[7 8] :blue]
     [[8 9] :red] [[6 9] :red] [[5 9] :blue]
     [[9 9] :red] [[1 1] :red] [[7 0] :red]
     [[9 2] :red] [[5 4] :blue] [[5 3] :blue]
     [[0 8] :blue]]}))

(def win-game
  (make-game
   {:cells
    [[[4 4] :red] [[4 3] :red] [[4 2] :red] [[4 5] :blue] [[4 6] :red]
     [[9 7] :red] [[0 8] :red] [[8 6] :red] [[7 5] :red] [[6 4] :red]
     [[0 4] :blue] [[1 3] :blue] [[2 2] :blue] [[3 1] :blue] [[4 0] :blue]]}))

;;;; Utilities

(defn v+ [a b] (mapv + a b))
(defn v* [n v] (mapv #(* n %) v))


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
(defn get-adj-stones [{:keys [size board]} coords]
  (filterv #(stones (second %))
           (mapv #(vector % (get-in board (move coords % size)))
                 adjacencies)))

(defn get-constraint [direction]
  (cond (diagonal direction)   diagonal
        (orthogonal direction) orthogonal
        :else false))

;;;; Legality Checks

(defn- throw-error! [e]
  (when e
    (throw
     (ex-info (str "Illegal Game Action: " (:error e))
               {::error e}))))

(defn- legal-position? [coords size]
  (and (= 2 (count coords))
       (let [[x y] coords]
         (and (int? x) (int? y)
              (<= 0 x) (< x size)
              (<= 0 y) (< y size)))))

(defn position-error [coords size]
  (or (when-not (vector? coords)
        {:error :position/invalid
         :details {:coords coords}})
      (when-not (= 2 (count coords))
        {:error :position/invalid
         :details {:coords coords}})
      (when-not (legal-position? coords size)
        {:error :position/invalid
         :details {:coords coords}})))

(defn place-error [{:keys [size blocked board]} coords]
  (or (position-error coords size)
      
      (when (= blocked coords)
        {:error :place/blocked-place
         :details {:coords coords}})
      
      (let [cell (get-in board coords)]
        (when-not (= :empty cell)
          {:error :place/full-place
           :details {:coords coords
                     :stone cell}}))))

(defn open-place-error [{:keys [size board] :as game} coords]
  (or (place-error game coords)
      
      (let [adj-stones (get-adj-stones game coords)]
        (when-not (empty? adj-stones)
          {:error :open-place/not-open
           :details {:coords coords
                     :stones adj-stones}}))))


(defn free-error [{:keys [size player prison board] :as game} coords-1 coords-2]
  (or (open-place-error game coords-1)
      (open-place-error game coords-2)

      (when-not (<= 2 (get prison player))
        {:error :free/lacking-stones
         :details {:prison prison
                   :player player}})
      
      (when (= coords-1 coords-2)
        {:error :free/coords-close
         :details {:coords-1 coords-1
                   :coords-2 coords-2}})
      
      (when (adjacent? coords-1 coords-2 size)
        {:error :free/coords-close :details
         {:coords-1 coords-1
          :coords-2 coords-2}})))

(defn hop-error
  ([game coords direction]
   (hop-error game coords direction adjacencies))
  
  ([{:keys [size player board]} coords direction constraint]
   (or (position-error coords size)
       
       ;; the hop cell a stone should be controlled by the player?
       (let [cell (get-in board coords)]
         (when (not= cell player)
           {:error :hop/invalid-start
            :details {:player player
                      :cell cell}}))
       
       ;; the hop direction should be adjacent / diagonal / orthogonal
       (when-not (constraint direction)
         {:error :hop/broken-constraint
          :details {:direction direction
                    :constraint constraint}})
       
       ;; the hopped-over cell should be a stone
       (let [hopped-coords (move coords direction size)
             hopped-cell (get-in board hopped-coords)]
         (when (= hopped-cell :empty)
           {:error :hop/hopped-cell-empty
            :details {:start-coords coords
                      :direction direction
                      :hopped-coords hopped-coords
                      :hopped-cell hopped-cell}}))
       
       ;; is the hop landing cell should be empty
       (let [dest-coords (move coords (v* 2 direction) size)
             dest-cell (get-in board dest-coords)]
         (when (stones dest-cell)
           {:error :hop/destination-is-full
            :details {:start-coords coords
                      :direction direction
                      :dest-coords dest-coords
                      :dest-cell dest-cell}})))))

;;;; Cell Masks

(defn cell-mask [f [b1 b2] {:keys [size] :as game}]
  (let [indices (range size)]
    {:size size
     :board
     (mapv
      (fn [x] (mapv
               (fn [y] (if (f game [x y]) b1 b2))
               indices))
      indices)}))

(defn any-cell? [f {:keys [size] :as game}]
  (boolean
   (some #(f game %)
         (for [y (range size)
               x (range size)]
           [x y]))))

(def open-place-mask
  (partial cell-mask #(not (open-place-error %1 %2)) [:blue :empty]))

(def any-open-place?
  (partial any-cell? #(not (open-place-error %1 %2))))

(def place-mask
  (partial cell-mask #(not (place-error %1 %2)) [:blue :empty]))

(def any-place?
  (partial any-cell? #(not (place-error %1 %2))))

(def hop-mask
  (partial cell-mask
           (fn [g c] (some nil? (mapv #(hop-error g c %) adjacencies)))
           [:blue :empty]))

(def any-hop?
  (partial any-cell?
           (fn [g c] ((some nil? (mapv #(hop-error g c %) adjacencies))))))

;;;; Win Condition

;; TODO: refactor to end evaluation on first evaluated win
(defn scan-line [{:keys [player size board]} coords direction]
  (->> (mapv #(v* % direction) (range -4 5))
       (mapv #(v+ coords %))
       (filterv #(legal-position? % size))
       (mapv #(get-in board %))
       (mapv #(= player %))
       (partition-by identity)
       (filterv first)
       (mapv count)
       (reduce max 0)))

;; Sloppy but works: get-adj-stones wraps.
;; Plenty of redundant work.
;; When will win be checked? A player can win by hopping (specially on an odd board)
(defn win? [{:keys [player size board] :as game} coords]
  (let [line-dirs (mapv first (filterv #(= (second %) player) (get-adj-stones game coords)))]
    (->> (mapv #(scan-line game coords %) line-dirs)
         (reduce max 0)
         (<= 5))))

(defn win [{:keys [player] :as game}]
  (assoc game :winner player))

;;;; Move Actions

(defn place [{:keys [player board] :as game} coords]
  (assoc game :board (assoc-in board coords player)))

(defn free [{:keys [player prison board] :as game} coords-1 coords-2]
  (-> game
      (update-in [:prison player] - 2)
      (place coords-1)
      (place coords-2)))

;; Either here or hops, need to check player against coords stone
(defn- hop
  ([game coords direction]
   (hop game coords direction false))
  
  ([{:keys [size prison board] :as game} coords direction blocked?]
   (let [hopped-coords (move coords direction size)
         landing-coords (move coords (v* 2 direction) size)
         attacker (get-in board coords)
         captured (get-in board hopped-coords)]
     
     {:coords landing-coords
      :game (assoc game
             :prison (update prison captured inc)
             :next-blocked (if blocked? hopped-coords nil) 
             :board (-> board
                        (assoc-in coords :empty)
                        (assoc-in hopped-coords :empty)
                        (assoc-in landing-coords attacker)))})))


;; TODO: directions could be something other than seq
(defn evaluate-hops [game coords directions]
  (let [constraint (get-constraint (first directions))
        blocked?   (= 1 (count directions))]
    (throw-error!
     (or (when (empty? directions)
           {:error :hops/no-directions
            :details {:directions directions}})
         
      (when-not constraint
           {:error :hops/no-constraint
            :details {:directions directions}})))
    
    (loop [{:keys [game coords]} {:game game :coords coords}
           remaining directions]
      (or
       (when (win? game coords)
         (assoc game :winner (:player game)))
       (when (empty? remaining) game)
       (throw-error! (hop-error game coords (first remaining) constraint))
       (recur (hop game coords (first remaining) blocked?)
              (rest remaining))))))


(defn place-action [game {:keys [coords]}]
  (throw-error! (place-error game coords))
  (let [game (place game coords)]
    (if (win? game coords)
      (assoc game :winner (:player game))
      game)))

(defn free-action [game {:keys [coords-1 coords-2]}]
  (throw-error! (free-error game coords-1 coords-2))
  (free game coords-1 coords-2))

(defn hop-action [game {:keys [coords directions place-coords]}]
  (throw-error! (position-error coords (:size game)))
  (let [{:keys [winner] :as game}
        (evaluate-hops game coords directions)]
    (or
     (when winner game)
     (when-not place-coords game)
     
     (throw-error!
      (or (open-place-error game place-coords)
          (when-not (<= 2 (count directions))
            {:error :hop-action/invalid-place
             :details {:directions directions
                       :place-coords place-coords}})))
     
     (place game place-coords))))

(def next-player
  {:red :blue
   :blue :red})

(defn next-turn [{:keys [player turn-number blocked next-blocked] :as game}]
  (assoc game
         :turn-number (inc turn-number)
         :player (player next-player)
         :blocked next-blocked
         :next-blocked nil))

;; TODO: process draws!
(defn- process-turn [game {:keys [action details] :as turn}]
  (throw-error!
   (when (:winner game)
     {:error :turn/game-already-ended
      :details {:winner (:winner game)}}))
  
  (let [{:keys [winner] :as game}
        (case action
          :place (place-action game details)
          :free (free-action game details)
          :hop (hop-action game details)
          (throw-error!
           {:error :turn/invalid-action
            :details {:action action}}))]
    
    (if winner
       (win game)
       (next-turn game))))

;;;; Differentiate: Server vs Local
;; Server uses accounts with salted auth
;; Server pulls its own copy of the game from SQLite
;; Local passes the game with the action message
;; Test the game player against the player of the user

(defn- get-game [id] (make-game))

(defn- save-game [game])

;; Validates Message Inputs
(defn- message-error [mssg] nil)

(defn- process-message [{:keys [persistent? id game turn] :as mssg}]
  (throw-error! (message-error mssg))
  (let [game (if persistent? (get-game id) game)
        game (process-turn game turn)]
    (if persistent?
      {:id id
       :game-hash (hash (save-game game))}
      {:game game
       :game-hash (hash game)})))

(defn evaluate [mssg]
  (try
    (process-message mssg)
    (catch clojure.lang.ExceptionInfo e
      (if-let [error (::error (ex-data e))]
        (do (println error) error)
        (throw e)))))

;;;; Display

(format "%3d" 99)

(defn- num-guides [rows]
  (mapv #(format "%2d" %) (range 1 (inc rows))))

(defn- letter-guides [columns]
  (subs " A B C D E F G H I J K L M N O P Q R S T U V W X Y Z"
        0 (* columns 2)))

(defn red-txt [s]
  (str "\u001b[31m" s "\u001b[0m"))

(defn blue-txt [s]
  (str "\u001b[34m" s "\u001b[0m"))

;; Add this-block to rendering
(defn show-board [{:keys [size blocked board]}]
  (let [nums (num-guides size)]
    (doseq [y (reverse (range size))]
      (print (get nums y))
      (doseq [x (range size)]
        (let [cell (get-in board [x y])
              cell (if (and (= cell :empty)
                            (= blocked [x y]))
                     :blocked cell)]
          (print 
           (case cell
             :empty   " ·"
             :blocked " ×" 
             :red     (red-txt " ◉")
             :blue    (blue-txt " ◉")
             " ?"))))
      (println)))
  (println " " (letter-guides size)))

(defn show-game [{:keys [turn-number player prison] :as game}]
  (let [red-prisoners  (format "%2d" (:red prison))
        blue-prisoners (format "%2d" (:blue prison))
        player-name    (case player :red "Red" :blue "Blue")
        color-fn       (case player :red red-txt :blue blue-txt)
        prefix         (str turn-number ": " player-name)
        padding        (apply str (repeat (max 0 (- 16 (count prefix))) \space))]
    (println)
    (println
     (str turn-number ": " (color-fn player-name) padding
          (red-txt red-prisoners) " |" (blue-txt blue-prisoners))))
  (show-board game)
  game)


(ns torosalto.display
  (:require [clojure.string :as string]))

(format "%3d" 99)

(defn- num-guides [rows]
  (mapv #(format "%2d" %) (range 1 (inc rows))))

(defn- letter-guides [columns]
  (subs " A B C D E F G H I J K L M N O P Q R S T U V W X Y Z"
        0 (* columns 2)))

;; For Humans

(defn red-txt [s]
  (str "\u001b[31m" s "\u001b[0m"))

(defn blue-txt [s]
  (str "\u001b[34m" s "\u001b[0m"))

;; Add this-block to rendering
(defn show-board [{:keys [size blocked board] :as game} highlight-f]
  (let [nums (num-guides size)]
    (doseq [y (reverse (range size))]
      (print (get nums y))
      (doseq [x (range size)]
        (let [cell (get-in board [x y])
              highlight? (highlight-f game [x y])
              cell (if (and (= cell :empty)
                            (= blocked [x y]))
                     :blocked cell)]
          (print
           (-> (case cell
                 :empty   " ·"
                 :blocked " ×" 
                 :red     (red-txt " ◉")
                 :blue    (blue-txt " ◉")
                 " ?")
               (#(if highlight?  (str "\u001b[48;2;40;44;52m" % "\u001b[49m") %))))))
      (println)))
  (println " " (letter-guides size)))

(defn show-game
  ([game]
   (show-game game (constantly false)))
  
  ([{:keys [turn-number player prison] :as game} highlight-f]
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
   (show-board game highlight-f)
   game))

;; For LLMs

(defn llm-show-game [{:keys [board turn-number player size prison blocked] :as game}]
  (let [player-name (if (= :red player) "Red" "Blue")
        nums (vec (range 1 (inc size)))]
    (println
     (str
      "Turn " turn-number ": " player-name "\n"
      "Prison: " "Red: " (:red prison) " | " "Blue: " (:blue prison) "\n"
      "Game Board: " "\n"

      (apply str
       (for [y (reverse (range size))]
         (str " "
          (apply str
           (for [x (reverse (range size))]
             (let [cell (get-in board [x y])
                   cell (if (and (= cell :empty)
                                 (= blocked [x y]))
                          :blocked cell)]
               (case cell
                 :empty   "· "
                 :blocked "× " 
                 :red     "R "
                 :blue    "B "
                 "? "))))
          " " (get nums y) "\n")))
      (letter-guides size)))))



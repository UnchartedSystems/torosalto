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

(defn show-board [{:keys [size blocked board] :as game}]
  (str
   (string/join
    (let [nums (num-guides size)]
      (for [y (reverse (range size))]
        (str
         (get nums y)
         (string/join
          (for [x (range size)]
            (let [cell (get-in board [x y])
                  cell (if (and (= cell :empty)
                                (= blocked [x y]))
                         :blocked cell)]
              (case cell
                :empty   " ·"
                :blocked " ×" 
                :red     (red-txt " ◉")
                :blue    (blue-txt " ◉")
                " ?"))))
         "\n"))))
   "  " (letter-guides size) "\n"))

(defn show-game [{:keys [turn-number player prison] :as game}]
  (let [red-prisoners  (format "%2d" (:red prison))
        blue-prisoners (format "%2d" (:blue prison))
        player-name    (case player :red "Red" :blue "Blue")
        color-fn       (case player :red red-txt :blue blue-txt)
        prefix         (str turn-number ": " player-name)
        padding        (apply str (repeat (max 0 (- 16 (count prefix))) \space))]
    (str "\n"
         turn-number ": " (color-fn player-name) padding
         (red-txt red-prisoners) " |" (blue-txt blue-prisoners)
         "\n" (show-board game))))


;; For LLMs

(defn llm-show-game [{:keys [board turn-number player size prison blocked] :as game}]
  (let [player-name (if (= :red player) "Red" "Blue")
        nums (vec (range 1 (inc size)))]
    (str
     "Turn " turn-number ": " player-name "\n"
    "Prison: " "Red: " (:red prison) " | " "Blue: " (:blue prison) "\n"
    "Game Board: " "\n"
    
     (string/join
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
     (letter-guides size))))



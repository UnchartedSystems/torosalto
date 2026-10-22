(ns torosalto.match
  (:require [torosalto.rules :as rules]
            [torosalto.notation :as notation]
            [torosalto.display :as display]))

(def formats
  {:show
   (fn [gs ts]
     (println (display/show-game (peek gs)))   
     (when (peek ts) (println (notation/format-turn (peek ts)))))
   :notation
   (fn [gs ts]
     (when (peek ts) (println (notation/format-turn (peek ts))))
     (println (notation/format-game (peek gs))))
   :raw
   (fn [gs ts]
     (when (peek ts) (println (peek ts)))
     (println (peek gs)))
   :llm-show
   (fn [gs ts]
     (println (display/llm-show-game (peek gs)))
     (when (peek ts) (println (notation/format-turn (peek ts)))))})

(def models
  {:human
   (fn [input]
     (println input)
     (read-line))})

(defn play-turn [games turns {:keys [model format] :as active-player} inactive-player]
  (let [action (model (format games turns))
        turn (notation/parse-turn action)
        next-game (rules/process-turn* (peek games) turn)]
    (recur (conj games next-game) (conj turns turn) inactive-player active-player)))

(defn prep-player [{:keys [model view]}]
  {:format (view formats)
   :model (model models)})

(defn start [game p1 p2]
  (let [p1 (prep-player p1)
        p2 (prep-player p2)]
    (play-turn [game] [] p1 p2)))



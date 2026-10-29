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
  {:cli
   (fn [input]
     (println input)
     (read-line))})

(defn play-turn [states turns {:keys [model format] :as active-player} inactive-player]
  (let [action (model (format states turns))
        turn (notation/parse-turn action)
        {:keys [result] :as next-state} (rules/process-turn* (peek states) turn)]
    (if result
      
      (recur (conj games next-state) (conj turns turn) inactive-player active-player))))

(defn prep-player [{:keys [model view]}]
  {:format (view formats)
   :model (model models)})

(defn start
  ([initial-state p1 p2]
   (start initial-state p1 p2 {}))
  
  ([initial-state p1 p2 {:keys [log]}]
   (let [p1 (prep-player p1)
         p2 (prep-player p2)
         game (play-turn [initial-state] [] p1 p2)]
     (when (fn? log)
       (log game p1 p2)))))



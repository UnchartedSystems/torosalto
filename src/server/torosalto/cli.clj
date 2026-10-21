(ns torosalto.cli
  (:require [torosalto.rules :as rules]
            [torosalto.notation :as notation]
            [torosalto.display :as display]))

(def representations
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

(defn play [game representation]
  (println representation)
  (let [input (read-line)
        turn (notation/parse-turn input)
        next-game (rules/process-turn* game turn)]
    (println (notation/format-game next-game))
    [turn next-game]))

(defn match [games turns {:keys [model view] :as p1} p2]
  (let [representation ((view representations) games turns)
        [turn game]
        (case model
          :human (play (peek games) representation))]
    (recur (conj games game) (conj turns turn) p2 p1)))

(defn start! [{:keys [mode]}]
  (match [(rules/make-game)] []
         {:model :human
          :view :show}
         {:model :human
          :view :llm-show}))

(start! :hello)

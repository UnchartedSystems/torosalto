(ns torosalto.cli
  (:require [torosalto.rules :as rules]
            [torosalto.notation :as notation]
            [torosalto.display :as display]))

(def representations
  {:all
   (fn [gs ts]
     (display/show-game (peek gs))
     (when (peek ts) (println (notation/format-turn (peek ts))))
     (println (notation/format-game (peek gs))))
   :notation
   (fn [gs ts]
     (when (peek ts) (println (notation/format-turn (peek ts))))
     (println (notation/format-game (peek gs))))
   :llm-show
   (fn [gs ts]
     (display/llm-show-game (peek gs))
     (when (peek ts) (println (notation/format-turn (peek ts)))))})

(defn play [game]
  (let [turn (notation/parse-turn (read-line))
        next-game (rules/process-turn* game turn)]
    (println (notation/format-game next-game))
    [turn next-game]))

(defn match [games turns {:keys [model view] :as p1} p2]
  (println ((view representations) games turns))
  (let [[turn game] (case model :human (play (peek games)))]
    (recur (conj games game) (conj turns turn) p2 p1)))

(defn start! [{:keys [mode]}]
  (match [(rules/make-game)] []
         {:model :human
          :view :all}
         {:model :human
          :view :notation}))

(start! :hello)

(ns torosalto.cli
  (:require [torosalto.rules :as rules]
            [torosalto.match :as match]))

(defn start! []
  (match/start (rules/make-game) 
               {:model :human
                :view :show}
               {:model :human
                :view :show}))

(start!)

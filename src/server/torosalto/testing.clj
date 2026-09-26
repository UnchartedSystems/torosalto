(ns torosalto.testing
  (:require [torosalto.rules :as rules]))


(def free-data
  {:local? true
   :game test-game
   :move :free
   :player :red
   :details {:coords-1 [5 1]
             :coords-2 [7 5]}
   :version 0})

(def place-data
  {:local? true
   :game test-game
   :move :place
   :player :red
   :details {:coords [2 4]}
   :version 0})

(def hop-data
  {:local? true
   :game test-game
   :move :hop
   :player :red
   :details {:coords [9 9]
             :directions [[0 -1] [-1 0] [0 1]]
             :place-coords [2 8]}
   :version 0})

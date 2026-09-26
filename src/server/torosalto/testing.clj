(ns torosalto.testing
  (:require [torosalto.rules :as rules]))

;;;; Create a test system for easy tests
;; Enable an easy notation for defining boards (and maybe moves)
;; Create the machinery to simulate full games using the rules in here.
;; Create multiple test moves and test games. Compare the output to a hash of a desired output.
;; Create a simulator and simulate full games.

;;;; Then we're off to the races! Go make a simple CLI, and then a website!

(def free-data
  {:version 0
   :local? true
   :game test-game
   :move :free
   :details {:coords-1 [5 1]
             :coords-2 [7 5]}})

(def place-data
  {:version 0
   :local? true
   :game test-game
   :move :place
   :details {:coords [2 4]}})

(def hop-data
  {:version 0
   :local? true
   :game test-game
   :move :hop
   :details {:coords [9 9]
             :directions [[0 -1] [-1 0] [0 1]]
             :place-coords [2 8]}})




()

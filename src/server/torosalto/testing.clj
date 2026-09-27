(ns torosalto.testing
  (:require [torosalto.rules :as rules]))

;;;; Create a test system for easy tests
;; Enable an easy notation for defining boards (and maybe moves)
;; Create the machinery to simulate full games using the rules in here.
;; Create multiple test moves and test games. Compare the output to a hash of a desired output.
;; Create a simulator and simulate full games.

;;;; Then we're off to the races! Go make a simple CLI, and then a website!

(def test-game
  (rules/make-game
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

(rules/show-game  test-game)
(rules/show-game (:game (rules/evaluate place-data)))
(rules/show-game (:game (rules/evaluate free-data)))
(rules/show-game (:game (rules/evaluate hop-data)))

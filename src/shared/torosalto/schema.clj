(ns  torosalto.schema
  (:require [malli.core :as m]
            [malli.error :as me]
            [malli.generator :as mg]))

;; Cell States
(def cell-states #{:empty :red :blue})
(def stones #{:red :blue})

;; Movement Constraints
(def adjacencies
  #{[1 -1]  [1 0]  [1 1]
    [0 -1]         [0 1]
    [-1 -1] [-1 0] [-1 1]})

(def diagonal
  #{[1 -1]  [1 1]
    [-1 -1] [-1 1]})

(def orthogonal
  #{[1 0] [0 1] [-1 0] [0 -1]})

(def Cell
  (into [:enum] cell-states))

(def Stone
  (into [:enum] stones))

(def Adjacency
  (into [:enum] adjacencies))

(def Orthogonal
  (into [:enum] orthogonal))

(def Diagonal
  (into [:enum] diagonal))

(def Coords
  [:tuple
   [:int {:min 0}]
   [:int {:min 0}]])

(def Place
  [:map
   [:action [:= :place]]
   [:details [:map [:coords Coords]]]])

(comment
  (m/validate
   Place
   {:action :place
    :details {:coords [0 7]}}))

(def Free
  [:map
   [:action [:= :free]]
   [:details
    [:map
     [:coords-1 Coords]
     [:coords-2 Coords]]]])

(comment
  (m/validate
   Free
   {:action :free
    :details {:coords-1 [7 1]
              :coords-2 [1 6]}}))

(def Hop
  [:map
   [:action [:= :hop]]
   [:details
    [:map
     [:coords Coords]
     [:place-coords Coords]
     [:directions
      [:or
       [:vector {:min 1} Orthogonal]
       [:vector {:min 1} Diagonal]]]]]])

(comment
  (m/validate
   Hop
   {:action :hop
    :details {:coords [3 3]
              :place-coords [9 0]
              :directions [[-1 1] [0 -1]]}}))


(defn board-schema [n]
  [:vector {:min n :max n}
   [:vector {:min n :max n} Cell]])

(defn bounded-coords [n]
  [:tuple
   [:int {:min 0 :max (dec n)}]
   [:int {:min 0 :max (dec n)}]])

(defn game-schema-for-size [n]
  (let [coords (bounded-coords n)]
    [:map
     [:size [:= n]]
     [:turn-number [:int {:min 1}]]
     [:blocked [:or coords [:enum false nil]]]
     [:next-blocked [:or coords [:enum false nil]]]
     [:version [:= 0]]
     [:player Stone]
     [:prison
      [:map
       [:red [:int {:min 0}]]
       [:blue [:int {:min 0}]]]]
     [:board (board-schema n)]]))

(def game-sizes [8 10])

(def Game
  (into [:multi {:dispatch :size}]
        (mapv (fn [n] [n (game-schema-for-size n)])
              game-sizes)))

(comment
  (->
   (m/explain
    Game
    {:size 10
     :turn-number 24
     :blocked [1 4]
     :next-blocked [4 0]
     :version 0
     :player :blue
     :prison {:red 3 :blue 0}
     :board
     [[:empty :empty :empty :red :empty :empty :blue :empty :empty :empty]
      [:empty :empty :empty :empty :empty :empty :empty :empty :empty :empty]
      [:empty :empty :empty :empty :empty :empty :empty :empty :red :red]
      [:empty :empty :empty :blue :blue :blue :empty :red :empty :empty]
      [:empty :empty :empty :empty :empty :blue :empty :empty :empty :red]
      [:empty :blue :empty :empty :empty :empty :empty :empty :empty :empty]
      [:empty :empty :empty :empty :empty :red :blue :empty :empty :empty]
      [:empty :empty :empty :empty :empty :empty :empty :empty :empty :empty]
      [:empty :empty :empty :empty :blue :empty :empty :empty :empty :empty]
      [:empty :empty :empty :empty :empty :empty :empty :empty :empty :empty]]})
   (me/humanize)))


(comment
  {:game game :turn place-action})

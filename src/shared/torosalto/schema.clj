(ns  torosalto.schema
  (:require [malli.core :as m]
            [malli.error :as me]
            [malli.generator :as mg]))

(defn throw-violation! [schema instance]
  (if-let [violation (m/explain schema instance)]
    (throw
     (ex-info (str "Schema Violation. ")
              {::error (me/humanize violation)}))))

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

(def Adjacencies
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

;; constraint checking is done seperately than schema checking.
(def Directions
  [:or
   [:vector {:min 1} Diagonal]
   [:vector {:min 1} Orthogonal]])

(def Hop
  [:map
   [:action [:= :hop]]
   [:details
    [:map
     [:coords Coords]
     [:place-coords Coords]
     [:directions
      [:vector {:min 1} Adjacencies]]]]])

(def Turn
  [:multi {:dispatch :action}
   [:place Place]
   [:free Free]
   [:hop Hop]])

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

(def Message
  [:map
   [:turn Turn]
   [:game {:optional true} :map]
   [:persistent? {:optional true} :boolean]
   [:id {:optional true} :in]])

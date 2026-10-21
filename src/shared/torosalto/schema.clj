(ns  torosalto.schema
  (:require [malli.core :as m]
            [malli.error :as me]
            [malli.generator :as mg]
            [malli.util :as mu]))

(defn throw-violation! [schema instance]
  (if-let [violation (m/explain schema instance)]
    (throw
     (let [explanation (me/humanize violation)]
       (ex-info (str "Schema Violation. \n" explanation)
                {::error {:error :schema/violation
                          :details explanation}})))))

;; Safe Max Int
(def safe-max 10000000000)

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
   [:int {:min 0 :max 25}]
   [:int {:min 0 :max 25}]])

(def Place
  [:map
   [:action [:= :place]]
   [:details [:map [:coords Coords]]]])

(def Free
  [:map
   [:action [:= :free]]
   [:details
    [:map
     [:coords-1 Coords]
     [:coords-2 Coords]]]])

;; Not included in Hop, used for legality error checking.
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
     [:place-coords {:optional true} [:or Coords [:enum false nil]]]
     [:directions
      [:vector {:min 1} Adjacencies]]]]])

(def Resign
  [:map
   [:action [:= :resign]]])

(def Turn
  [:multi {:dispatch :action}
   [:place Place]
   [:free Free]
   [:hop Hop]
   [:resign Resign]])

(defn board-schema [n]
  [:vector {:min n :max n}
   [:vector {:min n :max n} Cell]])

(defn bounded-coords [n]
  [:tuple
   [:int {:min 0 :max (dec n)}]
   [:int {:min 0 :max (dec n)}]])

(def Result
  [:or
   [:= :draw]
   [:map {:closed true} [:winner Stone]]
   [:map {:closed true} [:resigned Stone]]])

(defn game-schema-for-size [n]
  (let [coords (bounded-coords n)]
    [:map
     [:size [:= n]]
     [:turn-number [:int {:min 1 :max safe-max}]]
     [:blocked [:or coords [:enum false nil]]]
     [:next-blocked {:optional true} :nil]
     [:version [:= 0]]
     [:result {:optional true} [:maybe Result]]
     [:player Stone]
     [:prison
      [:map
       [:red [:int {:min 0 :max safe-max}]]
       [:blue [:int {:min 0 :max safe-max}]]]]
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
   [:id {:optional true} :int]])


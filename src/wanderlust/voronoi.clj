(ns wanderlust.voronoi
  (:import (be.humphreys.simplevoronoi Voronoi)))

(defn edge+sites
  [e]
  {:edge [[(.-x1 e) (.-y1 e)] [(.-x2 e) (.-y2 e)]]
   :sites [(.-site1 e) (.-site2 e)]})


(defn generate-diagram
  "`pts` - sequence of sites [[x1 y1] [x2 y2]]
  `boundary` - boundary rect [[xmin ymin] [xmax ymax]]
  `min-dist` - minimal distance between sites`"
  [pts boundary min-dist]
  (let [v (Voronoi. min-dist)
        xs (double-array (count pts) (map first pts))
        ys (double-array (count pts) (map second pts))
        [[minx miny] [maxx maxy]] boundary]
    (map edge+sites
         (.generateVoronoi v xs ys minx maxx miny maxy))))

(defn edges [points diagram]
  (map
   (fn [x]
     (let [f (fn [ss] (map #(nth points %) ss))]
       (update x :sites f)))
   diagram))

(defn cells [points diagram]
  (let [edge+sets (map
                   #(update % :sites set)
                   diagram)]
    (for [i (range (count points))]
      {:center (nth points i)
       :border (->> edge+sets
                   (filter
                    #((:sites %) i))
                   (mapcat :edge)
                   )})))

(comment

  
  
  (rand)
  (def pts (for [x (range 10 99 5)
               y (range 10 49 5)]
           [(+ x (* 5 (rand)))
            (+ y (* 5 (rand)))]))
(def bound [[0 0] [1000 500]])

  (.getSites (Voronoi. 1))
  (time
   (edges pts (generate-diagram pts bound 1))

   )
  (require '[dali.io :as io])
  (io/render-svg
   [:dali/page
    (into [:g]
          (map
           (fn [e]
             (into [:line {:stroke :black
                           :stroke-width 1}] (:cs e)))
           (generate-diagram pts bound)))]
   "test2.svg")
  )

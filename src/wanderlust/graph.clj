(ns wanderlust.graph
  (:require
   [wanderlust.geometry :as geometry]
   [loom.graph :as graph]
   [loom.alg :as alg]))

(graph/edges (alg/prim-mst (graph/weighted-graph
                            {:a {:b 1 :c 1}
                             :b {:d 1}
                             :c {:d 1 :e 1 :f 1}
                             :d {:f 1}
                             :f {:e 1}})))


(defn pathways-location-graph [pathways]
  (apply graph/weighted-graph
         (map #(vec (:locations %)
                    (:weight %)) pathways)))

(defn pathways-coord-graph [pathways]
  )


(defn mst-graph [coords]
  (->> coords
       (map #(conj % (* (geometry/line-length %) (inc (rand-int 5)))))
       vec
       (apply graph/weighted-graph)
       (alg/prim-mst)
       graph/edges))

(defn pathways-tree [pathways]
  (let [g (pathways-graph pathways)]
    (graph/edges (alg/prim-mst g))))


(comment
  (graph/weighted-graph
      [:a :b 2] [:b :c 3] [:a :c 4])
  )


(ns wanderlust.voronoi
  (:import (be.humphreys.simplevoronoi Voronoi)))

(defn- edge+sites
  [pts e]
  {:edge [[(.-x1 e) (.-y1 e)] [(.-x2 e) (.-y2 e)]]
   :sites [(nth pts (.-site1 e))
           (nth pts (.-site2 e))]})


(defn generate-diagram
  "`pts` - sequence of sites [[x1 y1] [x2 y2]]
  `boundary` - boundary rect [[xmin ymin] [xmax ymax]]
  `min-dist` - minimal distance between sites`"
  [pts boundary min-dist]
  (let [v (Voronoi. min-dist)
        xs (double-array (count pts) (map first pts))
        ys (double-array (count pts) (map second pts))
        [[minx miny] [maxx maxy]] boundary
        ]
    (map (partial edge+sites pts)
         (.generateVoronoi v xs ys minx maxx miny maxy))))


(defn mesh [diagram]
  (mapcat
   (fn [{:keys [edge sites]}]
     (let [[p1 p2] sites]
       [(conj edge p1)
        (conj edge p2)]))
   diagram))



(comment
(let [pts [[10 10] [20 20] [30 10] [20 5]]
        dia (generate-diagram
             pts
             [[0 0] [40 40]] 1)]
  (mesh dia))
  )

(comment
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
         :border (distinct
                  (->> edge+sets
                       (filter
                        #((:sites %) i))
                       (mapcat :edge)
                       ))})))

  (defn mesh [cells]
    (mapcat
     (fn [{:keys [center border]}]
       (let [edges (partition 2 1 border)]
         (map
          (fn [x] (conj x center))
          edges)))
     cells))


  (nth [:a :b :c] 0))

(comment
  (def colors
    [:aliceblue :antiquewhite :aqua :aquamarine :azure :beige :bisque :black :blanchedalmond :blue :blueviolet :brown :burlywood :cadetblue :chartreuse :chocolate :coral :cornflowerblue :cornsilk :crimson :cyanHex
     :darkblue :darkcyan :darkgoldenrod :darkgray :darkgreen :darkgrey :darkkhaki :darkmagenta :darkolivegreen :darkorange :darkorchid :darkred :darksalmon :darkseagreen :darkslateblue :darkslategray :darkslategrey :darkturquoise :darkviolet :deeppink :deepskyblue :dimgray :dimgrey :dodgerblue :firebrick :floralwhite :forestgreen :fuchsia :gainsboro :ghostwhite :gold :goldenrod
     :gray :green :greenyellow :grey :honeydew :hotpink :indianred :indigo :ivory :khaki :lavender :lavenderblush :lawngreen :lemonchiffon :lightblue :lightcoral :lightcyan :lightgoldenrodyellow :lightgray :lightgreen :lightgrey :lightpink :lightsalmon :lightseagreen :lightskyblue :lightslategray
     :lightsteelblue :lightyellow :lime :limegreen :linen :magentaia :maroon :mediumaquamarine :mediumblue :mediumorchid :mediumpurple :mediumseagreen :mediumslateblue :mediumspringgreen :mediumturquoise :mediumvioletred :midnightblue :mintcream :mistyrose :moccasin :navajowhite :navy :oldlace :olive
     :olivedrab :orange :orangered :orchid :palegoldenrod :palegreen :paleturquoise :palevioletred :papayawhip :peachpuff :peru :pink :plum :powderblue :purpled :red :rosybrown :royalblue :saddlebrown :salmon :sandybrown :seagreen :seashell :sienna :silverd :skyblue :slateblue :slategray :slategrey :snow :springgreen :steelblue :tan :teald :thistle :tomato :turquoise :violet :wheat :white :whitesmoke :yellow :yellowgreen])
  
  (let [
        dia (generate-diagram
             pts
             [[0 0] [40 40]] 1)
        mesh (mesh dia)
        ]
    (println pts)
    (clojure.pprint/pprint dia)
    (clojure.pprint/pprint mesh)
    (require '[dali.io :as io])
   
  (io/render-svg
   [:dali/page
    (into [:g]
          (map
           (fn [e]
             (into [:polygon
                    {:stroke :black
                     :fill (rand-nth colors)
                     :stroke-width 0.5}
                    ] e))
           mesh))]
   "test2.svg"))
  
  ;; cells not working properly
  (let [pts (for [x (range 10 99 5)
               y (range 10 49 5)]
           [(+ x (* 5 (rand)))
            (+ y (* 5 (rand)))])
        bound [[0 0] [1000 500]]]
    (clojure.pprint/pprint (first (cells pts (generate-diagram pts bound 1)))))
  
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

(ns wanderlust.svg-draft
  (:require
   [wanderlust.geometry :as geometry]
   [wanderlust.graph :as graph]
   [dali.io :as io]
   [dali.syntax :as s]
   [clj-xpath.core :refer :all]))


(def ^:private ids (atom {}))

(defn reset-ids! []
  (reset! ids {}))
(defn gen-id!
  ([prefix]
    (gen-id! prefix "-"))
  ([prefix div]
   (format "%s%s%05d"
           prefix div
           (if (get @ids prefix)
             (get (swap! ids update prefix inc) prefix)
             (get (swap! ids assoc prefix 0) prefix)))))


(defn locations-grid [w h s r]
  (for [x (range s w s)
        y (range s h s)]
    (let [opx (rand-nth [+ -])
          opy (rand-nth [+ -])
          rx (rand-int r)
          ry (rand-int r)]
      [(opx x rx) (opy y ry)])))


(defn generate-draft
  ([] (generate-draft {}))
  ([{:keys [name desc width height location-step location-deviation]
     :or   {name "Terra Incognita"
            desc "HC SVNT DRACONES"
            width  1000
            height 500
            location-step 50
            location-deviation 35}}]
   (reset-ids!)
   (let [locations (locations-grid width height location-step location-deviation) ]
     [:dali/page {:width width :height height}
      (s/css "circle.location {stroke: yellow; fill: red;}")
      #_(into [:g {:id "terrain" :inkscape:groupmode "layer" :inkscape:label "terrain"}]
              (voronoi-lines (locations-grid 1000 500 20 18)))
      [:g {:id "terrain shapes" :inkscape:groupmode "layer" :inkscape:label "terrain-shapes"}
       [:rect {:id "primal-ocean" :stroke :midnightblue :fill :midnightblue}
        [0 0] [width height]]]
      [:g {:id "connections" :inkscape:groupmode "layer" :inkscape:label "connections"}
       [:circle {:stroke :black :stroke-width 1 :fill :indigo} [0 0] 1]]
      [:g {:id "pathways" :inkscape:groupmode "layer" :inkscape:label "pathways"}
       [:circle {:stroke :black :stroke-width 1 :fill :indigo} [0 0] 1]]
      (into
        [:g {:id "locations" :inkscape:groupmode "layer" :inkscape:label "locations"}]
        (map (fn [coord] [:circle {:id (gen-id! "location") :class "location"} coord 3]) locations))
      [:g {:id "labels" :inkscape:groupmode "layer" :inkscape:label "labels"}
       [:circle {:stroke :black :stroke-width 1 :fill :indigo} [0 0] 1]]])))


(defn gen-locations [{:keys [width height] :as chart}]
  (let [{:keys [location-step location-deviation]} (:gen-params chart)
        location-points (locations-grid width height location-step location-deviation)
        locations (into {}
                        (map (fn [pt] [(gen-id! "location")
                                       {:coords pt}]) location-points))]
    (assoc chart :locations locations)))



(defn connect-locations [locations pathway]
  (let [[p1 p2] (:coords pathway)
        l1 (some
               (fn [[id {:keys [coords]}]]
                 (when (geometry/inside-circle? p1 coords 3)
                   id))
               locations)
        l2 (some
               (fn [[id {:keys [coords]}]]
                 (when (geometry/inside-circle? p2 coords 3)
                   id))
               locations)]
    (assoc pathway :locations (if (and l1 l2)
                                [l1 l2]
                                :lost))))


(comment
  (connect-locations [["loc1" {:coords [1 1]}]
                      ["loc2" {:coords [10 10]}]
                      ["loc3" {:coords [20 20]}]]
                     {:coords [[20 20] [10 10]]})
  (geometry/inside-circle? [10 10] [10 11] 2)
  )



(defn save-loops [])

(defn discard-long [pathways])

(defn gen-pathways [{:keys [locations] :as chart}]
  (let [weight (get-in chart [:gen-params :pathway-weight-range])
        pathways (->> locations
                   vals
                   (map :coords)
                   geometry/triangulate
                   graph/mst-graph              
                   (map set)
                   distinct
                   (map vec)
                   (map (fn [c] {:coords c
                                 :weight (inc (rand-int weight))
                                 :length (geometry/line-length c)}))
                   (map (partial connect-locations locations)))]
    (assoc chart :pathways
                 (into {}
                       (map (fn [p]
                              [(gen-id! "pathway") p]) pathways)))))


(defn gen-terrain [{:keys [width height] :as chart}]
  (let [{:keys [terrain-step terrain-deviation]} (:gen-params chart)
        terrain-points (locations-grid width height terrain-step terrain-deviation)
        terrain (geometry/voronoi terrain-points [[0 0] [width height]])
        ]
    (assoc chart :terrain {:points terrain-points
                           :edges  terrain})))

(defn generate-chart
  ([] (generate-chart {} {}))
  ([map-params gen-params]
   (reset-ids!)
   (let [default-map
         {:name "Terra Incognita"
          :desc "HC SVNT DRACONES"
          :width  1000
          :height 500
          :gen-params {:location-step 50
                       :location-deviation 35
                       :pathway-weight-range 5
                       :terrain-step 20
                       :terrain-deviation 5}
          :locations {}
          :pathways {}
          :terrain {}
          }
         chart (-> default-map
                   (merge map-params)
                   (update :gen-params merge gen-params))]
     (-> chart
         gen-locations
         gen-pathways
         gen-terrain
         ))))

(comment
  (time
    (io/render-svg
      (chart->svg-draft
        (generate-chart {:width 1024 :height 512} {:location-step 48 :location-deviation 36 :terrain-step 16 :terrain-deviation 8}))
      "test.svg"))

  (filter (fn [{:keys [edge sites]}] (every? (fn [x] (< x 10)) sites))
            (wanderlust.voronoi/generate-diagram
   (locations-grid 100 200 10 5) [[0 0] [100 200]] 1))
  

  (locations-grid 1000 1000 10 5)
  (geometry/voronoi (locations-grid 1000 1000 20 5) )
  (generate-chart {:width 500 :height 500} {:location-step 80 :location-deviation 50 :terrain-step 10 :terrain-deviation 7})

  (let [chart (generate-chart {:width 1000 :height 1000} {:location-step 80 :location-deviation 50})
        draft (chart->svg-draft chart)
        ]
    ;chart
    (io/render-svg draft "test.svg")
    )
)

(defn svg-draft->chart [svg-draft]
  (let [svg-doc (xml->doc svg-draft)]
    (let [locations
          (reduce
            (fn [res {:keys [attrs text]}]
              (prn attrs text)
              (assoc res (:id attrs)
                         {:coords [(Double/parseDouble (:cx attrs))
                                   (Double/parseDouble (:cy attrs))]
                          :data   (clojure.edn/read-string text)}))
            {} ($x "//svg/g[@id='locations']/circle" svg-doc))
          pathways
          (reduce
            (fn [res x]
              )
            {}
            ($x "//svg/g[@id='pathways']/path" svg-doc))]
      {:locations locations
       :pathways pathways})))

(comment
  (svg-draft->chart (slurp "./test.svg"))
  )

(defn location-draft [[id {:keys [coords]}]]
  [:circle {:id id
            :stroke :yellow
            :fill :red}
   coords
   3])

(defn pathways-draft [[id {:keys [coords weight length]}]]
  (let [[p1 p2] coords
        color (case weight
                1 :blue
                2 :darkmagenta
                3 :darkred
                4 :red
                5 :yellow)]
    [:line {:id id
            :stroke color
            :stroke-width 1}  p1 p2]))

(defn terrain-draft [points]
  (into
    [:polygon {:stroke :gray :fill :beige}] points))

(defn terrain-points [points]
  [:circle {:stroke :darkgray :stroke-width 1 :fill :blue} points 1])

;(pathways-draft {:coords [[1 2] [3 4]] :weight 3 :length 10.1})

(defn chart->svg-draft [chart]
  (let [{:keys [name desc width height locations pathways terrain]} chart]
    [:dali/page {:width width
                 :height height
                 :xmlns:dc "http://purl.org/dc/elements/1.1/"
                 :xmlns:cc "http://creativecommons.org/ns#"
                 :xmlns:rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                 :xmlns:svg "http://www.w3.org/2000/svg"
                 :xmlns "http://www.w3.org/2000/svg"
                 :xmlns:sodipodi "http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"
                 :xmlns:inkscape "http://www.inkscape.org/namespaces/inkscape"
                 :version "1.2"}
     [:title name] [:desc desc]
     (into
       [:g {:id                 "terrain shapes"
            :inkscape:groupmode "layer"
            :inkscape:label     "terrain-shapes"}]
       (map terrain-draft (:edges terrain)))
     (into
       [:g {:id                 "terrain points"
            :inkscape:groupmode "layer"
            :inkscape:label     "terrain-points"}]
       (map terrain-points (:points terrain)))
     (into [:g {:id "locations"
                :inkscape:groupmode "layer"
                :inkscape:label "locations"}]
           (map location-draft locations))
     (into [:g {:id "pathways"
                :inkscape:groupmode "layer"
                :inkscape:label "pathways"}]
           (map pathways-draft pathways))
     ]))

(comment
  (io/render-svg
    (generate-draft {:width 1000 :height 1000 :location-step 35 :location-deviation 30})
    "test.svg"))

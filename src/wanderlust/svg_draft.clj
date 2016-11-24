(ns wanderlust.svg-draft
  (:require
   [wanderlust.geometry :as geometry]
   [wanderlust.graph :as graph]
   [wanderlust.svg-colors :as svg-colors]
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
        terrain-polygons (geometry/voronoi terrain-points [[0 0] [width height]])
        terain-mesh ()
        ]
    (assoc chart :terrain {:points terrain-points
                           :edges  terrain-polygons})))

(defn gen-terrain-shapes [{:keys [width height] :as chart}]
  (let [ocean {:coords [[0 0] [0 height] [width height] [width 0]]
               :terrain :ocean}
        plains {:coords   [[0 0]
                          [0 height]
                          [width 0]]
                :terrain :plains}]
    (-> chart
        (assoc-in [:terrain :shapes] [ocean plains]))))

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
         gen-terrain-shapes
         ))))

(comment
  (time
    (io/render-svg
      (chart->svg-draft
        (generate-chart {:width 96 :height 96} {:location-step 10 :location-deviation 7 :terrain-step 4 :terrain-deviation 2}))
      "test.svg"))

  (filter (fn [{:keys [edge sites]}] (every? (fn [x] (< x 10)) sites))
            (wanderlust.voronoi/generate-diagram
   (locations-grid 100 200 10 5) [[0 0] [100 200]] 1))
  

  (locations-grid 1000 1000 10 5)
  (geometry/voronoi (locations-grid 1000 1000 20 5) )
  (generate-chart {:width 500 :height 500} {:location-step 80 :location-deviation 50 :terrain-step 10 :terrain-deviation 7})

  (time
   (let [chart (generate-chart {:width 1000 :height 1000} {:location-step 80 :location-deviation 50})
         draft (chart->svg-draft chart)
         ]
                                        ;chart
     (io/render-svg draft "test.svg")
     ))
)

(defn svg-draft->chart [svg-draft]
  (let [svg-doc (xml->doc svg-draft)]
    (let [locations
          (reduce
            (fn [res {:keys [attrs data]}]
              (assoc res (:id attrs)
                         {:coords [(Double/parseDouble (:cx attrs))
                                   (Double/parseDouble (:cy attrs))]
                          :data   (clojure.edn/read-string (:data attrs))}))
            {} ($x "//svg/g[@id='locations']/circle" svg-doc))

          pathways
          (reduce
           (fn [res {:keys [attrs data]}]
             (assoc res (:id attrs)
                    (merge (clojure.edn/read-string (:data attrs))
                           {:coords  (linerize-path-commands
                                      (path->commands (:d attrs)))})))
           {}
           ($x "//svg/g[@id='pathways']/path" svg-doc))

          terrain
          (reduce
           (fn [res {:keys [attr data]}]
             )
           {}
           ($x "//svg/g[@id='terrain-shapes']/path"))]
      {:locations locations
       :pathways pathways})))

(comment
  (let [ #_(io/render-svg
           (chart->svg-draft
            (generate-chart {:width 96 :height 96} {:location-step 16 :location-deviation 8 :terrain-step 8 :terrain-deviation 4}))
           "test.svg")
        x (svg-draft->chart (slurp "./test.svg"))
        y (chart->svg-draft x)]
       (io/render-svg y "test2.svg")
    y
    )
  )

(defn location-draft [[id {:keys [coords] :as location}]]
  [:circle {:id id
            :stroke :yellow
            :fill :red
            :data (str location)}
   coords
   3])


(defn polyline->path [polyline]
  (let [r (rest polyline)]
    (interleave (cons :M (repeat (count r) :L))
                        polyline)))


(defn pathways-draft [[id {:keys [coords weight length] :as pathway}]]
  (let [color (case weight
                1 :blue
                2 :darkmagenta
                3 :darkred
                4 :red
                5 :yellow
                :black)]
    (into
     [:path {:id id
             :stroke color
             :stroke-width 1
             :fill :none
             :data (str pathway)}]
     (polyline->path coords))))

(defn path->commands [path-str]
  (->> (re-seq #"([aAqQtTsScCvVhHlLzZmM])([\d ,.+-]+)" path-str)
       (mapv
         (fn [[_ c pts]]
           [(keyword c)
            (mapv #(Double/parseDouble %)
                  (let [res (re-seq #"[+-]*[\d.]+" pts)]
                    res))]))))

(defn linerize-path-commands [path-commands]
  (reduce
   (fn [res [c pts]]
     (into res
           (case c
             :M [pts]
             :L [pts]
             :C (if-let [points (map vec (geometry/de-casteljau (partition 2 pts) 0.1))]
                  points
                  [(first pts) (last pts)])
             :Q (if-let [points (map vec (geometry/de-casteljau (partition 2 pts) 0.1))]
                  points
                  [(first pts) (last pts)]))))
   []
   path-commands))

(comment 
  (linerize-path-commands (path->commands "M 77.142857,120.93363 L 65.714286,872.3622 L 677.14286,889.50506 L 654.28571,498.07649 C 629.28431,510.74052 561.12423,499.78185 554.28571,438.07649 C 540,438.07649 380,520.93363 380,520.93363 L 308.57143,412.3622 L 420,255.21935 L 374.28571,143.79078 L 257.14286,75.219348 L 157.14286,163.79078 L 85.714286,120.93363 Z"))
)

(defn terrain-draft [points]
  (into
   [:polygon {:stroke :gray
              :stroke-width 0.1
              :fill (rand-nth wanderlust.svg-colors/colors)}] points))

(defn terrain-points [points]
  [:circle {:stroke :darkgray :stroke-width 1 :fill :blue} points 1])


(def shapes-colors
  {:ocean :darkblue
   :plains :palegreen})

(defn terrain-shapes-draft [{:keys [terrain coords] :as shape}]
  (into
    [:path {:stroke (get shapes-colors terrain)
            :fill (get shapes-colors terrain)}] (polyline->path coords)))

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
            :inkscape:label     "terrain-shapes"
            :sodipodi:insensitive true}]
       (map terrain-shapes-draft (:shapes terrain)))
     (into
       [:g {:id                 "terrain mesh"
            :inkscape:groupmode "layer"
            :inkscape:label     "terrain-shapes"
            :sodipodi:insensitive true}]
       (map terrain-draft (:edges terrain)))
     (into
       [:g {:id                 "terrain points"
            :inkscape:groupmode "layer"
            :inkscape:label     "terrain-points"
            :sodipodi:insensitive true}]
       (map terrain-points (:points terrain)))
     (into [:g {:id "locations"
                :inkscape:groupmode "layer"
                :inkscape:label "locations"
                :sodipodi:insensitive true}]
           (map location-draft locations))
     (into [:g {:id "pathways"
                :inkscape:groupmode "layer"
                :inkscape:label "pathways"
                :sodipodi:insensitive true}]
           (map pathways-draft pathways))
     ]))

(comment
  (io/render-svg
    (generate-draft {:width 1000 :height 1000 :location-step 35 :location-deviation 30})
    "test.svg"))

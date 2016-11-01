(ns wanderlust.svg-draft
  (:require
   [wanderlust.geometry :as geom]
   [dali.io :as io]
   [dali.syntax :as s]
   [clj-xpath.core :refer :all]))


(def ^:private ids (atom {}))

(defn reset-ids []
  (reset! ids {}))
(defn gen-id
  ([prefix]
    (gen-id prefix "-"))
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
   (reset-ids)
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
        (map (fn [coord] [:circle {:id (gen-id "location") :class "location"} coord 3]) locations))
      [:g {:id "labels" :inkscape:groupmode "layer" :inkscape:label "labels"}
       [:circle {:stroke :black :stroke-width 1 :fill :indigo} [0 0] 1]]])))

(defn svg-draft->chart [svg-draft]
  (let [svg-doc (xml->doc svg-draft)]
    (prn svg-doc)
    (let [locations
          (reduce
            (fn [res {:keys [attrs text]}]
              (prn attrs text)
              (assoc res (:id attrs)
                         {:coords [(Double/parseDouble (:cx attrs))
                                   (Double/parseDouble (:cy attrs))]
                          :data   (clojure.edn/read-string text)}))
            {} ($x "//svg/g[@id='locations']/circle" svg-doc))]
      {:locations locations})))

(comment
  (svg-draft->chart (slurp "./test.svg"))
  )

(defn chart->svg-draft [chart]
  )

(comment
  (io/render-svg
    (generate-draft {:width 1000 :height 1000 :location-step 35 :location-deviation 30})
    "test.svg"))

(defproject wanderlust "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [dali "0.7.3"]
                 [aysylu/loom "0.6.0"]
                 [trystan/voronoi-diagram "1.0.0"]
                 [trystan/delaunay-triangulation "1.0.1"]
                 [com.github.kyleburton/clj-xpath "1.4.3"]]
  :main ^:skip-aot wanderlust.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

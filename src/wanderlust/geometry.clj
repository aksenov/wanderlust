(ns wanderlust.geometry)

(defn line-length
  [[[x1 y1] [x2 y2]]]
  (Math/hypot (- x1 x2)
              (- y1 y2)))

(defn polyline-length
  [polyline]
  (let [segments (partition 2 1 polyline)]
    (reduce + (map line-length segments))))

(defn point-at-line
  [dist line]
  (let [ratio (/ dist (line-length line))
        [[x1 y1] [x2 y2]] line]
    (when (<= ratio 1)
      [(+ (* x1 (- 1 ratio)) (* x2 ratio))
       (+ (* y1 (- 1 ratio)) (* y2 ratio))])))

(defn polyline->segments
  [polyline]
  (partition 2 1 polyline))

(defn point-at-polyline
  [len polyline]
  (let [segments (polyline->segments polyline)
        slen (->> segments
                  (map line-length)
                  (interleave segments)
                  (partition-all 2))]
    (loop [total-len 0
           ss slen]
      (if (empty? ss)
        (do (prn total-len)
            nil)
        (let [[pts l] (first ss)]
          (if (>= (+ total-len l) len)
            (point-at-line (- len total-len) pts)
            (recur (+ total-len l)
                   (rest ss))))))))

;; http://increasinglyfunctional.com/2013/12/08/point-polygon-clojure/
(defn- crossing-number
  "Determine crossing number for given point and segment of a polygon.
   See http://geomalgorithms.com/a03-_inclusion.html"
  [[px py] [[x1 y1] [x2 y2]]]
  (if (or (and (<= y1 py) (> y2 py))
          (and (> y1 py) (<= y2 py)))
    (let [vt (/ (- py y1) (- y2 y1))]
      (if (< px (+ x1 (* vt (- x2 x1))))
        1 0))
    0))

(defn inside-polygon?
  "Is point inside the given polygon?"
  [point polygon]
  (odd? (reduce +
                (for [n (range (- (count polygon) 1))]
                    (crossing-number point [(nth polygon n)
                                            (nth polygon (+ n
                                                            1))])))))

(defn polygon-bounding [polygon]
  (let [xs (map first polygon)
        ys (map second polygon)]
    [[(apply min xs)
      (apply min ys)]
     [(apply max xs)
      (apply max ys)]]))

(defn- sign [[x1 y1] [x2 y2] [x3 y3]]
  (- (* (- x1 x3) (- y2 y3))
     (* (- x2 x3) (- y1 y3))))

(defn inside-triangle? [p [v1 v2 v3]]
  (let [b1 (< (sign p v1 v2) 0)
        b2 (< (sign p v2 v3) 0)
        b3 (< (sign p v3 v1) 0)]
    (and (= b1 b2) (= b2 b3))))


(defn polygon->triangles [polygon]
  (map #(conj % (first polygon)) (partition 2 1 (rest polygon))))

(defn inside-convex-polygon? [point polygon]
  (boolean (some #(inside-triangle? point %) (polygon->triangles polygon))))


(comment

  (line-length [[10 40] [11 30]])

  
  (polyline-length [[0 0] [10 10] [20 0] [20 10]])

  
  (point-at-line 5 [[0 0] [10 10]])
  
  (point-at-polyline 15 [[0 0] [10 10] [30 -10] [40 0] [50 0]])


  (inside-polygon? [60 -20] [[10 20] [20 20] [40 110] [100 30] [80 -50]])

  (polygon-bounding [[10 20] [20 20] [40 110] [100 30] [80 -50]])

  
  (inside-convex-polygon? [0 0] [[-10 0] [10 10] [20 0] [20 -10] [10 -25] [0 -30]])

  (inside-polygon? [0 0] [[-10 0] [10 10] [20 0] [20 -10] [10 -25] [0 -30]])

  (polygon->triangles [[-10 0] [10 10] [20 0] [20 -10] [10 -25] [0 -30]])
  )

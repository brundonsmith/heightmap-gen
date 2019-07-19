(ns heightmap-gen.perlin
  (:require [clojure.core.matrix :as mtrx])
  (:require [heightmap-gen.utils.maps :as maps]))

; create random vector map
(defn rand-radians [] 
  (* (rand) (* 2 Math/PI)))

(defn rotate [vector radians]
  (let [x (first vector)
        y (second vector)
        sin (Math/sin radians)
        cos (Math/cos radians)]
    [(- (* x cos) (* y sin))
     (+ (* x sin) (* y cos))]))

(defn vector-map [size]
  (maps/new-map size (fn [x y] (rotate [1 0] (rand-radians)))))

(defn dotproducts [th])

; pixel map from vectors
(defn interpolate [size the-vector-map]
  ())


(defn perlin [size scale]
  (interpolate (vector-map (/ size (* scale 5)))))


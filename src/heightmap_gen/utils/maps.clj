(ns heightmap-gen.utils.maps
  (:require [clojure.tools.trace :as tr])
  (:require [heightmap-gen.utils.math :as math]))


(defn map-size [the-map]
  (Math/round (math/binary-root (count the-map) 2)))

(defn get-at [the-map x y]
  (let [size (map-size the-map)
        x-index (mod (+ x size) size)
        y-index (mod (+ y size) size)]
    (get the-map (+ x-index (* y-index size)))))

(defn set-at [the-map x y val]
  ;(assert (and (>= x 0) (>= y 0) (< x (count the-map)) (< y (count the-map))) "Tried to set value with X and Y that are outside the map")
  (assoc the-map (+ x (* y (map-size the-map))) val))



(defn new-vec
  ([length] (new-vec length (constantly 0)))
  ([length func]
   (vec (map
         func
         (range length)))))

(defn new-map
  ([size] (new-map size (constantly 0)))
  ([size func] (new-vec (* size size) (fn [x] (func (mod x size) (math/round-down (/ x size)))))))



(defn map-maps [func maps]
  (let [size (map-size (first maps))]
    (new-map size
             (fn [x y]
               (let [vals (map #(get-at % x y) maps)]
                 (apply func vals))))))


(let [the-map (new-map 17)]
  (assert (= 17 (map-size the-map))))

(let [the-map (new-map 33)]
  (assert (= 33 (map-size the-map))))

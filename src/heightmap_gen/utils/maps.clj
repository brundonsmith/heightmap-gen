(ns heightmap-gen.utils.maps
  (:require [heightmap-gen.utils.math :as math]))


(def possible-sizes
  (map #(int (+ (Math/pow 2 %) 1)) (range 1 20)))
  
(defn map-size [the-map]
  (first (filter #(= (count the-map) (* % %)) possible-sizes)))

(defn wraparound [num size]
  (mod (+ num size) size))

(defn get-at [the-map x y]
  (let [size (map-size the-map)
        x-index (wraparound x size)
        y-index (wraparound y size)]
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


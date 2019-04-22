(ns heightmap-gen.utils.maps
  (:require [clojure.tools.trace :as tr]))


(defn map-size [the-map]
  (count the-map))

(defn get-at [the-map x y]
  (let [size (map-size the-map)
        x-index (mod (+ x size) size)
        y-index (mod (+ y size) size)]
    (get (get the-map x-index) y-index)))

(defn set-at [the-map x y val]
  (assert (and (>= x 0) (>= y 0) (< x (count the-map)) (< y (count the-map))) "Tried to set value with X and Y that are outside the map")
  (assoc the-map x (assoc (get the-map x) y val)))



(defn new-vec
  ([length] (new-vec length (constantly 0)))
  ([length func]
   (vec (map
         func
         (range length)))))

(defn new-map
  ([size] (new-map size (constantly 0)))
  ([size func] (new-vec size (fn [x] (new-vec size (fn [y] (func x y)))))))



(defn map-maps [func maps]
  (let [size (map-size (first maps))]
    (new-map size
             (fn [x y]
               (let [vals (map #(get-at % x y) maps)]
                 (apply func vals))))))



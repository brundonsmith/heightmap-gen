(ns heightmap-gen.filters
  (:require [heightmap-gen.math :as math])
  (:require [heightmap-gen.maps :as maps]))

(defn average-maps [maps]
  (maps/map-maps math/average maps))

(defn add-maps [maps]
  (maps/map-maps + maps))

(defn subtract-maps [maps]
  (maps/map-maps - maps))

(defn multiply-maps [maps]
  (maps/map-maps * maps))

(defn max-maps [maps]
  (maps/map-maps max maps))

(defn min-maps [maps]
  (maps/map-maps min maps))

(defn power-maps [maps]
  (maps/map-maps math/pow maps))

(defn root-maps [maps]
  (maps/map-maps math/root maps))

(defn blend-maps [blend-map map-1 map-2]
  (maps/map-maps
   (fn [& vals]
     (let [t (nth vals 0)
           a (nth vals 1)
           b (nth vals 2)]
       (math/lerp a b t)))
   [blend-map map-1 map-2]))

; (defn weather ...

(ns heightmap-gen.basic-generators
  (:require [heightmap-gen.utils.maps :as maps]))

(defn flat-map [size val]
  (maps/new-map size (constantly val)))

(defn white-noise-map [size]
  (maps/new-map size (fn [x y] (rand))))

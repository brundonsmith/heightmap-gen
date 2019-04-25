(ns heightmap-gen.utils.maps-test
  (:require [clojure.test :refer :all]
            [heightmap-gen.utils.maps :refer :all]))


(deftest map-size-5
  (is (let [the-map (new-map 5)]
        (= 5 (map-size the-map)))))

(deftest map-size-17
  (is (let [the-map (new-map 17)]
        (= 17 (map-size the-map)))))

(deftest map-size-33
  (is (let [the-map (new-map 33)]
        (= 33 (map-size the-map)))))

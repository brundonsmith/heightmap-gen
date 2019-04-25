(ns heightmap-gen.utils.math-test
  (:require [clojure.test :refer :all]
            [heightmap-gen.utils.math :refer :all]))

; average
(deftest average-1
  (is (=
       (average 1)
       1)))

(deftest average-0
  (is (=
       (average 0)
       0)))

(deftest average-1-3
  (is (=
       (average 1 3)
       2)))

(deftest average-1-4-7
  (is (=
       (average 1 4 7)
       4)))

; clamp
(deftest clamp-min
  (is (=
       (clamp -1 0 1)
       0)))

(deftest clamp-max
  (is (=
       (clamp 2 0 1)
       1)))

(deftest clamp-in
  (is (=
       (clamp 0.1 0 1)
       0.1)))

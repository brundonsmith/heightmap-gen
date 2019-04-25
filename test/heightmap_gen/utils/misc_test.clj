(ns heightmap-gen.utils.misc-test
  (:require [clojure.test :refer :all]
            [heightmap-gen.utils.misc :refer :all]))

(deftest pipe-test
  (is (=
       (pipe 0 (map (fn [num] (fn [other-num] (+ num other-num))) [1 3 2 7]))
       13)))

(deftest pairs-test
  (is (=
         (pairs [0 1] [2 3])
         [[0 2] [0 3] [1 2] [1 3]])))

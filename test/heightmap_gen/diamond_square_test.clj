(ns heightmap-gen.diamond-square-test
  (:require [clojure.test :refer :all]
            [heightmap-gen.diamond-square :refer :all]
            [heightmap-gen.utils.misc :as misc]))



(deftest diamonds-test
  (is (=
       (get-diamonds 0 0 2 2)
       [[[1 1] [0 0] [2 0] [1 -1]]
        [[1 1] [2 0] [2 2] [3 1]]
        [[1 1] [2 2] [0 2] [1 3]]
        [[1 1] [0 2] [0 0] [-1 1]]])))

(deftest midpoint-test
  (is (=
       (set-midpoint [0.5 0 0 0  0.5
                      0   0 0 0  0
                      0   0 0 0  0
                      0   0 0 0  0
                      0.5 0 0 0  0.5]
                     (misc/pairs [0 4] [0 4])
                     0))
      [0.5 0 0 0  0.5
       0   0 0 0  0
       0   0 0.5 0  0
       0   0 0 0  0
       0.5 0 0 0  0.5]))

(deftest partitions-test
  (is (= (get-partitions 5 2)
         [[0 0 2 2]
          [0 2 2 4]
          [2 0 4 2]
          [2 2 4 4]])))
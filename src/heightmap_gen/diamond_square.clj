(ns heightmap-gen.diamond-square
  (:require [heightmap-gen.utils.math :as math])
  (:require [heightmap-gen.utils.maps :as maps])
  (:require [heightmap-gen.utils.misc :as misc])
  (:require [heightmap-gen.basic-generators :as basic-generators]))

(defn- get-trios [x1 y1 x2 y2]
  (let [mid [(math/average x1 x2) (math/average y1 y2)]]
    [[[x1 y1] [x2 y1] mid]
     [[x2 y1] [x2 y2] mid]
     [[x2 y2] [x1 y2] mid]
     [[x1 y2] [x1 y1] mid]]))

(assert (=
         (get-trios 0 0 2 2)
         [[[0 0] [2 0] [1 1]]
          [[2 0] [2 2] [1 1]]
          [[2 2] [0 2] [1 1]]
          [[0 2] [0 0] [1 1]]]))

(defn- mid-num [nums]
  (let [repeated-nums (into '() (into #{} (filter (fn [num] (not (misc/unique [0 0 1] num))) [0 0 1])))]
    (cond
      (= (count repeated-nums) 1) (first repeated-nums)
      :else (apply math/average nums))))

(defn- set-midpoint [the-map points rand-i]
  (let [all-x (map first points)
        all-y (map second points)
        mid-x (mid-num all-x)
        mid-y (mid-num all-y)
        midval (apply math/average (map #(maps/get-at the-map (first %) (second %)) points))]
    (maps/set-at the-map mid-x mid-y (+ midval (math/random-offset rand-i)))))

(assert (=
         (set-midpoint [[0 0 1]
                        [0 0 0]
                        [1 0 2]]
                       [[0 0] [2 0] [2 2] [0 2]]
                       0))
        [[0 0 1]
         [0 1 0]
         [1 0 2]])

(defn- apply-trios [the-map x1 y1 x2 y2 rand-i]
  (let [point-trios (get-trios x1 y1 x2 y2)]
    (misc/pipe
     (map (fn [trio] (fn [piped-map] (set-midpoint piped-map trio rand-i))) point-trios)
     the-map)))

(defn- get-quadrants [x1 y1 x2 y2]
  (let [mid-x (math/average x1 x2)
        mid-y (math/average y1 y2)]
    [[x1 y1 mid-x mid-y]
     [mid-x y1 x2 mid-y]
     [x1 mid-y mid-x y2]
     [mid-x mid-y x2 y2]]))

(assert (=
         (get-quadrants 0 0 2 2)
         [[0 0 1 1]
          [1 0 2 1]
          [0 1 1 2]
          [1 1 2 2]]))

(declare square-step)

(defn- diamond-step [the-map x1 y1 x2 y2 rand-i rand-s]
  (cond
    (or (= x1 x2) (= y1 y2) (= x1 (- x2 1)) (= y1 (- y2 1))) the-map
    :else (square-step
           (set-midpoint the-map (misc/pairs [x1 x2] [y1 y2]) rand-i)
           x1 y1 x2 y2
           (* rand-i rand-s)
           rand-s)))

(defn- square-step [the-map x1 y1 x2 y2 rand-i rand-s]
  (let [modified (apply-trios the-map x1 y1 x2 y2 rand-i)]
    (misc/pipe
     (map
      (fn [quadrant]
        (fn [piped-map]
          (diamond-step
           piped-map
           (nth quadrant 0)
           (nth quadrant 1)
           (nth quadrant 2)
           (nth quadrant 3)
           rand-i rand-s)))
      (get-quadrants x1 y1 x2 y2))
     the-map)))


(defn diamond-square-map [size rand-i rand-s]
  ;(assert (is-integer (Math/sqrt (- size 1))))
  (diamond-step (basic-generators/white-noise-map size) 0 0 (- size 1) (- size 1) rand-i rand-s))

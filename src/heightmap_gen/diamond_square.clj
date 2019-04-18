(ns heightmap-gen.diamond-square
  (:require [heightmap-gen.utils.math :as math])
  (:require [heightmap-gen.utils.maps :as maps])
  (:require [heightmap-gen.utils.misc :as misc])
  (:require [heightmap-gen.basic-generators :as basic-generators]))


(defn- init-corners [the-map]
  (let [size (count the-map)]
    (-> the-map
        (maps/set-at 0 0                   (* (rand) 0.5))
        (maps/set-at 0 (- size 1)          (* (rand) 0.5))
        (maps/set-at (- size 1) 0          (* (rand) 0.5))
        (maps/set-at (- size 1) (- size 1) (* (rand) 0.5)))))



(defn- mid-num [nums]
  (let [repeated-nums (into '() (into #{} (filter (fn [num] (not (misc/unique nums num))) nums)))]
    (cond
      (= (count repeated-nums) 1) (first repeated-nums)
      :else (apply math/average nums))))

(defn- set-midpoint [the-map points rand-i]
  (let [all-x (map first points)
        all-y (map second points)
        mid-x (mid-num all-x)
        mid-y (mid-num all-y)
        point-vals (map #(maps/get-at the-map (first %) (second %)) points)
        avg (apply math/average point-vals)
        offset (math/random-offset rand-i)]
    ;(println points point-vals)
    (maps/set-at the-map mid-x mid-y (+ avg offset))))



(defn- get-diamonds [x1 y1 x2 y2]
  (let [mid-x (math/average x1 x2)
        mid-y (math/average y1 y2)
        radius (- x2 x1)]
    [[[mid-x mid-y] [x1 y1] [x2 y1] [mid-x (- mid-y radius)]]
     [[mid-x mid-y] [x2 y1] [x2 y2] [(+ mid-x radius) mid-y]]
     [[mid-x mid-y] [x2 y2] [x1 y2] [mid-x (+ mid-y radius)]]
     [[mid-x mid-y] [x1 y2] [x1 y1] [(- mid-x radius) mid-y]]]))

(defn- apply-diamonds [the-map x1 y1 x2 y2 rand-i]
  (let [point-diamonds (get-diamonds x1 y1 x2 y2)]
    (misc/pipe
     (map (fn [diamond] (fn [piped-map] (set-midpoint piped-map diamond rand-i))) point-diamonds)
     the-map)))



(defn- get-quadrants [x1 y1 x2 y2]
  (let [mid-x (math/average x1 x2)
        mid-y (math/average y1 y2)]
    [[x1 y1 mid-x mid-y]
     [mid-x y1 x2 mid-y]
     [x1 mid-y mid-x y2]
     [mid-x mid-y x2 y2]]))



; steps
(declare square-step)

(defn- diamond-step [the-map x1 y1 x2 y2 rand-i rand-s]
  ;(println (set-midpoint the-map (misc/pairs [x1 x2] [y1 y2]) rand-i))
  (cond
    (or (= x1 x2) (= y1 y2) (= x1 (- x2 1)) (= y1 (- y2 1))) the-map
    :else (square-step
           (set-midpoint the-map (misc/pairs [x1 x2] [y1 y2]) rand-i)
           x1 y1 x2 y2
           (* rand-i rand-s)
           rand-s)))

(defn- square-step [the-map x1 y1 x2 y2 rand-i rand-s]
  ;(println the-map)
  (let [modified (apply-diamonds the-map x1 y1 x2 y2 rand-i)]
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
           rand-i
           rand-s)))
      (get-quadrants x1 y1 x2 y2))
     modified)))

; main function
(defn diamond-square-map [size rand-i rand-s]
  ;(assert (is-integer (Math/sqrt (- size 1))))
  (let [the-map (basic-generators/flat-map size 0.5)]
    (diamond-step the-map 0 0 (- size 1) (- size 1) rand-i rand-s)))

(comment
(assert (=
         (get-diamonds 0 0 2 2)
         [[[1 1] [0 0] [2 0] [1 -1]]
          [[1 1] [2 0] [2 2] [3 1]]
          [[1 1] [2 2] [0 2] [1 3]]
          [[1 1] [0 2] [0 0] [-1 1]]]))

(assert (=
         (set-midpoint [[0.5 0 0 0  0.5]
                        [0   0 0 0  0]
                        [0   0 0 0  0]
                        [0   0 0 0  0]
                        [0.5 0 0 0  0.5]]
                       (misc/pairs [0 4] [0 4])
                       0))
        [[0.5 0 0 0  0.5]
         [0   0 0 0  0]
         [0   0 0.5 0  0]
         [0   0 0 0  0]
         [0.5 0 0 0  0.5]])

(assert (=
         (get-quadrants 0 0 2 2)
         [[0 0 1 1]
          [1 0 2 1]
          [0 1 1 2]
          [1 1 2 2]]))
)
(ns heightmap-gen.diamond-square
  (:require [clojure.tools.trace :as tr])
  (:require [heightmap-gen.utils.math :as math])
  (:require [heightmap-gen.utils.maps :as maps])
  (:require [heightmap-gen.utils.misc :as misc])
  (:require [heightmap-gen.basic-generators :as basic-generators]))


(defn- init-corners [the-map]
  (let [size (maps/map-size the-map)]
    (-> the-map
        (maps/set-at 0 0                   (* (rand) 0.5))
        (maps/set-at 0 (- size 1)          (* (rand) 0.5))
        (maps/set-at (- size 1) 0          (* (rand) 0.5))
        (maps/set-at (- size 1) (- size 1) (* (rand) 0.5)))))


(defn- mid-num [nums]
  (let [repeated-nums (vec (into #{} (filter (fn [num] (not (misc/unique nums num))) nums)))]
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
        offset (math/random-offset rand-i)
        new-val (+ avg offset)]
    (maps/set-at the-map mid-x mid-y new-val)))



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
    (misc/pipe the-map
               (map
                (fn [diamond]
                  (fn [piped-map]
                    (set-midpoint piped-map diamond rand-i))) point-diamonds))))

(defn- get-partitions [size partitions]
  (let [partition-size (/ size partitions)]
    (for [x1 (range 0 size partition-size)
          y1 (range 0 size partition-size)]
      (let [x2 (+ x1 (* partition-size 0.999))
            y2 (+ y1 (* partition-size 0.999))]
        [(math/round-down x1) (math/round-down y1) (math/round-down x2) (math/round-down y2)]))))


; steps
(defn- each-partition [the-map partitions func]
  (let [size (maps/map-size the-map)]
    (misc/pipe the-map
               (map 
                (fn [partition] (fn [piped-map] 
                                  (func piped-map (nth partition 0) (nth partition 1) (nth partition 2) (nth partition 3))))
                (get-partitions size partitions)))))



(defn- diamond-pass [the-map partitions rand-i rand-s]
  (each-partition the-map partitions 
                  (fn [piped-map x1 y1 x2 y2]
                    (set-midpoint piped-map (misc/pairs [x1 x2] [y1 y2]) rand-i))))

(defn- square-pass [the-map partitions rand-i rand-s]
  (each-partition the-map partitions
                  (fn [piped-map x1 y1 x2 y2]
                    (apply-diamonds piped-map x1 y1 x2 y2 rand-i))))

(defn step [the-map partitions rand-i rand-s]
  (println "------------------ step ------------------")
  (let [size (maps/map-size the-map)]
    (cond
      (>= partitions (- size 1)) the-map
      :else                      (-> the-map
                                     (diamond-pass partitions rand-i rand-s)
                                     (square-pass partitions rand-i rand-s)
                                     (recur (* 2 partitions) (* rand-i rand-s) rand-s)))))
  

; main function
(defn diamond-square-map [size rand-i rand-s]
  ;(assert (is-integer (Math/sqrt (- size 1))))
  (let [the-map (basic-generators/flat-map size 0.5)]
    (step the-map 1 rand-i rand-s)))

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
)

(assert (= (get-partitions 5 2)
           [[0 0 2 2]
            [0 2 2 4]
            [2 0 4 2]
            [2 2 4 4]]))

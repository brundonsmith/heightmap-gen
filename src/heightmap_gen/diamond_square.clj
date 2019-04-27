(ns heightmap-gen.diamond-square
  (:require [heightmap-gen.utils.math :as math])
  (:require [heightmap-gen.utils.maps :as maps])
  (:require [heightmap-gen.utils.misc :as misc])
  (:require [heightmap-gen.basic-generators :as basic-generators]))


(defn init-corners [the-map]
  (let [size (maps/map-size the-map)]
    (-> the-map
        (maps/set-at 0 0                   (* (rand) 0.5))
        (maps/set-at 0 (- size 1)          (* (rand) 0.5))
        (maps/set-at (- size 1) 0          (* (rand) 0.5))
        (maps/set-at (- size 1) (- size 1) (* (rand) 0.5)))))


(defn mid-num [nums]
  (let [repeated-nums (vec (into #{} (filter (fn [num] (not (misc/unique nums num))) nums)))]
    (cond
      (= (count repeated-nums) 1) (first repeated-nums)
      :else (apply math/average nums))))

(defn set-midpoint [the-map points variance]
  (let [all-x (map first points)
        all-y (map second points)
        mid-x (mid-num all-x)
        mid-y (mid-num all-y)
        point-vals (map #(maps/get-at the-map (first %) (second %)) points)
        avg (apply math/average point-vals)
        offset (math/random-offset variance)
        new-val (+ avg offset)]
    (maps/set-at the-map mid-x mid-y new-val)))



(defn get-diamonds [x1 y1 x2 y2]
  (let [mid-x (math/average x1 x2)
        mid-y (math/average y1 y2)
        radius (- x2 x1)]
    [[[mid-x mid-y] [x1 y1] [x2 y1] [mid-x (- mid-y radius)]]
     [[mid-x mid-y] [x2 y1] [x2 y2] [(+ mid-x radius) mid-y]]
     [[mid-x mid-y] [x2 y2] [x1 y2] [mid-x (+ mid-y radius)]]
     [[mid-x mid-y] [x1 y2] [x1 y1] [(- mid-x radius) mid-y]]]))


(defn apply-diamonds [the-map x1 y1 x2 y2 variance]
  (let [point-diamonds (get-diamonds x1 y1 x2 y2)]
    (misc/pipe the-map
               (map
                (fn [diamond]
                  (fn [piped-map]
                    (set-midpoint piped-map diamond variance))) point-diamonds))))

(defn get-partitions [size partitions]
  (let [partition-size (/ size partitions)]
    (for [x1 (range 0 size partition-size)
          y1 (range 0 size partition-size)]
        [(math/round-down x1) 
         (math/round-down y1) 
         (math/round-down (+ x1 (* partition-size 0.999)))
         (math/round-down (+ y1 (* partition-size 0.999)))])))


; steps
(defn each-partition [the-map partitions func]
  (let [size (maps/map-size the-map)]
    (misc/pipe the-map
               (map
                (fn [partition] 
                  (fn [piped-map]
                    (func piped-map (nth partition 0) (nth partition 1) (nth partition 2) (nth partition 3))))
                (get-partitions size partitions)))))



(defn diamond-pass [the-map partitions variance]
  (each-partition the-map partitions 
                  (fn [piped-map x1 y1 x2 y2]
                    (set-midpoint piped-map (misc/pairs [x1 x2] [y1 y2]) variance))))

(defn square-pass [the-map partitions variance]
  (each-partition the-map partitions
                  (fn [piped-map x1 y1 x2 y2]
                    (apply-diamonds piped-map x1 y1 x2 y2 variance))))

(defn step [the-map partitions variance coarseness]
  (let [size (maps/map-size the-map)
        step-num (math/log2 partitions)
        total-steps (math/log2 (- size 1))
        rand-scale (- 1 (/ step-num total-steps))]
    (print "\rPass [" (int step-num) "/" (int total-steps) "]" (cond (= step-num total-steps) "\n" :else ""))
    (flush)
    (cond
      (>= partitions (- size 1)) the-map
      :else                      (-> the-map
                                     (diamond-pass partitions (* variance rand-scale))
                                     (square-pass partitions (* variance rand-scale))
                                     (recur (* 2 partitions) (* variance coarseness) coarseness)))))
  

(defn diamond-square-map [size variance coarseness]
  (assert (some #(= size %) maps/possible-sizes) "Heightmap size must be a power of 2, plus 1")
  (assert (>= variance 0) "Variance should be greater than or equal to 0")
  (assert (<= variance 1) "Variance should be less than or equal to 1")
  (assert (>= coarseness 0) "Coarseness should be greater than or equal to 0")
  (assert (<= coarseness 1) "Coarseness should be less than or equal to 1")
  (let [the-map (basic-generators/flat-map size 0.5)]
    (step the-map 1 variance coarseness)))
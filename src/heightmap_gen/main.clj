
; math
(defn average [& nums]
  (/ (reduce + nums) (count nums)))

(assert (=
         (average 1)
         1))

(assert (=
         (average 0)
         0))

(assert (=
         (average 1 3)
         2))

(assert (=
         (average 1 4 7)
         4))

(defn binary-root [n base]
  (Math/pow Math/E (/ (Math/log n) base)))

(defn root
  ([n base] (binary-root n base))
  ([n base & nums] (apply root (conj nums (binary-root n base)))))

(defn pow
  ([n exp] (Math/pow n exp))
  ([n exp & nums] (apply pow (conj nums (Math/pow n exp)))))

(defn lerp [a b t]
  (+ (* a t) (* b (- 1 t))))

(defn clamp [n min max]
  (cond
    (<= n min) min
    (>= n max) max
    :else n))

(defn is-integer [flt]
  (=
   (Math/round (Math/ceil flt))
   (Math/round (Math/floor flt))))

(defn random-offset [radius]
  (* (* (- (rand) 0.5) radius) 2))


; image data
(defn new-vec
  ([length] (new-vec length (constantly 0)))
  ([length func]
   (vec (map
         func
         (range length)))))

(defn new-map
  ([size] (new-map size (constantly 0)))
  ([size func] (new-vec size (fn [x] (new-vec size (fn [y] (func x y)))))))

(defn get-at [the-map x y]
  (get (get the-map x) y))

(defn set-at [the-map x y val]
  (assoc the-map x (assoc (get the-map x) y val)))

(defn map-maps [func maps]
  (let [size (count (first maps))]
    (new-map size
             (fn [x y]
               (let [vals (map #(get-at % x y) maps)]
                 (apply func vals))))))




;; generators
(defn flat-map [size val]
  (new-map size (constantly val)))

(defn white-noise-map [size]
  (new-map size (fn [x y] (rand))))

; diamond-square

(defn pipe [funcs val]
  (cond 
    (= (count funcs) 0) val
    :else (pipe (rest funcs) (apply (first funcs) [val]))))

(assert (=
         (pipe (map (fn [num] (fn [other-num] (+ num other-num))) [1 3 2 7]) 0)
         13))

(defn pairs [group1 group2]
  (for [a group1
        b group2]
    [a b]))

(assert (= 
         (pairs [0 1] [2 3]) 
         [[0 2] [0 3] [1 2] [1 3]]))

(defn get-trios [x1 y1 x2 y2]
  (let [mid [(average x1 x2) (average y1 y2)]]
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

(defn occurances [vec val]
  (reduce (fn [c element]
            (cond
              (= element val) (+ c 1)
              :else c))
          0
          vec))

(defn unique [vec val]
  (< (occurances vec val) 2))

(defn mid-num [nums]
  (let [repeated-nums (into '() (into #{} (filter (fn [num] (not (unique [0 0 1] num))) [0 0 1])))]
    (cond
      (= (count repeated-nums) 1) (first repeated-nums)
      :else (apply average nums))))

(defn set-midpoint [the-map points rand-i]
  (let [all-x (map first points)
        all-y (map second points)
        mid-x (mid-num all-x) 
        mid-y (mid-num all-y)
        midval (apply average (map #(get-at the-map (first %) (second %)) points))]
    (set-at the-map mid-x mid-y (+ midval (random-offset rand-i)))))

(random-offset 1)

(assert (=
         (set-midpoint [[0 0 1]
                        [0 0 0]
                        [1 0 2]]
                       [[0 0] [2 0] [2 2] [0 2]]
                       0))
        [[0 0 1]
         [0 1 0]
         [1 0 2]])

(defn apply-trios [the-map x1 y1 x2 y2 rand-i]
  (let [point-trios (get-trios x1 y1 x2 y2)]
    (pipe
     (map (fn [trio] (fn [piped-map] (set-midpoint piped-map trio rand-i))) point-trios)
     the-map)))

(defn get-quadrants [x1 y1 x2 y2]
  (let [mid-x (average x1 x2)
        mid-y (average y1 y2)]
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

(defn diamond-step [the-map x1 y1 x2 y2 rand-i rand-s]
  (cond
    (or (= x1 x2) (= y1 y2) (= x1 (- x2 1)) (= y1 (- y2 1))) the-map
    :else (square-step
            (set-midpoint the-map (pairs [x1 x2] [y1 y2]) rand-i)
            x1 y1 x2 y2
            (* rand-i rand-s)
            rand-s)))

(defn square-step [the-map x1 y1 x2 y2 rand-i rand-s]
  (let [modified (apply-trios the-map x1 y1 x2 y2 rand-i)]
    (pipe
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
  (diamond-step (white-noise-map size) 0 0 (- size 1) (- size 1) rand-i rand-s))

(let [the-map [[0 0 1]
               [0 0 0]
               [1 0 2]]
      x1 0 y1 0 x2 2 y2 2
      rand-i 0
      rand-s 1
      
      modified (apply-trios the-map x1 y1 x2 y2 rand-i)]
  (square-step
    (set-midpoint the-map (pairs [x1 x2] [y1 y2]) rand-i)
    x1 y1 x2 y2
    (* rand-i rand-s)
    rand-s))


; perlin


;; filters
(defn average-maps [maps]
  (map-maps average maps))

(defn add-maps [maps]
  (map-maps + maps))

(defn subtract-maps [maps]
  (map-maps - maps))

(defn multiply-maps [maps]
  (map-maps * maps))

(defn max-maps [maps]
  (map-maps max maps))

(defn min-maps [maps]
  (map-maps min maps))

(defn power-maps [maps]
  (map-maps pow maps))

(defn root-maps [maps]
  (map-maps root maps))

(defn blend-maps [blend-map map-1 map-2]
  (map-maps 
   (fn [& vals] 
     (let [t (nth vals 0)
           a (nth vals 1)
           b (nth vals 2)]
       (lerp a b t)))
   [blend-map map-1 map-2]))

; (defn weather ...




;; writing files
(import 'java.awt.image.BufferedImage 'javax.imageio.ImageIO 'java.awt.Color 'java.io.File)

(defn write-image [ height-map ]
  (let [size (count height-map)
        img (BufferedImage. size size BufferedImage/TYPE_BYTE_GRAY)
        gfx (.getGraphics img)]
    
    (doseq [x (range size)
            y (range size)]
      (let [val (get-at height-map x y)]
        (.setColor gfx (Color. 
                        (clamp (float val) 0 1) 
                        (clamp (float val) 0 1)
                        (clamp (float val) 0 1)))
        (.fillRect gfx x y 1 1)))
    
    (ImageIO/write img "png" (File. "test.png"))))




;; testing
;(write-image (white-noise-map 128))

(write-image (diamond-square-map 129 1 1))

(comment
(let [map-1 (white-noise-map 64)
      map-2 (white-noise-map 64)
      blender (flat-map 64 0.2)]
  (write-image (blend-maps blender map-1 map-2)))

(let [map-1 (white-noise-map 64)
      map-2 (white-noise-map 64)]
  (write-image (blend-maps (flat-map 64 0) map-1 map-2))
  (Thread/sleep 500)
  (write-image (blend-maps (flat-map 64 0.1) map-1 map-2))
  (Thread/sleep 500)
  (write-image (blend-maps (flat-map 64 0.2) map-1 map-2))
  (Thread/sleep 500)
  (write-image (blend-maps (flat-map 64 0.3) map-1 map-2))
  (Thread/sleep 500)
  (write-image (blend-maps (flat-map 64 0.4) map-1 map-2))
  (Thread/sleep 500)
  (write-image (blend-maps (flat-map 64 0.5) map-1 map-2))
  (Thread/sleep 500)
  (write-image (blend-maps (flat-map 64 0.6) map-1 map-2))
  (Thread/sleep 500)
  (write-image (blend-maps (flat-map 64 0.7) map-1 map-2))
  (Thread/sleep 500)
  (write-image (blend-maps (flat-map 64 0.8) map-1 map-2))
  (Thread/sleep 500)
  (write-image (blend-maps (flat-map 64 0.9) map-1 map-2))
  (Thread/sleep 500)
  (write-image (blend-maps (flat-map 64 1.0) map-1 map-2))
  (Thread/sleep 500))
)
(ns heightmap-gen.utils.math)

(defn average [& nums]
  (/ (reduce + nums) (count nums)))

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

(defn random-offset [range]
  (* (- (rand) 0.5) range))

(defn round-up [n]
  (Math/round (Math/ceil n)))

(defn round-down [n]
  (Math/round (Math/floor n)))

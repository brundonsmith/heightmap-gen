(ns heightmap-gen.utils.maps)

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



;; utils
(defn new-vec
  ([length] (new-vec length (constantly 0)))
  ([length func]
   (vec (map 
         func 
         (range length)))))

(defn new-map 
  ([size] (new-map size (constantly 0)))
  ([size func] (new-vec size (fn [x] (new-vec size (fn [y] (func x y)))))))

(new-map 5 (fn [x y] (+ x y)))

(defn get-at [map x y]
  (get (get map x) y))

(defn map-map [func & maps]
  (let [size (count (first maps))]
    (new-map size
             (fn [x y] 
               (let [vals (map #(get-at % x y) maps)]
                 (apply func vals))))))


; (defn clamp


;; generators
(defn flat-map [size val]
  (new-map size (constantly val)))

(defn white-noise-map [size]
  (new-map size (fn [x y] (rand))))

; (defn perlin-map...
; (defn diamond-square-map...


;; filters
; (defn average ...
; (defn add ...
; (defn subtract ...
; (defn multiply ...
; (defn max ...
; (defn min ...
; (defn power ...
; (defn root ...
; (defn weather ...

; (defn blend [blend-map map-1 map-2]...

;; writing files

(import 'java.awt.image.BufferedImage 'javax.imageio.ImageIO 'java.awt.Color 'java.io.File)

(defn write-image [ height-map ]
  (let [size (count height-map)
        img (BufferedImage. size size BufferedImage/TYPE_BYTE_GRAY)
        gfx (.getGraphics img)]
    
    (doseq [x (range size)
            y (range size)]
      (let [val (get-at height-map x y)]
        (.setColor gfx (Color. (float val) (float val) (float val)))
        (.fillRect gfx x y 1 1)))
    
    (ImageIO/write img "png" (File. "test.png"))))


;; testing
(write-image (white-noise-map 128))

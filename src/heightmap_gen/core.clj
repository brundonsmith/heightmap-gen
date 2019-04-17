
(ns heightmap-gen.core
  (:require [heightmap-gen.maps :as maps])
  (:require [heightmap-gen.math :as math])
  (:require [heightmap-gen.diamond-square :as diamond-square]))

;; generators

; perlin

;; writing files
(import 'java.awt.image.BufferedImage 'javax.imageio.ImageIO 'java.awt.Color 'java.io.File)

(defn write-image [height-map]
  (let [size (count height-map)
        img (BufferedImage. size size BufferedImage/TYPE_BYTE_GRAY)
        gfx (.getGraphics img)]

    (doseq [x (range size)
            y (range size)]
      (let [val (maps/get-at height-map x y)]
        (.setColor gfx (Color.
                        (math/clamp (float val) 0 1)
                        (math/clamp (float val) 0 1)
                        (math/clamp (float val) 0 1)))
        (.fillRect gfx x y 1 1)))

    (ImageIO/write img "png" (File. "test.png"))))




;; testing
(defn -main []
  (write-image (diamond-square/diamond-square-map 129 1 1)))

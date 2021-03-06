
(ns heightmap-gen.utils.image
  (:require [heightmap-gen.utils.maps :as maps])
  (:require [heightmap-gen.utils.math :as math]))

(import 'java.awt.image.BufferedImage 'javax.imageio.ImageIO 'java.awt.Color 'java.io.File)

(defn write-image [file the-map]
  (let [size (maps/map-size the-map)
        img (BufferedImage. size size BufferedImage/TYPE_BYTE_GRAY)
        gfx (.getGraphics img)]

    (doseq [x (range size)
            y (range size)]
      (let [val (maps/get-at the-map x y)]
        (.setColor gfx (Color.
                        (float (math/clamp val 0 1))
                        (float (math/clamp val 0 1))
                        (float (math/clamp val 0 1))))
        (.fillRect gfx x y 1 1)))

    (ImageIO/write img "png" (File. file))))

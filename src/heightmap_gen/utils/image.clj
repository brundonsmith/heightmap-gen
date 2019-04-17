
(ns heightmap-gen.utils.image
  (:require [heightmap-gen.utils.maps :as maps])
  (:require [heightmap-gen.utils.math :as math]))

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


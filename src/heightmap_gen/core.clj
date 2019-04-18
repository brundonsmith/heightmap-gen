
(ns heightmap-gen.core
  (:require [heightmap-gen.diamond-square :as diamond-square])
  (:require [heightmap-gen.utils.image :as image]))

;; testing
(defn -main []
  (image/write-image (diamond-square/diamond-square-map 257 0.5 0.9)))

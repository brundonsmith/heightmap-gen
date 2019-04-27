
(ns heightmap-gen.core
  (:gen-class)
  (:require [heightmap-gen.utils.misc :as misc])
  (:require [heightmap-gen.perlin :as perlin])
  (:require [heightmap-gen.diamond-square :as diamond-square])
  (:require [heightmap-gen.utils.image :as image]))

;; testing
(defn -main
  ([]                               (println "Usage: heightmap-gen <file> <size> <intensity> <coarseness>"))
  ([file]                           (-main file 129))
  ([file size]                      (-main file size 0.5))
  ([file size intensity]            (-main file size intensity 0.9))
  ([file size intensity coarseness] (image/write-image file
                                                       (diamond-square/diamond-square-map
                                                        (read-string (str size))
                                                        (read-string (str intensity))
                                                        (read-string (str coarseness))))))

(ns heightmap-gen.utils.misc)

(defn pipe [val funcs]
  ((apply comp funcs) val))

(assert (=
         (pipe 0 (map (fn [num] (fn [other-num] (+ num other-num))) [1 3 2 7]))
         13))

(defn occurances [vec val]
  (reduce (fn [c element]
            (cond
              (= element val) (+ c 1)
              :else c))
          0
          vec))

(defn unique [vec val]
  (< (occurances vec val) 2))

(defn pairs [group1 group2]
  (for [a group1
        b group2]
    [a b]))

(assert (=
         (pairs [0 1] [2 3])
         [[0 2] [0 3] [1 2] [1 3]]))

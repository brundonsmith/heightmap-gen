(ns heightmap-gen.utils.misc)

(defn pipe [val funcs]
  (cond
    (= (count funcs) 0) val
    :else               (recur ((first funcs) val) (rest funcs))))

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

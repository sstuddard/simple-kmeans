(ns simple-kmeans.util
  (:require clojure.java.io))

(defn remap [m f]
  (apply merge
    (map (fn [[k v]] {k (f v)})
      m)))

(defn get-lines [fname]
  (with-open [r (clojure.java.io/reader fname)]
    (doall (line-seq r))))

(defn sample
  "Returns key of random sample from a map whose values define the distrubition."
  [m]
  (let [sum (reduce + (vals m))
        target (rand sum)]
    (loop [distribution m
           accum 0]
      (let [[id value] (first distribution)
            remainder (rest distribution)]
       (if (>= (+ accum value) target) 
          id
          (recur remainder (+ accum value)))))))

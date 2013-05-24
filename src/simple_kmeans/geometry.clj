(ns simple-kmeans.geometry
  (:use [simple-kmeans.sparsevector]
        [clojure.set]))

(defn euclidean-distance
  "Return the Euclidean distance between two sparse vectors"
  [x y]
  (let [a (union-sparse-index x y)]
    (letfn [(square [z] (* z z)) ]
      (reduce + (map #(square (- (sparse-array-value x %) (sparse-array-value y %))) a))))) 

(defn jaccard-index
  "Return the Jaccard index between two sparse vectors"
  [x y]
  (let [x-indices (set (keys x))
        y-indices (set (keys y))]
    (/ (count (intersection x-indices y-indices)) (count (union x-indices y-indices)))))

(defn jaccard-distance
  "Return the Jaccard distance between two sparse vectors"
  [x y]
  (- 1 (double (jaccard-index x y))))

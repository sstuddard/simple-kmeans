(ns simple-kmeans.sparsevector
  (:use [clojure.set]))

(defn union-sparse-index
  "Given a list of sparse vectors, return the set of indices"
  [& v]
  (reduce union (map #(set (keys %)) v)))

(defn sparse-array-value 
  "Returns the index value of the sparse vector, or 0"
  [m i] 
  (or (m i) 0))

(defn sparse-array-values 
  "Returns the indexed values of the sparse vectors"
  [v i]
  (map #(sparse-array-value % i) v))


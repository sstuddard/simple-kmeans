(ns simple-kmeans.core
  (:gen-class)
  (:use [simple-kmeans.tokenization])
  (:require clojure.java.io))

(defn get-lines [fname]
  (with-open [r (clojure.java.io/reader fname)]
    (doall (line-seq r))))

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

(defn calculate-centroid
  "Calculate new centroid based on a list of vectors"
  [v]
  (let [n (count v)]
    (apply assoc {} 
      (flatten
        (for [i (apply union-sparse-index v)
            :let [s (sparse-array-values v i)]]
          [i (/ (reduce + s) n)])))))
  

(defn -main
  "Run k-means clustering on documents from a line-delimited file"
  [& args]
  (let [filepath (first args)
        data (get-lines filepath)]
    (println "Found" (count data) "documents.")
    (println "Tokenized docs: " (map tokenize data))))

(ns simple-kmeans.clustering
  (:use [clojure.set]
        [simple-kmeans.sparsevector])
  (:require [clojure.math.numeric-tower :as math]))

(defn calculate-centroid
  "Calculate new centroid based on a list of vectors"
  [v]
  (let [n (count v)]
    (apply assoc {} 
      (flatten
        (for [i (apply union-sparse-index v)
            :let [s (sparse-array-values v i)]]
          [i (/ (reduce + s) n)])))))

(defn assign-to-centroid
  "Returns the nearest centroid to a vector"
  [v c d]
  (first (apply min-key second (map #(vector % (d v %)) c))))

(defn construct-cluster
  "Builds a cluster given centroids, vectors, and distance function"
  [c v d]
  (let [assigned (map #(vector (assign-to-centroid % c d) %) v)]
    (reduce #(assoc %1 (first %2) (conj (get %1 (first %2) []) (second %2))) {} assigned)))

(defn update-cluster
  "Return an updated cluster"
  [e d]
  (let [x (vals e)
        v (flatten x)
        c (map calculate-centroid x)]
    (construct-cluster c v d)))

(defn cluster-error
  "Returns the error for a given cluster and distance function"
  [c d]
  (reduce + (flatten (map (fn [x] (map #(d x %) (c x))) (keys c)))))

(defn cluster
  "Return result of kmeans clustering, centroids c is k dimensions, 
    d is distance function, max m iterations to converge"
  [c v d m]
  (let [initial (construct-cluster c v d)]
    (println "Clustering with" (count c) "centroids...")
    (loop [cluster initial
           countdown m
           error (cluster-error initial d)]
      (println "\tCluster error" error)
      (let [new-cluster (update-cluster cluster d)
            new-error (cluster-error new-cluster d)]
        (if (or (= 0 countdown) (< (math/abs (- error new-error)) 0.0001))
          new-cluster
          (recur new-cluster (dec countdown) new-error))))))
        
(defn optimize-cluster
  "Runs through a list of centroid choices, returning the argmin cluster error"
  [c v d m]
  (let [clusters (map #(cluster % v d m) c)]
    (apply min-key #(cluster-error % d) clusters)))

(defn random-centroids
  "Returns k vectors from the supplied vectors"
  [k v]
  (letfn [(sample [f]
            (let [shuffled (shuffle f)]
              (cons (first shuffled) (lazy-seq (sample (rest shuffled))))))]
    (println "Generating random")
    (take k (sample v))))

(defn auto-centroids
  "Generates automatic centroids with k in (2..n/2)"
  [v]
  (let [max-k (int (/ (count v) 2))
        k (map inc (range 1 max-k))]
    (map #(random-centroids % v) k)))

(defn gen-centroids
  "Builds a sequence of centroid sets"
  [auto k v n]
  (reduce concat 
    (take n (repeat 
      (if auto
        (auto-centroids v)
        (list (random-centroids k v)))))))


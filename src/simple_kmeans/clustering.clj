(ns simple-kmeans.clustering
  (:use [clojure.set]
        [simple-kmeans.sparsevector]
        [simple-kmeans.util])
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

(defn cluster-vector-errors
  "Returns a map of errors per vector for the given cluster"
  [c d]
  (apply merge (map (fn [[x v]] (reduce #(assoc %1 %2 (d x %2)) {} v)) c)))

(defn cluster-error
  "Returns the error for a given cluster and distance function"
  [c d]
  (reduce + (vals (cluster-vector-errors c d))))

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

(defn kmeans-plusplus
  "Given k, distance fn, and vectors, generate centroids via k-means++ algorithm"
  [k v d]
  (let [initial-centroids (conj [] (first (shuffle v)))]
    (loop [centroids initial-centroids
           more-k (dec k)]
      (if (= more-k 0)
        centroids
        (let [cluster (construct-cluster centroids v d)
              sq-errors (remap (cluster-vector-errors cluster d) #(* % %))
              new-centroid (sample sq-errors)]
          (recur (conj centroids new-centroid) (dec more-k)))))))

(defn gen-centroids
  "Builds a sequence of centroid sets"
  [mink maxk v n d]
  (for [k (range mink (inc maxk))
        i (range n)]
    (kmeans-plusplus k v d)))


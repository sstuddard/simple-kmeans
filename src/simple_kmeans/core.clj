(ns simple-kmeans.core
  (:gen-class)
  (:use [simple-kmeans.tokenization]
        [simple-kmeans.sparsevector]
        [simple-kmeans.clustering]
        [simple-kmeans.geometry]
        [clojure.tools.cli :only [cli]]
        [clojure.set])
  (:require clojure.java.io))

(defn get-lines [fname]
  (with-open [r (clojure.java.io/reader fname)]
    (doall (line-seq r))))

(defn document-lookup
  "Build a document lookup given the vectors and keys"
  [v k]
  (zipmap v k))

(defn format-term
  "Prints a term and value"
  [t v l]
  (format "(%s: %.5f)" (l t) (double v)))

(defn format-doc-vector
  "Print a document vector with a lookup"
  [d l]
  (let [sorted (into (sorted-map-by (fn [key1 key2]
                         (compare [(get d key2) key2]
                                  [(get d key1) key1]))) d)]
    (map #(format-term (first %1) (second %1) l) sorted)))

(defn gen-centroids
  "Builds a sequence of centroid sets"
  [auto k v n]
  (reduce concat 
    (take n (repeat 
      (if auto
        (auto-centroids v)
        (list (random-centroids k v)))))))

(defn -main
  "Run k-means clustering on documents from a line-delimited file. The first token of a line 
    is the identifier for the document."
  [& args]
  (let [opts (cli args
               ["-k" "--clusters" "Specify number of clusters" :parse-fn #(Integer. %) :default 2] 
               ["-f" "--file" "The hostname"]
               ["-d" "--documentkey" "The first token of a document is its key" :flag true]
               ["-c" "--centroids" "Output centroids" :flag true]
               ["-a" "--autok" "Automatically optimize k" :flag true]
               ["-n" "--randomruns" "Run n times per centroids selection" :parse-fn #(Integer. %) :default 1]
               ["-m" "--iterations" "The max iterations for convergence" :parse-fn #(Integer. %) :default 10])]

    (let [filepath ((first opts) :file)
          data (get-lines filepath)
          documents (map tokenize data)
          vocabulary (get-vocabulary documents)
          vectors (map #(get-term-vector % (vocab-term-lookup vocabulary)) documents)
          term-lookup (vocab-index-lookup vocabulary)
          runs ((first opts) :randomruns)
          k ((first opts) :clusters)
          autok ((first opts) :autok)
          convergence-iterations ((first opts) :iterations)
          centroids (gen-centroids autok k vectors runs)
          document-key-included ((first opts) :documentkey)
          output-centroids ((first opts) :centroids)
          doc-lookup (document-lookup vectors (if document-key-included (map first documents) (range)))
          result (optimize-cluster centroids vectors euclidean-distance convergence-iterations)]
      (println "Found" (count data) "documents.")
      (println "Document key in documents: " document-key-included)
      (println "Testing" runs "times")
      (println "Found" (count (keys result)) "clusters")
      (println (format "Results (error: %.3f)" (double (cluster-error result euclidean-distance))))
      (doseq [[k v] result]
        (if output-centroids (println "Centroid:" (format-doc-vector k term-lookup)))
        (println "Documents:" (map doc-lookup v))
        (println "")))))


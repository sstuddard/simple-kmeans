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

(defn -main
  "Run k-means clustering on documents from a line-delimited file. The first token of a line 
    is the identifier for the document."
  [& args]
  (let [opts (cli args
               ["-k" "--clusters" "Specify number of clusters" :parse-fn #(Integer. %)] 
               ["-f" "--file" "The hostname"]
               ["-n" "--randomruns" "Do runs n times" :parse-fn #(Integer. %) :default 1]
               ["-m" "--iterations" "The max iterations for convergence" :parse-fn #(Integer. %) :default 10])]

    (let [filepath ((first opts) :file)
          data (get-lines filepath)
          documents (map tokenize data)
          vocabulary (get-vocabulary documents)
          vectors (map #(get-term-vector % (vocab-term-lookup vocabulary)) documents)
          runs ((first opts) :randomruns)
          k ((first opts) :clusters)
          convergence-iterations ((first opts) :iterations)
          distance-function euclidean-distance]
      (def centroids (take runs (repeat (random-centroids k vectors))))
      (def result (optimize-cluster centroids vectors distance-function convergence-iterations))
      (println "Found" (count data) "documents.")
      (println "Testing" runs "times")
      (println "Results (error:" (cluster-error result distance-function) "):" result))))

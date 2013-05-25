(ns simple-kmeans.core
  (:gen-class)
  (:use [simple-kmeans.tokenization]
        [simple-kmeans.sparsevector]
        [simple-kmeans.clustering]
        [simple-kmeans.geometry]
        [simple-kmeans.document]
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
               ["-k" "--clusters" "Specify number of clusters" :parse-fn #(Integer. %) :default 2] 
               ["-f" "--file" "The hostname"]
               ["-d" "--documentkey" "The first token of a document is its key" :flag true]
               ["-c" "--centroids" "Output centroids" :flag true]
               ["-j" "--jaccard" "Use Jaccard distance" :flag true]
               ["-v" "--verbose" "Verbose output" :flag true]
               ["-a" "--autok" "Automatically optimize k" :flag true]
               ["-n" "--randomruns" "Run n times per centroids selection" :parse-fn #(Integer. %) :default 1]
               ["-m" "--iterations" "The max iterations for convergence" :parse-fn #(Integer. %) :default 10])]

    (let [filepath ((first opts) :file)
          runs ((first opts) :randomruns)
          k ((first opts) :clusters)
          autok ((first opts) :autok)
          verbose ((first opts) :verbose)
          jaccard ((first opts) :jaccard)
          document-key-included ((first opts) :documentkey)
          output-centroids ((first opts) :centroids)
          convergence-iterations ((first opts) :iterations)
          data (get-lines filepath)
          tokenized (map tokenize data)
          distance-function (if jaccard jaccard-distance euclidean-distance)
          documents (map termify (if document-key-included (map rest tokenized) tokenized))
          vocabulary (get-vocabulary documents)
          vectors (map #(get-term-vector % (vocab-term-lookup vocabulary)) documents)
          term-lookup (vocab-index-lookup vocabulary)
          doc-lookup (document-lookup vectors (if document-key-included (map first tokenized) (range)))
          centroids (gen-centroids autok k vectors runs)
          result (optimize-cluster centroids vectors distance-function convergence-iterations)]
      (if verbose
        (do
          (println "Found" (count data) "documents.")
          (println "Document key in documents: " document-key-included)
          (println "Testing" runs "times")
          (println "Initial centroids per run:")
          (doseq [c centroids]
            (println (map doc-lookup c)))
          (println "Found" (count (keys result)) "clusters")
          (println (format "Results (error: %.3f)" (double (cluster-error result distance-function))))
          (doseq [[k v] result]
            (if output-centroids (println "Centroid:" (format-doc-vector k term-lookup)))
            (println "Documents:" (map doc-lookup v))
            (println "")))
        (do
          (doseq [[k v] result]
            (apply println (map doc-lookup v))))))))



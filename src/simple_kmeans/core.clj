(ns simple-kmeans.core
  (:gen-class)
  (:use [simple-kmeans.tokenization]
        [simple-kmeans.sparsevector]
        [simple-kmeans.clustering]
        [simple-kmeans.geometry]
        [simple-kmeans.document]
        [simple-kmeans.util]
        [clojure.tools.cli :only [cli]]
        [clojure.set]))

(defn build-data
  "Given a file, prepares all the data necessary for clustering."
  [filepath document-key-included]
  (let [data (get-lines filepath)
        tokenized (map tokenize data)
        documents (map termify (if document-key-included (map rest tokenized) tokenized))
        vocabulary (get-vocabulary documents)
        gidf (idf-fn documents)
        index-lookup (vocab-term-lookup vocabulary)
        term-lookup (vocab-index-lookup vocabulary)
        raw-vectors (map #(get-term-frequency-vector % index-lookup) documents)
        vectors (map #(normalize (get-tfidf-vector % gidf)) raw-vectors)
        doc-lookup (document-lookup vectors (if document-key-included (map first tokenized) (range)))
        ]
  {
    :data data
    :tokenized tokenized
    :documents documents
    :vocabulary vocabulary
    :gidf gidf
    :term-to-index index-lookup
    :raw-vectors raw-vectors
    :vectors vectors
    :index-to-term term-lookup
    :doc-lookup doc-lookup
  }))

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
               ["-n" "--randomruns" "Run n times per centroids selection" :parse-fn #(Integer. %) :default 1]
               ["-m" "--iterations" "The max iterations for convergence" :parse-fn #(Integer. %) :default 10])]

    (let [filepath ((first opts) :file)
          runs ((first opts) :randomruns)
          k ((first opts) :clusters)
          verbose ((first opts) :verbose)
          jaccard ((first opts) :jaccard)
          document-key-included ((first opts) :documentkey)
          output-centroids ((first opts) :centroids)
          convergence-iterations ((first opts) :iterations)
          build (build-data filepath document-key-included) 
          distance-function (if jaccard jaccard-distance euclidean-distance)
          vectors (:vectors build)
          doc-lookup (:doc-lookup build)
          index-to-term (:index-to-term build)
          centroids (gen-centroids k k vectors runs distance-function)
          result (optimize-cluster centroids vectors distance-function convergence-iterations)]
      (if verbose
        (do
          (println "Found" (count (:data build)) "documents.")
          (println "Document key in documents: " document-key-included)
          (println "Testing" runs "times")
          (println "Initial centroids per run:")
          (doseq [c centroids]
            (println (map doc-lookup c)))
          (println "Found" (count (keys result)) "clusters")
          (println (format "Results (error: %.3f)" (double (cluster-error result distance-function))))
          (doseq [[k v] result]
            (if output-centroids (println "Centroid:" (format-doc-vector k index-to-term)))
            (println "Documents:" (map doc-lookup v))
            (println "")))
        (do
          (doseq [[k v] result]
            (apply println (map doc-lookup v))))))))



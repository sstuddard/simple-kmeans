(ns simple-kmeans.core
  (:gen-class)
  (:use [simple-kmeans.tokenization]
        [simple-kmeans.sparsevector]
        [simple-kmeans.clustering]
        [simple-kmeans.geometry]
        [clojure.set])
  (:require clojure.java.io))

(defn get-lines [fname]
  (with-open [r (clojure.java.io/reader fname)]
    (doall (line-seq r))))

(defn -main
  "Run k-means clustering on documents from a line-delimited file. The first token of a line 
    is the identifier for the document."
  [& args]
  (let [filepath (first args)
        data (get-lines filepath)
        documents (map tokenize data)
        vocabulary (get-vocabulary documents)
        vectors (map #(get-term-vector % (vocab-term-lookup vocabulary)) documents)]
    (println "Found" (count data) "documents.")
    (println "Vectors: " vectors)))

(ns simple-kmeans.tokenization
  (:use [clojure.set]))

(defn remove-symbols [text]
  (clojure.string/replace text #"[!@#$%^&*()_\+-=,.<>/?\\\[\]|\"]" " "))

(defn stopword? [word]
  (let [stopwords ["the" "he" "she" "it" "and" "or" "if" "but" "i" "is" "me" "can" "of" "as" "to"]]
    (some #(= word %) stopwords)))

(defn tokenize 
  "Tokenizes a string, removing symbols, stopwords, and lowercasing."
  [text]
  (filter #(not (stopword? %)) 
    (re-seq #"\w+" text)))

(defn get-vocabulary 
  "Generates a set that is the vocabulary of the documents"
  [documents]
  (vec (reduce union (map set documents))))

(defn vocab-term-lookup
  "Generates term to index lookup for vocabulary"
  [vocabulary]
  (zipmap vocabulary (range)))

(defn vocab-index-lookup
  "Generates index to term lookup for vocabulary"
  [vocabulary]
  (zipmap (range) vocabulary))

(defn get-term-vector
  "Given a list of terms and a vocabulary term lookup, create a sparse term vector"
  [terms term-lookup]
  (let [freqs (frequencies terms)]
    (reduce (fn [new-map [k v]] (assoc new-map (term-lookup k) v)) {} freqs)))


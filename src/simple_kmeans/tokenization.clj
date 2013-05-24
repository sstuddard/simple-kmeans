(ns simple-kmeans.tokenization
  (:use [clojure.set]
        [simple-kmeans.sparsevector]))

(defn stopword? [word]
  (let [stopwords ["the" "he" "she" "it" "and" "or" "if" "but" "i" "is" "me" "can" "of" "as" "to" "a" "in"
                    "by" "are" "we" "was" "this" "for" "that" "these" "be" "than" "then" "from" "an" "his" "hers"
                    "her" "with" "says" "they" "on" "got" "what" "do" "there" "so" "has" "you" "who" "have"]]
    (some #(= word %) stopwords)))

(defn tokenize
  "Tokenizes a string into terms"
  [text]
  (re-seq #"\w+" text))

(defn termify
  "Filters tokens, removing stopwords and lowercasing."
  [tokens]
  (filter #(not (stopword? %)) 
    (map clojure.string/lower-case tokens)))

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
    (normalize (reduce (fn [new-map [k v]] (assoc new-map (term-lookup k) v)) {} freqs))))


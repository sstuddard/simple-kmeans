# simple-kmeans

A simple kmeans++ implementation in Clojure.  This is not industrial strength or performant.  

While the clustering is generalized to sparse vectors (represented as maps), the executable is built to cluster text documents.  Tokenization is naive, no stemming.  The default distance measurement is Euclidean, but Jaccard distance is supported.

## Usage

    $ java -jar simple-kmeans-0.1.0-standalone.jar [args]

## Options

  ["-k" "--clusters"]
Defines the number of clusters 

  ["-f" "--file"]
Defines the input file to use, one line per document

  ["-d" "--documentkey"]
Indicates the first token on a line is the identifier of the document, otherwise they are numbered

  ["-c" "--centroids"]
Output the raw centroid term vectors

  ["-j" "--jaccard"]
Use Jaccard distance measure, Euclidean is default

  ["-v" "--verbose"]
Verbose loggin

  ["-n" "--randomruns"]
Defines the number of iterations per selection of k

  ["-m" "--iterations"]
Defines the maximum number of iterations per k for convergence

## Examples

  -f "./my_corpus.txt" -k 10 -n 10 -m 100 -d -v

Using the file my_corpus.txt, cluster with 10 clusters, running 10 iterations and picking the best.  Maximum iterations per clustering is 100, the first token of each line is an identifier for the document, and verbose logging is on.

## License

Copyright © 2013 Shayne Studdard

Distributed under the Eclipse Public License, the same as Clojure.

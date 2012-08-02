(ns golfure.core
  (:use golfure.interpreter)
  (:require [golfure.builtins :as builtins]))

(defn execute
  ([program initial-stack symbols]
    (execute-block
      (string-to-block program symbols)
      initial-stack
      symbols))
  ([program initial-stack]
    (execute program initial-stack builtins/default-symbols))
  ([program]
    (execute program [])))

(defn -main
  ([source input]
    (let [script (clojure.java.io/file source)
          input-file (clojure.java.io/file input)]
      (execute
        (if (.isFile script)
          (slurp script)
          source)
        (if (.isFile input-file)
          [(apply str (line-seq (clojure.java.io/reader input-file)))]
          input))))
  ([source]
    (-main source *in*)))
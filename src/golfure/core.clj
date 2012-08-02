(ns golfure.core
  (:require [golfure.builtins :as builtins])
  (:use [clojure.pprint :only (pprint)]))

(def golfscript-symbols
  {"~" builtins/tilde
   "`" builtins/grave-accent
   "!" builtins/shebang
   "@" builtins/at
   "$" builtins/dollar
   "+" builtins/plus})

(def debug-symbols
  (into
    golfscript-symbols
    {"stack" (fn [stack symbols]
               (println "Stack: ")
               (pprint stack)
               stack)
     "symbols" (fn [stack symbols]
                 (println "Symbols: ")
                 (pprint symbols)
                 stack)}))

;;;

(defn execute
  "Execute a GolfureScript program."
  ([program initial-stack initial-symbols]
    ((golfure.lang/string-to-block program initial-symbols)
      initial-stack
      initial-symbols))
  ([program initial-stack]
    (execute program initial-stack debug-symbols))
  ([program]
    (execute program [])))

(defn -main
  "Read a GolfScript 'source' and execute it, initializing
  the stack to 'input'.

  Both 'input' and 'source' can be either files or strings.

  If 'input' is missing, stdin is read."
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
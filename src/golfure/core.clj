(ns golfure.core
  (:require [golfure.builtins :as builtins])
  (:use clojure.tools.cli)
  (:use [clojure.pprint :only (pprint)])
  (:gen-class))

(def golfscript-symbols
  {"~" builtins/tilde
   "`" builtins/grave-accent
   "!" builtins/shebang
   "@" builtins/at
   "$" builtins/dollar
   "+" builtins/plus
   "-" builtins/minus
   "*" builtins/asterisk})

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
  [& args]
  (let [[options args banner]
        (cli args
             ["-r" "--run" "Run string (ignores [code])"]
             ["-i" "--input" "Input file (stdin if missing)"]
             ["-s" "--stack" "Stack (ignores --input)"]
             ["-y" "--symbols" "Symbol table [golscript, debug]" :default "golfscript"]
             ["-h" "--help" "Show this message" :default false :flag true])]
    (when (:help options)
      (println banner)
      (System/exit 0))
    (println (apply str (flatten (reverse
      (execute
        (or (:run options)
            (slurp (first args)))
        (or (and (:stack options)
                 ((golfure.lang/string-to-block (:stack options) {}) [] {}))
            (and (:input options)
                 (slurp (:input options)))
            [])
        (eval (symbol
                (str "golfure.core/"
                     (:symbols options)
                     "-symbols"))))))))))
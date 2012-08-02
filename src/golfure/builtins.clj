(ns golfure.builtins
  (:use [golfure.lang :only (builtin)])
  (:use [clojure.pprint :only (pprint)])
  (:require [golfure.interpreter :as interpreter]))

(builtin tilde
  ([:int] [x]
    (cons (bit-not x) stack))
  ([:str] [x]
    (interpreter/execute-block
      (interpreter/string-to-block x symbols)
      stack
      symbols))
  ([:blk] [x]
    (interpreter/execute-block x stack symbols))
  ([:arr] [x]
    (reduce cons (reverse x) stack)))

(builtin grave-accent
  ([:str] [x]
    (cons (str \" (clojure.string/escape x {\" "\\\""}) \") stack))
  ([:blk] [x]
    (cons (apply str (map :token x)) stack))
  ([_] [x]
    (cons (str x) stack)))

(builtin shebang
  ([:int] [x]
    (cons (if (zero? x) 1 0) stack))
  ([_] [x]
    (cons (if (empty? x) 1 0) stack)))

(builtin at
  ([_ _ _] [a b c]
    (concat [c a b] stack)))

(builtin dollar
  ([:int] [n]
    (cons (nth stack n) stack))
  ([:str] [s]
    (cons (apply str (sort s)) stack))
  ([:blk :arr] [b a]
    (cons (map second
               (sort (map
                       #(vector
                          interpreter/execute-block b [%] symbols
                          %)
                       a)))
          stack))
  ([:arr] [a]
    (cons (sort a) stack)))

(builtin plus
  ([:int :int] [a b]
    (cons (+ a b) stack))
  ([:str :str] [a b]
    (cons (str a b) stack)))

(def default-symbols
  {"stack" (fn [stack symbols]
             (println "Stack: ")
             (pprint stack)
             stack)
   "symbols" (fn [stack symbols]
               (println "Symbols: ")
               (pprint symbols)
               stack)
   "~" tilde
   "`" grave-accent
   "!" shebang
   "@" at
   "$" dollar
   "+" plus})
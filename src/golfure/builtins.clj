(ns golfure.builtins
  (:use [golfure.lang :only (builtin)])
  (:use [clojure.pprint :only (pprint)])
  (:require [golfure.interpreter :as interpreter]))

(defmacro builtin
  "Macro for builtin definition, defined as:

    (builtin def-name
      docstring?
      ([:type arg1 :type arg2 ... :type argN]
        body1)
      ...
      ([:type arg1 :type arg2 ... :type argN]
        bodyN))

  Method precedence is top-down."
  [builtin-name & methods]
  (assert (every? #(= (count (first %)) (count (fnext %))) methods))
  `(defn ~builtin-name [~'whole-stack ~'symbols]
     (match [(map golfure.lang/golf-type ~'whole-stack)]
            ~@(apply concat
                (for [[types# vars# & body#] methods]
                  `[[~(list `[~@types# & ~'rest] :seq)]
                    (let [[~@vars# & ~'stack] ~'whole-stack] ~@body#)]))
            [~'_] (throw (Exception. (str "Couldn't match " ~'whole-stack))))))

(builtin tilde
  ([:int] [x]
    (cons (bit-not x) stack))
  ([:str] [x]
    ((interpreter/string-to-block x symbols)
      stack
      symbols))
  ([:blk] [x]
    (x stack symbols))
  ([:arr] [x]
    (reduce cons (reverse x) stack)))

(builtin grave-accent
  ([:str] [x]
    (cons (str \" (clojure.string/escape x {\" "\\\""}) \") stack))
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
                       #(vector (b [%] symbols) %)
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
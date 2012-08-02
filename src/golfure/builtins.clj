(ns golfure.builtins
  "Builtin definition (DSL-like) + some predefined builtins.
    See: http://www.golfscript.com/golfscript/builtin.html"
  (:use [clojure.core.match :only (match)])
  (:require [golfure.lang :as lang]))

(defmacro builtin
  "Macro for builtin definition, defined as:

    (builtin def-name
      docstring?
      ([:type1 :type2 ... :typeN] [var1 var2 ... :varN]
        body1)
      ...
      ([:type1 :type2 ... :typeN] [var1 var2 ... :varN]
        bodyN))

  Method precedence is top-down."
  [builtin-name & methods]
  (let [docstring (first methods)
        docstring? (and docstring
                        (= (type docstring) String))
        methods (if docstring?
                  (rest methods)
                  methods)]
    (assert (every? #(= (count (first %))
                        (count (fnext %)))
                    methods)
            "(count types) = (count args)")
    `(defn ~builtin-name
       ~(if docstring? docstring "")
       ~['whole-stack 'symbols]
       (match
         [(map golfure.lang/golf-type ~'whole-stack)]
         ~@(apply concat
                  (for [[types# vars# & body#] methods]
                    `[[~(list `[~@types# & ~'rest] :seq)]
                      (let [[~@vars# & ~'stack] ~'whole-stack] ~@body#)]))
         [~'_] (throw (Exception. (str "Couldn't match " ~'whole-stack)))))))

;;;

(builtin tilde
  "~ (args: 1)
  ------------
  Bitwise not for integers.
    5~ -> -6
  Evaluate for strings and blocks.
    \"1 2+\"~ -> 3
    {1 2+}~ -> 3
  Dump elements for arrays.
    [1 2 3]~ -> 1 2 3"
  ([:int] [x]
    (cons (bit-not x) stack))
  ([:str] [x]
    ((lang/string-to-block x symbols)
      stack
      symbols))
  ([:blk] [x]
    (x stack symbols))
  ([:arr] [x]
    (reduce cons (reverse x) stack)))

(builtin grave-accent
  "` (args: 1)
  ------------
  The inverse of ~, generates a string that if evaluated
  returns the original. Gets its name from python where
  `...` has a similar effect.
    1` -> \"1\"
    [1 [2] 'asdf']` -> \"[1 [2] \\\"asdf\\\"]\"
    \"1\"` -> \"\\\"1\\\"\"
    {1}` -> \"{1}\""
  ([:str] [x]
    (cons (str \" (clojure.string/escape x {\" "\\\""}) \") stack))
  ([_] [x]
    (cons (str x) stack)))

(builtin shebang
  "! (args: 1)
  ------------
  0 [] \"\" {} yield 1, everything else 0"
  ([:int] [x]
    (cons (if (zero? x) 1 0) stack))
  ([_] [x]
    (cons (if (empty? x) 1 0) stack)))

(builtin at
  "@ (args: 3)
  ------------
  Rotates the top 3 elements of the stack so that the 3rd down is now on top."
  ([_ _ _] [a b c]
    (concat [c a b] stack)))

(comment
  "# (args:)
   ---------
   Not actually a built in variable, it is part of the syntax ignoring everything until newline.")

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
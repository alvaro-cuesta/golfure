(ns golfure.lang
  "GolfureScript language d"
  (:use [clojure.core.match :only [match]]))

(defrecord Element [fun token])

(defn golf-type [x]
  "Returns x's GolfureScript type, one of:
     :int, :str, :blk, :arr"
  (cond
    (= (type x) Long) :int
    (= (type x) String) :str
    (coll? x) (if (every? #(= (type %) Element) x)
                :blk
                :arr)))

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
(ns golfure.test.lang
  (:require [golfure.lang :as lang])
  (:use [golfure.core :only (golfscript-symbols)])
  (:use clojure.test))



(comment
(defrecord Element [fun token]
  java.lang.Object
  (toString [this]
    (:token this))
  clojure.lang.IFn
  (invoke [this] (:fun this))
  (invoke [this stack symbols]
    ((:fun this) stack symbols))
  (applyTo [this args]
    (clojure.lang.AFn/applyToHelper this args)))

(defn- compile-token
  "Compiles 'token' using 'symbols' as a symbol map.

  If 'token' isn't found in the symbol map, a function is
  returned for later execution.

  This function will try to evaluate in a future symbol-map.
  If it isn't found there either, it will try to evaluate
  the token using Clojure's eval to parse strings/ints.

  Public altrnative:
    ((token-to-element token symbols))"
  [token symbols]
  (or (symbols token)
      (fn [stack symbols]
        (or (symbols token)
            (try (let [value (eval (read-string token))]
                   (if (and value (golf-type value))
                     (cons value stack)
                     stack))
              (catch RuntimeException e
                stack))))))

(defn token-to-element
  "Each element is a function which takes args:
    [p1 p2 ...  pn & r]  ; the whole stack
    symbols              ; a symbol map 
   And returns a new stack (usually a cons operation):
    (cons (+ p1 p2) r)"
  [token symbols]
  (Element. (compile-token token symbols) token))

;;;

(defrecord Block [elements]
  java.lang.Object
  (toString [this]
    (apply str "{" (apply str (map :token elements)) "}"))
  clojure.lang.IFn
  (invoke [this] elements)
  (invoke [this stack symbols]
    (execute-block elements stack symbols))
  (applyTo [this args]
    (clojure.lang.AFn/applyToHelper this args)))
)

(deftest test-execute-block
  (is (= [6]
        (#'lang/execute-block
          ((lang/string-to-block "3 3 +" golfscript-symbols))
          []
          golfscript-symbols))
      "Simple expression")
  
  (is (= [6]
         (#'lang/execute-block
           ((first
              (#'lang/execute-block
                ((lang/string-to-block "{3 3 +}" golfscript-symbols))
                []
                golfscript-symbols)))
           []
           golfscript-symbols))
      "Block syntax")
  
  (is (= [6]
         (#'lang/execute-block
           ((first
              (#'lang/execute-block
                ((lang/string-to-block "{3 {3}~ +}" golfscript-symbols))
                []
                golfscript-symbols)))
           []
           golfscript-symbols))
      "Nested block syntax")
  
  (is (= [6]
         (#'lang/execute-block
           ((lang/string-to-block "3:a a +" golfscript-symbols))
           []
           golfscript-symbols))
      "Assignment"))

(deftest test-string-to-block
  (is (= ((lang/string-to-block "3 3 +" golfscript-symbols) [] {})
         [6])
      "Simple expression")
  (is (= ((lang/string-to-block "3 3 # - sa89g ioug\n+" golfscript-symbols) [] {})
         [6])
      "Ignore comments until \\n"))

;;

(deftest test-golf-type
  (is (= :int (lang/golf-type 3)))
  (is (= :int (lang/golf-type 333333333333333333333333)))
  (is (= :str (lang/golf-type "")))
  (is (= :str (lang/golf-type "this is a string")))
  (is (= :blk (lang/golf-type (lang/string-to-block
                                "this is a block1 12+124`+`´ç´g"
                                golfscript-symbols))))
  (is (= :arr (lang/golf-type [])))
  (is (= :arr (lang/golf-type [1 2 3])))
  (is (thrown-with-msg? RuntimeException #"is not a GolfScript type"
                        (lang/golf-type '(1 2 3 4 5)))
      "list")
  (is (thrown-with-msg? RuntimeException #"is not a GolfScript type"
                        (lang/golf-type `(+ 1 3)))
      "quoted list")
  (is (thrown-with-msg? RuntimeException #"is not a GolfScript type"
                        (lang/golf-type {:test :map}))
      "map")
  (is (thrown-with-msg? RuntimeException #"is not a GolfScript type"
                        (lang/golf-type #{1 3 2 5 8 "what"}))
      "set")
  (is (thrown-with-msg? RuntimeException #"is not a GolfScript type"
                        (lang/golf-type +))
      "fn"))

(deftest test-coerce
  (is (= 3 (lang/coerce 3 :int {}))
      ":int -> :int")
  (testing "to :arr"
    (is (= [3] (lang/coerce 3 :arr {}))
        ":int -> :arr")
    (is (= [3] (lang/coerce [3] :arr {}))
        ":arr -> :arr"))
  (testing "to :str"
    (is (= "3" (lang/coerce 3 :str {}))
        ":int -> :str")
    (is (= "234" (lang/coerce [50 51 52] :str {}))
        ":arr -> :str")
    (is (= "234" (lang/coerce "234" :str {}))
        ":str -> :str"))
  (testing "to :blk"
    (is (= [1] ((lang/coerce 1 :blk {}) [] {}))
        ":int -> :blk")
    (is (= [4 3 2 1] ((lang/coerce [1 2 3 4] :blk golfscript-symbols) [] golfscript-symbols))
        ":arr -> :blk - simple expression")
    (is (= [234 5 1] ((lang/coerce [1 2 3 "+" [50 51 52]] :blk golfscript-symbols) [] golfscript-symbols))
        ":arr -> :blk - mixed types")
    (is (= [7 2 1]
           ((lang/coerce "1 2 3 4+" :blk golfscript-symbols)
             []
             golfscript-symbols))
        ":str -> :blk")
    (is (= [7 2 1]
           ((lang/coerce (lang/string-to-block
                           "1 2 3 4+"
                           golfscript-symbols)
                         :blk golfscript-symbols)
             []
             golfscript-symbols))
        ":blk -> :blk"))
  
    (testing "invalid coercion -"
      (is (thrown-with-msg? RuntimeException #"Invalid coercion"
                            (lang/coerce [1 2 3] :int {} ))
          ":arr -> :int")
      (is (thrown-with-msg? RuntimeException #"Invalid coercion"
                            (lang/coerce "hola" :int {}))
          ":str -> :int")
      (is (thrown-with-msg? RuntimeException #"Invalid coercion"
                            (lang/coerce "hola" :arr {}))
          ":str -> :arr")
      (is (thrown-with-msg? RuntimeException #"Invalid coercion"
                            (lang/coerce (lang/string-to-block
                                           "1 2 3+"
                                           golfscript-symbols) :int {}))
          ":blk -> :int")
      (is (thrown-with-msg? RuntimeException #"Invalid coercion"
                            (lang/coerce (lang/string-to-block
                                           "1 2 3+"
                                           golfscript-symbols) :arr {}))
          ":blk -> :arr")
      (is (thrown-with-msg? RuntimeException #"Invalid coercion"
                            (lang/coerce (lang/string-to-block
                                           "1 2 3+"
                                           golfscript-symbols) :str {}))
          ":blk -> :str")))
(ns golfure.test.builtins
  (:require [golfure.builtins :as builtins])
  (:require [golfure.lang :as lang])
  (:use [golfure.core :only (golfscript-symbols)])
  (:use clojure.test))

(deftest test-tilde
  (testing "tilde (~)"
    (testing "int (bitwise not)"
      (is (= [-1] (builtins/tilde [0] {}))
          "zero")
      (is (= [0] (builtins/tilde [-1] {}))
          "-1")
      (is (= [-6] (builtins/tilde [5] {}))
          "positive")
      (is (= [5] (builtins/tilde [-6] {}))
          "negative"))

    (is (= [6] (builtins/tilde
                 ["3 3 +"]
                 golfscript-symbols))
        "string (evaluate)")
    
    (is (= [6] (builtins/tilde
                 [(lang/string-to-block
                    "3 3 +"
                    golfscript-symbols)]
                 golfscript-symbols))
        "block (evaluate)")
    
    (is (= [5 4 3 2 1] (builtins/tilde [[1 2 3 4 5]] {}))
        "array (dump elements)")))
  
(deftest test-grave-accent
  (testing "grave-accent (`)"
    (is (= ["0"] (builtins/grave-accent [0] {}))
        "int (to string)")

    (testing "string (to quoted and escaped string)"
      (is (= ["\"test string\""] (builtins/grave-accent ["test string"] {})))
      (is (= ["\"test \\\"string\""] (builtins/grave-accent ["test \"string"] {}))
          "needs escaping"))
    
    (testing "block (to string)"
      (is (= ["3 3 +"] (builtins/grave-accent
                         [(lang/string-to-block
                            "3 3 +"
                            golfscript-symbols)]
                         golfscript-symbols)))
      (is (= ["3 3 +"] (builtins/grave-accent
                         [(lang/string-to-block
                            "3 3 +# ignore this comment"
                            golfscript-symbols)]
                         golfscript-symbols))
          "ignore comments"))
    
    (is (= ["[1 2 3 4]"] (builtins/grave-accent [[1 2 3 4]] {}))
        "array (to string)")))

(deftest test-shebang
  (testing "shebang (!)"
    (testing "int (0 = 1, else = 0)"
      (is (= [1] (builtins/shebang [0] {}))
          "zero")
      (is (= [0] (builtins/shebang [5] {}))
          "positive")
      (is (= [0] (builtins/shebang [-5] {}))
          "negative")))
    (testing "string (empty string = 1, else = 0)"
      (is (= [1] (builtins/shebang [""] {}))
          "empty")
      (is (= [0] (builtins/shebang ["test"] {}))
          "non-empty"))
    (testing "array (empty = 1, else = 0)" 
      (is (= [1] (builtins/shebang [[]] {}))
          "empty")
      (is (= [0] (builtins/shebang [[1]] {}))
          "non-empty"))
    (testing "block (empty = 1, else = 0)"
      (is (= [1] (builtins/shebang [(lang/string-to-block "" {})] {}))
          "empty")
      (is (= [0] (builtins/shebang [[(lang/string-to-block
                                       "3 3 +"
                                       golfscript-symbols)]] {}))
          "non-empty")))

(deftest test-at
  (testing "at (@) - rotate top 3 elements"
    (is (= [2 4 3 1] (builtins/at [4 3 2 1] {}))
        "all longs")
    (is (= [2 "test string" [3 4] 1] (builtins/at ["test string" [3 4] 2 1] {}))
        "mixed types")))

(deftest test-dollar
  (testing "dollar ($)"
    (testing "int (copy nth element from stack)"
      (is (= [3 5 4 3 2 1] (builtins/dollar [2 5 4 3 2 1] {}))
          "positive >1")
      (is (= [4 5 4 3 2 1] (builtins/dollar [1 5 4 3 2 1] {}))
          "one")
      (is (= [5 5 4 3 2 1] (builtins/dollar [0 5 4 3 2 1] {}))
          "zero")
      (is (thrown? IndexOutOfBoundsException (builtins/dollar [100 5 4 3 2 1] {}))
          "overflow stack (exception)")
      (is (thrown? IndexOutOfBoundsException (builtins/dollar [-1 5 4 3 2 1] {}))
          "underflow stack (exception)"))

    (testing "block (sort by mapping)"
      (is (= [[5 4 3 2 1]] (builtins/dollar [(lang/string-to-block
                                               "-1*"
                                               golfscript-symbols)
                                             [5 3 4 1 2]] {}))
          "array")
      (is (= ["wrronmgeeaa210"] (builtins/dollar [(lang/string-to-block
                                                    "-1*"
                                                    golfscript-symbols)
                                                  "agromenawer102"] {}))
          "string")
      (is false
          "block"))
    
    (is (= [[1 2 3 4]] (builtins/dollar [[3 2 4 1]] {}))
        "array (sort)")
    (is (= ["abcd"] (builtins/dollar ["bcad"] {}))
        "string (sort)")))

(deftest test-plus
  (testing "plus (+)" 
    (is (= [7 4 3 2 1] (builtins/plus [2 5 4 3 2 1] {}))
        "int int (sum)")

    (testing "array (concat)"
      (is (= [[4 5 6 1 2 3]] (builtins/plus [[1 2 3] [4 5 6]] {}))
          "array")
      (is (= [[1 2]] (builtins/plus [[2] 1] {}))
          "int")
      (is (= [[2 1]] (builtins/plus [1 [2]] {}))
          "int (reverse)"))
    
    (testing "string (concat)"
      (is (= ["holamundo"] (builtins/plus ["mundo" "hola"] {}))
          "string")
      (is (= ["2adios"] (builtins/plus ["adios" 2] {}))
          "int")
      (is (= ["adios2"] (builtins/plus [2 "adios"] {}))
          "int (reverse)")
      (is (= ["23hola"] (builtins/plus ["hola" [50 51]] {}))
          "array (array as chars)")
      (is (= ["hola23"] (builtins/plus [[50 51] "hola"] {}))
          "array (reverse, array as chars)"))
    
    (testing "block (add to block)"
      (is (= [4] ((first
                      (builtins/plus [(lang/string-to-block "2 +" golfscript-symbols)
                                      2]
                                     {}))
                     [] golfscript-symbols))
          "int")
      (is (= [2 4 3] ((first
                    (builtins/plus [2
                                    (lang/string-to-block "3 4" golfscript-symbols)]
                                   {}))
                   [] golfscript-symbols))
          "int (reverse)")
      (is (= [9 234 2 1]
             ((first
                (builtins/plus
                  [(lang/string-to-block
                     "4 5 +"
                     golfscript-symbols)
                   [1 2 [50 51 52]]]
                  {}))
               [] golfscript-symbols))
          "array (coercing elements)")
      (is (= [234 2 1 9]
             ((first
                (builtins/plus
                  [[1 2 [50 51 52]]
                   (lang/string-to-block
                     "4 5 +"
                     golfscript-symbols)]
                  {}))
               [] golfscript-symbols))
          "array (reverse, coercing elements)")
      (is (= [9 3 2 1]
             ((first
                (builtins/plus
                  [(lang/string-to-block
                     "4 5 +"
                     golfscript-symbols)
                   "1 2 3"]
                  {}))
               [] golfscript-symbols))
          "string (parsing string)")
      (is (= [9 3 2 1]
             ((first
                (builtins/plus
                  [(lang/string-to-block
                     "4 5 +"
                     golfscript-symbols)
                   "1 2 3"]
                  golfscript-symbols))
               [] golfscript-symbols))
          "string (reverse, parsing string)")
      (is (= [9 3 2 1]
             ((first
                (builtins/plus
                  [(lang/string-to-block
                     "4 5 +"
                     golfscript-symbols)
                   (lang/string-to-block
                     "1 2 3"
                     golfscript-symbols)]
                  golfscript-symbols))
               [] golfscript-symbols))
          "block"))))
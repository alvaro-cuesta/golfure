(ns golfure.test.builtins
  (:require [golfure.builtins :as builtins])
  (:require [golfure.lang :as lang])
  (:require [golfure.core])
  (:use clojure.test))

(deftest test-tilde
  (is (= [-1] (builtins/tilde [0] {}))
      "tilde (~) - int (bitwise not) - zero")
  (is (= [0] (builtins/tilde [-1] {}))
      "tilde (~) - int (bitwise not) - -1")
  (is (= [-6] (builtins/tilde [5] {}))
      "tilde (~) - int (bitwise not) - positive")
  (is (= [5] (builtins/tilde [-6] {}))
      "tilde (~) - int (bitwise not) - negative")

  (is (= [6] (builtins/tilde
               ["3 3 +"]
               golfure.core/golfscript-symbols))
      "tilde (~) - string (evaluate)")

  (is (= [6] (builtins/tilde
               [(lang/string-to-block
                  "3 3 +"
                  golfure.core/golfscript-symbols)]
               golfure.core/golfscript-symbols))
      "tilde (~) - block (evaluate)")

  (is (= [5 4 3 2 1] (builtins/tilde [[1 2 3 4 5]] {}))
      "tilde (~) - array (dump elements)"))

(deftest test-grave-accent
  (is (= ["0"] (builtins/grave-accent [0] {}))
      "grave-accent (`) - int (string)")
  
  (is (= ["\"test string\""] (builtins/grave-accent ["test string"] {}))
      "grave-accent (`) - string (quoted and escaped string)")
  (is (= ["\"test \\\"string\""] (builtins/grave-accent ["test \"string"] {}))
      "grave-accent (`) - string (quoted and escaped string) - needs escaping")
  
  (is (= ["3 3 +"] (builtins/grave-accent
                     [(lang/string-to-block
                        "3 3 +"
                        golfure.core/golfscript-symbols)]
                     golfure.core/golfscript-symbols))
      "grave-accent (`) - block (block code)")
  (is (= ["3 3 +"] (builtins/grave-accent
                     [(lang/string-to-block
                        "3 3 +# ignore this comment"
                        golfure.core/golfscript-symbols)]
                     golfure.core/golfscript-symbols))
      "grave-accent (`) - block (block code) - ignore comments")
  
  (is (= ["[1 2 3 4]"] (builtins/grave-accent [[1 2 3 4]] {}))
      "grave-accent (`) - array (string)"))

(deftest test-shebang
  (is (= [1] (builtins/shebang [0] {}))
      "shebang (!) - int (0 = 1, else = 0) - zero")
  (is (= [0] (builtins/shebang [5] {}))
      "shebang (!) - int (0 = 1, else = 0) - positive")
  (is (= [0] (builtins/shebang [-5] {}))
      "shebang (!) - int (0 = 1, else = 0) - negative")
    
  (is (= [1] (builtins/shebang [""] {}))
      "shebang (!) - string (empty string = 1, else = 0) - empty string")
  (is (= [0] (builtins/shebang ["test"] {}))
      "shebang (!) - string (empty string = 1, else = 0) - non-empty string")
  
  (is (= [1] (builtins/shebang [[]] {}))
      "shebang (!) - block/array (empty = 1, else = 0) - empty block/array")
  (is (= [0] (builtins/shebang [[(lang/string-to-block
                                   "3 3 +"
                                   golfure.core/golfscript-symbols)]] {}))
      "shebang (!) - block (empty = 1, else = 0) - non-empty block")
  (is (= [0] (builtins/shebang [[1]] {}))
      "shebang (!) - array (empty = 1, else = 0) - non-empty array"))

(deftest test-at
  (is (= [2 4 3 1] (builtins/at [4 3 2 1] {}))
      "at (@) - rotate top 3 elements - all longs")
  (is (= [2 "test string" [3 4] 1] (builtins/at ["test string" [3 4] 2 1] {}))
      "at (@) - rotate top 3 elements - mixed types"))

(deftest test-dollar
  (is (= [3 5 4 3 2 1] (builtins/dollar [2 5 4 3 2 1] {}))
      "dollar ($) - int (copy nth element from stack) - > 1")
  (is (= [4 5 4 3 2 1] (builtins/dollar [1 5 4 3 2 1] {}))
      "dollar ($) - int (copy nth element from stack) - one")
  (is (= [5 5 4 3 2 1] (builtins/dollar [0 5 4 3 2 1] {}))
      "dollar ($) - int (copy nth element from stack) - zero")
  (is (= [5 5 4 3 2 1] (builtins/dollar [100 5 4 3 2 1] {}))
      "dollar ($) - int (copy nth element from stack) - overflow stack")
  (is (= [5 5 4 3 2 1] (builtins/dollar [-1 5 4 3 2 1] {}))
      "dollar ($) - int (copy nth element from stack) - underflow sstack")
  
  (is (= ["abcd"] (builtins/dollar ["bcad"] {}))
      "dollar ($) - string (sort)")
  
  (is (= [[5 4 3 2 1]] (builtins/dollar [(lang/string-to-block
                                           "-1*"
                                           golfure.core/golfscript-symbols) [5 3 4 1 2]] {}))
      "dollar ($) - block array (sort by mapping)")
  (is (= ["wrronmgeeaa210"] (builtins/dollar [(lang/string-to-block
                                           "-1*"
                                           golfure.core/golfscript-symbols) "agromenawer102"] {}))
      "dollar ($) - block string (sort by mapping)")
  (is false
      "dollar ($) - block block (sort by mapping)")
  
  (is (= [[1 2 3 4]] (builtins/dollar [[3 2 4 1]] {}))
      "dollar ($) - array (sort)"))

(deftest test-plus
  (is (= [7 4 3 2 1] (builtins/plus [2 5 4 3 2 1] {}))
      "plus (+) - int int (sum)")
  
  (is (= [[1 2 3 4 5 6]] (builtins/plus [[1 2 3] [4 5 6]] {}))
      "plus (+) - array array (concat)")
  (is (= [[1 2]] (builtins/plus [1 [2]] {}))
      "plus (+) - array int (concat)")
  (is (= [[2 1]] (builtins/plus [[2] 1] {}))
      "plus (+) - int array (concat)")
  
  (is (= ["holamundo"] (builtins/plus ["hola" "mundo"] {}))
      "plus (+) - string string (concat)")
  (is (= ["2adios"] (builtins/plus [2 "adios"] {}))
      "plus (+) - string int (concat)")
  (is (= ["adios2"] (builtins/plus ["adios" 2] {}))
      "plus (+) - int string (concat)")
  (is (= ["hola23"] (builtins/plus [[50 51] "hola"] {}))
      "plus (+) - string array (concat array chars)")
  (is (= ["23hola"] (builtins/plus ["hola" [50 51]] {}))
      "plus (+) - array string (concat array chars)")

  (is false
      "plus (+) - block block (add to block)")
  (is false
      "plus (+) - block int (add to block cons operation)")
  (is false
      "plus (+) - int block (add to block cons operation)")
  (is false
      "plus (+) - block array (add to block coercing elements)")
  (is false
      "plus (+) - array block (add to block coercing elements)")
  (is false
      "plus (+) - block string (add to block parsing string)")
  (is false
      "plus (+) - string block (add to block parsing string)"))
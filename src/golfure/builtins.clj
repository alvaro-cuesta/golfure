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
       ~(if docstring? docstring "nil")
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
  ([:blk] [x]
    (cons (apply str (map :token (x))) stack))
  ([_] [x]
    (cons (str x) stack)))

(builtin shebang
  "! (args: 1)
  ------------
  0 [] \"\" {} yield 1, everything else 0"
  ([:int] [x]
    (cons (if (zero? x) 1 0) stack))
  ([:blk] [x]
    (cons (if (empty? (x)) 1 0) stack))
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

(let [sort-with-mapping
      (fn [x map-block pre-fn post-fn stack symbols]
        (loop [[e & more] x
               built []
               stack stack]
          (if e
            (let [[newe & more-stack] (map-block (cons (pre-fn e) stack) symbols)]
              (recur more
                     (conj built [newe e])
                     more-stack))
            (cons (post-fn
                    (map second
                         (sort-by first built)))
                  stack))))]
  (builtin dollar
    "$ (args: 1 or 2)
  -----------------
  If arg is an integer, copys nth item from top of $tack.
    1 2 3 4 5  1$ -> 1 2 3 4 5 4
  For arrays (including strings) a $ort is performed.
    'asdf'$ -> \"adfs\"
  For blocks, sort by some mapping.
    [5 4 3 1 2]{-1*}$ -> [5 4 3 2 1]"
    ([:int] [n]
      (cons (nth stack n) stack))
    ([:str] [s]
      (cons (apply str (sort s)) stack))
    ([:blk :arr] [b a]
      (sort-with-mapping
        a b
        identity
        identity
        stack symbols))
    ([:blk :str] [b s]
      (sort-with-mapping
        (map int s) b
        identity
        (comp (partial apply str)
              (partial map char))
        stack symbols))
    ([:blk :blk] [b1 b2]
      (sort-with-mapping
        (b2) b1
        :token
        #(golfure.lang.Block. %)
        stack symbols))
    ([:arr] [a]
      (cons (sort a) stack))))

(let [concat-blocks #(golfure.lang.Block. (concat (%) (%2)))]
  (builtin plus
    "+ (args: coerce)
  -----------------
  Adds two numbers or concatenate
    5 7+ -> 12
    'asdf'{1234}+ -> {asdf 1234}
    [1 2 3][4 5]+ -> [1 2 3 4 5]"
    ([:blk _] [b x]
      (cons (concat-blocks (lang/coerce x :blk symbols) b)
            stack))
    ([_ :blk] [x b]
      (cons (concat-blocks b (lang/coerce x :blk symbols))
            stack))
  
    ([:str _] [s x]
      (cons (str (lang/coerce x :str symbols) s)
            stack))
    ([_ :str] [x s]
      (cons (str s (lang/coerce x :str symbols))
            stack))
  
    ([:arr _] [a x]
      (cons (concat (lang/coerce x :arr symbols) a)
            stack))
    ([_ :arr] [x a]
      (cons (concat a (lang/coerce x :arr symbols))
            stack))
  
  ([:int :int] [a b]
    (cons (+ a b) stack))))

(let [set-difference
      (fn [a b]
        (filter #(not-any? (fn [x] (= x %)) a)
                b))]
  (builtin minus
    "- (args: coerce)
  -----------------
  Note the way - is parsed in the first example.
    1 2-3+ -> 1 -1
    1 2 -3+ -> 1 -1
    1 2- 3+ -> 2
    [5 2 5 4 1 1][1 2]- -> [5 5 4]"
    ([:blk _] [b x]
      (cons (golfure.lang.Block.
              (set-difference (b)
                              ((lang/coerce x :blk symbols))))
            stack))
    ([_ :blk] [x b]
      (cons (golfure.lang.Block.
              (set-difference ((lang/coerce x :blk symbols))
                              (b)))
            stack))
  
    ([:str _] [s x]
      (cons (apply str
                   (set-difference s
                                   (lang/coerce x :str symbols)))
            stack))
    ([_ :str] [x s]
      (cons (apply str
                   (set-difference (lang/coerce x :str symbols)
                                   s))
            stack))
  
    ([:arr _] [a x]
      (cons (set-difference a
                            (lang/coerce x :arr symbols))
            stack))
    ([_ :arr] [x a]
      (cons (set-difference (lang/coerce x :arr symbols)
                            a)
            stack))
  
    ([:int :int] [a b]
      (cons (- b a) stack))))

(builtin asterisk
  "* (args: order)
  ----------------
  * can mean many things, the choice of behavior is determined by the type.
  Multiplication
    2 4* -> 8
  Execute a block a certain number of times, note the order of operands
  does not matter because these are automatically ordered first.
    2 {2*} 5* -> 64
  Array/string repeat
    [1 2 3]2* -> [1 2 3 1 2 3]
    3'asdf'* -> \"asdfasdfasdf\"
  Join
    [1 2 3]','* -> \"1,2,3\"
    [1 2 3][4]* -> [1 4 2 4 3]
    'asdf'' '* -> \"a s d f\"
    [1 [2] [3 [4 [5]]]]'-'* -> \"1-\002-\003\004\005\"
    [1 [2] [3 [4 [5]]]][6 7]* -> [1 6 7 2 6 7 3 [4 [5]]]
  Fold. Symbol choice for fold comes from ruby golf trick: eval [1,2,3,4,5]*\"+\".
    [1 2 3 4]{+}* -> 10
    'asdf'{+}* -> 414"
  ([:int :int] [a b]
    (cons (* a b) stack))
  ([:int :arr] [n a]
    (cons (reduce into [] (repeat n a)) stack))
  ([:arr :int] [a n]
    (cons (reduce into [] (repeat n a)) stack))
  ([:int :str] [n s]
    (cons (apply str (repeat n s)) stack))
  ([:str :int] [s n]
    (cons (apply str (repeat n s)) stack))
  
  ([:int :blk] [n b]
    (loop [stack stack
           n n]
      (if (zero? n)
        stack
        (recur (b stack symbols) (dec n)))))
  ([:blk :int] [b n]
    (loop [stack stack
           n n]
      (if (zero? n)
        stack
        (recur (b stack symbols) (dec n)))))

  ([:str :arr] [s a] ;; PROBABLEMENTE MAL
    (cons (apply str (interpose s a)) stack))
  ([:arr :str] [a s] ;; PROBABLEMENTE MAL
    (cons (apply str (interpose a s)) stack))
  ([:arr :arr] [a b]
    (cons (mapcat #(if-not (coll? %) [%] %)
                  (interpose a b) stack)))
  ([:str :str] [a b]
    (cons (apply str (interpose a b)) stack))

  ([:blk :arr] [b a]
    (cons (apply concat (reduce b a)) stack))
  ([:arr :blk] [a b]
    (cons (apply concat (reduce b a)) stack))
  ([:blk :str] [b s]
    (cons (apply concat (reduce b s)) stack))
  ([:str :blk] [s b]
    (cons (apply concat (reduce b s)) stack)))
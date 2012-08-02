(ns golfure.lang
  "GolfureScript interpreter/compiler.")

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
                   (if value
                     (cons value stack)))
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

(defn- execute-block
  "Executes elements from 'block' until it's empty and
  returns the resulting stack.

  Public alternative :
    (block stack symbols)"
  [[element & elements] stack symbols]
  (cond
    (= (str element) ":")
      (recur
        (rest elements)
        stack
        (into symbols
              {(str (first elements)) (first stack)}))
    element
      (recur
        elements
        (element stack symbols)
        symbols)
    :else stack))

(deftype Block [elements]
  java.lang.Object
  (toString [this]
    (apply str (map :token elements)))
  clojure.lang.IFn
  (invoke [this] elements)
  (invoke [this stack symbols]
    (execute-block elements stack symbols))
  (applyTo [this args]
    (clojure.lang.AFn/applyToHelper this args)))

(defn string-to-block
  "Converts a string to a compiled GolfureScript block,
  using symbols as a symbol map.

  Original RegEx from
   http://www.golfscript.com/golfscript/syntax.html"
  [string symbols]
  (Block.
    (map #(token-to-element % symbols)
         (keep
           #(when-not (= ((vec %) 0) \#) %) ; Discard comments
           (re-seq
             #"[a-zA-Z_][a-zA-Z0-9_]*|'(?:\\.|[^'])*'?|\"(?:\\.|[^\"])*\"?|-?[0-9]+|#[^\n\r]*|."
             string)))))

;;;

(defn golf-type [x]
  "Returns x's GolfureScript type, one of:
     :int, :str, :blk, :arr"
  (cond
    (= (type x) Long) :int
    (= (type x) String) :str
    (= (type x) Block) :blk
    (coll? x) :arr))
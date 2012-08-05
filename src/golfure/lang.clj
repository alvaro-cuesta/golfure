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

(declare golf-type)
(declare execute-block)

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
    (= (str element) "{")
      (recur
        (drop-while #(not= (str %) "}")
                    elements)
        (cons (Block. (take-while #(not= (str %) "}")
                                 elements))
              stack)
        symbols)
    element
      (recur
        elements
        (element stack symbols)
        symbols)
    :else stack))

(defn string-to-block
  "Converts a string to a compiled GolfureScript block,
  using symbols as a symbol map.

  Original RegEx from
   http://www.golfscript.com/golfscript/syntax.html"
  [string symbols]
  (Block.
    (map #(token-to-element % symbols)
         (keep
           #(when-not (= ((vec %) 0) \#) %) ; discard comments
           (re-seq
             #"[a-zA-Z_][a-zA-Z0-9_]*|'(?:\\.|[^'])*'?|\"(?:\\.|[^\"])*\"?|-?[0-9]+|#[^\n\r]*|."
             string)))))

;;;

(defn golf-type [x]
  "Returns x's GolfScript type, one of:
     :int, :str, :blk, :arr"
  (condp = (type x)
    Integer :int
    Long :int
    clojure.lang.BigInt :int
    String :str
    Block :blk
    clojure.lang.PersistentVector :arr
    (throw (RuntimeException.
             (str x " is not a GolfScript type (is: " (type x) ")")))))

(defn coerce [value type symbols]
  "Coerces a GolfScript value to another type."
  (condp = [(golf-type value) type]
    [:blk :blk] value
    [:str :blk] (string-to-block value symbols)
    [:arr :blk] (Block. (map #(string-to-block (coerce % :str symbols) symbols)
                             value))
    [:int :blk] (Block. [(fn [stack symbols]
                           (cons value stack))])

    [:str :str] value
    [:arr :str] (apply str
                       (mapcat #(Character/toChars %)
                               value))
    [:int :str] (str value)

    [:arr :arr] value
    [:int :arr] [value]
    
    [:int :int] value
    
    (throw (RuntimeException.
             (str "Invalid coercion: " value " (type " (golf-type value) ") cannot be coerced to" type)))))
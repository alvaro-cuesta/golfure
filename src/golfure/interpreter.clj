(ns golfure.interpreter
  "GolfureScript interpreter/compiler."
  (:require [golfure.lang])
  (:import [golfure.lang Element]))

(defn parse-token
  "Parse 'token' using 'symbols' as a symbol map.

  If 'token' isn't found in the symbol map, a function
  is returned for later execution.

  This function will try to evaluate in a future
  symbol map. If it isn't found there either
  it will try to evaluate the token using Clojure's
  eval to parse strings/ints/¿vectors?."
  [token symbols]
  (or (symbols token)
      (fn [stack symbols]
        (or (symbols token)
            (try (let [value (eval (read-string token))]
                   (if value
                     (cons value stack)))
              (catch RuntimeException e
                stack))))))

(defn string-to-block
  [string symbols]
  "Parses 'string' into a compiled block using 'symbols'
  as a symbol map. Each token is parsed in 'parse-token'
  and compiled into an array.

  Each block element is a function which takes:
    [p1 p2 ...  pn & r] ; The whole stack
  And returns a new stack (usually a cons operation):
    (cons (+ p1 p2) r)

  Original RegEx from
   http://www.golfscript.com/golfscript/syntax.html"
  (map #(golfure.lang/Element. (parse-token % symbols) %)
       (keep #(when-not (= ((vec %) 0) \#) %)
             (re-seq
               #"[a-zA-Z_][a-zA-Z0-9_]*|'(?:\\.|[^'])*'?|\"(?:\\.|[^\"])*\"?|-?[0-9]+|#[^\n\r]*|."
               string))))

(defn execute-block
  [[{:keys [fun token]} & elements] stack symbols]
  "Consumes elements from 'block' (see string-to-block)
  until no more are found, and returns the resulting stack. 

  Executes a series of functions in a looping fashion
  to avoid stack overflows. Internal GolfScript stack is
  initialized to 'stack' and passed as an argument to
  each function."
  (cond
    (= token ":") (recur
                    (rest elements)
                    stack
                    (into symbols
                          {((first elements) :token) (first stack)}))
    fun (recur
          elements
          (fun stack symbols)
          symbols)
    :else stack))
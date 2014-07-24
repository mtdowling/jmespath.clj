(ns jmespath.tree
  "Traverses and interprets JMESPath ASTs. JMESPath AST nodes are visited
  using the visit multimethod."
  (:require [jmespath.functions]
            [cheshire.core :as cheshire])
  (:import [com.fasterxml.jackson.core JsonParseException]))

(defmulti visit (fn [ast data options] (first ast)))

(defmethod visit :identifier [ast data options]
  "Visits an identifier node, returning either the string
  referenced by the node, or a keyword referenced by the
  node (e.g., 'a' or :a). If the argument is not a map,
  then this method return nil."
  (when (map? data)
    (let [n (get ast 1)]
      (or (data n) (data (keyword n))))))

(defmethod visit :index [ast data options]
  "Returns the nth value of a sequence, or nil"
  (when (or (list? data) (vector? data))
    (get data (get ast 1))))

(defn- subexpr [ast data options]
  "Returns the value of the right expression passed into the
  left expression."
  (visit (get ast 2)
         (visit (get ast 1) data options) options))

(defmethod visit :sub-expr [ast data options] (subexpr ast data options))
(defmethod visit :pipe-expr [ast data options] (subexpr ast data options))
(defmethod visit :current-node [ast data options] data)
(defmethod visit :identity [ast data options] data)

(defmethod visit :literal [ast data options]
  "Visits a literal node and JSON decodes the value if it looks like JSON"
  (let [v (nth ast 1)]
    (let [like-json ["{" "[" "\"" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "-"]
          fchar (str (get v 0))]
      ; If the first character of the string does not look like JSON, then
      ; just return the literal value
      (if (not (some #(= fchar %) like-json))
        v
        ; Otherwise, attempt to JSON parse the string
        (try
          (cheshire/decode (nth ast 1))
          (catch JsonParseException e (nth ast 1)))))))

(defmethod visit :unary-condition [ast data options]
  "Visits a unary condition, a condition statement that has a single member"
  (visit (nth ast 1) data options))

(defmethod visit :negation [ast data options]
  "Returns the negation of a value"
  (not (visit (nth ast 1) data options)))

(defmethod visit :binary-condition [ast data options]
  "Returns the result of a binary condition"
  (let [lhs (get ast 1)
        type (get-in ast [2 1])
        rhs (get ast 3)]
    (cond
      (= type "==") (= (visit lhs data options) (visit rhs data options))
      (= type "!=") (not= (visit lhs data options) (visit rhs data options))
      (= type "&&") (and (visit lhs data options) (visit rhs data options))
      (= type "||") (or (visit lhs data options) (visit rhs data options))
      ; Other symbols can be used literally (e.g., <, >, <=, >=)
      :default (let [left (visit lhs data options)
                     right (visit rhs data options)]
                 (boolean (and (and (number? left) (number? right))
                               ((resolve (symbol type)) left right)))))))

(defn- project
  "Applies a projection node based on a map function"
  [ast data guard mapfn options]
  (let [lhs (visit (get-in ast [1 1]) data options)]
    (if-let [guarded (guard lhs)]
      (filter
        #(not (nil? %))
        (map mapfn guarded)))))

(defmethod visit :value-projection [ast data options]
  "Applies a value-project only to maps"
  (let [rexp (get-in ast [2 1])]
    (project ast
             data
             #(when (map? %) %)
             #(visit rexp (get % 1) options)
             options)))

(defmethod visit :index-projection [ast data options]
  "Applies an index-projection to vectors or lists"
  (let [rexp (get-in ast [2 1])]
    (project ast
             data
             #(when (sequential? %) %)
             #(visit rexp % options)
             options)))

(defmethod visit :filter-projection [ast data options]
  "Applies a filter-projection to vectors or lists"
  (let [condition (get ast 2)
        rexp (get-in ast [3 1])]
    (project ast
             data
             #(when (sequential? %) %)
             #(when (visit condition % options)
                (visit rexp % options))
             options)))

(defn- flatten-data
  "Takes a sequence of data and flattens arrays up one level if they are
  sequential."
  [x]
  (mapcat (fn [x] (if (sequential? x) x (list x))) x))

(defmethod visit :flatten-projection [ast data options]
  "Creates a projection that evaluates the left expression, flattens it, then
  passes each flattened value to the right expression."
    (let [rexp (get-in ast [2 1])]
      (project ast
               data
               #(when (sequential? %) (flatten %))
               #(visit rexp % options)
               options)))

(defmethod visit :multi-select-hash [ast data options]
  "Creates an array-map based on a list of key-value pair expressions"
  (apply
    array-map
    (flatten
      (map
        (fn [node]
          [(get-in node [1 1 1] node)
           (visit (get-in node [2 1]) data options)])
        (rest ast)))))

(defmethod visit :multi-select-list [ast data options]
  "Creates a vector based on a list of expressions"
  (map (fn [node] (visit node data options)) (rest ast)))

(defmethod visit :function-expr [ast data options]
  "Invokes a function with a list of arguments using the :fnprovided found
  in the options map."
  ((:fnprovider options)
    (get-in ast [1 1])
    (map (fn [node] (visit node data options)) (rest (nth ast 2)))))

(defmethod visit :expr-type [ast data options]
  "Returns a function that can be invoked to provide an expression result"
  (fn [with-data]
    (visit (nth ast 1) with-data options)))

(defn interpret
  "Interprets the given AST with the provided data. Accepts an AST in
  hiccup format, the data to interpret, and the following keyword arguments

  :fnprovider Function invoked to handle JMESPath function calls. This
              keyword is required."
  [ast data &{:as options}]
  {:pre [(contains? options :fnprovider)]}
  (visit (first ast) data options))

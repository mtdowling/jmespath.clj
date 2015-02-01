(ns jmespath.interpreter
  "Traverses and interprets JMESPath ASTs. JMESPath AST nodes are visited
  using the visit multimethod."
  (:require [jmespath.functions]
            [cheshire.core :as cheshire])
  (:import [com.fasterxml.jackson.core JsonParseException]))

(defmulti visit (fn [ast data opts] (first ast)))

(defmethod visit :identifier [ast data opts]
  "Visits an identifier node, returning either the string
  referenced by the node, or a keyword referenced by the
  node (e.g., 'a' or :a). If the argument is not a map,
  then this method return nil."
  (when (map? data)
    (let [key (get ast 1)]
      (or (get data key nil) (get data (keyword key) nil)))))

(defmethod visit :index [ast data opts]
  "Returns the nth value of a sequence, or nil"
  (when (sequential? data)
    (nth data (get ast 1) nil)))

(defn- subexpr [ast data opts]
  "Returns the value of the right expression passed into the
  left expression."
  (let [lhs (visit (get ast 1) data opts)]
    (visit (get ast 2) lhs opts)))

(defmethod visit :current-node [ast data opts] data)

(defmethod visit :subexpr [ast data opts] (subexpr ast data opts))

(defmethod visit :pipe [ast data opts]
  (subexpr ast data opts))

(defmethod visit :or [ast data opts]
  (or (visit (get ast 1) data opts)
      (visit (get ast 2) data opts)))

(defmethod visit :and [ast data opts]
  (and (visit (get ast 1) data opts)
       (visit (get ast 2) data opts)))

(defmethod visit :literal [ast data opts]
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

(defmethod visit :not [ast data opts]
  (not (visit (nth ast 1) data opts)))

(defn- project
  "Applies a projection node based on a map function"
  [left-node guard mapfn data opts]
  (let [lhs (visit left-node data opts)]
    (if-let [guarded (guard lhs)]
      (filter #(not (nil? %)) (map mapfn guarded)))))

(defmethod visit :object-projection [ast data opts]
  (let [rhs (nth ast 2)]
    (project (nth ast 1)
             #(when (map? %) %)
             #(visit rhs (get % 1) opts)
             data
             opts)))

(defmethod visit :array-projection [ast data opts]
  (project (nth ast 1)
           #(when (sequential? %) %)
           #(visit (nth ast 2) % opts)
           data
           opts))

(defn- flatten-data
  [x]
  (mapcat (fn [x] (if (sequential? x) x (list x))) x))

(defmethod visit :flatten-projection [ast data opts]
  (let [rhs (nth ast 2)]
    (project (nth ast 1)
             #(when (sequential? %) (flatten %))
             #(visit rhs % opts)
             data
             opts)))

(defmethod visit :multi-hash [ast data opts]
  (apply
    array-map
    (flatten
      (map
        (fn [node]
          [(get-in node [1 1]) (visit (nth node 2) data opts)])
        (rest ast)))))

(defmethod visit :multi-list [ast data opts]
  (map (fn [node] (visit node data opts)) (rest ast)))

(defmethod visit :function [ast data opts]
  (let [args (map (fn [node] (visit node data opts))
                  (rest (nth ast 2)))]
    ((:fnprovider opts) (nth ast 1) args)))

(defmethod visit :expref [ast data opts]
  (fn [with-data] (visit (nth ast 1) with-data opts)))

(defmethod visit :filter-condition [ast data opts]
  (when (visit (nth ast 1) data opts) data))

(defn cmp? [fn ast data opts]
  (let [lhs (visit (nth ast 1))
        rhs (visit (nth ast 2))]
    (boolean
      (and (and (number? lhs) (number? rhs))
           (fn lhs rhs)))))

(defn- is-equal? [ast data opts]
  (= (visit (nth ast 1) data opts)
     (visit (nth ast 2) data opts)))

(defmethod visit :eq [ast data opts] (is-equal? ast data opts))
(defmethod visit :ne [ast data opts] (not (is-equal? ast data opts)))
(defmethod visit :gt [ast data opts] (cmp? > ast data opts))
(defmethod visit :gte [ast data opts] (cmp? >= ast data opts))
(defmethod visit :lt [ast data opts] (cmp? < ast data opts))
(defmethod visit :lte [ast data opts] (cmp? <= ast data opts))

(defn interpret
  "Interprets the given AST with the provided data. Accepts an AST in
  hiccup format, the data to interpret, and the following keyword arguments
  :fnprovider (required) fn invoked to handle JMESPath function calls."
  [ast data &{:as opts}]
  {:pre [(contains? opts :fnprovider)]}
  (visit ast data opts))

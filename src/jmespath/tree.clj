(ns jmespath.tree
  "Traverses and interprets JMESPath ASTs")

(defmulti visit
  "Double dispatch method used to visit nodes by name"
  (fn [ast _]
    (get ast 0)))

(defmethod visit :identifier [ast data]
  (if (map? data) (get data (get ast 1))))

(defmethod visit :index [ast data]
  (if (or (list? data) (vector? data))
    (get data (get ast 1))))

(defmethod visit :sub-expr [ast data]
  (visit (get ast 2) (visit (get ast 1) data)))

(defmethod visit :pipe-expr [ast data]
  (visit (get ast 2) (visit (get ast 1) data)))

(defmethod visit :literal [ast data]
  (get ast 1))

(defmethod visit :current-node [ast data] data)
(defmethod visit :identity [ast data] data)

(defmethod visit :unary-condition [ast data]
  (let [type (get-in ast [1 0])
        value (get-in ast [1 1])]
    (cond
      (= type :negation) (not (visit value data)))))

(defmethod visit :binary-condition [ast data]
  (let [lhs (get ast 1)
        type (get-in ast [2 1])
        rhs (get ast 3)]
    (cond
      (= type "==") (= (visit lhs data) (visit rhs data))
      (= type "!=") (not= (visit lhs data) (visit rhs data))
      (= type "&&") (and (visit lhs data) (visit rhs data))
      (= type "||") (or (visit lhs data) (visit rhs data))
      :default ((resolve (symbol type))
        (visit lhs data) (visit rhs data)))))

(defn- project
  "Applies a projection node based on a guard and map function"
  [ast data guard mapfn]
  (let [lhs (visit (get-in ast [1 1]) data)]
    (if (guard lhs)
      (filter
        (fn [item] (not (nil? item)))
        (map mapfn lhs)))))

(defmethod visit :value-projection [ast data]
  "Applies a value-project only to maps"
  (let [rexp (get-in ast [2 1])]
    (project ast data
      (fn [lhs] (map? lhs))
      (fn [item] (visit rexp (get item 1))))))

(defmethod visit :index-projection [ast data]
  "Applies an index-projection to vectors or lists"
  (let [rexp (get-in ast [2 1])]
    (project ast data
      (fn [lhs] (or (list? lhs) (vector? lhs)))
      (fn [item] (visit rexp item)))))

(defmethod visit :filter-projection [ast data]
  "Applies a filter-projection to vectors or lists"
  (println ast)
  (let [condition (get ast 2)
        rexp (get-in ast [3 1])]
    (project ast data
      (fn [lhs] (or (list? lhs) (vector? lhs)))
      (fn [item]
        (if (visit condition item)
          (visit rexp item))))))

(defn interpret [ast data]
  "Interprets the given AST with the provided data"
  (visit (first ast) data))

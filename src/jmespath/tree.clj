(ns jmespath.tree
  "Rewrites an Instparse parse tree into an AST"
  (:require [jmespath.functions :refer (invoke)]
            [jmespath.interpreter :refer (interpret)]
            [instaparse.core :refer (transform)]
            [cheshire.core :refer (parse-string)]))

(def ^:private projection-nodes
  #{:object-projection
    :array-projection
    :flatten-projection
    :filter-projection})

(defn- empty-projection [type]
  "Creates an empty projection with current nodes and default metadata"
  (constantly [type [:current-node] [:current-node]]))

(defn- is-projection? [node]
  (projection-nodes (get node 0)))

(defn- xf-skip-middle [node]
  (fn [lhs _ rhs] [node lhs rhs]))

(defn- xf-multi-list
  "Normalizes multi-lists that have one or multiple values."
  [& nodes]
  (let [nodes (->> nodes (drop 1) (drop-last) vec)]
    (if (= :multiple-values (get-in nodes [0 0]))
      (into [:multi-list] (->> nodes first rest (take-nth 2)))
      (into [:multi-list] nodes))))

(defn- xf-filter [& nodes]
  [:array-projection [:current-node] (nth nodes 2)])

(defn- list-with-csv [nodes]
  (->> nodes (drop 1) (drop-last) (take-nth 2) vec))

(defn- xf-csv [node-name]
  (fn [& nodes]
    (into [node-name] (list-with-csv nodes))))

(defn- right-projection
  "Replace the subexpr with a projection where the left node is replaced with
  the subexpr left node, and the right node of the projection remains."
  [left right]
  (assoc right 1 left))

(defn- left-projection [left right]
  "Replace the subexpr with a projection that consumes the right node."
  (let [right-node (nth left 2)
        right-type (nth right-node 0)]
    (cond
      ; When the right node is current-node, it means that it was a default
      ; inserted node, so it can be replaced.
      (= right-type :current-node)
        (assoc left 2 right)
      ; Replace the R node of the projection with the projection, but insert
      ; L's right node into the projection's left node.
      (is-projection? right)
        (assoc left 2 (assoc right 1 right-node))
      ; If the L node is a projection, then push the visited R node onto the
      ; L node's R node (recursively if needed).
      (is-projection? right-node)
        (assoc left 2 (left-projection right-node right))
      ; Replace with a subexpr where the L node is the current R node
      ; of the projection, and the R node is the R node of the subexpr.
      :default
        (assoc left 2 [:subexpr right-node right]))))

(defn rewrite
  "Transforms the given Instaparse tree to make it nicer to work with"
  [tree]
  (transform
    {:ALPHA str
     :DIGIT str
     :DQUOTE str
     :escaped-char str
     :unescaped-char str
     :escaped-literal str
     :unescaped-literal str
     :escape str
     :char identity
     :non-test identity
     :root-expr identity
     :non-terminal identity
     :terminal identity
     :terminal-rhs identity
     :arg identity
     :expr identity
     :comparator identity
     :array-subexpr-lhs identity
     :array-subexpr-rhs identity
     :object-subexpr-lhs identity
     :object-subexpr-rhs identity

     ; JSON parsing
     :true (constantly true)
     :false (constantly false)
     :null (constantly nil)
     :digit1-9 str
     :int (comp read-string str)
     :json-number (comp read-string str)
     :json-value identity
     :literal-value identity
     :literal (fn [_ v _] [:literal v])
     :non-json-value str
     :exp str
     :frac str
     :member (fn [name _ value] [name value])
     :array (fn [& nodes]
              (->> nodes
                   (drop 1)
                   (drop-last)
                   (take-nth 2)
                   (vec)))
     :object (fn [& nodes]
               (->> nodes
                    (drop 1)
                    (drop-last)
                    (take-nth 2)
                    (flatten)
                    (apply array-map)))

     :comparison (fn [l c r] [(nth c 0) l r])
     :identifier (fn [id] [:identifier (str id)])
     :index (fn [& s] [:index (get (vec s) 1)])
     :number (comp read-string str)
     :quoted-string (fn [& s] (apply str (drop 1 (drop-last s))))
     :unquoted-string (comp read-string str)
     :or (xf-skip-middle :or)
     :and (xf-skip-middle :and)
     :pipe (xf-skip-middle :pipe)
     :root-multi-list xf-multi-list
     :object-subexpr (xf-skip-middle :subexpr)
     :function (fn [name args] [:function (str name) args])
     :array-subexpr (fn [lhs rhs] [:subexpr lhs rhs])
     :keyval (xf-skip-middle :keyval)
     :expref (fn [_ t] [:expref t])
     :multi-list xf-multi-list
     :multi-hash (xf-csv :multi-hash)
     :arg-list (xf-csv :arg-list)
     :wildcard-values (empty-projection :object-projection)
     :wildcard-index (empty-projection :array-projection)
     :flatten (empty-projection :flatten-projection)
     :current-node (constantly [:current-node])
     :group (fn [_ expr _] expr)
     :filter xf-filter
     :not (fn [_ expr] [:not expr])
     :subexpr (fn [node]
                (let [left (nth node 1) right (nth node 2)]
                  (cond
                    (is-projection? left) (left-projection left right)
                    (is-projection? right) (right-projection left right)
                    :else [:subexpr left right])))}
    tree))

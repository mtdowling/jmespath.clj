(ns jmespath.tree
  "Rewrites an Instparse parse tree into an AST"
  (:require [jmespath.functions :refer (invoke)]
            [jmespath.interpreter :refer (interpret)]
            [instaparse.core :refer (transform)]
            [cheshire.core :refer (parse-string)]))

(def ^:private projection-nodes
  "This is a list of the nodes used in projections."
  #{:object-projection
    :array-projection
    :flatten-projection
    :filter-projection})

(defn- empty-projection
  "Creates an empty projection with current nodes and default metadata"
  [type]
  (constantly [type [:current-node] [:current-node]]))

(defn- is-projection?
  "Given a node, returns true if the node is a projection node."
  [node]
  (projection-nodes (get node 0)))

(defn- xf-skip-middle
  "Given a list of nodes, returns a list in which the middle node is removed."
  [node]
  (fn [lhs _ rhs] [node lhs rhs]))

(defn- xf-multi-list
  "Normalizes multi-lists that have one or multiple values."
  [& nodes]
  (let [nodes (->> nodes (drop 1) (drop-last) vec)]
    (if (= :multiple-values (get-in nodes [0 0]))
      (into [:multi-list] (->> nodes first rest (take-nth 2)))
      (into [:multi-list] nodes))))

(defn- xf-filter
  "Creates a projection for a filter node."
  [& nodes]
  [:array-projection [:current-node] (nth nodes 1)])

(defn- list-with-csv
  "Given a list of comma separated nodes, remove the commas or separators and
  return a vector."
  [nodes]
  (->> nodes (drop 1) (drop-last) (take-nth 2) vec))

(defn- xf-csv
  "Returns a function that is used to transform a CSV list of nodes into a
  named node in which the CSV separator (i.e., ',') is removed from the list
  and returned as a vec."
  [node-name]
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

(defn- xf-escape
  "Expand JSON escapes."
  [& nodes]
  (let [c (apply str (drop 1 nodes))]
    (cond
      (= "\"" c) "\""
      (= "\\" c) "\\"
      (= "b" c) "\b"
      (= "f" c) "\f"
      (= "n" c) "\n"
      (= "r" c) "\r"
      (= "t" c) "\t"
      :default (read-string (str "\\" c)))))

(defn- xf-wrapped-string
  "Given a list of tokens in which the first and last tokens are characters
  wrapping a string (e.g., '), return a string containing the characters
  inside of the delimiters."
  [& nodes]
  (apply str (->> nodes (drop 1) (drop-last))))

(defn- remove-escape
  [delim]
  (fn [& chars]
    (if (= chars '("\\" delim))
      delim
      (last chars))))

(defn rewrite
  "Transforms the given Instaparse tree to make it nicer to work with"
  [tree]
  (transform
    {:ALPHA str
     :DIGIT str
     :DQUOTE str
     :HEXDIG str
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

     ; String parsing
     :escape str
     :char identity
     :unescaped-char str
     :escaped-char xf-escape

     ; Raw string literal parsing
     :raw-string-escape (remove-escape "'")
     :raw-string-char identity
     :raw-string xf-wrapped-string

     ; Literal parsing
     :literal (fn [_ v _] [:literal v])
     :literal-char identity
     :unescaped-literal str
     :escaped-literal (remove-escape "`")

     ; JSON parsing
     :true (constantly true)
     :false (constantly false)
     :null (constantly nil)
     :digit1-9 str
     :int str
     :json-number (comp read-string str)
     :json-string xf-wrapped-string
     :decimal-point str
     :json-value identity
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

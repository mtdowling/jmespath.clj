(ns jmespath.core
  "Parses and evaluated JMESPath expr over Clojure data structures."
  {:author "Michael Dowling"}
  (:require [jmespath.functions :refer (invoke)]
            [jmespath.interpreter :refer (interpret)]
            [instaparse.core :as insta]
            [instaparse.failure :as failure]
            [cheshire.core :as cheshire]))

(def ^:private parser
  (insta/parser
    (clojure.java.io/resource "jmespath.txt")
    :auto-whitespace :standard
    :input-format :abnf))

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

(defn- xf-json
  "JSON decodes the provided characters, adding quotes if necessary."
  [chars]
  (let [s (apply str chars)]
    ; JSON decode if it looks like JSON, otherwise add quotes then decode.
    (if (re-find #"(true|false|null)|(^[\[\"{])|(^\-?[0-9]*(\.[0-9]+)?([e|E][+|\-][0-9]+)?$)" s)
      (cheshire/parse-string s true)
      (cheshire/parse-string (str "\"" s "\"") true))))

(defn- xf-literal
  "Parses a literal by dropping '`' and safely parsing the inner-JSON value."
  [& chars]
  [:literal (->> chars drop-last rest (apply str) xf-json)])

(defn- xf-multi-list
  "Normalizes multi-lists that have one or multiple values."
  [& nodes]
  (let [nodes (->> nodes (drop 1) (drop-last) vec)]
    (if (= :multiple-values (get-in nodes [0 0]))
      (into [:multi-list] (->> nodes first rest (take-nth 2)))
      (into [:multi-list] nodes))))

(defn- xf-filter [& nodes]
  (->> nodes
       (drop 1)
       (drop-last)
       (into [:filter-projection [:current-node] [:current-node]])))

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
  ; Replace the right node of the projection.
  (let [right-node (nth left 2)
        right-type (nth right-node 0)]
    (if (= right-type :current-node)
      ; Replace with the right node of the visited subexpr.
      (assoc left 2 right)
      ; Replace with a subexpr where the left node is the current right node of
      ; the projection, and the right node is the right node of the subexpr.
      (assoc left 2 [:subexpr right-node right]))))

; Rewriting the tree for projections:
;
; 1. When a projection token node is encountered, a projection node is created
;    in which the left and right children of the node are "@" nodes. This
;    default structure allows projections to work correctly as root values that
;    are not wrapped by subexpr nodes.
; 2. When a subexpr is visited and the left node is a projection, return
;    the projection, replacing the right node of the projection with a new
;    node, adhering to the following rules:
;    a. When the right node of the projection is a current node, replace the
;       right node of the projection with the right node of the visited
;       subexpr. For example, "*.b" is parsed as (. * b), which is
;       rewritten as (. (* @ @) b) when the wildcard is visited, which is
;       rewritten as (* @ b) when the subexpression is visited.
;    b. Otherwise, replace the right node of the projection with a subexpr in
;       which the left node is the current right node of the projection, and
;       the right node is the right node of the visited subexpr. For example,
;       "a.*.b" is parsed as (. (. a *) b). When the projection is visited, it
;       is translated to (. (. a (* @ @)) b). When the "a" subexpr is visited,
;       it is translated to (. (* a @) b) (see step #3). When the top subexpr
;       is visited, the subexpr is converted to the final form, (* a b). This
;       is because the right node of the projection is a current node, which
;       causes it to be replaced with the right node of the visited subexpr.
; 3. When a subexpr is visited and the right node is a projection (e.g., a.*),
;    return a projection node in which the left node of the projection is the
;    left node of the subexpr, and the right node continues to be the current
;    node of the default projection form (i.e., (* a @)).
(defn- xf-parse-tree
  "Transforms the given Instaparse tree to make it nicer to work with"
  [tree]
  (insta/transform
    {:ALPHA str
     :DIGIT str
     :DQUOTE str
     :unescaped-char str
     :unescaped-literal str
     :escaped-literal str
     :escape (constantly "")
     :non-test identity
     :root-expr identity
     :non-terminal identity
     :terminal identity
     :terminal-rhs identity
     :arg identity
     :expr identity
     :identifier (fn [id] [:identifier (str id)])
     :index (fn [& s] [:index (get (vec s) 1)])
     :literal xf-literal
     :number (comp read-string str)
     :quoted-string (fn [& s] (xf-json s))
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

(defn parse
  "Parses a JMESPath expr into an AST. Accepts an expr as a
  string and returns a sequence of hiccup data. Throws an
  IllegalArgumentException if the expr fails to parse."
  [exp]
  (let [tree (parser exp)]
    (if (insta/failure? tree)
      (throw (IllegalArgumentException. (failure/pprint-failure tree)))
      (->> exp parser xf-parse-tree))))

(defn search
  "Returns data from the input that matches the provided JMESPath expr.
  Accepts an expr as a string, the data to search, and an optional list
  of keyword arguments:
  :fnprovider Function that accepts a function name and sequence of arguments
              and returns the result of invoking the function. If no value is
              provided, then the default jmespath.functions/invoke multimethod
              is utilized.
  If the provided expr is invalid, an IllegalArgumentException is thrown."
  [expr data &{:as options}]
  (interpret
    (parse expr)
    data
    :fnprovider (get options :fnprovider invoke)))

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

(defn- is-projection [node]
  (projection-nodes (get node 0)))

(defn- xf-skip-middle [node]
  (fn [lhs _ rhs] [node lhs rhs]))

(defn- xf-expr
  "Adds a right/left nodes to projections if needed."
  [node]
  (if (not (is-projection node))
    node
    (let [c (count node)]
      (cond
        (= 2 c) (conj node [:current-node])
        (= 1 c) (conj node [:current-node] [:current-node])
        :default node))))

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
  (->> nodes (drop 1) (drop-last) (into [:filter-projection])))

(defn- list-with-csv [nodes]
  (->> nodes (drop 1) (drop-last) (take-nth 2) vec))

(defn- xf-csv [node-name]
  (fn [& nodes]
    (into [node-name] (list-with-csv nodes))))

(defn- right-projection [left right]
  (conj right left [:current-node]))

(defn- left-projection [left right]
  (cond
    (= 1 (count left)) (conj left [:current-node] right)
    (= :current-node (get-in left [2 0])) (conj (vec (drop-last left)) right)
    :default
      (let [last (last left)]
        (conj (vec (drop-last left)) [:subexpr last right]))))

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
     :function-arg identity
     :expr xf-expr
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
     :array-subexpr (xf-skip-middle :subexpr)
     :keyval (xf-skip-middle :keyval)
     :expref (fn [_ t] [:expref t])
     :multi-list xf-multi-list
     :multi-hash (xf-csv :multi-hash)
     :function-args (xf-csv :function-args)
     :wildcard-values (constantly [:object-projection])
     :wildcard-index (constantly [:array-projection])
     :flatten (constantly [:flatten-projection])
     :current-node (constantly [:current-node])
     :group (fn [_ expr _] expr)
     :filter xf-filter
     :not (fn [_ expr] [:not expr])
     :subexpr (fn [node]
       (let [left (nth node 1) right (nth node 2)]
         (cond
           (is-projection right) (right-projection left right)
           (is-projection left) (left-projection left right)
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
              provided, then the default jmespath.function/invoke multimethod
              is utilized.
  If the provided expr is invalid, an IllegalArgumentException is thrown."
  [expr data &{:as options}]
  (interpret
    (parse expr)
    data
    :fnprovider (get options :fnprovider invoke)))

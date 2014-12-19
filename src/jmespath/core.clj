(ns jmespath.core
  "Parses and evaluated JMESPath expression over Clojure data structures."
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

(defn- xf-multi-select-list
  "Normalizes multi-select-lists that have one or multiple values."
  [& nodes]
  (let [nodes (->> nodes (drop 1) (drop-last) vec)]
    (if (= :multiple-values (get-in nodes [0 0]))
      (into [:multi-select-list] (->> nodes first rest (take-nth 2)))
      (into [:multi-select-list] nodes))))

(defn- list-with-csv [nodes]
  (->> nodes (drop 1) (drop-last) (take-nth 2) vec))

(defn- xf-csv [node-name]
  (fn [& nodes]
    (into [node-name] (list-with-csv nodes))))

(defn- is-projection [node]
  (projection-nodes (get node 0)))

(defn- right-projection [left right]
  (conj right left [:current-node]))

(defn- left-projection [left right]
  (cond
    (= 1 (count left)) (conj left [:current-node] right)
    (= :current-node (get-in left [2 0])) (conj (vec (drop-last left)) right)
    :default
      (let [last (last left)]
        (conj (vec (drop-last left)) [:sub-expression last right]))))

(defn- xf-parse-tree
  "Transforms the given Instaparse tree to make it nicer to work with"
  [tree]
  (insta/transform {:ALPHA str
                    :DIGIT str
                    :DQUOTE str
                    :unescaped-char str
                    :unescaped-literal str
                    :escaped-literal (comp str #(replace % "\\" ""))
                    :index-expression (fn [& s]
                      [:index-expression (get (vec s) 1)])
                    :literal xf-literal
                    :number (comp read-string str)
                    :quoted-string (fn [& s] (xf-json s))
                    :unquoted-string (comp read-string str)
                    :or-expression (fn [l _ r] [:or-expression l r])
                    :pipe-expression (fn [l _ r] [:pipe-expression l r])
                    :root-multi-select-list xf-multi-select-list
                    :root-expression identity
                    :object-predicate (fn [_ pred] pred)
                    :array-predicate identity
                    :expression-type (fn [_ t] [:expression-type t])
                    :keyval-expr (fn [k _ v] [:keyval-expr k v])
                    :expression (fn [node]
                      ; Adds a right node to projections if needed.
                      (if (and (is-projection node) (= 2 (count node)))
                        (conj node [:current-node])
                        node))
                    :multi-select-list xf-multi-select-list
                    :multi-select-hash (xf-csv :multi-select-hash)
                    :one-or-more-args (xf-csv :one-or-more-args)
                    :wildcard-values (constantly [:object-projection])
                    :wildcard-index (constantly [:array-projection])
                    :flatten (constantly [:flatten])
                    :current-node (constantly [:current-node])
                    :no-args (constantly [:no-args])
                    :terminating-expression identity
                    :subexpression (fn [left right]
                      (cond
                        (is-projection right) (right-projection left right)
                        (is-projection left) (left-projection left right)
                        :else [:subexpression left right]))}
                   tree))

(defn parse
  "Parses a JMESPath expression into an AST. Accepts an expression as a
  string and returns a sequence of hiccup data. Throws an
  IllegalArgumentException if the expression fails to parse."
  [exp]
  (let [tree (parser exp)]
    (if (insta/failure? tree)
      (throw (IllegalArgumentException. (failure/pprint-failure tree)))
      (->> exp parser xf-parse-tree))))

(defn search
  "Returns data from the input that matches the provided JMESPath expression.
  Accepts an expression as a string, the data to search, and an optional list
  of keyword arguments:

  :fnprovider Function that accepts a function name and sequence of arguments
              and returns the result of invoking the function. If no value is
              provided, then the default jmespath.function/invoke multimethod
              is utilized.

  If the provided expression is invalid, and IllegalArgumentException is
  thrown."
  [exp data &{:as options}]
  (interpret
    (parse exp)
    data
    :fnprovider (get options :fnprovider invoke)))

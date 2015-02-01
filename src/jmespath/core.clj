(ns jmespath.core
  "Parses and evaluated JMESPath expr over Clojure data structures."
  {:author "Michael Dowling"}
  (:require [jmespath.functions :refer (invoke)]
            [jmespath.interpreter :refer (interpret)]
            [instaparse.core :as insta]
            [jmespath.tree :refer (rewrite)]))

(def ^:private parser
  (insta/parser
    (clojure.java.io/resource "jmespath.txt")
    :input-format :abnf))

(defn parse
  "Parses a JMESPath expr into an AST. Accepts an expr as a string and returns
  a sequence of hiccup data. If the expr cannot be parsed, then this function
  throws a clojure.lang.ExceptionInfo containing the :expr and :err keys where
  the :err key is an instaparse/failure.Failure object"
  [expr]
  (let [tree (parser expr)]
    (if (insta/failure? tree)
      (throw (ex-info (instaparse.failure/pprint-failure tree)
                      {:expr expr :err tree}))
      (->> expr parser rewrite))))

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

(ns jmespath.core
  "Parses and evaluated JMESPath expression over Clojure data structures."
  {:author "Michael Dowling"}
  (:require [jmespath.functions :refer (invoke)]
            [jmespath.tree :refer (interpret)]
            [instaparse.core :as insta]
            [instaparse.failure :as failure]))

(def ^:private parser
  (insta/parser
    "<exp>                 = stat | pipe-expr | flatten-projection
     pipe-expr             = exp <'|'> exp
     <stat>                = group | greedy-projection | literal | condition |
                             index | chunk | sub-expr | current-node
     <chunk>               = identifier | function-expr |
                             multi-select-list | multi-select-hash
     sub-expr              = (exp <'.'> chunk) | (exp index)
     index                 = <'['> number <']'>
     number                = '-'* digit+
     <digit>               = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' |
                             '8' | '9'
     <group>               = <'('> exp <')'>
     identifier            = unquoted-string | quoted-string
     <unquoted-string>     = #'[A-Za-z]+[0-9A-Za-z_]*'
     <quoted-string>       = <'\"'> #'(?:\\|\\\"|[^\"])*' <'\"'>
     literal               = <'`'> #'(?:\\|\\`|[^`])*' <'`'>
     <greedy-projection>   = value-projection |
                             index-projection |
                             filter-projection
     projection-subject    = identity | exp
     projection-predicate  = identity | stat | <'.'> stat
     identity              = EPSILON
     value-projection      = projection-subject [<'.'>] <'*'> projection-predicate
     index-projection      = projection-subject <'[*]'> projection-predicate
     flatten-projection    = projection-subject <'[]'> projection-predicate
     filter-projection     = projection-subject <'[?'> condition <']'> projection-predicate
     <condition>           = unary-condition | binary-condition
     binary-condition      = stat binop stat
     unary-condition       = negation | stat
     negation              = <'!'> stat
     binop                 = '<' | '<=' | '>' | '>=' | '==' | '!=' | '&&' | '||'
     function-expr         = function-name <'('> function-args <')'>
     function-name         = unquoted-string
     function-args         = [explist]
     <explist>             = explist-item [{<','> explist-item}]
     <explist-item>        = exp | expr-type
     current-node          = <'@'>
     expr-type             = <'&'> exp
     multi-select-list     = <'['> explist <']'>
     multi-select-hash     = <'{'> keyval-list <'}'>
     <keyval-list>         = keyval-exp [{<','> keyval-exp}]
     keyval-exp            = keyval-key <':'> keyval-value
     keyval-key            = identifier
     keyval-value          = exp"
    :auto-whitespace (insta/parser "whitespace = #'\\s+'")))

(defn- transform-tree
  "Transforms the given Instaparse tree to make it nicer to work with"
  [tree]
  (insta/transform {:number (comp read-string str)
                    :digit str}
                   tree))

(defn parse
  "Parses a JMESPath expression into an AST. Accepts an expression as a
  string and returns a sequence of hiccup data. Throws an
  IllegalArgumentException if the expression fails to parse."
  [exp]
  (let [tree (parser exp)]
    (if (insta/failure? tree)
      (throw (IllegalArgumentException. (failure/pprint-failure tree)))
      (transform-tree (parser exp)))))

(defn search
  "Returns data from the input that matches the provided JMESPath expression.

  Accepts an expression as a string and an optional list of keyword
  arguments:

  :fnprovider Function that accepts a function name and sequence of arguments
              and returns the result of invoking the function. If no value is
              provided, then the default jmespath.function/invoke multimethod
              is utilized.

  If the provided expression is invalid, and IllegalArgumentException is
  thrown."
  [exp data &{:as options}]
  (let [fnprovider (get options :fnprovider invoke)]
    (interpret (parse exp) data :fnprovider fnprovider)))

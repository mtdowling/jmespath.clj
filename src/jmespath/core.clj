(ns jmespath.core
  "Parses JMESPath ressions"
  {:author "Michael Dowling"}
  (:require [instaparse.core :as insta]))

(def ^:private parser
  (insta/parser
    "<exp>                 = stat | pipe-expr
     pipe-expr             = exp <'|'> exp
     <stat>                = group | projection | literal | condition |
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
     <projection>          = value-projection |
                             flatten-projection |
                             index-projection |
                             filter-projection
     projection-subject    = identity | exp
     projection-predicate  = identity | stat | <'.'> stat
     identity              = EPSILON
     value-projection      = projection-subject [<'.'>] <'*'> projection-predicate
     index-projection      = projection-subject <'[*]'> projection-predicate
     flatten-projection    = projection-subject <'[]'> projection-predicate
     filter-projection     = projection-subject <'[?'> condition <']'> projection-predicate
     condition             = stat binop stat | unop stat
     binop                 = '<' | '<=' | '>' | '>=' | '==' | '!=' | '&&' | '||'
     unop                  = <'!'>
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

(defn parse [exp]
  "Parses a JMESPath expression into an AST"
  (->> (parser exp) (insta/transform {
    :number (comp read-string str)
    :digit str})))

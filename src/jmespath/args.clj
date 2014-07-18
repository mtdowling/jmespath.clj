(ns jmespath.args
  "Parses and validates JMESPath function signatures"
  (:use [clojure.test :only (function?)])
  (:use [clojure.string :only (join)]))

(defn gettype [subject]
  "Get the JMESPath type of a Clojure variable as a string"
  (cond
    (map? subject)              "object"
    (list? subject)             "array"
    (vector? subject)           "array"
    (string? subject)           "string"
    (instance? Boolean subject) "boolean"
    (nil? subject)              "null"
    (number? subject)           "number"
    (function? subject)         "expression"))

(defn- get-expected-str [validator]
  (get (meta validator) :validation))

(defn- valid-type?
  "Determines if the provided argument matches the provided type"
  [expected arg]
  (cond
    (= expected "any") true
    :else (= expected (gettype arg))))

(defn- invalid-arity! [fn-name args expected variadic]
  "Throws the correct exception for when an invalid argument arity"
  (let [expected-str (if variadic (str expected " or more") expected)]
    (throw (Exception. (str fn-name " expects " expected-str " arguments. "
                          (count args) " arguments were provided.")))))

(defn- invalid-type! [fn-name pos expected actual arg-type]
  "Throws an exception for when an invalid type is encountered"
  (let [expected-str (get-expected-str expected)]
    (throw (Exception. (str "Invalid " arg-type " provided to argument "
                          (+ 1 pos) " of " fn-name ". Expected "
                          expected-str ", got `" actual "`.")))))

(defn- invalid-positional! [fn-name pos expected actual]
  (invalid-type! fn-name pos expected actual "argument"))

(defn- invalid-variadic! [fn-name pos expected actual]
  (invalid-type! fn-name pos expected actual "variadic argument"))

(defn- validate-arg [fn-name positional variadic args pos]
  "Validates a single argument of a function"
  (let [p (get positional pos),
        a (get args pos)]
    (cond
      (and (not p) (not variadic))
        (invalid-arity! fn-name args (count positional) variadic)
      (and (not p) (variadic a)) a
      (not p) (invalid-variadic! fn-name pos variadic a)
      (p a) a
      (contains? args pos) (invalid-positional! fn-name pos p a)
      :else (invalid-arity! fn-name args (count positional) variadic))))

(defn arg-type [expected]
  "Returns a function that validates a single type"
  (with-meta
    (fn [arg] (valid-type? expected arg))
    {:validation expected}))

(defn arg-alts [& alts]
  "Returns a function that validates one or more types"
  (with-meta
    (fn [arg] (some #(= % (gettype arg)) alts))
    {:validation (join " or " alts)}))

(defn arg-seq [& types]
  "Returns a function that ensures a collection uses a consistent type"
  (with-meta
    (fn [arg]
      (and (= (gettype arg) "array")
           (let [first-type (gettype (first arg))]
             (and (some #(= first-type %) types)
                  (every? #(= first-type (gettype %)) arg)))))
    {:validation (str "a sequence of " (join " or " types)
                      " elements")}))

(defn validate-fn
  "Validates the arguments of a function"
  [{fn-name :name
    positional :positional
    variadic :variadic
    args :args}]
  (let [positional (or positional [])]
    (map
      #(validate-arg fn-name positional variadic args %)
      (range (max (count args) (count positional))))))

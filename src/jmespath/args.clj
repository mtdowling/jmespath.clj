(ns jmespath.args
  "Parses and validates JMESPath function signatures. These methods are used
  when registering functions with the JMESPath interpreter. Specifically,
  the functions arg-type, arg-seq, arg-expr, arg-any, and arg-alts are used
  in jmespath.functions to implement validation of JMESPath function
  arguments."
  (:require [clojure.test :refer (function?)]
            [clojure.string :refer (join)]))

(defn gettype
  "Get the JMESPath type of a Clojure variable as a string"
  [subject]
  (cond
    (vector? subject)           "array"
    (sequential? subject)       "array"
    (map? subject)              "object"
    (string? subject)           "string"
    (instance? Boolean subject) "boolean"
    (nil? subject)              "null"
    (number? subject)           "number"
    (function? subject)         "expression"))

(defn- invalid-arity
  "Throws the correct exception for when an invalid argument arity"
  [fname args expected variadic]
  (let [expected-str (if variadic (str expected " or more") expected)]
    (throw
      (IllegalArgumentException.
        (str fname " expects " expected-str " arguments. " (count args)
             " arguments were provided.")))))

(defn- invalid-type
  "Throws an exception for when an invalid type is encountered"
  [fname pos expected actual]
  (let [expected-str (get (meta expected) :validation)]
    (throw
      (IllegalArgumentException.
        (str "Invalid argument provided to argument " (+ 1 pos) " of "
             fname ". Expected " expected-str "; got " (gettype actual)
             ", `" actual "`.")))))

(defn- get-positioned-parameter
  "Gets a positional parameter from a list of parameters (positional) for the
  fname function. If a postional parameter does not exist at the given pos,
  then a variadic parameter is returned if available, or an exeception is
  thrown if it is not available."
  [fname positional variadic pos args]
  (if-let [p (or (get positional pos) variadic)]
    p (invalid-arity fname args (count positional) variadic)))

(defn- validate-arg
  "Validates a single argument of a function by ensuring the correct
  function arity and that each argument type matches the expected argument
  type. If the provided argument at nth position pos does not satisfy the
  constraint of the corresponding positional argument or variadic argument,
  then an Exception is thrown. This method returns the validated argument
  value, including wrapped expression types."
  [fname positional variadic args pos]
  (let [p (get-positioned-parameter fname positional variadic pos args)
        a (get args pos)]
    (let [result (p a)]
      (if (nth result 0)
        (nth result 1)
        (invalid-type fname pos p a)))))

(defn arg-any
  "Accepts any argument and always returns [arg, true]"
  []
  (with-meta
    (fn [arg] [true arg])
    {:validation "any"}))

(defn arg-type
  "Returns a function that checks if an argument is valid based on type"
  [expected]
  (with-meta
    (fn [arg] [(= expected (gettype arg)) arg])
    {:validation expected}))

(defn arg-alts
  "Returns a function that ensures an argument satisfies one of the
  provided conditional functions. If one of the arguments is a string, then
  an arg-type validator will be utilized."
  [& conds]
  (let [conds (map (fn [x] (if (string? x) (arg-type x) x)) conds)]
    (with-meta
      (fn [arg]
        [(some #(% arg) conds) arg])
      {:validation (join " or " (map #(:validation (meta %)) conds))})))

(defn arg-seq
  "Returns a function that ensures an argument collection uses a
  consistent type. This method accepts a variadic number of type strings.
  As data is read from a sequence this function ensures that the first item
  in a sequence matches one of the provided types. After the first item is
  read, this validator ensures that each subsequent item is of the same
  type."
  [& types]
  (with-meta
    (fn [arg]
      [(and (= (gettype arg) "array")
            (let [first-type (gettype (first arg))]
              (and (some #(= first-type %) types)
                   (every? #(= first-type (gettype %)) arg))))
       arg])
    {:validation (str "a sequence of " (join " or " types)
                      " elements")}))

(defn arg-expr
  "Returns a function that validates that the provided argument is an
  expression and ensures that the return type of each expression is valid."
  [fname validator]
  (let [err-msg (str "expression type that returns "
                     (:validation (meta validator)))]
    (let [validator
          (with-meta validator
                     {:validation (str err-msg ". One of the results returned"
                                       " an invalid value")})]
      (with-meta
        (fn [arg]
          (if (nth ((arg-type "expression") arg) 0)
            [true (fn [x] (validate-arg fname nil validator [(arg x)] 0))]
            [false arg]))
      {:validation err-msg}))))

(defn validate
  "Validates the arguments of a function. Accepts a hash that contains the
  following symbol keys:

  - :name - The name of the function to validate this name is used in all
      error messages. This value is required.
  - :positional - A vector of positional arguments. Each item in the
      positional argument vector must be a validation function
      (e.g., arg-type)
  - :variadic - A single validation function (e.g., arg-seq)
  - :args - A vector of function arguments that are validated

  This function validates the provided argument and returns a sequence of
  arguments that may provide further validation as the argument are evaluated
  in a corresponding function."
  [{:keys [name positional variadic args]
    :or {positional [], args []}}]
  (let [arg-count (count args), pos-count (count positional)]
    (if (< arg-count pos-count)
      ; The minimum number of arguments were not supplied
      (invalid-arity name args pos-count variadic)
      ; Validate over each argument
      (let [iterations (max arg-count pos-count)]
        (for [pos (range iterations)]
          (validate-arg name positional variadic args pos))))))

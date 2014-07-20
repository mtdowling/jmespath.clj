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

(defn- valid-type?
  "Determines if the provided argument matches the provided type"
  [expected arg]
  (or (= expected "any") ; "any" matches any given argument type
      (= expected (gettype arg))))

(defn- invalid-arity [fname args expected variadic]
  "Throws the correct exception for when an invalid argument arity"
  (let [expected-str (if variadic (str expected " or more") expected)]
    (throw (Exception. (str fname " expects " expected-str " arguments. "
                          (count args) " arguments were provided.")))))

(defn- invalid-type [fname pos expected actual]
  "Throws an exception for when an invalid type is encountered"
  (let [expected-str (get (meta expected) :validation)]
    (throw (Exception. (str "Invalid argument provided to argument "
                          (+ 1 pos) " of " fname ". Expected "
                          expected-str ", got `" actual "`.")))))

(defn- get-positioned-parameter [fname positional variadic pos args]
  "Gets a positional parameter from a list of parameters. If a postional
   parameter does not exist at the given pos, then a variadic parameter
   is returned if available, or an exeception is thrown if it is not
   available."
  (if-let [p (or (get positional pos) variadic)]
    p (invalid-arity fname args (count positional) variadic)))

(defn- validate-arg [fname positional variadic args pos]
  "Validates a single argument of a function by ensuring the correct
   function arity and that each argument type matches the expected type."
  (let [p (get-positioned-parameter fname positional variadic pos args)
        a (get args pos)]
    (if (p a) a (invalid-type fname pos p a))))

(defn arg-type [expected]
  "Returns a function that checks if an argument is valid based on type"
  (with-meta
    (fn [arg] (valid-type? expected arg))
    {:validation expected}))

(defn arg-alts [& alts]
  "Returns a function that checks if an argument is of one or more types"
  (with-meta
    (fn [arg] (some #(= % (gettype arg)) alts))
    {:validation (join " or " alts)}))

(defn arg-seq [& types]
  "Returns a function that ensures an argument collection uses a
   consistent type"
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

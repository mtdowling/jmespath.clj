(ns jmespath.functions
  "Provides default implementations of the various JMESPath functions defined
  in the specification. By default, JMESPath for Clojure uses the double-
  dispatch method jmespath.functions/invoke in order to provide functions to
  the tree interpreter that is utilized in jmespath.core/search."
  (:require [jmespath.args :refer :all]
            [instaparse.core :as insta]
            [cheshire.core :as cheshire]))

(defmulti invoke (fn [fname _] fname))

(defmethod invoke "abs" [fname args]
  "Returns the absolute value of the provided argument."
  (let [args (validate {:name fname
                        :positional [(arg-type "number")]
                        :args (vec args)})]
    ; Returns the maximum of the argument or the negative value
    ; of the argument
    (max (nth args 0) (- (nth args 0)))))

(defmethod invoke "avg" [fname args]
  "Returns the average of the elements in the provided array."
  (let [args (validate {:name fname
                        :positional [(arg-seq "number")]
                        :args (vec args)})]
    ; Creates an average of a list of numbers by dividing sum by count
    (/ (reduce + (nth args 0))
       (count (nth args 0)))))

(defmethod invoke "contains" [fname args]
  "Returns true if the given $subject contains the provided $search string."
  (let [args (validate {:name fname
                        :positional [(arg-alts "array" "string")
                                     (arg-any)]
                        :args (vec args)})]
    (let [haystack (nth args 0)
          needle   (nth args 1)]
      (if (string? haystack)
        (not= (.indexOf haystack needle) -1)
        (boolean (some #(= % needle) haystack))))))

(defmethod invoke "ceil" [fname args]
  "Returns the next highest integer value by rounding up if necessary."
  (let [args (validate {:name fname
                        :positional [(arg-type "number")]
                        :args (vec args)})]
    (int (Math/ceil (nth args 0)))))

(defmethod invoke "floor" [fname args]
  "Returns the next lowest integer value by rounding down if necessary."
  (let [args (validate {:name fname
                        :positional [(arg-type "number")]
                        :args (vec args)})]
    (int (Math/floor (nth args 0)))))

(defmethod invoke "join" [fname args]
  "Returns all of the elements from the provided $stringsarray array
  joined together using the $glue argument as a separator between each."
  (let [args (validate {:name fname
                        :positional [(arg-type "string")
                                     (arg-seq "string")]
                        :args (vec args)})]
    (clojure.string/join (nth args 0) (nth args 1))))

(defmethod invoke "keys" [fname args]
  "Returns an array containing the keys of the provided object."
  (let [args (validate {:name fname
                        :positional [(arg-type "object")]
                        :args (vec args)})]
    (keys (nth args 0))))

(defmethod invoke "length" [fname args]
  "Returns the length of the given argument."
  (let [args (validate {:name fname
                        :positional [(arg-alts "string" "array" "object")]
                        :args (vec args)})]
    (count (nth args 0))))

(defn- min-max [fname args meth]
  "Shared method used to implement the min and max functions"
  (let [args (validate {:name fname
                        :positional [(arg-seq "number")]
                        :args (vec args)})]
    (when (count (nth args 0))
      (apply meth (nth args 0)))))

(defmethod invoke "max" [fname args]
  "Returns the highest found number in the provided array argument."
  (min-max fname args max))

(defn- proxy-by [fname args]
  "Validates the arguments (args) of a *_by function referred to as fname and
  returns the validated arguments. The first argument MUST be an array and
  the second argument MUST be an expression type that returns a number. Each
  expression type return value is tested to ensure that it returns the
  correct type."
  (validate {:name fname
             :positional [(arg-type "array")
                          (arg-expr fname (arg-type "number"))]
             :args (vec args)}))

(defmethod invoke "max_by" [fname args]
  "Return the maximum element in an array using the expression expr as
  the comparison key."
  (let [args (proxy-by fname args)]
    (apply max-key (nth args 1) (nth args 0))))

(defmethod invoke "min" [fname args]
  "Returns the lowest found number in the provided array argument."
  (min-max fname args min))

(defmethod invoke "min_by" [fname args]
  "Return the minimum element in an array using the expression expr as
   the comparison key."
  (let [args (proxy-by fname args)]
    (apply min-key (nth args 1) (nth args 0))))

(defmethod invoke "not_null" [fname args]
  "Returns the first argument that does not resolve to null."
  (let [args (validate {:name fname
                        :variadic (arg-any)
                        :args (vec args)})]
    (first (filter #(not= % nil) args))))

(defmethod invoke "sort" [fname args]
  "This function accepts an array $list argument and returns the sorted
  elements of the $list as an array."
  (let [args (validate {:name fname
                        :positional [(arg-alts (arg-seq "string")
                                               (arg-seq "number"))]
                        :args (vec args)})]
    (sort (nth args 0))))

(defmethod invoke "sort_by" [fname args]
  "Sort an array using an expression expr as the sort key."
  (let [args (proxy-by fname args)]
    (sort-by (nth args 1) (nth args 0))))

(defmethod invoke "sum" [fname args]
  "Returns the sum of the provided array argument."
  (let [args (validate {:name fname
                        :positional [(arg-seq "number")]
                        :args (vec args)})]
    (apply + (nth args 0))))

(defmethod invoke "to_string" [fname args]
  "Returns the provided value as a string. If a string value is provided, the
  string is returned unchanged. Any other type of value will be returned as
  a JSON encoded string"
  (let [args (validate {:name fname
                        :positional [(arg-any)]
                        :args (vec args)})]
    (let [arg (nth args 0)]
      (if (string? arg) arg (cheshire/generate-string arg)))))

(defmethod invoke "to_number" [fname args]
  "Returns the provided value as a number or nil. If the value is a number,
  the number is passed through unchanged. If the value is a string that looks
  like a number, it is parsed using Clojure's read-string function. Other
  types of values will return nil."
  (let [args (validate {:name fname
                        :positional [(arg-any)]
                        :args (vec args)})]
    (let [arg (nth args 0), provided-type (gettype (nth args 0))]
      (cond
        ; Number types pass through without modification
        (= provided-type "number") arg
        ; Parse strings to numbers if they match the json-number production
        (and (= provided-type "string")
             (re-matches  #"^\-?[0-9]*(\.[0-9]+)?([e|E][+|\-][0-9]+)?$" arg))
             (read-string arg)
       :default nil))))

(defmethod invoke "type" [fname args]
  "Returns the JavaScript type of the given $subject argument as a
  string value."
  (let [args (validate {:name fname
                        :positional [(arg-any)]
                        :args (vec args)})]
    (gettype (nth args 0))))

(defmethod invoke "values" [fname args]
  "Returns the values of the given object as an array."
  (let [args (validate {:name fname
                        :positional [(arg-type "object")]
                        :args (vec args)})]
    (vals (nth args 0))))

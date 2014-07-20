(ns jmespath.functions
  "Executes JMESPath functions"
  (:use jmespath.args)
  (:require [instaparse.core :as insta]))

(defmulti invoke (fn [fname _] fname))

(defmethod invoke "abs" [fname args]
  "Returns the absolute value of the provided argument."
  (let [args (validate-fn {:name fname
                           :positional [(arg-type "number")]
                           :args (vec args)})]
    (max (nth args 0) (- (nth args 0)))))

(defmethod invoke "avg" [fname args]
  "Returns the average of the elements in the provided array."
  (let [args (validate-fn {:name fname
                           :positional [(arg-seq "number")]
                           :args (vec args)})]
    ; Creates an average of a list of numbers by dividing sum by count
    (/ (reduce + (nth args 0))
       (count (nth args 0)))))

(defmethod invoke "not_null" [fname args]
  "Returns the first argument that does not resolve to null."
  (let [args (validate-fn {:name "not_null"
                           :variadic (arg-type "any")
                           :args (vec args)})]
    (first (filter #(not= % nil) args))))

(defmethod invoke "length" [fname args]
  "Returns the length of the given argument."
  (let [args (validate-fn {:name fname
                           :positional [(arg-alts "string" "array" "object")]
                           :args (vec args)})]
    (count (nth args 0))))

(defmethod invoke "type" [fname args]
  "Returns the JavaScript type of the given $subject argument as a
   string value."
  (let [args (validate-fn {:name fname
                           :positional [(arg-type "any")]
                           :args (vec args)})]
    (gettype (nth args 0))))

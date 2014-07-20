(ns jmespath.functions
  "Executes JMESPath functions"
  (:use jmespath.args)
  (:require [instaparse.core :as insta]))

(defmulti invoke (fn [fname _] fname))

(defmethod invoke "type" [fname args]
  (let [args (validate-fn
    {:name fname
     :positional [(arg-type "any")]
     :args (vec args)})]
    (gettype (nth args 0))))

(defmethod invoke "not_null" [fname args]
  (let [args (validate-fn
    {:name "not_null"
     :variadic (arg-type "any")
     :args (vec args)})]
    (first (filter #(not= % nil) args))))

(defmethod invoke "length" [fname args]
  (let [args (validate-fn
    {:name fname
     :positional [(arg-alts "string" "array" "object")]
     :args (vec args)})]
    (count (nth args 0))))

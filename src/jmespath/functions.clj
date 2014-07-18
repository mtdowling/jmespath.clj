(ns jmespath.functions
  "Executes JMESPath functions"
  (:use jmespath.args)
  (:require [instaparse.core :as insta]))

(defmulti invoke (fn [fn-name _] fn-name))

(defmethod invoke "type" [fn-name args]
  (let [args (validate-fn
    {:name fn-name
     :positional [(arg-type "any")]
     :args (vec args)})]
    (gettype (nth args 0))))

(defmethod invoke "not_null" [fn-name args]
  (let [args (validate-fn
    {:name "not_null"
     :variadic (arg-type "any")
     :args (vec args)})]
    (first (filter #(not= % nil) args))))

(defmethod invoke "length" [fn-name args]
  (let [args (validate-fn
    {:name fn-name
     :positional [(arg-alts "string" "array" "object")]
     :args (vec args)})]
    (count (nth args 0))))

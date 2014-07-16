(ns jmespath.functions
  "Executes JMESPath functions")

(defmulti invoke (fn [name _] name))

(defn- gettype [subject]
  (cond
    (map? subject)              "object"
    (list? subject)             "array"
    (vector? subject)           "array"
    (string? subject)           "string"
    (instance? Boolean subject) "boolean"
    (nil? subject)              "null"))

(defn- invalid-type [name pos expected actual]
  "Throws an exception when an invalid type is supplied for an argument"
  (throw
    (Exception.
     (str "Invalid type supplied for argument " pos
          " of " name ". Expected " expected "."))))

(defmethod invoke "type" [name args]
  (gettype (nth args 0)))

(defmethod invoke "not_null" [name args]
  (not (nil? (nth args 0))))

(defmethod invoke "length" [name args]
  (let [subject (nth args 0)]
    (try
      (count (nth args 0))
      (catch Exception e
        (invalid-type name 0 "array|string|object" subject)))))

(ns jmespath.test.core
  (:require [clojure.test :refer :all]
            [jmespath.core :as jmespath]
            [clojure.java.io :as io]
            [cheshire.core :refer (parse-string)]))

(defn- load-suite
  "Loads a JMESPath test suite"
  [f]
  [(.getName f), (-> f slurp parse-string)])

(defn- get-test-suites
  "Returns a lazy sequence of test files"
  []
  (map #(load-suite %)
       (filter #(.isFile %)
               (-> "compliance/" io/resource io/file file-seq))))

(defn get-test-cases
  "Gets a sequence of test cases from the filesystem. Each yielded map
  contains the following keys:

  :file - Name of the file that contains the test
  :given - Given parameters for the test case
  :expr - Test case expression
  :result - Expected result
  :error - Expected error"
  []
  (flatten (for [test-suite (get-test-suites)]
    (for [suite (nth test-suite 1)]
      (for [case (get suite "cases")]
        {:file   (nth test-suite 0)
         :given  (get suite "given")
         :expr   (get case "expression")
         :result (get case "result")
         :error  (get case "error")})))))

(defn convert-result-value [x]
  (cond
    (instance? clojure.lang.Ratio x) (float x)
    (instance? clojure.lang.LazySeq x) (into [] x)
    :default x))

(defn convert-result [result]
  (clojure.walk/walk
    convert-result-value
    convert-result-value
    result))

(deftest passes-compliance
  (doseq [{:keys [file given expr result error]} (get-test-cases)]
    (testing (str file ": " expr)
      (try
        (let [actual (convert-result (jmespath/search expr given))]
          (is (nil? error)
              (str "Should have failed: " error))
          (is (= result actual)
              (str "Expected " result ", but got " (str actual))))
        (catch Exception e
          (is (string? error)
              (str "Should not have failed: " e)))))))

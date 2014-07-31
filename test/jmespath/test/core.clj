(ns jmespath.test.core
  [:require [clojure.test :refer :all]
            [jmespath.core :as jmespath]
            [clojure.java.io :as io]
            [cheshire.core :refer (parse-string)]])

(deftest searches
  (testing "Basic operations"
    (is (= (jmespath/search "foo" {"foo" "bar"})))
    (is (= '([:identifier "foo"]) (jmespath/parse "foo")))
    (is (instance? clojure.lang.LazySeq
                   (jmespath/search "[]" [1 2 3])))
    (is (= [1 2] (jmespath/search "[]" [1 2] :doall true)))))

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

(deftest passes-compliance
  (doseq [test-suite (get-test-suites)]
    (doseq [suite (seq (nth test-suite 1))]
      (let [given (get suite "given")
            cases (get suite "cases")]
        (doseq [case (seq cases)]
          (let [expr     (get case "expression")
                expected (get case "result")
                err      (get case "error")]
            (testing (str (nth test-suite 0) ": " expr)
              (try
                (let [result (jmespath/search expr given :doall true)]
                  (is (nil? err)
                      (str "Should have failed: " err))
                  (is (= expected result)
                      (str "Expected " expected ", but got " (seq result))))
                (catch Exception e
                  (is (string? err)
                      (str "Should not have failed: " e)))))))))))

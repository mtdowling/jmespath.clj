(ns jmespath.test.core
  [:require [clojure.test :refer :all]
            [jmespath.core :as jmespath]
            [clojure.java.io :as io]])

(deftest searches
  (testing "Basic operations"
    (is (= (jmespath/search "foo" {"foo" "bar"}))))
    (is (= '([:identifier "foo"]) (jmespath/parse "foo"))))

(deftest passes-compliance
  (doseq [f (-> "compliance/" io/resource io/file file-seq)]
    (if (.isFile f)
      (testing (str "Compliance " (.getName f))
        (let [contents (slurp f)])))))

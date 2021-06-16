(ns clojure.test-clojure.lambda
  (:use clojure.test)
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :refer [starts-with?]])
  (:import [java.util.stream Stream Collectors]
           [clojure.lang IFn]
           [clojure.test LambdaTestFns]))

(deftest test-lambda-conversion
  (testing "Calling a static method which have a FunctionalInterface parameter"
      (is (= "Yes"
             (LambdaTestFns/test (fn [value] (starts-with? value "s")) "start")
          )
      )
  )

  (testing "Calling a instance method which have a FunctionalInterface parameter with dot macro"
      (is (= ["A"]
             (.. (Stream/of "a")
                 (map (fn [item] (.toUpperCase item)))
                 (collect (Collectors/toList))
             )
           )
       )
  )
)
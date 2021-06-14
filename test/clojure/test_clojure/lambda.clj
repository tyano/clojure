(ns clojure.test-clojure.lambda
  (:use clojure.test)
  (:import [java.util.stream Stream Collectors]
           [clojure.lang IFn]
           [java.util.function Function]))

(deftest test-lambda-conversion
  (is (= ["A"]
         (.. (Stream/of "a")
             (map (fn [item] (.toUpperCase item)))
             ;;(map (reify Function (apply [this v] (.toUpperCase v))))
             (collect (Collectors/toList))
             )
           )))
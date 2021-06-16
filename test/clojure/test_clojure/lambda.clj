(ns clojure.test-clojure.lambda
  (:use clojure.test)
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :refer [starts-with?]])
  (:import [java.util.stream Stream Collectors]
           [java.util.function BiFunction Function Supplier]
           [java.util Iterator]
           [java.util.concurrent Callable]
           [clojure.lang Reflector IFn APersistentMap ISeq]
           [lambda LambdaTestFns SamInterfaceWithoutAnnotation SamInterfaceWithObjectsMethods 
            NotSamInterfaceWithEquals NotSamInterfaceWithHashcode NotSamInterfaceWithToString]))

(deftest test-lambda-conversion
  (testing "Calling a static method which have a FunctionalInterface parameter"
    (is (= "Yes" 
           (LambdaTestFns/test (fn [value] (starts-with? value "s")) "start"))))

  (testing "Calling a instance method which have a FunctionalInterface parameter with dot macro"
    (is (= ["A"]
           (.. (Stream/of "a")
               (map (fn [item] (.toUpperCase item)))
               (collect (Collectors/toList))))))

  (testing "can convert a fn to a FunctionalInterface by Reflector/lambdaConversion"
    (is (instance? BiFunction (Reflector/lambdaConversion BiFunction (fn [a b] (+ a b)))))))

(deftest test-reflector-functions-for-lambda
  (testing "canLambdaConversion"
    (is (true? (Reflector/canLambdaConversion Function IFn)))
    (is (true? (Reflector/canLambdaConversion Supplier APersistentMap)))
    (is (true? (Reflector/canLambdaConversion SamInterfaceWithoutAnnotation IFn)))
    (is (false? (Reflector/canLambdaConversion Iterator IFn)))
    (is (false? (Reflector/canLambdaConversion Function ISeq)))

    ;; A interface with many methods that have only one method without methods of same signature with Object class is a SAM type.
    (is (true? (Reflector/canLambdaConversion SamInterfaceWithObjectsMethods IFn)))
    
    ;; Runnable and Callable are FunctionalInterfaces but IFn already implements them.
    (is (false? (Reflector/canLambdaConversion Callable IFn))) 
    (is (false? (Reflector/canLambdaConversion Runnable IFn)))
    
    ;; Interfaces with a one method but it have same signature with Object's methods are not SAM type.
    (is (false? (Reflector/canLambdaConversion NotSamInterfaceWithEquals IFn)))
    (is (false? (Reflector/canLambdaConversion NotSamInterfaceWithHashcode IFn)))
    (is (false? (Reflector/canLambdaConversion NotSamInterfaceWithToString IFn)))

    )
  
  (testing "findSingleAbstractMethod"
    (let [method (.get (Reflector/findSingleAbstractMethod SamInterfaceWithObjectsMethods))]
      (is (= "sample" (.getName method)))
      (is (= "int" (.getName (.getReturnType method)))))
    
    (is (thrown-with-msg? IllegalArgumentException 
                          #".* is not a SAM type." 
                          (Reflector/findSingleAbstractMethod Iterator))))
  
  (testing "convertArgs"
    (let [parameters (into-array Class [String Function Long])
          args       (into-array Object ["test" (fn [arg1] (.toUpperCase arg1)) (Long/valueOf 9)])
          converted  (Reflector/convertArgs parameters args)]
      (is (instance? String (first converted)))
      (is (instance? Function (second converted)))
      (is (instance? Long (nth converted 2)))
      (is (= "test" (first converted)))
      (is (= (Long/valueOf 9) (nth converted 2)))
      (is (= "TESTVALUE" (.apply (second converted) "testvalue")))))
  )
    

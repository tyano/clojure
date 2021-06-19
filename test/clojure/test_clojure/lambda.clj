(ns clojure.test-clojure.lambda
  (:use clojure.test)
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :refer [starts-with?]])
  (:import [java.util.stream Stream Collectors IntStream LongStream]
           [java.util.function BiFunction Function Supplier LongUnaryOperator]
           [java.util Iterator]
           [java.util.concurrent Callable]
           [clojure.lang Reflector IFn APersistentMap ISeq ExprAccessor]
           [lambda LambdaTestFns SamInterfaceWithoutAnnotation SamInterfaceWithObjectsMethods 
            NotSamInterfaceWithEquals NotSamInterfaceWithHashcode NotSamInterfaceWithToString]))



(defn generate-upper-case-fn
  []
  (fn [item] (.toUpperCase item)))

(def numberRef (atom 2))

(defn doubler
  [v]
  (* 2 v))

(deftest test-lambda-conversion
  (testing "Calling a static method which have a FunctionalInterface parameter"
    (is (= "Yes" 
           (LambdaTestFns/test (fn [value] (starts-with? value "s")) "start"))))

  (testing "Calling a instance method which have a FunctionalInterface parameter with dot macro"
    (is (= ["A"]
           (.. (Stream/of "a")
               (map (fn [item] (.toUpperCase item)))
               (collect (Collectors/toList))))))
  
  (testing "Can use any expression which generate a fn at the place where FunctionalInterface is required."
    (is (= "Yes"
           (LambdaTestFns/test (let [target-str "s"] (fn [value] (starts-with? value target-str))) "start")))

    (is (= [2 4 6]
           (.. [1 2 3]
               (stream)
               (map (let [number @numberRef] #(* number %)))
               (collect (Collectors/toList)))))

    (is (= [2 4 6]
           (.. [1 2 3]
               (stream)
               (map doubler)
               (collect (Collectors/toList)))))

    (is (= ["A"]
           (.. (Stream/of "a")
               (map (generate-upper-case-fn))
               (collect (Collectors/toList))))))
  
  (testing "Generate a FunctionalInterface which have a primitive return type"
    (let [result (.. ["1" "2"]
                     (stream)
                     (mapToInt (fn [v] (Integer/parseInt ^String v)))
                     (toArray))]
      (is (= 2 (alength result)))
      (is (= 1 (aget result 0)))
      (is (= 2 (aget result 1)))))
  
  (testing "Can IFn handle primitive stream like IntStream"
    (let [result (.. (IntStream/of (int-array [1 2 3]))
                     (map (fn [value] (* value 2)))
                     (toArray))]
      (is (= (Class/forName "[I") (class result)))
      (is (= 2 (aget result 0)))
      (is (= 4 (aget result 1)))
      (is (= 6 (aget result 2)))))
  
  (testing "invokePrim must be used on a fn with correct type-hints instead of IFn.invoke"
    (let [type-result (atom nil)
          result (.. (LongStream/of (long-array [1]))
                     ;; LongUnaryOperator#applyAsLong: (long) -> long. 
                     ;; If fn is type-hinted as (long) -> long, invokePrim must be called instead of invoke.
                     (map (fn ^long [^long value]
                            (let [stacktrace-element (first (.getStackTrace ^Throwable (ex-info "dummy" {})))]
                              (reset! type-result (.getMethodName stacktrace-element)))
                            (* value 2)))
                     (toArray))]
      
      (is (= (Class/forName "[J") (class result)))
      (is (= 2 (aget result 0)))
      
      (is (= "invokePrim" @type-result))))
  
  (testing "invokePrim is faster than IFn.invoke"
    (let [start1  (System/currentTimeMillis)
          result1 (.. (LongStream/of (long-array (range 0 10000000)))
                      (map (fn ^long [^long value] (* value 2)))
                      (toArray))
          end1    (System/currentTimeMillis)
          result2 (.. (LongStream/of (long-array (range 0 10000000)))
                      (map (fn [value] (* value 2)))
                      (toArray))
          end2    (System/currentTimeMillis)
          time1   (- end1 start1)
          time2   (- end2 end1)]

      (println (str "time1 = " time1 "ms. time2 = " time2 "ms."))
      (is (< time1 time2))))

  (testing "can convert a fn to a FunctionalInterface by Reflector/tryLambdaConversion"
    (is (instance? BiFunction (Reflector/tryLambdaConversion BiFunction (fn [a b] (+ a b))))))
  
  (testing "The functionalInterface made of Reflector/tryLambdaConversion works well"
    (let [f      (Reflector/tryLambdaConversion LongUnaryOperator (fn [value] (* value 2)))
          result (.. (LongStream/of (long-array [1]))
                     (map f)
                     (toArray))]
      (is (= (Class/forName "[J") (class result)))
      (is (= 2 (aget result 0)))))
  
  (testing "Also Reflector/tryLambdaConversion use invokePrim instead of invoke if a IFn implements a correct primitive interface"
    (let [type-result (atom nil)
          f      (Reflector/tryLambdaConversion LongUnaryOperator (fn ^long [^long value]
                                                                    (let [stacktrace-element (first (.getStackTrace ^Throwable (ex-info "dummy" {})))]
                                                                      (reset! type-result (.getMethodName stacktrace-element)))
                                                                    (* value 2)))
          result (.. (LongStream/of (long-array [1]))
                     (map f)
                     (toArray))]
      
      (is (= "invokePrim" @type-result))))

  ;; (testing "Reflection warning will be printed when compiler can not recognize the type of expression is IFn"
  ;;   (let [expr   '(do
  ;;                  (import [java.util.stream Collectors])
  ;;                  (import [java.io StringWriter])
  ;;                  (let [writer (new java.io.StringWriter)]
  ;;                    (binding [*err* writer
  ;;                              *warn-on-reflection* true]
  ;;                      (.. [1 2]
  ;;                          (stream)
  ;;                          (map (let [m 2] (fn [value] (* value m))))
  ;;                          (collect (Collectors/toList))))
  ;;                    (str writer)))]
  ;;     (is (= "Reflection"
  ;;            (binding [*warn-on-reflection* true] (eval expr))))))

  (testing "if a fn expression is placed at FunctionalInterface, Arity-checking must work."
    (let [ex (try 
               (eval '(do (import [java.util.stream Collectors])
                          (.. [1 2]
                              (stream)
                              (map (fn [a b] (+ a b)))
                              (collect (Collectors/toList)))))
               (catch Throwable ex ex))]
      (is (= "clojure.lang.Compiler$CompilerException" (.. ex (getClass) (getName))))
      (is (= "java.lang.IllegalArgumentException" (.. ex (getCause) (getClass) (getName))))
      (is (re-matches #"No 1 arity function is found in a supplied fn. Target SAM type =.*" (.getMessage (.getCause ex)))))))

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
    (is (false? (Reflector/canLambdaConversion NotSamInterfaceWithToString IFn))))

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
      (is (= "TESTVALUE" (.apply (second converted) "testvalue"))))))

(deftest test-clj-eval
  (is (= ["A"]
         (eval (read-string "(do (import '[java.util.stream Stream Collectors])
                                 (.. (Stream/of \"a\")
                                     (map (fn [item] (.toUpperCase item)))
                                     (collect (Collectors/toList))))")))))

(deftest test-lambdaexpr-eval
  (let [fndata (read-string "(fn [value] (.toUpperCase value))")
        lambda (.call (ExprAccessor/lambdaExpr Function fndata))]
    (is (= ["A"]
           (.. (Stream/of "a")
               (map lambda)
               (collect (Collectors/toList)))))))
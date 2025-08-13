(ns Main.Bytecode-test
  (:require [clojure.test :refer [deftest is]]
            [Main.Bytecode :refer [replace-at insert-at labels-inserted-before
                                   add-labels make-labels-map get-instructions]]))

(deftest replace-at-test
  (is (= '((:a :1) (:b) (:c) (:d) (:e)) (replace-at '((:a :0) (:b) (:c) (:d) (:e)) :1 0)))
  (is (= '((:a) (:b :1) (:c) (:d) (:e)) (replace-at '((:a) (:b) (:c) (:d) (:e)) :1 1)))
  (is (= '((:a) (:b) (:c :1) (:d) (:e)) (replace-at '((:a) (:b) (:c) (:d) (:e)) :1 2)))
  (is (= '((:a) (:b) (:c) (:d :1) (:e)) (replace-at '((:a) (:b) (:c) (:d) (:e)) :1 3)))
  (is (= '((:a) (:b) (:c) (:d) (:e :1)) (replace-at '((:a) (:b) (:c) (:d) (:e)) :1 4))))

(deftest insert-at-test
  (is (= '((:a) (:b) (:c) (:1) (:d) (:e)) (insert-at '((:a) (:b) (:c) (:d) (:e)) '(:1) 3)))
  (is (= '((:a) (:b) (:1) (:c) (:d) (:e)) (insert-at '((:a) (:b) (:c) (:d) (:e)) '(:1) 2)))
  (is (= '((:a) (:1) (:b) (:c) (:d) (:e)) (insert-at '((:a) (:b) (:c) (:d) (:e)) '(:1) 1)))
  (is (= '((:a) (:b) (:c) (:d) (:1) (:e)) (insert-at '((:a) (:b) (:c) (:d) (:e)) '(:1) 4)))
  (is (= '((:a) (:b) (:c) (:d) (:e) (:1)) (insert-at '((:a) (:b) (:c) (:d) (:e)) '(:1) 5)))
  (is (= '((:1) (:a) (:b) (:c) (:d) (:e)) (insert-at '((:a) (:b) (:c) (:d) (:e)) '(:1) 0)))
  (is (= '((:a) (:1) (:b) (:c) (:d) (:e)) (insert-at '((:a) (:b) (:c) (:d) (:e)) '(:1) 1)))
  (is (= '((:a) (:b) (:1) (:c) (:d) (:e)) (insert-at '((:a) (:b) (:c) (:d) (:e)) '(:1) 2)))
  (is (= '((:a) (:b) (:c) (:d) (:1) (:e)) (insert-at '((:a) (:b) (:c) (:d) (:e)) '(:1) 4)))
  (is (= '((:a) (:b) (:c) (:d) (:e) (:1)) (insert-at '((:a) (:b) (:c) (:d) (:e)) '(:1) 5)))
  (is (= '((:a) (:b) (:c) (:1) (:d) (:e)) (insert-at '((:a) (:b) (:c) (:d) (:e)) '(:1) 3)))
  (is (= '((:a) (:b) (:1) (:c) (:d) (:e)) (insert-at '((:a) (:b) (:c) (:d) (:e)) '(:1) 2))))

(deftest lablels-inserted-before-test
  (is (= (labels-inserted-before 1 1 '{1 2 2 0}))))

(deftest add-labels-test
  (is (= '((:label_0) (:iload_0) (:goto :label_0) (:ireturn))
         (add-labels '((:iload_0) (:goto -1) (:ireturn)) {1 0})))
  (is (= '((:iload_0) (:goto :label_0) (:label_0) (:istore_1) (:ireturn))
         (add-labels '((:iload_0) (:goto 1) (:istore_1) (:ireturn)) {1 2})))
  (is (= '((:iload_0) (:goto :label_0) (:istore_1) (:label_0) (:ireturn))
         (add-labels '((:iload_0) (:goto 2) (:istore_1) (:ireturn)) {1 3})))
  (is (= '((:label_0) (:bipush 1) (:goto :label_0) (:ireturn))
         (add-labels '((:bipush 1) (:goto -1) (:ireturn)) {1 0})))
  (is (= '((:label_0) (:label_1) (:iload_0) (:goto :label_0) (:goto :label_1) (:ireturn))
         (add-labels '((:iload_0) (:goto -1) (:goto -2) (:ireturn)) {1 0 2 0})))
  (is (= '((:iload_0) (:dup) (:dup) (:dup) (:swap) (:ifgt :label_0) (:iinc 0 0) (:label_0) (:ireturn))
         (add-labels '((:iload_0) (:dup) (:dup) (:dup) (:swap) (:ifgt 2) (:iinc 0 0) (:ireturn)) {5 7})))
  (is (= '((:iload_0) (:dup) (:dup) (:ifne :label_0) (:swap) (:label_0) (:ifgt :label_1) (:iinc 0 0) (:label_1) (:ireturn))
         (add-labels '((:iload_0) (:dup) (:dup) (:ifne 2) (:swap) (:ifgt 2) (:iinc 0 0) (:ireturn)) '{3 5 5 7}))))

(deftest make-labels-map-test
  (is (= 1 (count (make-labels-map (add-labels '((:iload_0) (:goto -1) (:ireturn)) '{1 0}))))))

(deftest get-instructions-test
  (is (= 4 (. (get-instructions '{ :code ((:iload_0) (:ifeq -1) (:ireturn)) :jumps {1 0}}) size)))
  (is (= 2 (. (get-instructions '{ :code ((:iload_0) (:ireturn)) :jumps {}}) size)))
  (is (= 1 (. (get-instructions '{ :code ((:ireturn)) :jumps {}}) size))))

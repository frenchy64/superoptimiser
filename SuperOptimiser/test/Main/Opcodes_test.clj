(ns Main.Opcodes-test
  (:require [clojure.test :refer [deftest is]]
            [Main.Opcodes :refer [opcode-sequence-new contains-no-redundant-pairs?]]))

(deftest opcode-sequence-new-test
  (is (= '([(:iload_0) (:ireturn)])
         (opcode-sequence-new 2 1)))
  (is (= '([(:iload_0) (:ireturn)]
           [(:iload_0) (:iinc) (:ireturn)]
           [(:iload_0) (:dup) (:ireturn)]
           [(:iload_0) (:iload_0) (:ireturn)]
           [(:iload_0) (:ineg) (:ireturn)])
         (opcode-sequence-new 3 1)))
  (is (= [[(:iload_0) (:istore_2) (:iconst_3) (:iflt) (:iconst_3) (:iflt) (:iconst_3) (:iload_2) (:iload_1) (:ireturn)]
          [(:iload_0) (:istore_2) (:iconst_3) (:iflt) (:iconst_3) (:iflt) (:iconst_3) (:iload_1) (:iload_2) (:ireturn)]
          [(:iload_0) (:istore_2) (:iconst_3) (:iflt) (:iconst_3) (:iflt) (:iinc) (:iload_2) (:iload_1) (:ireturn)]
          [(:iload_0) (:istore_2) (:iconst_3) (:iflt) (:iconst_3) (:iflt) (:iinc) (:iload_1) (:iload_2) (:ireturn)]
          [(:iload_0) (:istore_2) (:iconst_3) (:iflt) (:iconst_3) (:iflt) (:iconst_2) (:iload_2) (:iload_1) (:ireturn)]
          [(:iload_0) (:istore_2) (:iconst_3) (:iflt) (:iconst_3) (:iflt) (:iconst_2) (:iload_1) (:iload_2) (:ireturn)]
          [(:iload_0) (:istore_2) (:iconst_3) (:iflt) (:iconst_3) (:iflt) (:iconst_4) (:iload_2) (:iload_1) (:ireturn)]
          [(:iload_0) (:istore_2) (:iconst_3) (:iflt) (:iconst_3) (:iflt) (:iconst_4) (:iload_1) (:iload_2) (:ireturn)]
          [(:iload_0) (:istore_2) (:iconst_3) (:iflt) (:iconst_3) (:iflt) (:iload_2) (:iflt) (:iload_1) (:ireturn)]
          [(:iload_0) (:istore_2) (:iconst_3) (:iflt) (:iconst_3) (:iflt) (:iload_2) (:iconst_3) (:iload_1) (:ireturn)]]
         (opcode-sequence-new 10 2)))
  )
  

;(deftest is-valid?-test)
(deftest contains-no-redundant-pairs?-test
  (is (= false (contains-no-redundant-pairs? '((:ixor) (:swap) (:swap)))))
  (is (= false (contains-no-redundant-pairs? '((:swap) (:swap)))))
  (is (= false (contains-no-redundant-pairs? '((:swap) (:swap) (:ixor)))))
  (is (= true (contains-no-redundant-pairs? '((:ixor) (:swap) (:ixor) (:swap))))))

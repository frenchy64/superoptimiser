(ns Drivers.Inc
  (:require [Main.Opcodes :as opcodes]))
(use 'Main.Superoptimise)

; Superoptimises the Inc function
;
; ILOAD_0 IRETURN is the optimal answer :)

(def class-name "IncTest")
(def method-name "inc")
(def method-signature "(I)I")

(defn constraints []
  (map (fn [i]
         (fn [clazz]
           (= (inc i)
              (invoke-method clazz method-name i))))
       (range 10)))

(comment
  (invoke-method clojure.lang.Numbers "inc" 0)
  (doseq [constraint (constraints)]
    (assert (constraint clojure.lang.Numbers)))
  )

(def seq-len 3)

(comment
  (first (Main.Opcodes/opcode-sequence-new 3 1)))

(defn -main []
  (time
    (doall
      (superoptimise-pmap seq-len class-name method-name method-signature (constraints))))
  (shutdown-agents))

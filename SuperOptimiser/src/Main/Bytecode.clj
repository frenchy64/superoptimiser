(ns Main.Bytecode
  (:use [clojure.tools.logging :only (info)]))
(use 'Main.Global)
(import '(clojure.lang DynamicClassLoader))
(import '(java.io FileOutputStream))
(import '(org.objectweb.asm ClassWriter Opcodes))
(import '(org.objectweb.asm.tree AbstractInsnNode VarInsnNode InsnNode IincInsnNode JumpInsnNode IntInsnNode ClassNode MethodNode InsnList LabelNode))

; This package handles the creation of Java class files.

(def class_num (atom 0))

(defn instantiate-classloader
  "Returns a new instance of the Class Loader - used to recreate it when necessary"
  [cur]
  (new clojure.lang.DynamicClassLoader))

(def classloader (atom (new clojure.lang.DynamicClassLoader)))

(defn is-a-label?
  "Is the keyword in the sequence passed in a label?"
  [op]
  (.startsWith (name (first op)) "label_"))

(defn add-opcode
  "Creates a child of an AbstractInsNode and returns it"
  [op labels & argseq]
  (let [args (flatten argseq) opcode (first op)]

  (if (:jump (opcode opcodes)) (new JumpInsnNode ((opcodes opcode) :opcode) ((first args) labels))
    
    ; Look up any label node from the labels map passed in

    (if (is-a-label? op) (get labels opcode)
		  (or
	      (case opcode
	          :istore (new VarInsnNode 54 (first args))
	          :istore_0 (new VarInsnNode 54 0)
	          :istore_1 (new VarInsnNode 54 1)
	          :istore_2 (new VarInsnNode 54 2)
	          :istore_3 (new VarInsnNode 54 3)
	          :iload (new VarInsnNode 21 (first args))
	          :iload_0 (new VarInsnNode 21 0)
	          :iload_1 (new VarInsnNode 21 1)
	          :iload_2 (new VarInsnNode 21 2)
	          :iload_3 (new VarInsnNode 21 3)
	          :iinc (new IincInsnNode (first args) (second args)) 
	          :bipush (new IntInsnNode 16 (first args)) 
	          nil)
       (if (nil? ((opcodes opcode) :args)) (new InsnNode ((opcodes opcode) :opcode))))))))
    
(defn add-opcode-and-args
  "Pulls an opcode off the sequence provided, adds it and any arguments to the insnlist, returns the remainder of the sequence"
  [insnlist ocs labels]
  (let [op-tuple (first ocs) op (first op-tuple) num-args (if (is-a-label? op-tuple) 0 (count ((opcodes op) :args)))]
    (. insnlist add (add-opcode op-tuple labels (rest op-tuple)))
    (rest ocs)))

(defn replace-at
  "Create a new sequence consisting of the input sequence s with the item at position p having its argument replaced by i"
  [s i p]
  (concat (take p s) (list (list (first (nth s p)) i)) (nthrest s (inc p))))

(defn insert-at
  "Create a new sequence consisting of the input sequence s with an extra item i inserted at a position distance d from p"
  [s i p]
  (concat (take p s) (list i) (nthrest s p)))
 
(defn labels-inserted-before
  "How many of the jumps in the map jl have a src node before max-jump-src and insert a label before node?"
  [max-jump-src node jl]
  (reduce #(if (and (< %2 max-jump-src) (<= (get jl %2) node)) (inc %1) (identity %1)) 0 (keys jl)))

(defn add-labels
  "Takes a sequence of opcodes+arguments and a map of jumps; uses the map to add appropriate label entries to correspond to branch destinations"
  [code jumps]
  (loop [remainder jumps output code jump-num 0]
      (if (empty? remainder) output
        (let [cur-jump (first remainder)
              src (first cur-jump)
              dst (second cur-jump)
              labels-before-src-raw (labels-inserted-before src src jumps)
              labels-before-src (if (< dst src) (inc labels-before-src-raw) labels-before-src-raw)
              labels-before-dst (labels-inserted-before src dst jumps)
              label-key (keyword (str "label_" jump-num))]
          (recur (rest remainder)
                 (replace-at
                   (insert-at output (list label-key) (+ dst labels-before-dst))
                   label-key
                   (+ src labels-before-src))
                 (inc jump-num))))))
  
(defn make-labels-map
  "Take the sequence of opcodes provided and make a map of name to LabelNode, for each label"
  [o]
  (into {} (map #(assoc {} (first %) (new LabelNode))
                (distinct
                  (filter is-a-label? o)))))

(defn get-instructions
  "Turns the supplied map containing a list of opcodes and arguments into an InsnList"
  [a]
  (try
	  (let [l (new InsnList) labelled-opcodes (add-labels (:code a) (:jumps a)) labels-map (make-labels-map labelled-opcodes)]
	    (loop [codes labelled-opcodes]
	      (if (empty? codes) l
	        (recur (add-opcode-and-args l codes labels-map)))))
   (catch Exception e (do
                        (println "Exception " e a)
                        (throw e)))))

(defn get-class-bytes
  "Creates a Java Class from the supplied data, returns an array of bytes representing that class. Input should be a map containing keys
   :length, :vars and :code, containing the number of opcodes, the max. number of local variables and a list of opcodes and arguments"
  [sequence className methodName methodSig]
  (let [cn (new ClassNode)
        cw (new ClassWriter ClassWriter/COMPUTE_MAXS)
        mn (new MethodNode (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) methodName methodSig nil nil)
        ins (get-instructions sequence)]
    (set! (. cn version) Opcodes/V1_5)
    (set! (. cn access) Opcodes/ACC_PUBLIC)
    (set! (. cn name) className)
    (set! (. cn superName) "java/lang/Object")
    
    (doto (. mn instructions) (.add ins))
    (doto (. cn methods) (.add mn))
    (. cn accept cw)
    (. cw toByteArray)))

(defn write-bytes
  "Write a byte array to the filename specified"
  [fn b]
    (with-open [out (FileOutputStream. fn)]
      (.write out b)))

(defn load-class
  "Load a class of the given name from the given bytecode"
  ([name bytecode cl]
      (.defineClass cl name bytecode '())))

(defn get-class
  "Creates and loads a class file with the given name"
  [classmap className methodName methodSig seqnum]
    (try
      (let [full-class-name (str className "-" seqnum)
            num (swap! class_num inc)]
        (if (= 0 (mod num 1000000)) (info "created class #" num))
        (load-class full-class-name
                    (get-class-bytes classmap full-class-name methodName methodSig)
                    (if (= 0 (mod num 50000)) (swap! classloader instantiate-classloader) @classloader)))
      (catch ClassFormatError cfe (do
                                    (println cfe)
                                    nil))))

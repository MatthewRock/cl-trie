(in-package #:cl-trie)

(defclass node ()
  ((value :initarg :value :accessor value)
   (children :initarg :children :accessor children :type node)
   (key :initarg :key :reader key :initform (error "You need to provide a key for node!"))
   (activep :initarg :activep :accessor activep :type boolean
            :documentation
            "Indicates whether the node is actively holding any value,
 or if it's just a part of a chain of nodes leading to other active node."))
  (:default-initargs
    :value nil
    :children nil
    :activep nil)
  (:documentation "Node of trie, allowing to hold key and value, as well as references to next nodes. The node can be either active and hold some value, or inactive, being part of chain and value being not important."))

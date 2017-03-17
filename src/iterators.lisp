(defpackage #:cl-trie/iterators
  (:use :cl))

(in-package #:cl-trie/iterators)

(defun string-iterator ())


;; Alternatives:
;; OR
;; Provide an "Iterator" interface; user has to provide an iterator, which is a function,
;; which will iterate over key. Example:
;; Usage:
;; (string-iterator "key" function)
;; Example of lookup function(pseudocode):
(defmethod lookup (key trie)
  (string-iterator key (lambda (x)
                         (if (funcall (comparator trie) key (all-keys-of trie))
                             (if (activep found-node)
                                 (value node))
                             (return-from lookup nil)))))

;; As we can see, there are several problems:
;; (1) Need to go recursive
;; (2) Function is a bit harder to understand
;; (3) Need to rewrite trie so that each node is proper trie.

;; However, this might not be a bad option. Trie's branch is another trie after all, and
;; iterating would be faster than splitting into something.

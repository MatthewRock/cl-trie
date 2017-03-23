(defpackage #:cl-trie/tests
  (:use :cl :fiveam)
  (:export run-tests))

(in-package #:cl-trie/tests)

(defun run-tests ()
  (run! 'cl-trie-suite))

(def-suite cl-trie-suite
    :description "Tests for cl-trie package.")

(in-suite cl-trie-suite)

(test trie-creation
  ;; No key should signal a warning.
  (signals cl-trie:empty-key-warning (make-instance 'cl-trie:trie))

  (finishes (make-instance 'cl-trie:trie :value 5 :activep t))
  (finishes (make-instance 'cl-trie:trie :value 5))
  (finishes (make-instance 'cl-trie:trie :activep nil))
  (finishes (make-instance 'cl-trie:trie :key "a"))
  (finishes (make-instance 'cl-trie:trie :children nil))
  (finishes (make-instance 'cl-trie:trie :key "a" :value 5))
  (finishes (make-instance 'cl-trie:trie :key "a" :activep t))
  (finishes (make-instance 'cl-trie:trie :key "a" :value 5 :activep t))
  (finishes (make-instance 'cl-trie:trie :key "a" :value 5 :activep t :children nil)))

(defpackage #:cl-trie/tests
  (:use :cl :fiveam)
  (:export run-tests))

(in-package #:cl-trie/tests)

(defun run-tests ()
  (run! 'cl-trie-suite))

(defmacro ignore-warnings (&body body)
  `(handler-bind ((warning #'muffle-warning))
     ,@body))

(def-suite cl-trie-suite
    :description "Tests for cl-trie package.")

(in-suite cl-trie-suite)

(test trie-creation-sanity
  ;; No key should signal a warning.
  (signals cl-trie:empty-key-warning (make-instance 'cl-trie:trie))
  ;; Various keyword arguments configuration
  (ignore-warnings
    (finishes (make-instance 'cl-trie:trie :value 5 :activep t))
    (finishes (make-instance 'cl-trie:trie :value 5))
    (finishes (make-instance 'cl-trie:trie :activep nil))
    (finishes (make-instance 'cl-trie:trie :key #\a))
    (finishes (make-instance 'cl-trie:trie :children nil))
    (finishes (make-instance 'cl-trie:trie :key #\a :value 5))
    (finishes (make-instance 'cl-trie:trie :key #\a :activep t))
    (finishes (make-instance 'cl-trie:trie :key #\a :value 5 :activep t))
    (finishes (make-instance 'cl-trie:trie :key #\a :value 5 :activep t :children nil))
    ;; Hack - fail if warning is raised
    (finishes (handler-bind ((warning (lambda (x)
                                        (error "Warning ~A encountered, no warnings should appear here!"
                                               x))))
                (make-instance 'cl-trie:trie :verbose nil)))))

(test trie-activep-sanity
  (ignore-warnings
    (is-false (cl-trie:activep (make-instance 'cl-trie:trie)))
    (is-false (cl-trie:activep (make-instance 'cl-trie:trie :key #\a)))
    (is-true (cl-trie:activep (make-instance 'cl-trie:trie :key #\a :value 5)))
    (is-true (cl-trie:activep (make-instance 'cl-trie:trie :key #\a :value 5 :activep t)))
    (is-true (cl-trie:activep (make-instance 'cl-trie:trie :key #\a :value 5 :activep t :children nil)))
    (is-false (cl-trie:activep (make-instance 'cl-trie:trie :key #\a :value 5 :activep nil :children nil)))
    (is-false (cl-trie:activep (make-instance 'cl-trie:trie :key #\a :value 5 :activep nil)))
    (is-false (cl-trie:activep (make-instance 'cl-trie:trie :key #\a :activep nil)))
    (is-false (cl-trie:activep (make-instance 'cl-trie:trie :activep nil)))
    (is-false (cl-trie:activep (make-instance 'cl-trie:trie :activep nil :value 5)))
    (is-false (cl-trie:activep (make-instance 'cl-trie:trie :activep nil :value 5 :children nil)))
    (is-false (cl-trie:activep (make-instance 'cl-trie:trie :activep nil :children nil)))
    (is-true (cl-trie:activep (make-instance 'cl-trie:trie :activep t :children nil)))
    (is-true (cl-trie:activep (make-instance 'cl-trie:trie :activep t :children nil :value 5)))
    (is-true (cl-trie:activep (make-instance 'cl-trie:trie :activep t :value 5)))
    (is-true (cl-trie:activep (make-instance 'cl-trie:trie :value 5)))
    (is-true (cl-trie:activep (make-instance 'cl-trie:trie :value 5 :children nil)))))

(test trie-value-sanity
  (ignore-warnings
    (let ((trie (make-instance 'cl-trie:trie :value 13 :activep t)))
      (is (= (cl-trie:value trie) 13))
      (finishes (setf (cl-trie:value trie) 22))
      (is (= (cl-trie:value trie) 22))
      (setf (cl-trie:activep trie) nil)
      ;; Activep doesn't affect the value
      (is (= (cl-trie:value trie) 22))
      (setf (cl-trie:value trie) 13)
      ;; But setting the vavlue does affect activep
      (is-true (cl-trie:activep trie)))))

(test trie-key-sanity
  (let ((trie (make-instance 'cl-trie:trie :key #\a)))
    (is (char= (cl-trie:key trie) #\a)))
  (signals cl-trie:wrong-key-type-error (make-instance 'cl-trie:trie :key "a")))

(test trie-children-sanity
  (let* ((trie (make-instance 'cl-trie:trie :key #\a :value 5))
         (trie2 (make-instance 'cl-trie:trie :key #\m :children (list trie)))
         (trie3 (make-instance 'cl-trie:trie :key nil :children (list trie2))))
    (is (null (cl-trie:children trie)))
    (is-false (null (cl-trie:children trie2)))
    (is (= 5 (cl-trie:lookup trie3 "ma")))))

(test trie-find-node
  (let* ((trie (make-instance 'cl-trie:trie :key nil)))
    (is (zerop (length (cl-trie:children trie))))
    (is-false (cl-trie:find-node trie "a"))
    (let ((new-node (cl-trie:find-node trie "a" :create-new t)))
      (is (= 1 (length (cl-trie:children trie))))
      (is (eq (car (cl-trie:children trie))
              new-node)))
    (let ((new-node (cl-trie:find-node trie "da" :create-new t)))
      (is (= 2 (length (cl-trie:children trie))))
      (is (eq (car (cl-trie:children
                    (car (cl-trie:children trie))))
              new-node))))
  (let* ((trie (make-instance 'cl-trie:trie :key nil))
         (a-node (cl-trie:insert 5 trie "a"))
         (ali-node (cl-trie:insert 5 trie "ali"))
         (bali-node (cl-trie:insert 5 trie "bali")))
    (is (eq trie (cl-trie:find-node trie "")))
    (is (eq a-node (cl-trie:find-node trie "a")))
    (is (eq ali-node (cl-trie:find-node trie "ali")))
    (is (eq bali-node (cl-trie:find-node trie "bali")))))

(test trie-setf
  (let ((trie (make-instance 'cl-trie:trie :key nil)))
    (is (not (cl-trie:activep trie)))
    (is-false (cl-trie:key trie))
    (is (every #'null (multiple-value-list (cl-trie:lookup trie "word"))))
    (is (not (null (setf (cl-trie:lookup trie "word") 3))))
    ;; Check if all return values are non-nil under successful key search.
    (is (every #'identity (multiple-value-list (cl-trie:lookup trie "word"))))
    (is (= (cl-trie:lookup trie "word") 3))
    (setf (cl-trie:lookup trie "word") 4)
    (is (every #'identity (multiple-value-list (cl-trie:lookup trie "word"))))
    (is (= (cl-trie:lookup trie "word") 4))
    (is (every #'null (multiple-value-list (cl-trie:lookup trie "wor"))))
    (setf (cl-trie:lookup trie "wor") 15)
    (is (every #'identity (multiple-value-list (cl-trie:lookup trie "wor"))))
    (is (= (cl-trie:lookup trie "wor") 15))))

;; The same test as trie-setf.
(test trie-insert
  (let ((trie (make-instance 'cl-trie:trie :key nil)))
    (is (not (cl-trie:activep trie)))
    (is-false (cl-trie:key trie))
    (is (every #'null (multiple-value-list (cl-trie:lookup trie "word"))))
    (cl-trie:insert 3 trie "word")
    ;; Check if all return values are non-nil under successful key search.
    (is (every #'identity (multiple-value-list (cl-trie:lookup trie "word"))))
    (is (= (cl-trie:lookup trie "word") 3))
    (cl-trie:insert 4 trie "word")
    (is (every #'identity (multiple-value-list (cl-trie:lookup trie "word"))))
    (is (= (cl-trie:lookup trie "word") 4))
    (is (every #'null (multiple-value-list (cl-trie:lookup trie "wor"))))
    (cl-trie:insert 15 trie "wor")
    (is (every #'identity (multiple-value-list (cl-trie:lookup trie "wor"))))
    (is (= (cl-trie:lookup trie "wor") 15)))  )

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
  (let ((trie (make-instance 'cl-trie:trie :verbose nil)))
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

(test trie-remove-node
  (let ((trie (make-instance 'cl-trie:trie :key #\d :value 5)))
    (is (= (cl-trie:value trie) 5))
    (is (cl-trie:activep trie))
    (finishes (cl-trie:remove-node trie))
    (is-false (cl-trie:activep trie))))

;; TODO: More tests
;; TODO: See if remove-index should delete parents.
(test trie-remove-index
  (let ((trie (make-instance 'cl-trie:trie :verbose nil)))
    (setf (cl-trie:lookup trie "lisp") 1)
    (setf (cl-trie:lookup trie "lis") 3)
    (setf (cl-trie:lookup trie "dona") 5)
    ;; Check if it deletes the last node
    (is (= 1 (length (cl-trie:children (cl-trie:find-node trie "lis")))))
    (is-true (cl-trie:remove-index trie "lisp"))
    (is (= 0 (length (cl-trie:children (cl-trie:find-node trie "lis")))))))

(test trie-hash-table->trie
  (let ((hash-map (make-hash-table :test #'equal)))
    (setf (gethash "dada" hash-map) 5)
    (setf (gethash "dad" hash-map) 4)
    (setf (gethash "midori" hash-map) 12)
    (setf (gethash "" hash-map) nil)
    (let ((trie (cl-trie:hash-table->trie hash-map)))
      (is (= (cl-trie:lookup trie "dada") 5))
      (is (= (cl-trie:lookup trie "dad") 4))
      (is (= (cl-trie:lookup trie "midori") 12))
      (is (null (cl-trie:lookup trie "")))
      (is-false (cl-trie:activep (cl-trie:find-node trie "da" )))
      (is-false (cl-trie:find-node trie "dadad")))))

(test trie-all-keys
  (let ((trie (make-instance 'cl-trie:trie :verbose nil)))
    (setf (cl-trie:lookup trie "dada") 5)
    (setf (cl-trie:lookup trie "dad") 5)
    (setf (cl-trie:lookup trie "mom") 5)
    (setf (cl-trie:lookup trie "a") 5)
    (let ((keys (cl-trie:all-keys trie)))
      (is (equal keys (list  "dad" "dada" "mom" "a")))))
  (let ((trie (make-instance 'cl-trie:trie :key #\d)))
    (setf (cl-trie:lookup trie "ada") 5)
    (setf (cl-trie:lookup trie "ad") 5)
    (setf (cl-trie:lookup trie "mom") 5)
    (setf (cl-trie:lookup trie "a") 5)
    (let ((keys (cl-trie:all-keys trie)))
      (is (equal keys (list "da" "dad" "dada" "dmom"))))))

(test trie-all-values
  (let ((trie (make-instance 'cl-trie:trie :verbose nil)))
    (setf (cl-trie:lookup trie "dada") 3)
    (setf (cl-trie:lookup trie "dad") 5)
    (setf (cl-trie:lookup trie "mom") 8)
    (setf (cl-trie:lookup trie "a") 13)
    (let ((keys (cl-trie:all-values trie)))
      (is (equal keys (list 5 3 8 13)))))
  (let ((trie (make-instance 'cl-trie:trie :key #\d)))
    (setf (cl-trie:lookup trie "ada") 8)
    (setf (cl-trie:lookup trie "ad") 5)
    (setf (cl-trie:lookup trie "mom") 13)
    (setf (cl-trie:lookup trie "a") 3)
    (let ((keys (cl-trie:all-values trie)))
      (is (equal keys (list 3 5 8 13))))))

(test trie-emptyp
  (let ((trie (make-instance 'cl-trie:trie :verbose nil)))
    (is (cl-trie:emptyp trie))
    (setf (cl-trie:lookup trie "dad") 3)
    (is-false (cl-trie:emptyp trie))
    (cl-trie:remove-index trie "dad")
    (is (cl-trie:emptyp trie)))
  (let ((trie (make-instance 'cl-trie:trie :key #\d :value 3)))
    (is-false (cl-trie:emptyp trie))))

(test trie-size
  (let ((trie (make-instance 'cl-trie:trie :verbose nil)))
    (setf (cl-trie:lookup trie "dada") 3)
    (setf (cl-trie:lookup trie "dad") 5)
    (setf (cl-trie:lookup trie "mom") 8)
    (setf (cl-trie:lookup trie "a") 13)
    (is (= 4 (cl-trie:size trie))))
  (let ((trie (make-instance 'cl-trie:trie :key #\a :value 3)))
    (setf (cl-trie:lookup trie "dada") 3)
    (setf (cl-trie:lookup trie "dad") 5)
    (setf (cl-trie:lookup trie "mom") 8)
    (setf (cl-trie:lookup trie "a") 13)
    (is (= 5 (cl-trie:size trie))))
  (let ((trie (make-instance 'cl-trie:trie :verbose nil)))
    (is (= 0 (cl-trie:size trie)))))

(test trie-clear
  (let ((trie (make-instance 'cl-trie:trie :verbose nil)))
    (setf (cl-trie:lookup trie "dada") 3)
    (setf (cl-trie:lookup trie "dad") 5)
    (setf (cl-trie:lookup trie "mom") 8)
    (setf (cl-trie:lookup trie "a") 13)
    (is (= 4 (cl-trie:size trie)))
    (cl-trie:clear trie)
    (is (= 0 (cl-trie:size trie))))
  (let ((trie (make-instance 'cl-trie:trie :key #\a :value 3)))
    (setf (cl-trie:lookup trie "dada") 3)
    (setf (cl-trie:lookup trie "dad") 5)
    (setf (cl-trie:lookup trie "mom") 8)
    (setf (cl-trie:lookup trie "a") 13)
    (is (= 5 (cl-trie:size trie)))
    (cl-trie:clear trie)
    (is (= 0 (cl-trie:size trie)))))

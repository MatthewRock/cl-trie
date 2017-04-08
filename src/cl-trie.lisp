;; Copyright (c) 2017 Mateusz Malisz


;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:


;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.


;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(defpackage #:cl-trie
  (:use #:cl)
  (:export
   ;; Trie and its accessors
   trie
   key
   value
   activep
   children

   ;; Trie generic functions
   lookup
   insert
   find-node
   remove-index

   ;; Conditions
   empty-key-warning
   wrong-key-type-error
   ))

(in-package #:cl-trie)

(defgeneric (setf lookup) (new-value trie index)
  (:documentation "Set value of item at INDEX in TRIE to NEW-VALUE. Return the node that will hold the value."))

(defgeneric find-node (trie index &key create-new)
  (:documentation "Find node under INDEX in TRIE, or return NIL if no node has been found. If CREATE-NEW is T, all the nodes, including the searched one, will be created if they do not exist, effectively creating the node at index."))

(defgeneric lookup (trie index)
  (:documentation "Check if there is something at INDEX in TRIE.
Return two values, the first one being value at TRIE, and second one being
T if anything was found at index and NIL if not."))

(defgeneric insert (elem trie index)
  (:documentation "Insert ELEM as value of item at INDEX in TRIE to NEW-VALUE. Alias to (setf lookup)."))

(defgeneric remove-index (trie index)
  (:documentation "Remove INDEX entry from TRIE."))

(defgeneric all-keys (trie)
  (:documentation "Return vector of all keys of TRIE. Might be very long."))

(defgeneric all-values (trie)
  (:documentation "Return vector of all values of TRIE. Might be very long."))

(defgeneric emptyp (trie)
  (:documentation "Return T if TRIE is empty."))

(defgeneric clear (trie)
  (:documentation "Clear TRIE of its contents, leaving it empty."))

(defgeneric size (trie)
  (:documentation "Return size of TRIE, where size is number of elements found in the trie."))

(defgeneric attach (parent-trie children-trie &key on-conflict)
  (:documentation "Attach CHILDREN-TRIE to PARENT-TRIE.
If conflict happens(attached trie has the same key as already existing branch),
the on-conflict describes the chosen strategy.
Possible options:
NIL - do nothing
:merge - try to merge tries
:error - raise an error"))

(defgeneric mapkeys (fn trie)
  (:documentation "Apply function FN to each key in TRIE"))

(define-condition empty-key-warning (warning)
  ()
  (:report (lambda (con stream)
             (declare (ignore con))
             (format stream "Key for trie not provided, possibly an overlook! See documentation for more information.")))
  (:documentation "A warning emmited when key for trie is not provided."))

(define-condition wrong-key-type-error (error)
  ()
  (:report (lambda (con stream)
             (declare (ignore con))
             (format stream "A key for trie is of a wrong type! See documentation for more information.")))
  (:documentation "An error emmited when key is of a wrong type."))

(defclass trie ()
  ((%children :initarg :children :accessor children :type list
             :documentation "Children nodes of the trie.")
   (%key :initarg :key :initform (warn 'empty-key-warning) :reader key
        :documentation "A part of the sequence, that indicates that this node represents a sequence of all keys from the root up to this node, including this node.")
   (%value :initarg :value :accessor value :initform nil)
   (%activep :initarg :activep :accessor activep :type boolean :initform nil
            :documentation "A flag that tells whether a node is active and value is of interest, or is inactive and value can be ignored."))
  (:default-initargs
      :children nil
      :verbose t)
  (:documentation
   "A tree data structure that allows for efficient representation of large sets of sequential data, like strings."))

(defmethod initialize-instance :around ((instance trie)
                                        &key key verbose)
  ;; The only valid type of key is character or NIL.
  (when key
    (unless (typep key 'character)
      (error 'wrong-key-type-error)))

  (if verbose
      (call-next-method)
      ;; Muffle our warnings if we're not werbose.
      (handler-bind ((empty-key-warning #'muffle-warning))
        (call-next-method))))

(defmethod initialize-instance :after ((instance trie)
                                       &key (value nil value-provided-p)
                                         (activep nil active-provided-p))
  (declare (ignorable value activep))
  (if (and value-provided-p (not active-provided-p))
      (setf (activep instance) t)))

(defmethod (setf value) :after (value (trie trie))
  (setf (activep trie) t))

(defmethod find-node ((trie trie) (index string) &key (create-new nil))
  (if (string= index "")
      trie
      (loop for char across index
         for current-node = (or (find char (children trie) :test #'char= :key #'key)
                                ;; TODO: Insert in proper place instead of pushing
                                ;; To take advantage of binary search
                                (when create-new (car (push (make-instance 'trie :key char)
                                                            (children trie)))))
         then (or (find char (children current-node) :test #'char= :key #'key)
                  (when create-new (car (push (make-instance 'trie :key char)
                                              (children current-node)))))
         while current-node
         finally (return current-node))))

(defmethod lookup ((trie trie) (index string))
  (let ((node (find-node trie index)))
    (if (and node (activep node))
        (values (value node) t)
        (values nil nil))))

(defmethod (setf lookup) (new-value (trie trie) (index string))
  (let ((node (find-node trie index :create-new t)))
    (setf (value node) new-value)
    node))

(defmethod insert (elem (trie trie) (index string))
  (setf (lookup trie index) elem))

(defmethod remove-index ((trie trie) (index string))
  (if (string= index "")
      (setf (activep trie) nil)
      ;; Get a node before the deleted one
      (let* ((text-len (length index))
            (previous-node (find-node trie (subseq index 0 (1- text-len))))
            (node-to-delete (and previous-node (find (aref index (1- text-len))
                                                     (children previous-node)
                                                     :key #'key))))
        ;; Delete only if the nodes exist
        (when node-to-delete
          ;; If node has ancestors, only deactivate and remove value.
          (if (children node-to-delete)
              ;; Need to setf value first because (setf value) flips activep to true
              (progn (setf (value node-to-delete) nil)
                     (setf (activep node-to-delete) nil))
              ;; If node has no ancestors, we can safely remove it.
              (setf (children previous-node) (remove node-to-delete (children previous-node) :test #'eq)))
          t))))

(defun hash-map->trie (hash-map)
  "Convert hash-map to a trie.")

(defmacro dokeys ((val trie &optional ret-val) &body body)
  "Iterate over each key in the trie, evaluating body."
  'not-implemented)

(defmacro dovals ((val trie &optional ret-val) &body body)
  "Iterate over each value in the trie, evaluating body.")

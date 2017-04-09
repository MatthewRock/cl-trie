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

(in-package #:cl-trie)

(defclass trie (basic-trie)
  ((%activep :initarg :activep :accessor activep :type boolean :initform nil
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

(defmethod all-keys ((trie trie))
  (let (container)
    (labels ((count-all-keys (trie prefix)
               (declare (type trie trie)
                        (type string prefix))
               ;; Create a new prefix. If node is NIL, use empty string.
               (let ((new-prefix (concatenate 'string prefix
                                              (or (let ((k (key trie)))
                                                    (when k (string k)))
                                                  ""))))
                 (mapc (lambda (x) (count-all-keys x new-prefix)) (children trie))
                 (when (activep trie)
                   (push new-prefix container)))))
      (count-all-keys trie ""))
    container))

(defmethod all-values ((trie trie))
  (let (container)
    (labels ((count-all-vals (trie)
               (declare (type trie trie))
               (mapc #'count-all-vals (children trie))
               (when (activep trie)
                 (push (value trie) container))))
      (count-all-vals trie))
    container))

(defmethod emptyp ((trie trie))
  (labels ((is-empty-trie (trie)
             (declare (type trie trie))
             (if (activep trie)
                 nil
                 (every #'is-empty-trie (children trie)))))
    (is-empty-trie trie)))

(defun hash-table->trie (hash-map)
  "Convert hash-table to a trie."
  (declare (type hash-table hash-map))
  (let ((new-trie (make-instance 'trie :verbose nil)))
    (maphash (lambda (key val) (setf (lookup new-trie key) val))
             hash-map)
    new-trie))

(defmacro dokeys ((val trie &optional ret-val) &body body)
  "Iterate over each key in the trie, evaluating body."
  'not-implemented)

(defmacro dovals ((val trie &optional ret-val) &body body)
  "Iterate over each value in the trie, evaluating body.")

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

(defgeneric (setf lookup) (new-value thing index)
  (:documentation "Set value of item at INDEX in THING to NEW-VALUE. Return the node that will hold the value."))

(defgeneric lookup (thing index)
  (:documentation "Check if there is something at INDEX in THING.
Return two values, the first one being value at THING, and second one being
T if anything was found at index and NIL if not."))

(defgeneric insert (elem thing index)
  (:documentation "Insert ELEM as value of item at INDEX in THING to NEW-VALUE. Alias to (setf lookup)."))

(defgeneric remove-index (thing index)
  (:documentation "Remove INDEX entry from THING."))

(defgeneric all-keys (thing)
  (:documentation "Return vector of all keys of THING. Might be very long."))

(defgeneric all-values (thing)
  (:documentation "Return vector of all values of THING. Might be very long."))

(defgeneric emptyp (thing)
  (:documentation "Return T if THING is empty."))

(defgeneric clear (thing)
  (:documentation "Clear THING of its contents, leaving it empty."))

(defgeneric size (thing)
  (:documentation "Return size of THING, where size is number of elements found in the thing."))

(defgeneric mapkeys (fn thing)
  (:documentation "Apply function FN to each key in THING"))

(defclass trie ()
  ((children :initarg :children :accessor children :type list
             :documentation "Children nodes of the trie.")
   (key :initarg :key :initform (warn "Key for trie not provided, possibly an overlook!") :reader key
        :documentation "A part of the sequence, that indicates that this node represents a sequence of all keys from the root up to this node, including this node.")
   (value :initarg :value :accessor value)
   (activep :initarg :activep :accessor activep :type boolean
            :documentation "A flag that tells whether a node is active and value is of interest, or is inactive and value can be ignored."))
  (:default-initargs
   :children nil
    :value nil
    :activep nil)
  (:documentation
   "A tree data structure that allows for efficient representation of large sets of sequential data, like strings."))

(defmethod lookup ((thing trie) (index string))
  (if (string= index "")
      (if (activep thing)
          (values (value thing) t)
          (values nil nil))
      (loop for char across index
         for current-node = (find char (children thing) :test #'char= :key #'key)
         then (find char (children current-node) :test #'char= :key #'key)
         while current-node
         finally (if (and current-node (activep current-node))
                     (return (values (value current-node) t))
                     (return (values nil nil))))))

(defmethod (setf lookup) (new-value (thing trie) (index string))
  (if (string= index "")
      (progn
        (setf (value thing) new-value)
        (setf (activep thing) t)
        thing)
      (loop for char across index
         for current-node = (or
                             (find char (children thing)
                                   :test #'char= :key #'key)
                             ;; TODO: Insert in proper place instead of pushing.
                             ;; To take advantage of binary search
                             (car (push (make-instance 'trie :key char)
                                        (children thing))))
         then (or
               (find char (children current-node)
                     :test #'char= :key #'key)
               ;; TODO: Insert in proper place instead of pushing.
               ;; To take advantage of binary search
               (car (push (make-instance 'trie :key char)
                          (children current-node))))
         finally (progn
                   (setf (value current-node) new-value)
                   (setf (activep current-node) t)
                   (return current-node)))))

(defun hash-map->trie (hash-map)
  "Convert hash-map to a trie.")

(defmacro dokeys ((val trie &optional ret-val) &body body)
  "Iterate over each key in the trie, evaluating body."
  'not-implemented)

(defmacro dovals ((val trie &optional ret-val) &body body)
  "Iterate over each value in the trie, evaluating body.")

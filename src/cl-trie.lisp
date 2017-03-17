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
  (:documentation "Set value of item at INDEX in THING to NEW-VALUE."))

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
  ((first-level
    :initarg :first-level
    :documentation "A first level of nodes - conceptually these are all children nodes of root node.")
   (comparator :initarg :comparator :type function
               :documentation "A function used to test supplied key.")
   (iterator :initarg :iterator :type function
             :documentation "A function used to iterate over supplied key."))
  (:default-initargs
   :comparator #'equal
    :iterator
    :first-level nil)
  (:documentation
   "A tree data structure that allows for efficient representation of large sets of sequential data, like strings."))

(defmethod lookup ((thing trie) index)
  )

(defun hash-map->trie (hash-map)
  "Convert hash-map to a trie.")

(defmacro dokeys ((val trie &optional ret-val) &body body)
  "Iterate over each key in the trie, evaluating body."
  'not-implemented)

(defmacro dovals ((val trie &optional ret-val) &body body)
  "Iterate over each value in the trie, evaluating body.")

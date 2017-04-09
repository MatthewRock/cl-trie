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

(defgeneric remove-node (node &key preserve-value)
  (:documentation "Deactivate NODE, removing the value unless preserve-value is non-nil."))

(defgeneric all-keys (trie)
  (:documentation "Return list of all keys of TRIE."))

(defgeneric all-values (trie)
  (:documentation "Return list of all values of TRIE."))

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

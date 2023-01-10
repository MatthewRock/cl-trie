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

;; At the lowest level, the trie should consist of key, the value it holds
;; and children nodes.
;; Basic-trie is intended to work as a base class that other trie implementations will derive from.
;; basic-trie is not meant to be used by itself.

(defclass basic-trie ()
  ((%children :initarg :children :accessor children :type list
              :documentation "Children nodes of the trie.")
   (%key :initarg :key :initform (warn 'empty-key-warning) :reader key
         :documentation "A part of the sequence, that indicates that this node represents a sequence of all keys from the root up to this node, including this node.")
   (%value :initarg :value :accessor value :initform nil)))

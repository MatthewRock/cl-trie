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
   "A lexicographically sorted trie."))

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
  (labels ((add-node (children-list char)
             ;; Insert node in lexicographical order
             (let ((new-node (make-instance 'trie :key char)))
               (values new-node
                       (if (null children-list)
                           (list new-node)
                           (let ((tail (member-if (lambda (node)
                                                    (char-greaterp char (key node)))
                                                  children-list)))
                             (nconc (ldiff children-list tail)
                                    (list new-node)
                                    tail)))))))
    (if (string= index "")
        trie
        (loop for char across index
           for current-node = (or (find char (children trie) :test #'char= :key #'key)
                                  (when create-new
                                    (multiple-value-bind (node children)
                                        (add-node (children trie) char)
                                      (setf (children trie) children)
                                      node)))
           then (or (find char (children current-node) :test #'char= :key #'key)
                    (when create-new
                      (multiple-value-bind (node children)
                          (add-node (children current-node) char)
                        (setf (children current-node) children)
                        node)))
           while current-node
           finally (return current-node)))))

(defmethod lookup ((trie trie) (index string) &optional (default nil default-passed-p))
  (let ((node (find-node trie index)))
    (cond
      ;; Node was found
      ((and node (activep node)) (values (value node) t))
      ;; Node not found but we have a default
      (default-passed-p (values default nil))
      (t (values nil nil)))))

(defmethod (setf lookup) (new-value (trie trie) (index string))
  (let ((node (find-node trie index :create-new t)))
    (setf (value node) new-value)
    node))

(defmethod insert (elem (trie trie) (index string))
  (setf (lookup trie index) elem))

(defmethod remove-node ((node trie) &key preserve-value)
  ;; Need to setf value first because (setf value) flips activep to true
  (unless preserve-value
    (setf (value node) nil))
  (setf (activep node) nil)
  node)

(defmethod remove-index ((trie trie) (index string))
  (if (string= index "")
      (remove-node trie)
      ;; Get a node before the deleted one
      (let* ((text-len (length index))
             (previous-node (find-node trie (subseq index 0 (1- text-len))))
             (node-to-delete (and previous-node (find (aref index (1- text-len))
                                                      (children previous-node)
                                                      :key #'key))))
        ;; Delete only if the nodes exist
        (when node-to-delete
          ;; If node has ancestors, only deactivate it.
          (if (children node-to-delete)
              (remove-node node-to-delete)
              ;; If node has no ancestors, we can safely remove it.
              (setf (children previous-node) (remove node-to-delete (children previous-node) :test #'eq))))
        trie)))

(defmethod leafp ((trie trie))
  (emptyp trie))

(defmethod mapkeys ((fn function) (trie trie))
  (labels ((recursive-fun (trie prefix)
             (declare (type trie trie)
                      (type list prefix))
             (let ((new-prefix
                     (let ((k (key trie)))
                       (if k
                           (cons (string k) prefix)
                           prefix))))
               (mapc (lambda (x) (recursive-fun x new-prefix)) (children trie))
               (when (activep trie)
                 (let* ((len (reduce #'+ new-prefix :key #'length))
                        (string (make-string len)))
                   (loop for p in new-prefix
                         for end = len then start
                         for start = (- end (length p))
                         do (replace string p :end1 end
                                              :start1 start))
                   (funcall fn string))))))
    (recursive-fun trie '()))
  trie)

(defmethod mapvalues ((fn function) (trie trie))
  (labels ((recursive-fun (trie)
             (declare (type trie trie))
             (mapc #'recursive-fun (children trie))
             (when (activep trie)
               (funcall fn (value trie)))))
    (recursive-fun trie))
  trie)

(defmethod all-keys ((trie trie))
  (let (container)
    (mapkeys (lambda (x)
               (push x container))
             trie)
    container))

(defmethod all-values ((trie trie))
  (let (container)
    (mapvalues (lambda (x)
                 (push x container))
               trie)
    container))

(defmethod emptyp ((trie trie))
  ;; Mapvalues returns T upon successful completion
  (mapvalues (lambda (val)
               (declare (ignore val))
               (return-from emptyp nil))
             trie)
  t)

(defmethod clear ((trie trie))
  (labels ((clear-trie (trie)
             (declare (type trie trie))
             (mapc #'clear-trie (children trie))
             (when (activep trie)
               (remove-node trie))))
    (clear-trie trie)
    trie))

(defmethod size (trie)
  (let ((counter 0))
    (labels ((count-trie (trie)
               (declare (type trie trie))
               (mapc #'count-trie (children trie))
               (when (activep trie)
                 (incf counter))))
      (count-trie trie))
    counter))

(defun hash-table->trie (hash-map)
  "Convert hash-table to a trie."
  (declare (type hash-table hash-map))
  (let ((new-trie (make-instance 'trie :verbose nil)))
    (maphash (lambda (key val) (setf (lookup new-trie key) val))
             hash-map)
    new-trie))

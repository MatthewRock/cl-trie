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

(defpackage #:cl-trie-examples/basic
  (:use :cl)
  (:export analyze-file))

(in-package #:cl-trie-examples/basic)

;; We need these to process the file and store its contents.
(defun load-into-trie (file)
  (with-open-file (in file)
    ;; By default, trie warns you when you don't set the key
    ;; The only valid key is either character or NIL.
    ;; We muffle warning by setting verbose to NIL, making key take the default value
    ;; The default value for key is NIL.
    (loop with trie = (make-instance 'cl-trie:trie :verbose nil)
       for line = (read-line in nil 'eof nil)
       until (eql 'eof line)
       ;; For word in line
       for splitted-line = (cl-ppcre:split "\\s" line)
       ;; Increase the number of occurances of words.
       do (mapc (lambda (word) (setf (cl-trie:lookup trie word)
                                  ;; lookup accepts default returned value.
                                  ;; By default there are 0 words in text.
                                  (1+ (cl-trie:lookup trie word 0))))
                splitted-line)
       finally (return trie))))


(defun load-into-hash-table (file)
  (with-open-file (in file)
    (loop with ht = (make-hash-table :test #'equal)
       for line = (read-line in nil 'eof nil)
       until (eql 'eof line)
       ;; For word in line
       for splitted-line = (cl-ppcre:split "\\s" line)
       ;; Increase the number of occurances of words.
       do (mapc (lambda (word) (setf (gethash word ht)
                                     ;; By default there are 0 words in text.
                                  (1+ (gethash word ht 0))))
                splitted-line)
       finally (return ht))))

(defun print-sorted-dictionary (trie)
  ;; Since default trie implementation ensures that keys are sorted lexicographically, we can simply print all the keys
  (mapc #'print (cl-trie:all-keys trie))
  ;; There exists an all-values function, which returns all values instead of the keys.
  )

(defun print-dictionary-size (trie)
  ;; Trie's size is number of unique strings(keys) it holds.
  (print (cl-trie:size trie)))

(defun count-all-words (trie)
  (let ((counter 0))
    (cl-trie:mapvalues (lambda (x) (incf counter x)) trie)
    counter)
  ;; There exists a mapkeys function, which works the same as mapvalues, but function is applied on keys.
  ;; The order stays the same (lexicographical)
  )

(defun word-occurances (trie word)
  ;; We can pass a default value that will be returned if there is no word in dictionary.
  ;; If there are no words in dictionary, that means there are 0 words.
  (cl-trie:lookup trie word 0))

(defun print-is-empty (trie)
  ;; Emptyp is a function that returns T when trie is empty and NIL otherwise.
  (if (cl-trie:emptyp trie)
      (princ "Yes")
      (princ "No"))
  (terpri))

(defun has-substring (trie substring)
  ;; Return substring node if it was found, NIL otherwise
  (let ((substring-node (cl-trie:find-node trie substring)))
    (when substring-node
      substring-node)))

(defun announce (string)
  (terpri)
  (loop repeat 25 do (princ "=") finally (terpri))
  (format t "Now ~A!~%" string)
  (loop repeat 25 do (princ "=") finally (terpri))
  (terpri))

(defun analyze-file (file)
  ;; Measure time of loading the trie
  (announce "loading the file")
  (let ((trie (time (load-into-trie file))))
    (announce "printing the sorted dictionary")
    (print-sorted-dictionary trie)
    (announce "printing dictionary size")
    (print-dictionary-size trie)
    (announce "printing all words count (including duplicates)")
    (print (count-all-words trie))
    (announce "printing occurances of the word \"The\"")
    (print (word-occurances trie "The"))
    ;; We can set values of keys using (setf lookup) or #'insert.
    (cl-trie:insert (1+ (word-occurances trie "The")) trie "The")
    (announce "printing occurances of the word \"The\", after increasing it by 1")
    (print (word-occurances trie "The"))
    ;; Now since "The" is in the trie, we can be pretty sure it has "Th" substring in there:
    (announce "Printing whether there is a substring \"Th\" in there (should be!)")
    (print (has-substring trie "Th"))
    ;; Since we wrote has-substring so that it returns the node that it found,
    ;; we can also find that it has a substring "e" (comleting a word "The")
    ;; we can do that, because each node of trie is also a trie.
    (announce "Printing whether there is a substring \"e\" in the substring \"Th\"(should be!)")
    (print (has-substring (has-substring trie "Th") "e"))
    ;; Now remove the word "The" from the trie.
    (announce "Removing the \"The\" from the trie!")
    (cl-trie:remove-index trie "The")
    (announce "Printing occurances of the word \"The\" (should be 0)")
    (print (word-occurances trie "The"))
    (announce "Is the trie empty?")
    (print-is-empty trie)
    ;; Just to show how the function works, we clear the trie and reload it...
    (announce "Clearing the trie.")
    (cl-trie:clear trie)
    (announce "Is the trie empty?")
    (print-is-empty trie)
    ;; But this time we load it into the hash-table and convert it into the trie.
    (announce "Loading the file once again, but this time to hash-table and then converting it to trie.")
    (time (setf trie (cl-trie:hash-table->trie (load-into-hash-table file))))))

;; This example did not cover the remove-node function. I'm very sorry for that,
;; but since the code is easy, you can read it: basically, it just sets the trie's node to "off" mode.
;; Because trie nodes with values are in "on" mode.
;; It will also remove the value for garbage collector to pick up. Should you not like that behaviour,
;; e.g. because you want to filp it "on" later and don't want to lose the value,
;; you can set :preserve-value to T. This will keep the value.

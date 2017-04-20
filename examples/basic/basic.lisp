;; We need these to process the file and store its contents.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:cl-trie :cl-ppcre)))

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
       do (mapc (lambda (x) (setf (cl-trie:lookup trie x)
                                  ;; lookup accepts default returned value.
                                  ;; By default there are 0 words in text.
                                  (1+ (cl-trie:lookup trie x 0))))
                splitted-line)
       finally (return trie))))


(defun load-into-hash-table (file)
  (with-open-file (in file)
    ;; By default, trie warns you when you don't set the key
    ;; The only valid key is either character or NIL.
    ;; We muffle warning by setting verbose to NIL, making key take the default value
    ;; The default value for key is NIL.
    (loop with ht = (make-hash-table :test #'equal)
       for line = (read-line in nil 'eof nil)
       until (eql 'eof line)
       ;; For word in line
       for splitted-line = (cl-ppcre:split "\\s" line)
       ;; Increase the number of occurances of words.
       do (mapc (lambda (x) (setf (gethash x ht)
                                  ;; lookup accepts default returned value.
                                  ;; By default there are 0 words in text.
                                  (1+ (gethash x ht 0))))
                splitted-line)
       finally (return ht))))

(defun print-sorted-dictionary (trie)
  ;; Since default trie implementation ensures that keys are sorted lexicographically, we can simply print all the keys
  (cl-trie:mapkeys #'print trie)
  ;; There exists a mapvalues function, which works the same as mapkeys, but function is applied on values.
  ;; The order stays the same (lexicographical)
  )

(defun print-dictionary-size (trie)
  ;; Trie's size is number of strings it holds.
  (print (cl-trie:size trie)))

(defun count-all-words (trie)
  ;; This is not the most effective way to do this; a faster way would probably use mapvalues
  (reduce #'+ (cl-trie:all-values trie))
  ;; There exists an all-keys function, which returns all keys instead of the values.
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

(defun analyze-file (file)
  ;; Measure time of loading the trie
  (let ((trie (time (load-into-trie file))))
    (print-sorted-dictionary trie)
    (print-dictionary-size trie)
    (print (count-all-words trie))
    (print (word-occurances trie "The"))
    ;; We can set values of keys using (setf lookup) or #'insert.
    (cl-trie:insert (1+ (word-occurances trie "The")) trie "The")
    (print (word-occurances trie "The"))
    ;; Now since "The" is in the trie, we can be pretty sure it has "Th" substring in there:
    (print (has-substring trie "Th"))
    ;; Since we wrote has-substring so that it returns the node that it found,
    ;; we can also find that it has a substring "e" (comleting a word "The")
    ;; we can do that, because each node of trie is also a trie.
    (print (has-substring (has-substring trie "Th") "e"))
    ;; Now remove the word "The" from the trie.
    (cl-trie:remove-index trie "The")
    (print (word-occurances trie "The"))
    (print-is-empty trie)
    ;; Just to show how the function works, we clear the trie and reload it...
    (cl-trie:clear trie)
    (print-is-empty trie)
    ;; But this time we load it into the hash-table and convert it into the trie.
    (time (setf trie (cl-trie:hash-table->trie (load-into-hash-table file))))))

;; This example did not cover the remove-node function. I'm very sorry for that,
;; but since the code is easy, you can read it: basically, it just sets the trie's node to "off" mode.
;; Because trie nodes with values are in "on" mode.
;; It will also remove the value for garbage collector to pick up. Should you not like that behaviour,
;; e.g. because you want to filp it "on" later and don't want to lose the value,
;; you can set :preserve-value to T. This will keep the value.

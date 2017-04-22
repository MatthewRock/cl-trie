[![Build Status](https://travis-ci.org/MatthewRock/cl-trie.svg?branch=master)](https://travis-ci.org/MatthewRock/cl-trie)

Overview
========

cl-trie is a package that contains an implementation of a [trie](https://en.wikipedia.org/wiki/Trie) data structure. It utilizes CLOS. By default the trie is indexed by strings, but can be extended to other data types using inheritance on the base trie class.

License
========

cl-trie is licensed with MIT license, which means that you can do pretty much everything with it. Enjoy!


Compatibility
========

I am trying to build the library on as many compilers that I can. I have encountered some problems with UIOP on CLISP; therefore, CLISP is not supported, which means that I do not guarantee that code works on CLISP, but it should work (since it works on all other conforming compilers).

API Reference
========
I have not decided how to present the API reference yet. You can find all the generic functions in the src/generics.lisp; each generic function has a documentation string.
The only missing piece is accessors and utility functions, but the only utility right now is hash-table->trie, which is pretty self explanatory.


Example
========

This example will simply use the basic example from the `#:cl-trie-examples` package.

Basic idea
--------
In this example, we will define a function that shows cl-trie features and prints some data about the text file.

Loading file
--------

We want to create a function that loads a text file to trie.
We will do that by splitting the line on whitespace (using cl-ppcre).

``` common-lisp
(defun load-into-trie (file)
  (with-open-file (in file)
    ;; By default, trie warns you when you don't set the key
    ;; The only valid key is either character or NIL.
    ;; We muffle warning by setting verbose to NIL, making key take the default value
    ;; The default value for key is NIL.
    (loop with trie = (make-instance 'cl-trie:trie :verbose nil)
       for line = (read-line in nil 'eof nil)
       until (eql 'eof line)
       for splitted-line = (cl-ppcre:split "\\s" line)
       do (mapc (lambda (word) (setf (cl-trie:lookup trie word)
                                  ;; lookup accepts default returned value.
                                  ;; By default there are 0 words in text.
                                  (1+ (cl-trie:lookup trie word 0))))
                splitted-line)
       finally (return trie))))
```

As you can see, we create a fresh trie(and use :verbose nil to stop it from warning us about having no key).
We use (setf cl-trie:lookup) and cl-trie:lookup to count words.

This is a good place to talk about how tries are defined when it comes to keys and children.

Each node of trie is a trie too. This means that you attach tries to other tries and create circular tries. While the first idea is quite okay, the second one is very likely to break things, so tread carefully.
Also, notice that we did not put any key. This makes trie partially broken. It's okay if trie does not start with any key, but it can't be attached later, so that is something one has to consider.
Any valid trie has key of either character or NIL, but a valid subtrie can only have a key of CHAR.

Keys are also meant to be immutable. Changing the key of trie would affect many other keys. Therefore, this operation is discouraged and not part of the official interface.
You *can* change the key using (slot-value trie cl-trie::%key). Author discourages this practice.


Now we will add a function that will load file into hash-table. It will be of use later.

``` common-lisp
(defun load-into-hash-table (file)
  (with-open-file (in file)
    (loop with ht = (make-hash-table :test #'equal)
       for line = (read-line in nil 'eof nil)
       until (eql 'eof line)
       for splitted-line = (cl-ppcre:split "\\s" line)
       do (mapc (lambda (word) (setf (gethash word ht)
                                  ;; lookup accepts default returned value.
                                  ;; By default there are 0 words in text.
                                  (1+ (gethash word ht 0))))
                splitted-line)
       finally (return ht))))
```

Printing the dictionary.
--------
After we have loaded the file, we might want to take a look at all the unique words that we have there (our dictionary).
To do that, we will use #'cl-trie:all-keys method, which accepts a trie and returns a list of all keys.

``` common-lisp
(defun print-sorted-dictionary (trie)
  ;; Since default trie implementation ensures that keys are sorted lexicographically, we can simply print all the keys
  (mapc #'print (cl-trie:all-keys trie))
  ;; There exists an all-values function, which returns all values instead of the keys.
  )
```

As per comments, there is a sister function #'cl-trie:all-values, and both functions go through the trie in lexicographical order, so we will get our keys sorted. The sorting part is done during tree creation.

The naming might be a bit inconsistent, but let me explain.
When I am talking that tree has a key, I mean its key slot.
When I am talking that tree has keys, I mean the strings that create nodes with values.
Therefore all-keys is a function that works on keys - strings that we loaded, not single characters.

Printing dictionary size.
--------

Dictionary size might also be of interest for us. We will print it using #'cl-trie:size. It accepts trie as an argument and is pretty self-explanatory:

``` common-lisp
(defun print-dictionary-size (trie)
  ;; Trie's size is number of unique strings(keys) it holds.
  (print (cl-trie:size trie)))
```

Printing word count
--------

Next part is printing the word count. Since we initially used word as a key for word count, we will simply sum all the values in the trie.
For that, we will use #'cl-trie:mapvalues which accepts a one-argument function and a trie. It applies function to each value in the trie.

``` common-lisp
(defun count-all-words (trie)
  (let ((counter 0))
    (cl-trie:mapvalues (lambda (x) (incf counter x)) trie)
    counter)
  ;; There exists a mapkeys function, which works the same as mapvalues, but function is applied on keys.
  ;; The order stays the same (lexicographical)
  )
```

As with #'all-keys, there is a sister function for #'cl-trie:mapvalues called #'cl-trie:mapkeys.
Mapkeys also works on whole strings and not single characters - so, keys of trie and not single key.

Counting single word occurances.
--------

Now we would like to be able to count how many times does a given word appear in the text. We also want to say that word appears 0 times if it has never been seen in the text. This operation is quite simple:

``` common-lisp
(defun word-occurances (trie word)
  ;; We can pass a default value that will be returned if there is no word in dictionary.
  ;; If there are no words in dictionary, that means there are 0 words.
  (cl-trie:lookup trie word 0))
```

The lookup has an O(n) worst-case complexity where n is length of the word. If we find it, we have travelled through N nodes. If there are missing nodes anywhere on the road(e.g. no "e" after "m" in "omegle" search), it is terminated immediately.

#'cl-trie:lookup returns two values: a value that it found, and bollean indicating whether or not a value has been found. This means that one can have NILs as values and distinguish them from NILs that simply mean "no such node found".

Checking if trie is empty
--------

Trie can be empty if we clear it, or if we didn't insert anything yet. We use #'cl-trie:emptyp generic function for that.

``` common-lisp
(defun print-is-empty (trie)
  ;; Emptyp is a function that returns T when trie is empty and NIL otherwise.
  (if (cl-trie:emptyp trie)
      (princ "Yes")
      (princ "No"))
  (terpri))
```

Warning: Trie is considered empty if it does not contains any values! This means that you can have a trie with preserved structure, but all the values discarded. This will make future insertions faster (because you will not have to create all the nodes on your way), but trie will take a bit more size.

You clear trie using #'cl-trie:clear. Clear leaves structure behind; if you want to free the memory, set children of the root to NIL instead.

Checking for substring
--------

Now we want to see if there is a word in trie that contains a given substring. The substring can be the string itself.

``` common-lisp
(defun has-substring (trie substring)
  ;; Return substring node if it was found, NIL otherwise
  (let ((substring-node (cl-trie:find-node trie substring)))
    (when substring-node
      substring-node)))
```

Since #'cl-trie:find-node returns NIL if no node has been found, we could have just returned its result.
#'cl-trie:find-node looks for the node and returns it. If no node has been found, it returns NIL.

Find-node takes also a :create-new keyword argument, which by default is NIL. If the argument is T, find-node will create all the nodes needed to get to the target node, and that node, meaning that it:
* can be used for node insertion
* when :create-new is T, find-node will always return a node and never NIL.


Putting the functions together
--------

Now we just need to gather the functions together.

First, we will define a small utility to announce what we are currently doing.

``` common-lisp
(defun announce (string)
  (terpri)
  (loop repeat 25 do (princ "=") finally (terpri))
  (format t "Now ~A!~%" string)
  (loop repeat 25 do (princ "=") finally (terpri))
  (terpri))
```

Now that we have it, let's take a look at the final function, analyze-file:

``` common-lisp
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
```
Quite lengthy, but pretty simple. We:

1. Load the trie.
2. Print sorted dictionary.
3. Print dictionary size(word count).
4. Print total number of words in the text.
5. Print how many times does the word "The" appear in the text.
6. Increase i ts occurances by 1.
7. do 5. again
8. Check for the "Th" substring in the text. Then, we check for the "e" substring in "Th" node (meaning that if there is, then "The" substring is also there).
9. Remove "The" from the trie.
10. Show that "The" is no longer in the trie.
11. Show that trie is not empty.
12. Clear the trie.
13. Show that trie is empty afterwards.
14. We finally reload the trie, but we do so by first loading the file into the hash-table, and then transforming it into trie.

Removing the node
--------

This tutorial did not cover the remove-node function. We could have used it to remove the "The" node:

``` common-lisp
(cl-trie:remove-node (cl-trie:find-node trie "The"))
```
which is basically what cl-trie:remove-index does. **However**, this also deletes the value. We might want to just "remove" the word from the trie, but keep the value for the future. We use :preserve-value for that:

``` common-lisp
(cl-trie:remove-node (cl-trie:find-node trie "The") :preserve-value t)
```

Tests
========
To test the library, load it and use asdf:

``` common-lisp
(ql:quickload :cl-trie)
(asdf:test-system 'cl-trie)
```

Contact
========

Feel free to contact me by my e-mail (which can be found in cl-trie.asd), or by creating an issue.

Author
========

This package has been created by Mateusz Malisz.

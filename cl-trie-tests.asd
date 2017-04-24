(asdf:defsystem #:cl-trie-tests
  :description "Test suite for cl-trie library."
  :author "Mateusz Malisz <maliszmat@gmail.com>"
  :license "MIT"
  :depends-on (#:cl-trie #:fiveam)
  :pathname "t"
  :components ((:file "trie-tests"))
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :cl-trie-tests :run-tests)))

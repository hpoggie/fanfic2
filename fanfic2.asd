;;;; fanfic2.asd

(asdf:defsystem #:fanfic2
  :description "Describe fanfic2 here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:dexador #:cl-html5-parser #:cl-arrows #:split-sequence #:cl-strings #:trivia
                         #:eager-future2)
  :components ((:file "package")
               (:file "util")
               (:file "desc")
               (:file "dir")
               (:file "fanfic2")
               (:file "test")))

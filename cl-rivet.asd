;;;; cl-rivet.asd

(asdf:defsystem #:cl-rivet
  :description "Describe cl-rivet here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:iterate
                #:cl-ppcre
                #:mt19937) 
  :components ((:file "package")
               (:file "util")
               (:file "riv")
               (:file "mem-lex")))


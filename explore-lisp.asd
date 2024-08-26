;;;; explore-lisp.asd

(asdf:defsystem #:explore-lisp
  :description "Explore Common Lisp with lisp coding"
  :author "Dafu Chen <qchen2015@hotmail.com>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl)
  :components ((:file "package")
               (:file "utils")
               (:file "explore-lisp")))

;;;; package.lisp

(defpackage #:explore-lisp
  (:use #:cl)
  (:export "fing-package-ignore-case"
           "part-in-list-p"
           "find-package-with-part"
           "dir"
           "dir-package"
           "describe-str"
           "search-ignore-case"
           "search-symbol-in-package"
           "format-symbol-list"
           "save-symbol-list"
           "print-name-value"))

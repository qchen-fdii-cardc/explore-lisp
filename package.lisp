;;;; package.lisp

(defpackage #:explore-lisp
  (:nicknames :el)
  (:use #:cl)
  (:export :describe-symbol
           :dir
           :export-descriptions
           :export-all-external-symbols-to-stream
           :export-all-external-symbols
           :format-descriptions
           :lookfor
           :search-symbols
           :help
           :?))

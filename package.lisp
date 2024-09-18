;;;; package.lisp

(defpackage #:explore-lisp
  (:nicknames :el)
  (:use #:cl)
  (:export :export-all-external-symbols
           :dir
           :describe-symbol
           :format-descriptions
           :export-descriptions
           :search-symbols
           :export-all-external-symbols-to-stream))

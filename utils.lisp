(in-package #:explore-lisp)

;; defin a macro to print name and value


(defmacro print-name-value (name)
  `(format t "~a ==> ~a~%" ',name ,name))

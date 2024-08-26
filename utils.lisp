(in-package #:explore-lisp)

;; defin a macro to print name and value


(defmacro print-name-value (name)
  `(format t "~a ==> ~a~%" ',name ,name))


(with-open-file
    (s "output/symbol-list.md" :direction :output :if-exists :supersede)
  (print-name-value (enough-namestring s))
  (print-name-value (pathname s))
  (print-name-value (truename s))
  (print-name-value (file-namestring s))
  (print-name-value (file-string-length s "string-list.md"))
  (print-name-value (directory-namestring s))
  (print-name-value (namestring s))
  (print-name-value (nstring-capitalize (namestring s)))
  (print-name-value (probe-file s)))
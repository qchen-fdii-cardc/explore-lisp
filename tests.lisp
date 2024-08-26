(in-package #:explore-lisp)

;; usage example

(format-symbol-list '(car cdr cons) 2)

(with-open-stream
    (s (open "output/symbol-list.md" :direction :output :if-exists :supersede))
  (format s (format-symbol-list '(car cdr cons))))

(save-symbol-list
  (sort
      (search-symbol-in-package 'cl "value" :doc-string nil) 'string-lessp)
  "output/value.md")

(defun sort-symbols (names)
  "Sort a list of symbol names"
  (sort names #'string-lessp))

(save-symbol-list
  (sort-symbols (search-symbol-in-package 'cl "string" :doc-string nil))
  "output/string.md")

(save-symbol-list
  (sort-symbols
    '(concatenate length remove
       copy-seq map remove-duplicates count map-into remove-if
       count-if merge remove-if-not count-if-not mismatch replace
       delete notany reverse
       delete-duplicates notevery search
       delete-if nreverse some
       delete-if-not nsubstitute sort
       elt nsubstitute-if stable-sort
       every nsubstitute-if-not subseq
       fill position substitute
       find position-if substitute-if
       find-if position-if-not substitute-if-not find-if-not reduce))
  "output/sequence.md" 2)
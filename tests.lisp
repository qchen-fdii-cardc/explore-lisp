(in-package #:explore-lisp)

(dir 'explore-lisp)

;; usage example
(format-symbol-list '(car cdr cons) 2)

(with-open-stream
    (s (open "output/symbol-list.md" :direction :output :if-exists :supersede))
  (format s (format-symbol-list '(car cdr cons))))

(save-symbol-list
  (sort
      (search-symbol-in-package 'cl "value" :doc-string nil) 'string-lessp)
  "output/value.md")


(dir 'el)
(eq (first (dir 'el)) 'dir)
(dir-package 'el)
(type-of (first (dir 'el)))
(type-of 'dir)
(fboundp (first (dir 'el)))
(fboundp 'dir)
(describe-str (first (dir 'el)))
(mapcar 'describe-str (dir 'el))

(fboundp (first (dir 'cl)))


(format-symbol-list '(dir) 2)
(save-symbol-list (sort-symbols (dir 'el)) "output/el.md" 2)

(describe 'dir)

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
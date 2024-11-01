(in-package #:explore-lisp)

(defun format-symbol-list (symbols indent)
  "Formats a list of symbols with the given indent."
  (mapconcat (lambda (sym) (format nil "~vT~A" indent sym)) symbols #\Newline))

(dir 'explore-lisp)


(dir 'el)
(eq (first (dir 'el)) 'dir)
(dir 'el)
(type-of (first (dir 'el)))
(type-of 'dir)
(fboundp (first (dir 'el)))
(fboundp 'dir)
(describe-str (first (dir 'el)))
(mapcar 'describe-str (dir 'el))

(fboundp (first (dir 'cl)))


(format-symbol-list '(dir) 2)


(describe 'dir)


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


(lookfor "sequence" nil)

(one-sentence-doc 'mapcar)

(describe 'lookfor)

(format-descriptions (nth-value 1 (lookfor "sequence" nil nil)))


(nth-value 1 (lookfor "string" nil nil))


(lookfor "string" nil nil)
;;;; explore-lisp.lisp

(in-package #:explore-lisp)

(defun find-package-ignore-case (name)
  "Find a package by name, ignoring case"
  (if (stringp name)
      (find-package (string-upcase name))
      (find-package name)))


(defun part-in-list-p (part list)
  "Return true if any element of list contains part"
  (find-if (lambda (x) (search part x)) list))

(defun find-package-with-part (part)
  "Find a package by part of its name or nickname"
  (let ((result '())
        (part-upcase (string-upcase part)))
    (dolist (package (list-all-packages))
      (if (or (search part-upcase (package-name package))
              (part-in-list-p part-upcase (package-nicknames package)))
          (push package result)))
    result))


;; list all external symbols in a package
(defun dir (package)
  (let ((result '())
        (len 0))
    (do-external-symbols (s package (values result len))
      ;   (format t "~a~%" s)
      (push s result)
      (incf len))))

;; print all symbols and doc strings in a file
(defun dir-package (package)
  "List all external symbols in a package and their doc strings in a file"
  (let ((sorted-names (sort (dir package) #'string<))
        (fn (format nil "~a.md" package)))
    (with-open-stream (s (open fn :direction :output :if-exists :supersede))
      ;; header line
      (format s "# ~a external symbols~%~%~%" package)
      ;; list all external symbols     
      (let ((index 1))
        (dolist (name sorted-names)
          (format s "~d. [~A](#~A)~%" index name name)
          (incf index)))
      (format s "~%~%")
      ;; describe all external symbols
      (dolist (name sorted-names)
        (format s "##  `~a`~%~%" name)
        (format s "```lisp~%")
        (describe name s)
        (format s "```~%")))))


;; get describe output as a string
(defun describe-str (name)
  "Describe a symbol and return the output as a string"
  (with-output-to-string (s)
    (cl:describe name s)
    s))


(defun search-ignore-case (s name)
  "Search for a string in a case-insensitive way"
  (search (string-upcase s) (string-upcase name)))

;; search for string in symbol names and doc strings
(defun search-symbol-in-package (package name &key (doc-string t))
  "Search for string in symbol names and doc strings in a package"
  (let ((result '()))
    (do-external-symbols (s package result)
      (when (or (search-ignore-case name (symbol-name s))
                (and doc-string
                     (search-ignore-case name (describe-str s))))
            (push s result)))))

(search-symbol-in-package 'cl "string" :doc-string nil)

;; format markdown for symbol list
(defun format-symbol-list (name-list)
  "Format a list of symbol names as markdown"
  (with-output-to-string (s)
    (format s "# Symbol list~%~%")
    (let ((index 1))
      (dolist (name name-list)
        (format s "~d. [~A](#~A)~%" index name name)
        (incf index)))
    (format s "~%~%")
    (dolist (name name-list)
      (format s "## `~a`~%~%" name)
      (format s "```lisp~%")
      (describe name s)
      (format s "~%```~%~%"))))

(format-symbol-list '(car cdr cons))

(with-open-stream
    (s (open "symbol-list.md" :direction :output :if-exists :supersede))
  (format s (format-symbol-list '(car cdr cons))))


;; save symbol list to a file
(defun save-symbol-list (name-list fn)
  "Save a list of symbol names to a file"
  (with-open-stream (s (open fn :direction :output :if-exists :supersede))
    (format s (format-symbol-list name-list))))

(save-symbol-list
 (sort
     (search-symbol-in-package 'cl "value" :doc-string nil) 'string<)
 "value.md")


(defun sort-symbols (names)
  "Sort a list of symbol names"
  (sort names #'string<))

(sort-symbols (search-symbol-in-package 'cl "string" :doc-string nil))

;; defin a macro to print name and value
(defmacro print-name-value (name)
  `(format t "~a ==> ~a~%" ',name ,name))


(with-open-file
    (s "symbol-list.md" :direction :output :if-exists :supersede)
  (print-name-value (enough-namestring s))
  (print-name-value (pathname s))
  (print-name-value (truename s))
  (print-name-value (file-namestring s))
  (print-name-value (file-string-length s "string-list.md"))
  (print-name-value (directory-namestring s))
  (print-name-value (namestring s))
  (print-name-value (nstring-capitalize (namestring s)))
  (print-name-value (probe-file s)))

;;;; explore-lisp.lisp

(in-package #:explore-lisp)

(defun find-package-ignore-case (name)
  "Find a package by name, ignoring case"
  (if (stringp name)
      (find-package (string-upcase name))
      (find-package name)))

(defun part-in-list-p (part list)
  "Return true if any element of list contains part"
  (find-if #'(lambda (x) (search part x)) list))

(defun find-package-with-part (part)
  "Find a package by part of its name or nickname"
  (let ((result '())
        (part-upcase (string-upcase part)))
    (dolist (package (list-all-packages))
      (if (or (search part-upcase (package-name package))
              (part-in-list-p part-upcase (package-nicknames package)))
          (push package result)))
    result))


;; list all external symbols in a package  :EXPORT
(defun dir (package)
  "List all external symbols in a package, return a list of symbol names and its length"
  (let ((result '())
        (len 0))
    (do-external-symbols (s package (values result len))
      ;   (format t "~a~%" s)
      (push s result)
      (incf len))))


(defun sort-symbols (names)
  "Sort a list of symbol names"
  (sort names #'string-lessp))

; (search-symbol-in-package 'cl "sup" :doc-string nil)
(defun markdown-nth-header (n)
  "Return a string of n sharp characters for markdown headers"
  (make-string n :initial-element #\#))

;; print all symbols and doc strings in a file :EXPORT
(defun export-all-external-symbols-to-stream
    (package s &key (start-level 1))
  "List all external symbols in a package and their doc strings into a stream ~s~"
  (let ((sorted-names (sort-symbols (dir package))))
    ;; header line
    (format s "~A ~A external symbols~%~%~%"
      (markdown-nth-header start-level) package)
    ;; list all external symbols     
    (let ((index 1))
      (dolist (name sorted-names)
        (format s "~d. [~A](#~A)~%" index name (string-downcase name))
        (incf index)))
    (format s "~%~%")
    ;; describe all external symbols
    (dolist (name sorted-names)
      (format s "~A  `~A`~%~%"
        (markdown-nth-header (+ 1 start-level)) name)
      (format s "```lisp~%")
      (describe name s)
      (format s "```~%"))))

;; print all symbols and doc strings in a file :EXPORT
(defun export-all-external-symbols
    (package &key (fn "" fn-p) (start-level 1))
  "List all external symbols in a package and their doc strings into a file ~package~.md"
  (let ((_fn (if fn-p fn (format nil "~a.md" package))))
    (with-open-stream (s (open _fn :direction :output :if-exists :supersede))
      (export-all-external-symbols-to-stream package s :start-level start-level))))

;; get describe output as a string :EXPORT
(defun describe-symbol (name)
  "Describe a symbol and return the output as a string"
  (with-output-to-string (s)
    (describe name s)
    s))

(defun search-ignore-case (s name)
  "Search for a string `s` in `name` in a case-insensitive way"
  (search s name :test #'char-equal))

;; search for string in symbol names and doc strings
(defun search-symbols (name package &key (doc-string nil))
  "Search for string in symbol names and doc strings in a package"
  (let ((result '()))
    (do-external-symbols (s package result)
      (when (or (search-ignore-case name (symbol-name s))
                (and doc-string
                     (search-ignore-case name (describe-symbol s))))
            (push s result)))))

;; format markdown for symbol list :EXPORT
(defun format-descriptions (name-list &optional (start-level 1))
  "Format a list of symbol names as markdown, with optional start level for headers"
  (with-output-to-string (s)
    (format s "~A Symbol list~%~%"
      (markdown-nth-header start-level))
    (let ((index 1))
      (dolist (name name-list)
        (format s "~d. [~A](#~A)~%" index name (string-downcase name))
        (incf index)))
    (format s "~%~%")
    (dolist (name name-list)
      (format s "~A `~A`~%~%"
        (markdown-nth-header (+ start-level 1)) name)
      (format s "```lisp~%")
      (describe name s)
      (format s "~%```~%~%"))))

;; save symbol list to a file :EXPORT
(defun export-descriptions (name-list fn &optional (start-level 1))
  "Save a list of symbol names to a file"
  (with-open-stream (s (open fn :direction :output :if-exists :supersede))
    (format s "~A~%" (format-descriptions name-list start-level))))


(defun one-sentence-doc (name)
  "Return the first sentence after `Documentation:`"
  (let ((doc (describe-symbol name))
        (tag "Documentation:"))
    (if (search tag doc)
        (let* ((start (search tag doc))
               (len (length tag))
               (start3 (+ start len 1))
               (end (search (string #\Newline) doc :start2 start3)))
          (string-left-trim (string #\space) (subseq doc start3 end)))
        "No documentation found")))

;; lookfor
(defun lookfor (name &optional (output t) (doc-string t))
  "Look for symbols in all installed packages that contain `name`"
  (let ((packages (list-all-packages))
        (names '()))
    (if (null packages)
        (format t "No package found with part ~a~%" name)
        (loop for package in packages
              do (let ((symbols (search-symbols name package :doc-string doc-string)))
                   (when (not (null symbols))
                         (loop for symbol in symbols
                               do (push symbol names)
                                 when output
                               do (format t "[~a:~a]  ~A~%" (package-name package) symbol (one-sentence-doc symbol)))))))
    (values (length names) names)))

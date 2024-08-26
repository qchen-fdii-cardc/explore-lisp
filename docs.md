## EL external symbols


1. [DESCRIBE-SYMBOL](#DESCRIBE-SYMBOL)
2. [DIR](#DIR)
3. [EXPORT-ALL-EXTERNAL-SYMBOLS](#EXPORT-ALL-EXTERNAL-SYMBOLS)
4. [EXPORT-DESCRIPTIONS](#EXPORT-DESCRIPTIONS)
5. [FORMAT-DESCRIPTIONS](#FORMAT-DESCRIPTIONS)


###  `DESCRIBE-SYMBOL`

```lisp
EXPLORE-LISP:DESCRIBE-SYMBOL
  [symbol]

DESCRIBE-SYMBOL names a compiled function:
  Lambda-list: (NAME)
  Derived type: (FUNCTION (T) (VALUES SIMPLE-STRING &OPTIONAL))
  Documentation:
    Describe a symbol and return the output as a string
  Source file: C:/Users/qchen/quicklisp/local-projects/explore-lisp/explore-lisp.lisp
```
###  `DIR`

```lisp
EXPLORE-LISP:DIR
  [symbol]

DIR names a compiled function:
  Lambda-list: (PACKAGE)
  Derived type: (FUNCTION (T) (VALUES LIST UNSIGNED-BYTE &OPTIONAL))
  Documentation:
    List all external symbols in a package, return a list of symbol names and its length
  Source file: C:/Users/qchen/quicklisp/local-projects/explore-lisp/explore-lisp.lisp
```
###  `EXPORT-ALL-EXTERNAL-SYMBOLS`

```lisp
EXPLORE-LISP:EXPORT-ALL-EXTERNAL-SYMBOLS
  [symbol]

EXPORT-ALL-EXTERNAL-SYMBOLS names a compiled function:
  Lambda-list: (PACKAGE &KEY (FN  FN-P) (START-LEVEL 1))
  Derived type: (FUNCTION (T &KEY (:FN T) (:START-LEVEL T))
                 (VALUES NULL &OPTIONAL))
  Documentation:
    List all external symbols in a package and their doc strings into a file ~package~.md
  Source form:
    (LAMBDA
        (PACKAGE
         &KEY (EXPLORE-LISP::FN "" EXPLORE-LISP::FN-P)
         (EXPLORE-LISP::START-LEVEL 1))
      "List all external symbols in a package and their doc strings into a file ~package~.md"
      (BLOCK EXPLORE-LISP:EXPORT-ALL-EXTERNAL-SYMBOLS
        (LET ((EXPLORE-LISP::SORTED-NAMES
               (EXPLORE-LISP::SORT-SYMBOLS (EXPLORE-LISP:DIR PACKAGE)))
              (EXPLORE-LISP::_FN
               (IF EXPLORE-LISP::FN-P
                   EXPLORE-LISP::FN
                   (FORMAT NIL "~a.md" PACKAGE))))
          (WITH-OPEN-STREAM
              (EXPLORE-LISP::S
               (OPEN EXPLORE-LISP::_FN :DIRECTION :OUTPUT :IF-EXISTS
                     :SUPERSEDE))
            (FORMAT EXPLORE-LISP::S "~A ~A external symbols~%~%~%"
                    (EXPLORE-LISP::MARKDOWN-NTH-HEADER
                     EXPLORE-LISP::START-LEVEL)
                    PACKAGE)
            (LET ((EXPLORE-LISP::INDEX 1))
              (DOLIST (EXPLORE-LISP::NAME EXPLORE-LISP::SORTED-NAMES)
                (FORMAT EXPLORE-LISP::S "~d. [~A](#~A)~%"
                        EXPLORE-LISP::INDEX EXPLORE-LISP::NAME
                        EXPLORE-LISP::NAME)
                (INCF EXPLORE-LISP::INDEX)))
            (FORMAT EXPLORE-LISP::S "~%~%")
            (DOLIST (EXPLORE-LISP::NAME EXPLORE-LISP::SORTED-NAMES)
              (FORMAT EXPLORE-LISP::S "~A  `~A`~%~%"
                      (EXPLORE-LISP::MARKDOWN-NTH-HEADER
                       (+ 1 EXPLORE-LISP::START-LEVEL))
                      EXPLORE-LISP::NAME)
              (FORMAT EXPLORE-LISP::S "```lisp~%")
              (DESCRIBE EXPLORE-LISP::NAME EXPLORE-LISP::S)
              (FORMAT EXPLORE-LISP::S "```~%"))))))
```
###  `EXPORT-DESCRIPTIONS`

```lisp
EXPLORE-LISP:EXPORT-DESCRIPTIONS
  [symbol]

EXPORT-DESCRIPTIONS names a compiled function:
  Lambda-list: (NAME-LIST FN &OPTIONAL (START-LEVEL 1))
  Derived type: (FUNCTION (T T &OPTIONAL T) (VALUES NULL &OPTIONAL))
  Documentation:
    Save a list of symbol names to a file
  Source file: C:/Users/qchen/quicklisp/local-projects/explore-lisp/explore-lisp.lisp
```
###  `FORMAT-DESCRIPTIONS`

```lisp
EXPLORE-LISP:FORMAT-DESCRIPTIONS
  [symbol]

FORMAT-DESCRIPTIONS names a compiled function:
  Lambda-list: (NAME-LIST &OPTIONAL (START-LEVEL 1))
  Derived type: (FUNCTION (T &OPTIONAL T)
                 (VALUES SIMPLE-STRING &OPTIONAL))
  Documentation:
    Format a list of symbol names as markdown, with optional start level for headers
  Source file: C:/Users/qchen/quicklisp/local-projects/explore-lisp/explore-lisp.lisp
```

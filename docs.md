# EL external symbols


1. [?](#?)
2. [DESCRIBE-SYMBOL](#describe-symbol)
3. [DIR](#dir)
4. [EXPORT-ALL-EXTERNAL-SYMBOLS](#export-all-external-symbols)
5. [EXPORT-ALL-EXTERNAL-SYMBOLS-TO-STREAM](#export-all-external-symbols-to-stream)
6. [EXPORT-DESCRIPTIONS](#export-descriptions)
7. [FORMAT-DESCRIPTIONS](#format-descriptions)
8. [HELP](#help)
9. [LOOKFOR](#lookfor)
10. [SEARCH-SYMBOLS](#search-symbols)


##  `?`

```lisp
EXPLORE-LISP:?
  [symbol]

? names a macro:
  Lambda-list: (&REST ARGS)
  Documentation:
    Just another acronym for describe
  Source file: /home/qchen/quicklisp/local-projects/explore-lisp/explore-lisp.lisp
```
##  `DESCRIBE-SYMBOL`

```lisp
EXPLORE-LISP:DESCRIBE-SYMBOL
  [symbol]

DESCRIBE-SYMBOL names a compiled function:
  Lambda-list: (NAME)
  Derived type: (FUNCTION (T) (VALUES SIMPLE-STRING &OPTIONAL))
  Documentation:
    Describe a symbol and return the output as a string
  Source file: /home/qchen/quicklisp/local-projects/explore-lisp/explore-lisp.lisp
```
##  `DIR`

```lisp
EXPLORE-LISP:DIR
  [symbol]

DIR names a compiled function:
  Lambda-list: (PACKAGE)
  Derived type: (FUNCTION (T) (VALUES LIST UNSIGNED-BYTE &OPTIONAL))
  Documentation:
    List all external symbols in a package, return a list of symbol names and its length
  Source file: /home/qchen/quicklisp/local-projects/explore-lisp/explore-lisp.lisp
```
##  `EXPORT-ALL-EXTERNAL-SYMBOLS`

```lisp
EXPLORE-LISP:EXPORT-ALL-EXTERNAL-SYMBOLS
  [symbol]

EXPORT-ALL-EXTERNAL-SYMBOLS names a compiled function:
  Lambda-list: (PACKAGE &KEY (FN  FN-P) (START-LEVEL 1))
  Derived type: (FUNCTION (T &KEY (:FN T) (:START-LEVEL T)) *)
  Documentation:
    List all external symbols in a package and their doc strings into a file ~package~.md
  Source file: /home/qchen/quicklisp/local-projects/explore-lisp/explore-lisp.lisp
```
##  `EXPORT-ALL-EXTERNAL-SYMBOLS-TO-STREAM`

```lisp
EXPLORE-LISP:EXPORT-ALL-EXTERNAL-SYMBOLS-TO-STREAM
  [symbol]

EXPORT-ALL-EXTERNAL-SYMBOLS-TO-STREAM names a compiled function:
  Lambda-list: (PACKAGE S &KEY (START-LEVEL 1))
  Derived type: (FUNCTION (T T &KEY (:START-LEVEL T))
                 (VALUES NULL &OPTIONAL))
  Documentation:
    List all external symbols in a package and their doc strings into a stream ~s~
  Source file: /home/qchen/quicklisp/local-projects/explore-lisp/explore-lisp.lisp
```
##  `EXPORT-DESCRIPTIONS`

```lisp
EXPLORE-LISP:EXPORT-DESCRIPTIONS
  [symbol]

EXPORT-DESCRIPTIONS names a compiled function:
  Lambda-list: (NAME-LIST FN &OPTIONAL (START-LEVEL 1))
  Derived type: (FUNCTION (T T &OPTIONAL T)
                 (VALUES (OR STRING NULL) &OPTIONAL))
  Documentation:
    Save a list of symbol names to a file
  Source file: /home/qchen/quicklisp/local-projects/explore-lisp/explore-lisp.lisp
```
##  `FORMAT-DESCRIPTIONS`

```lisp
EXPLORE-LISP:FORMAT-DESCRIPTIONS
  [symbol]

FORMAT-DESCRIPTIONS names a compiled function:
  Lambda-list: (NAME-LIST &OPTIONAL (START-LEVEL 1))
  Derived type: (FUNCTION (T &OPTIONAL T)
                 (VALUES SIMPLE-STRING &OPTIONAL))
  Documentation:
    Format a list of symbol names as markdown, with optional start level for headers
  Source file: /home/qchen/quicklisp/local-projects/explore-lisp/explore-lisp.lisp
```
##  `HELP`

```lisp
EXPLORE-LISP:HELP
  [symbol]

HELP names a macro:
  Lambda-list: (&REST ARGS)
  Documentation:
    Just another acronym for describe
  Source file: /home/qchen/quicklisp/local-projects/explore-lisp/explore-lisp.lisp
```
##  `LOOKFOR`

```lisp
EXPLORE-LISP:LOOKFOR
  [symbol]

LOOKFOR names a compiled function:
  Lambda-list: (NAME &OPTIONAL (DOC-STRING NIL) (OUTPUT T)
                (PACKAGES NIL))
  Derived type: (FUNCTION (T &OPTIONAL T T T)
                 (VALUES (MOD 4611686018427387901) LIST &OPTIONAL))
  Documentation:
    Look for symbols in common-lisp (default)/all installed packages (packages = :all) that contain `name`
      doc-string: search in doc strings if t (default is nil)
      output: print the result if t (default is t)
      packages: set to :ALL list of packages to search, default is nil for just common-lisp

  Source file: /home/qchen/quicklisp/local-projects/explore-lisp/explore-lisp.lisp
```
##  `SEARCH-SYMBOLS`

```lisp
EXPLORE-LISP:SEARCH-SYMBOLS
  [symbol]

SEARCH-SYMBOLS names a compiled function:
  Lambda-list: (NAME PACKAGE &KEY (DOC-STRING NIL))
  Derived type: (FUNCTION (T T &KEY (:DOC-STRING T))
                 (VALUES LIST &OPTIONAL))
  Documentation:
    Search for string in symbol names and doc strings in a package
  Source file: /home/qchen/quicklisp/local-projects/explore-lisp/explore-lisp.lisp
```

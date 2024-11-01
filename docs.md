# EL external symbols


1. [DESCRIBE-SYMBOL](#describe-symbol)
2. [DIR](#dir)
3. [EXPORT-ALL-EXTERNAL-SYMBOLS](#export-all-external-symbols)
4. [EXPORT-ALL-EXTERNAL-SYMBOLS-TO-STREAM](#export-all-external-symbols-to-stream)
5. [EXPORT-DESCRIPTIONS](#export-descriptions)
6. [FORMAT-DESCRIPTIONS](#format-descriptions)
7. [LOOKFOR](#lookfor)
8. [SEARCH-SYMBOLS](#search-symbols)


##  `DESCRIBE-SYMBOL`

```lisp
EXPLORE-LISP:DESCRIBE-SYMBOL
  [symbol]

DESCRIBE-SYMBOL names a compiled function:
  Lambda-list: (NAME)
  Derived type: (FUNCTION (T) (VALUES SIMPLE-STRING &OPTIONAL))
  Documentation:
    Describe a symbol and return the output as a string
  Source file: C:/Users/User/quicklisp/local-projects/explore-lisp/explore-lisp.lisp
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
  Source file: C:/Users/User/quicklisp/local-projects/explore-lisp/explore-lisp.lisp
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
  Source file: C:/Users/User/quicklisp/local-projects/explore-lisp/explore-lisp.lisp
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
  Source file: C:/Users/User/quicklisp/local-projects/explore-lisp/explore-lisp.lisp
```
##  `EXPORT-DESCRIPTIONS`

```lisp
EXPLORE-LISP:EXPORT-DESCRIPTIONS
  [symbol]

EXPORT-DESCRIPTIONS names a compiled function:
  Lambda-list: (NAME-LIST FN &OPTIONAL (START-LEVEL 1))
  Derived type: (FUNCTION (T T &OPTIONAL T) (VALUES NULL &OPTIONAL))
  Documentation:
    Save a list of symbol names to a file
  Source file: C:/Users/User/quicklisp/local-projects/explore-lisp/explore-lisp.lisp
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
  Source file: C:/Users/User/quicklisp/local-projects/explore-lisp/explore-lisp.lisp
```
##  `LOOKFOR`

```lisp
EXPLORE-LISP:LOOKFOR
  [symbol]

LOOKFOR names a compiled function:
  Lambda-list: (NAME &OPTIONAL (DOC-STRING T) (OUTPUT T))
  Derived type: (FUNCTION (T &OPTIONAL T T)
                 (VALUES (UNSIGNED-BYTE 38) LIST &OPTIONAL))
  Documentation:
    Look for symbols in all installed packages that contain `name`
  Source file: C:/Users/User/quicklisp/local-projects/explore-lisp/explore-lisp.lisp
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
  Source file: C:/Users/User/quicklisp/local-projects/explore-lisp/explore-lisp.lisp
```

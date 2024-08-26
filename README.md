# explore-lisp
### _Dafu Chen <qchen2015@hotmail.com>_

参考博客文章：

- [Common Lisp文档和符号探索](https://www.windtunnel.cn/posts/005-explore-lisp/)
- [Common Lisp项目管理](https://www.windtunnel.cn/posts/002-lazy-process/)


## 建立工程

```lisp

(defun make-project-el ()
  (progn

   (ql:quickload 'quickproject)

   (require 'quickproject)

   (quickproject:make-project "explore-lisp"
                              :depends-on '(cl)
                              :author "Dafu Chen <qchen2015@hotmail.com>"
                              :license "MIT")))

(make-project-el)


```


## 使用工程

```lisp

; Usage of the project
(require 'uiop)
(require 'asdf)

(push
  (uiop/pathname:merge-pathnames* "./explore-lisp/") asdf:*central-registry*)


(ql:quickload :explore-lisp)
```


## 主要功能函数

[文档](docs.md)


## License

MIT


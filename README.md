<script async src="//busuanzi.ibruce.info/busuanzi/2.3/busuanzi.pure.mini.js"></script>
  <meta name="referrer" content="no-referrer-when-downgrade">

# explore-lisp
### _Dafu Chen <qchen2015@hotmail.com>_

[Github Repository](https://github.com/qchen-fdii-cardc/explore-lisp)

参考博客文章：

- [Common Lisp文档和符号探索](https://www.windtunnel.cn/posts/005-explore-lisp/)
- [Common Lisp项目管理](https://www.windtunnel.cn/posts/002-lazy-process/)

## 介绍

explore-lisp是一个Common Lisp工程，用于探索Common Lisp的文档和符号。

### 安装

前提条件，安装Sbcl和Quicklisp。

请参考：[Common Lisp奇先生](https://www.windtunnel.cn/categories/lisp/)。

1. 下载源代码到quicklisp的local-projects目录下
```shell
cd ~/quicklisp/local-projects
git clone https://github.com/qchen-fdii-cardc/explore-lisp
```

利用Quicklisp加载工程。

```lisp
(ql:quickload :explore-lisp)
```

如果不希望安装在quicklisp的local-projects目录下，还有两种办法：

1. 在Quicklisp的local-projects目录下建立一个软链接，指向explore-lisp的源代码目录。
2. 在clone的目录下运行下面的代码：

```lisp

; Usage of the project
(require 'uiop)
(require 'asdf)

(push
  (uiop/pathname:merge-pathnames* "./explore-lisp/") asdf:*central-registry*)


(ql:quickload :explore-lisp)
```

### 使用

如本项目的[文档](docs.md)所示，explore-lisp提供了一些函数，用于探索Common Lisp的文档和符号。

这个文档本身，就是利用这个explore-lisp生成的。

```lisp
; Usage of the project
(require 'explore-lisp)
(el:export-all-external-symbols 'el :fn "docs.md") 
```

### 主要功能函数

[函数参考](docs.md)

下面也给出每个函数使用的例子。

首先是`dir`函数，列出一个包的所有外部符号。

```lisp
(el:dir 'cl)
```

这样就能大概知道一个包里面有哪些符号可供使用。其后就能够利用`describe`函数查看一个符号的详细信息。

```lisp
(describe 'cl:mapcar)
```

或者，explore-lisp提供了函数`describe-symbols`，可以把帮助文档输出为一个字符串。
  
```lisp
(el:describe-symbols 'cl)
```

类似于Matlab的`lookfor`函数，explore-lisp提供了`search-symbols`函数，可以搜索包中的符号。

```lisp
(el:search-symbols 'cl "map")
```

这个时候只搜索符号名称，如果需要搜索符号的文档，可以使用`search-symbols`函数的命名参数`doc-string`。

```lisp
(el:search-symbols 'cl "map" :doc-string t)
```

最后，explore-lisp提供了`export-all-external-symbols`函数，可以把一个包的所有外部符号和文档输出到一个文件。

```lisp
(el:export-all-external-symbols 'cl :fn "cl.md")
```

而`export-all-external-symbols-to-stream`函数可以把一个包的所有外部符号和文档输出到一个流。

```lisp
(with-open-file (s "cl.md" :direction :output)
  (el:export-all-external-symbols-to-stream 'cl s))
```

大概，常用的函数就是这么一些。


## License

MIT


<div class="busuanzi-footer">
  <span id="busuanzi_container_site_pv">
    本站总访问量<span id="busuanzi_value_site_pv"></span>次
  </span>
  <span id="busuanzi_container_site_uv">
    本站访客数<span id="busuanzi_value_site_uv"></span>人次
  </span>
</div>

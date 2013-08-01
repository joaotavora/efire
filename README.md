efire
=====

**efire** is a campfire client for emacs.

It's based on the libraries of the excelent [circe][circe]

[circe]:    https://github.com/jorgenschaefer/circe

Installation
------------

To install you first need to `package-install` of `el-get` [circe], then compile
or load `efire.el`. But I'll provide some kind of recipe for these package
managers soon.

Usage
-----
Set:

    (setq efire-token "yourtoken")
    (setq efire-host "yourhost.campfirenow.com")

And then `M-x campfire-join-room`

;;; -*- Scheme -*-

(sf "$scheme/etc/direct")
(load "$scheme/etc/direct" system-global-environment)

(sf (directory-read "$scheme/runtime/*.scm"))

(%cd "$scheme/sf")
(load "sfsf.scm")
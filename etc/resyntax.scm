;;; -*- Scheme -*-

(sf "$scheme/etc/direct")
(load "$scheme/etc/direct" system-global-environment)

(sf "$scheme/microcode/utabmd" "$scheme/runtime/")

(sf (directory-read "$scheme/runtime/*.scm"))

(sf (directory-read "$scheme/sf/*.scm"))
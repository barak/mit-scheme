;;; -*- Scheme -*-

((access finish-load runtime-system))

(set-working-directory-pathname! "../sf")

(load "make.bin" system-global-environment)

(set-working-directory-pathname! "../runtime")

(disk-save "scheme.bin")

(load "sbuild.bin" system-global-environment)

(disk-save "sicp.bin")

(%exit)

#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/cref/toplev.scm,v 1.5 1991/11/04 20:34:26 cph Exp $

Copyright (c) 1988-91 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Package Model: Top Level

(declare (usual-integrations))

(define (generate/common kernel)
  (lambda (filename)
    (let ((pathname (merge-pathnames filename)))
      (let ((pmodel (read-package-model pathname)))
	(read-file-analyses! pmodel)
	(resolve-references! pmodel)
	(kernel pathname pmodel)))))

(define (cref/generate-trivial-constructor filename)
  (let ((pathname (merge-pathnames filename)))
    (write-constructor pathname (read-package-model pathname))))

(define cref/generate-cref
  (generate/common
   (lambda (pathname pmodel)
     (write-cref pathname pmodel))))

(define cref/generate-cref-unusual
  (generate/common
   (lambda (pathname pmodel)
     (write-cref-unusual pathname pmodel))))

(define cref/generate-constructors
  (generate/common
   (lambda (pathname pmodel)
     (write-cref-unusual pathname pmodel)
     (write-globals pathname pmodel)
     (write-constructor pathname pmodel)
     (write-loader pathname pmodel))))

(define cref/generate-all
  (generate/common
   (lambda (pathname pmodel)
     (write-cref pathname pmodel)
     (write-globals pathname pmodel)
     (write-constructor pathname pmodel)
     (write-loader pathname pmodel))))

(define (write-constructor pathname pmodel)
  (let ((constructor (construct-constructor pmodel)))
    (with-output-to-file (pathname-new-type pathname "con")
      (lambda ()
	(write-string ";;; -*-Scheme-*-")
	(newline)
	(write-string ";;; program to make package structure")
	(for-each (lambda (expression)
		    (pp expression (current-output-port) true))
		  constructor)))))

(define (write-loader pathname pmodel)
  (let ((loader (construct-loader pmodel)))
    (with-output-to-file (pathname-new-type pathname "ldr")
      (lambda ()
	(write-string ";;; -*-Scheme-*-")
	(newline)
	(write-string ";;; program to load package contents")
	(for-each (lambda (expression)
		    (pp expression (current-output-port) true))
		  loader)))))

(define (write-cref pathname pmodel)
  (with-output-to-file (pathname-new-type pathname "cref")
    (lambda ()
      (format-packages pmodel))))

(define (write-cref-unusual pathname pmodel)
  (with-output-to-file (pathname-new-type pathname "cref")
    (lambda ()
      (format-packages-unusual pmodel))))

(define (write-globals pathname pmodel)
  (fasdump (map binding/name
		(list-transform-positive
		    (btree-fringe
		     (package/bindings (pmodel/root-package pmodel)))
		  binding/source-binding))
	   (pathname-new-type pathname "glob")))
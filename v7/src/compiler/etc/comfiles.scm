#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/etc/comfiles.scm,v 1.2 1991/08/21 20:04:52 cph Exp $

Copyright (c) 1989-91 Massachusetts Institute of Technology

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

;;;; Stage recompilation checks

(declare (usual-integrations))

(define compiler-directories
  '("back" "base" "fggen" "fgopt" "machine" "rtlbase" "rtlgen" "rtlopt"))

(define runtime-directories
  '("runtime" "sf" "cref"))

(define (->string name)
  (cond ((string? name) name)
	((symbol? name) (symbol->string name))
	(else (error "->string: Can't coerce" name))))

(define (for-each-file proc directories)
  (for-each (lambda (dname)
	      (for-each proc
			(directory-read
			 (string-append (->string dname)
					"/*.scm"))))
	    directories))

(define (for-each-compiler-file proc)
  (for-each-file proc compiler-directories))

(define (for-each-runtime-file proc)
  (for-each-file proc runtime-directories))

;; This assumes that the working directory contains the copy of the compiler
;; to check.

(define (check-stage stage directories)
  (let ((stage (->string stage)))
    (for-each-file
     (lambda (name)
       (let ((path (pathname-new-type (->pathname name) "com")))
	 (show-differences path
			   (pathname-new-directory
			    path
			    (append (pathname-directory path)
				    `(,stage))))))
     directories)))

(define (check-compiler #!optional stage)
  (check-stage (if (default-object? stage) '"STAGE2" stage)
	       compiler-directories))
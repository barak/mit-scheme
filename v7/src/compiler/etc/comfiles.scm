#| -*-Scheme-*-

$Id: comfiles.scm,v 1.3 1993/06/30 21:39:32 gjr Exp $

Copyright (c) 1989-1993 Massachusetts Institute of Technology

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

;; This assumes that the working directory contains the copy of the compiler
;; to check.

(define (check-stage directories #!optional stage)
  (let ((stage
	 (if (default-object? stage)
	     "STAGE2"
	     (->string stage))))
    (for-each-file
     (lambda (name)
       (let* ((path0 (->pathname name))
	      (path1 (pathname-new-type (->pathname path0) "com"))
	      (path2 (pathname-new-directory
		      path1
		      (append (pathname-directory path1)
			      `(,stage)))))
	 (cond ((not (file-exists? path1))
		(if (file-exists? path2)
		    (warn "Directory mismatch"
			  `(,path2 exists ,path1 does not))
		    (warn "Missing compiled files for" path0)))
	       ((not (file-exists? path2))
		(warn "Directory mismatch"
		      `(,path1 exists ,path2 does not)))
	       (else
		(show-differences path1 path2)))))
     directories)))

(define (check-compiler #!optional stage)
  (check-stage compiler-directories
	       (if (default-object? stage) "STAGE2" stage)))
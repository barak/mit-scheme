#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/packag.scm,v 14.3 1988/07/14 07:40:31 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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

;;;; Simple Package Namespace
;;; package: (package)

(declare (usual-integrations))

(define-structure (package
		   (constructor make-package (parent %name environment))
		   (conc-name package/)
		   (print-procedure false))
  (parent false read-only true)
  (children '())
  (%name false read-only true)
  (environment false read-only true))

(define (package/child package name)
  (let loop ((children (package/children package)))
    (and (not (null? children))
	 (if (eq? name (package/%name (car children)))
	     (car children)
	     (loop (cdr children))))))

(define (package/name package)
  (let loop ((package package) (result '()))
    (if (package/parent package)
	(loop (package/parent package) (cons (package/%name package) result))
	result)))

(define (name->package name)
  (let loop ((path name) (package system-global-package))
    (if (null? path)
	package
	(let ((child (package/child package (car path))))
	  (and child
	       (loop (cdr path) child))))))

(define (find-package name)
  (let loop ((path name) (package system-global-package))
    (if (null? path)
	package
	(loop (cdr path)
	      (or (package/child package (car path))
		  (error "Unable to find package"
			 (list-difference name (cdr path))))))))

(define (list-difference list tail)
  (let loop ((list list))
    (if (eq? list tail)
	'()
	(cons (car list) (loop (cdr list))))))

(define (package/add-child! package name environment)
  (if (package/child package name)
      (error "Package already has child of given name" package name))
  (let ((child (make-package package name environment)))
    (set-package/children! package (cons child (package/children package)))
    child))

(define system-global-package)

(define (package/system-loader filename options load-interpreted?)
  (let ((pathname (->pathname filename)))
    (with-working-directory-pathname (pathname-directory-path pathname)
      (lambda ()
	(fluid-let ((load/default-types
		     (if (if (eq? load-interpreted? 'QUERY)
			     (prompt-for-confirmation "Load interpreted? ")			     load-interpreted?)
			 '("bin" "scm")
			 load/default-types)))
	  (load (pathname-new-type pathname "bcon") system-global-environment)
	  ((load (pathname-new-type pathname "bldr") system-global-environment)
	   (let ((syntax-table (nearest-repl/syntax-table)))
	     (lambda (filename environment)
	       (load filename environment syntax-table true)))
	   options)))))
  *the-non-printing-object*)

(define-integrable (package/reference package name)
  (lexical-reference (package/environment package) name))

(define (initialize-package!)
  (set! system-global-package
	(make-package false false system-global-environment)))

(define (initialize-unparser!)
  (unparser/set-tagged-vector-method! package
    (unparser/standard-method 'PACKAGE
      (lambda (state package)
	(unparse-object state (package/name package))))))
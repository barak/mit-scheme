#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/dbgutl.scm,v 14.1 1988/05/20 00:55:52 cph Exp $

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

;;;; Debugger Utilities
;;; package: debugger-utilities-package

(declare (usual-integrations))

(define (initialize-package!)
  (set! rename-list
	`((,lambda-tag:unnamed . LAMBDA)
	  (,lambda-tag:internal-lambda . LAMBDA)
	  (,lambda-tag:internal-lexpr . LAMBDA)
	  (,lambda-tag:let . LET)
	  (,lambda-tag:fluid-let . FLUID-LET)
	  (,lambda-tag:make-environment . MAKE-ENVIRONMENT))))

(define (print-user-friendly-name frame)
  (let ((name (environment-name frame)))
    (let ((rename (assq name rename-list)))
      (if rename
	  (begin (write-string "a ")
		 (write (cdr rename))
		 (write-string " special form"))
	  (begin (write-string "the procedure ")
		 (write name))))))

(define (environment-name environment)
  (lambda-components* (procedure-lambda (environment-procedure environment))
    (lambda (name required optional rest body)
      required optional rest body
      name)))

(define (special-name? symbol)
  (assq symbol rename-list))

(define rename-list)

(define (show-frame frame depth)
  (if (eq? system-global-environment frame)
      (begin
	(newline)
	(write-string "This frame is the system global environment"))
      (begin
	(newline)
	(write-string "Frame created by ")
	(print-user-friendly-name frame)
	(if (>= depth 0)
	    (begin (newline)
		   (write-string "Depth (relative to starting frame): ")
		   (write depth)))
	(newline)
	(let ((bindings (environment-bindings frame)))
	  (if (null? bindings)
	      (write-string "Has no bindings")
	      (begin
		(write-string "Has bindings:")
		(newline)
		(for-each print-binding
			  (sort bindings
				(lambda (x y)
				  (string<? (symbol->string (car x))
					    (symbol->string (car y))))))))))))

(define (print-binding binding)
  (let ((x-size (output-port/x-size (current-output-port)))
	(write->string
	 (lambda (object length)
	   (let ((x (write-to-string object length)))
	     (if (and (car x) (> length 4))
		 (substring-move-right! " ..." 0 4 (cdr x) (- length 4)))
	     (cdr x)))))
    (newline)
    (write-string
     (let ((s (write->string (car binding) (quotient x-size 2))))
       (if (null? (cdr binding))
	   (string-append s " is unassigned")
	   (let ((s (string-append s " = ")))
	     (string-append s
			    (write->string (cadr binding)
					   (max (- x-size (string-length s))
						0)))))))))
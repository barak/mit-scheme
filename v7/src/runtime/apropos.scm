#| -*-Scheme-*-

$Id: apropos.scm,v 1.1 1993/11/18 04:30:52 adams Exp $

Copyright (c) 1988-1992 Massachusetts Institute of Technology

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

;;;; Apropos command
;;; package: (runtime apropos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (usual-integrations))

(define (apropos text #!optional package/env search-parents?)
  (let* ((env
	  (cond ((default-object? package/env)    (nearest-repl/environment))
		((eq? package/env #t)             (nearest-repl/environment))
		(else                             (->environment package/env))))
	 (search-parents?
	  (or (default-object? package/env)
	      (and (not (default-object? search-parents?))
		   search-parents?))))
    (aproposer text env search-parents? apropos-describe-env apropos-describe)))


(define (apropos-list text #!optional package/env search-parents?)
  (let* ((env
	  (cond ((default-object? package/env)    (nearest-repl/environment))
		((eq? package/env #t)             (nearest-repl/environment))
		(else                             (->environment package/env))))
	 (search-parents?
	  (or (default-object? package/env)
	      (and (not (default-object? search-parents?))
		   search-parents?))))
    (let ((names '()))
      (define (add-name name env) (set! names (cons name names)))
      (aproposer text env search-parents? (lambda (env) env) add-name)
      names)))


(define (aproposer text env search-parents? process-env process-symbol)
  (let*  ((text   (if (symbol? text) (symbol-name text) text)))
    (process-env env)
    (for-each (lambda (symbol)
		(if (substring? text (symbol-name symbol))
		    (process-symbol symbol env)))
	      (sort (environment-bound-names env) symbol<?))
    (if (and search-parents? (environment-has-parent? env))
	(aproposer text (environment-parent env) search-parents?
		   process-env process-symbol))))


(define (apropos-describe symbol env)
  (newline)
  (display symbol))

(define (apropos-describe-env env)
  (let ((package (environment->package env)))
    (newline)
    (display (or package env))))


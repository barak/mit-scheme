#| -*-Scheme-*-

$Id: apropos.scm,v 1.3 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1993, 1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; Apropos command
;;; package: (runtime apropos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (usual-integrations))

(define (apropos text #!optional package/env search-parents?)
  (let* ((env
	  (cond ((default-object? package/env)   (nearest-repl/environment))
		((eq? package/env #t)            (nearest-repl/environment))
		(else                            (->environment package/env))))
	 (search-parents?
	  (or (default-object? package/env)
	      (and (not (default-object? search-parents?))
		   search-parents?))))
    (aproposer text env search-parents?
	       apropos-describe-env apropos-describe)))


(define (apropos-list text #!optional package/env search-parents?)
  (let* ((env
	  (cond ((default-object? package/env)   (nearest-repl/environment))
		((eq? package/env #t)            (nearest-repl/environment))
		(else                            (->environment package/env))))
	 (search-parents?
	  (or (default-object? package/env)
	      (and (not (default-object? search-parents?))
		   search-parents?))))
    (let ((names '()))
      (define (add-name name env)
	env
	(set! names (cons name names))
	unspecific)
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
  env
  (newline)
  (display symbol))

(define (apropos-describe-env env)
  (let ((package (environment->package env)))
    (newline)
    (display (or package env))))
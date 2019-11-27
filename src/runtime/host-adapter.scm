#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; Adapt host system.
;;; package: (user)

(declare (usual-integrations))

;;; This file is loaded by the cross-syntaxer and cross-compiler to
;;; hack the host e.g. to add bindings that were added to the new
;;; runtime AND used in the new CREF/SF/LIAR.  It is NOT loaded into
;;; the new runtime.  It contains temporary hacks that will be kept
;;; only until the new runtime is released.

(if (lexical-unreferenceable? (->environment '()) 'target-bytes-per-object)
    (environment-define (->environment '())
			'target-bytes-per-object
			bytes-per-object))

(if (lexical-unreferenceable? (->environment '()) 'cref/source-root)
    (environment-define (->environment '())
			'cref/source-root
			#!default))

(if (lexical-unreferenceable? (->environment '()) 'cref/object-root)
    (environment-define (->environment '())
			'cref/object-root
			#!default))

(let ((env (->environment '(runtime))))
  (if (not (environment-bound? env 'define-deferred-procedure))
      (eval '(define-syntax define-deferred-procedure
	       (er-macro-transformer
		(lambda (form rename compare)
		  (declare (ignore compare))
		  (syntax-check '(_ identifier expression expression) form)
		  (let ((name (cadr form))
			(dependency (caddr form))
			(expr (cadddr form))
			(args (new-identifier 'args)))
		    `(,(rename 'begin)
		      (,(rename 'define) ,name
		       (,(rename 'lambda) ,args
			(,(rename 'defer-boot-action) ,dependency
			 (,(rename 'lambda) ()
			  (,(rename 'apply) ,name ,args)))))
		      (,(rename 'defer-boot-action) ,dependency
		       (,(rename 'lambda) ()
			(,(rename 'set!) ,name ,expr)
			,(rename 'unspecific))))))))
	    env)))

(let ((env (->environment '(scode-optimizer expansion))))

  (define (remove-at-index! index items setter)
    (if (= index 0)
	(setter (cdr items))
	(remove-at-index! (- index 1)
			  (cdr items)
			  (pair-setter items))))

  (define (pair-setter pair)
    (lambda (tail)
      (set-cdr! pair tail)))

  (define (env-getter env name)
    (lambda ()
      (environment-lookup env name)))

  (define (env-setter env name)
    (lambda (tail)
      (environment-assign! env name tail)))

  (let ((get-names (env-getter env 'usual-integrations/expansion-names))
	(set-names! (env-setter env 'usual-integrations/expansion-names))
	(get-vals (env-getter env 'usual-integrations/expansion-values))
	(set-vals! (env-setter env 'usual-integrations/expansion-values)))

    (define (remove-one name)
      (let ((names (get-names)))
	(let ((i
	       (list-index (lambda (name*) (eq? name* name))
			   names)))
	  (if i
	      (begin
		(remove-at-index! i names set-names!)
		(remove-at-index! i (get-vals) set-vals!))))))

    (remove-one 'call-with-values)
    (remove-one 'with-values)
    (remove-one 'values)

    (environment-assign! env
			 'usual-integrations/expansion-alist
			 (map cons
			      (get-names)
			      (get-vals)))))

unspecific
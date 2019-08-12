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

(let ()

  (define (unbound? env name)
    (eq? 'unbound (environment-reference-type env name)))

  (let ((env (->environment '())))

    (if (unbound? env 'guarantee)
	(eval `(define (guarantee predicate object #!optional caller)
		 (if (predicate object)
		     object
		     (error:wrong-type-argument
		      object
		      (string-append "object satisfying "
				     (call-with-output-string
				       (lambda (port)
					 (write predicate port))))
		      caller)))
	      env))

    (if (unbound? env 'bundle)
	(eval '(define-syntax bundle
		 (syntax-rules ()
		   ((_ predicate name ...)
		    (alist->bundle predicate
				   (list (cons 'name name) ...)))))
	      env))

    (if (unbound? env 'delay-force)
	(eval '(begin
		 (define-syntax delay-force
		   (syntax-rules ()
		     ((delay-force expression)
		      (make-unforced-promise (lambda () expression)))))
		 (define-syntax delay
		   (syntax-rules ()
		     ((delay expression)
		      (delay-force (make-promise expression))))))
	      env))

    (if (unbound? env 'parameterize)
	(eval '(define-syntax parameterize
		 (syntax-rules ()
		   ((parameterize ((param value) ...) form ...)
		    (parameterize* (list (cons param value) ...)
				   (lambda () form ...)))))
	      env)))

  (if (name->package '(scode-optimizer))
      (begin
	(let ((env (->environment '(scode-optimizer))))
	  (eval '(if (not (memq 'scode-lambda-name:unnamed
				global-constant-objects))
		     (begin
		       (environment-define system-global-environment
					   'scode-lambda-name:unnamed
					   lambda-tag:unnamed)
		       (set! global-constant-objects
			     (cons 'scode-lambda-name:unnamed
				   global-constant-objects))
		       (usual-integrations/cache!)))
		env))
	(let ((env (->environment '(scode-optimizer expansion))))
	  (eval '(let ((pred
			(let ((names
			       '(set-string-length!
				 string->char-syntax
				 string-allocate
				 string-length
				 string-ref
				 string-set!
				 string?
				 vector-8b-ref
				 vector-8b-set!)))
			  (lambda (p)
			    (memq (car p) names)))))
		   (if (any pred usual-integrations/expansion-alist)
		       (begin
			 (set! usual-integrations/expansion-alist
			       (remove! pred
					usual-integrations/expansion-alist))
			 (set! usual-integrations/expansion-names
			       (map car usual-integrations/expansion-alist))
			 (set! usual-integrations/expansion-values
			       (map cdr usual-integrations/expansion-alist)))))
		env))))

  (let ((env (->environment '(runtime))))
    (if (unbound? env 'select-on-bytes-per-word)
	(begin
	  (eval '(define-syntax select-on-bytes-per-word
		   (er-macro-transformer
		    (lambda (form rename compare)
		      rename compare
		      (syntax-check '(_ expression expression) form)
		      (let ((bpo (bytes-per-object)))
			(case bpo
			  ((4) (cadr form))
			  ((8) (caddr form))
			  (else (error "Unsupported bytes-per-object:" bpo)))))))
		env)
	  (eval '(define (bytes-per-object)
		   (vector-ref (gc-space-status) 0))
		system-global-environment)))
    (if (unbound? env 'variable-setter)
	(eval '(define-syntax variable-setter
		 (syntax-rules ()
		   ((_ identifier)
		    (lambda (value)
		      (set! identifier value)
		      unspecific))))
	      env)))

  (let ((env (->environment '(runtime microcode-tables))))
    (if (eval '(or (not (microcode-type/name->code 'bytevector))
		   (not (microcode-type/name->code 'tagged-object))
		   (not (microcode-type/name->code 'unicode-string)))
	      env)
        (begin
          (eval '(define (microcode-type name)
                   (or (microcode-type/name->code name)
                       (cond ((eq? name 'bytevector) #x33)
                             ((eq? name 'tagged-object) #x25)
                             ((eq? name 'unicode-string) #x1B)
                             (else #t))
                       (error "MICROCODE-TYPE: Unknown name:" name)))
                env)
          (link-variables system-global-environment 'microcode-type
                          env 'microcode-type)))))

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
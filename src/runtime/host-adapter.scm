#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

  (define (provide-rename env old-name new-name)
    (if (unbound? env new-name)
	(eval `(define ,new-name ,old-name) env)))

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
    (if (unbound? env 'bytes-per-object)
	(eval '(define (bytes-per-object)
		 (vector-ref (gc-space-status) 0))
	      env))

    (if (unbound? env 'runtime-environment->syntactic)
	(eval '(define (runtime-environment->syntactic object)
		 object)
	      env))

    (provide-rename env 'random-byte-vector 'random-bytevector)
    (provide-rename env 'string-downcase 'string-foldcase)
    (provide-rename env 'lambda-tag:unnamed 'scode-lambda-name:unnamed)
    (provide-rename env 'lambda-tag:let 'scode-lambda-name:let)
    (provide-rename env 'lambda-tag:fluid-let 'scode-lambda-name:fluid-let)

    (for-each (lambda (old-name)
		(provide-rename env old-name (symbol 'scode- old-name)))
	      '(access-environment
		access-name
		access?
		assignment-name
		assignment-value
		assignment?
		block-declaration-text
		block-declaration?
		combination-operands
		combination-operator
		combination?
		comment-expression
		comment-text
		comment?
		conditional-alternative
		conditional-consequent
		conditional-predicate
		conditional?
		constant?
		declaration-expression
		declaration-text
		declaration?
		definition-name
		definition-value
		definition?
		delay-expression
		delay?
		disjunction-alternative
		disjunction-predicate
		disjunction?
		lambda-components
		lambda-body
		lambda-name
		lambda?
		open-block-actions
		open-block-declarations
		open-block-names
		open-block?
		quotation-expression
		quotation?
		sequence-actions
		sequence?
		the-environment?
		unassigned?-name
		unassigned??
		variable-name
		variable?))
    (for-each (lambda (root)
		(provide-rename env
				(symbol 'make- root)
				(symbol 'make-scode- root)))
	      '(access
		assignment
		block-declaration
		combination
		comment
		conditional
		declaration
		definition
		delay
		disjunction
		lambda
		open-block
		quotation
		sequence
		the-environment
		unassigned?
		variable))
    (provide-rename env 'set-lambda-body! 'set-scode-lambda-body!)
    (provide-rename env
		    'undefined-conditional-branch
		    'undefined-scode-conditional-branch))

  (let ((env (->environment '(runtime))))
    (if (unbound? env 'select-on-bytes-per-word)
	(eval '(define-syntax select-on-bytes-per-word
		 (er-macro-transformer
		  (lambda (form rename compare)
		    rename compare
		    (syntax-check '(keyword expression expression) form)
		    (let ((bpo (bytes-per-object)))
		      (case bpo
			((4) (cadr form))
			((8) (caddr form))
			(else (error "Unsupported bytes-per-object:" bpo)))))))
	      env))
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
                          env 'microcode-type))))

  (let ((env (->environment '(runtime syntax))))
    (provide-rename env 'make-compiler-item 'compiler-item)
    (provide-rename env 'compile-item/expression 'compile-expr-item)
    (if (unbound? env 'classify-form)
	(eval '(define (classify-form form env)
		 (classify/form form env env))
	      env)))

  (let ((env (->environment '(package))))
    (if (eval '(not (link-description? '#(name1 (package name) name2 #f)))
	      env)
	(eval
	 '(begin
	    (define (link-description? object)
	      (and (vector? object)
		   (cond ((fix:= (vector-length object) 2)
			  (and (symbol? (vector-ref object 0))
			       (package-name? (vector-ref object 1))))
			 ((fix:= (vector-length object) 3)
			  (and (symbol? (vector-ref object 0))
			       (package-name? (vector-ref object 1))
			       (symbol? (vector-ref object 2))))
			 ((fix:= (vector-length object) 4)
			  (and (symbol? (vector-ref object 0))
			       (package-name? (vector-ref object 1))
			       (symbol? (vector-ref object 2))
			       (or (eq? #f (vector-ref object 3))
				   (eq? 'deprecated (vector-ref object 3)))))
			 (else #f))))

	    (define (create-links-from-description description)
	      (let ((environment
		     (find-package-environment
		      (package-description/name description))))
		(let ((bindings (package-description/exports description)))
		  (let ((n (vector-length bindings)))
		    (do ((i 0 (fix:+ i 1)))
			((fix:= i n))
		      (let ((binding (vector-ref bindings i)))
			(link-variables (find-package-environment
					 (vector-ref binding 1))
					(if (fix:= (vector-length binding) 3)
					    (vector-ref binding 2)
					    (vector-ref binding 0))
					environment
					(vector-ref binding 0))))))
		(let ((bindings (package-description/imports description)))
		  (let ((n (vector-length bindings)))
		    (do ((i 0 (fix:+ i 1)))
			((fix:= i n))
		      (let ((binding (vector-ref bindings i)))
			(let ((source-environment
			       (find-package-environment
				(vector-ref binding 1)))
			      (source-name
			       (if (fix:>= (vector-length binding) 3)
				   (vector-ref binding 2)
				   (vector-ref binding 0))))
			  (guarantee-binding source-environment source-name)
			  (link-variables environment
					  (vector-ref binding 0)
					  source-environment
					  source-name)))))))))
	 env))))
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

;;;; Predicates
;;; package: (runtime predicate)

(declare (usual-integrations))

(define (predicate-name predicate)
  (dispatch-tag-name (predicate->dispatch-tag predicate)))

(define (predicate<= predicate1 predicate2)
  (dispatch-tag<= (predicate->dispatch-tag predicate1)
		  (predicate->dispatch-tag predicate2)))

(define (predicate>= predicate1 predicate2)
  (predicate<= predicate2 predicate1))

(define (dispatch-tag= tag1 tag2)
  (guarantee dispatch-tag? tag1 'dispatch-tag=)
  (guarantee dispatch-tag? tag2 'dispatch-tag=)
  (eq? tag1 tag2))

(define (dispatch-tag<= tag1 tag2)
  (guarantee dispatch-tag? tag1 'dispatch-tag<=)
  (guarantee dispatch-tag? tag2 'dispatch-tag<=)
  (cached-dispatch-tag<= tag1 tag2))

(define (dispatch-tag>= tag1 tag2)
  (dispatch-tag<= tag2 tag1))

(define (cached-dispatch-tag<= tag1 tag2)
  (hash-table-intern! dispatch-tag<=-cache
		      (cons tag1 tag2)
		      (lambda () (uncached-dispatch-tag<= tag1 tag2))))

(define (uncached-dispatch-tag<= tag1 tag2)
  (or (eq? tag1 tag2)
      (dispatch-tag-is-bottom? tag1)
      (dispatch-tag-is-top? tag2)
      (and (not (dispatch-tag-is-top? tag1))
	   (not (dispatch-tag-is-bottom? tag2))
	   (let ((v
		  (find (lambda (v)
			  (and ((vector-ref v 0) tag1)
			       ((vector-ref v 1) tag2)))
			dispatch-tag<=-overrides)))
	     (if v
		 ((vector-ref v 2) tag1 tag2)
		 (any-dispatch-tag-superset (lambda (tag)
					      (cached-dispatch-tag<= tag tag2))
					    tag1))))))

(define (define-dispatch-tag<= test1 test2 handler)
  (set! dispatch-tag<=-overrides
	(cons (vector test1 test2 handler)
	      dispatch-tag<=-overrides))
  unspecific)

;; TODO(cph): should be a weak-key table, but we don't have tables that have
;; weak compound keys.
(define-deferred dispatch-tag<=-cache (make-equal-hash-table))
(define dispatch-tag<=-overrides '())

(define (any-object? object)
  (declare (ignore object))
  #t)

(define (no-object? object)
  (declare (ignore object))
  #f)

(define (top-dispatch-tag) the-top-dispatch-tag)
(define (bottom-dispatch-tag) the-bottom-dispatch-tag)

(define-integrable (dispatch-tag-is-top? tag)
  (eq? the-top-dispatch-tag tag))

(define-integrable (dispatch-tag-is-bottom? tag)
  (eq? the-bottom-dispatch-tag tag))

(define-deferred the-top-dispatch-tag
  (make-compound-tag any-object? 'conjoin '()))

(define-deferred the-bottom-dispatch-tag
  (make-compound-tag no-object? 'disjoin '()))

(define get-predicate-tag)
(add-boot-init!
 (lambda ()
   (let ((table (make-hashed-metadata-table)))
     (set! predicate? (bundle-ref table 'has?))
     (set! get-predicate-tag (bundle-ref table 'get))
     (set! set-predicate-tag! (bundle-ref table 'put!)))
   (set! predicate->dispatch-tag
	 (named-lambda (predicate->dispatch-tag predicate)
	   (let ((tag (get-predicate-tag predicate #f)))
	     (if (not tag)
		 (error:not-a predicate? predicate))
	     tag)))
   (run-deferred-boot-actions 'set-predicate-tag!)
   (set! register-predicate!
	 (let ((make-simple-tag
		(dispatch-metatag-constructor
		 (make-dispatch-metatag 'simple-tag)
		 'register-predicate!)))
	   (named-lambda (register-predicate! predicate name . keylist)
	     (guarantee keyword-list? keylist 'register-predicate!)
	     (let ((tag
		    (let ((tag (get-predicate-tag predicate #f)))
		      (if tag
			  (begin
			    (if (not (eq? name (dispatch-tag-name tag)))
				(error "Can't re-register predicate:"
				       predicate name))
			    tag)
			  (make-simple-tag name predicate)))))
	       (for-each (lambda (superset)
			   (set-predicate<=! predicate superset))
			 (get-keyword-values keylist '<=))
	       tag))))
   (set! set-dispatch-tag<=!
	 (named-lambda (set-dispatch-tag<=! tag superset)
	   (if (dispatch-tag>= tag superset)
	       (error "Not allowed to create a superset loop:" tag superset))
	   (add-dispatch-tag-superset tag superset)
	   (hash-table-clear! dispatch-tag<=-cache)))
   (set! set-predicate<=!
	 (named-lambda (set-predicate<=! predicate superset)
	   (set-dispatch-tag<=! (predicate->dispatch-tag predicate)
				(predicate->dispatch-tag superset))))
   unspecific))

(add-boot-init!
 (lambda ()
   (register-predicate! %record? '%record)
   (register-predicate! %tagged-object? 'tagged-object)
   (register-predicate! predicate? 'predicate)))

;;; Registration of standard predicates
(add-boot-init!
 (lambda ()
   ;; R7RS
   (register-predicate! boolean? 'boolean)
   (register-predicate! bytevector? 'bytevector)
   (register-predicate! char? 'char)
   (register-predicate! default-object? 'default-object)
   (register-predicate! eof-object? 'eof-object)
   (register-predicate! list? 'list)
   (register-predicate! number? 'number)
   (register-predicate! pair? 'pair)
   (register-predicate! procedure? 'procedure)
   (register-predicate! string? 'string)
   (register-predicate! symbol? 'symbol)
   (register-predicate! vector? 'vector)

   (register-predicate! real? 'real-number '<= number?)
   (register-predicate! rational? 'rational-number '<= real?)
   (register-predicate! integer? 'integer '<= rational?)

   (register-predicate! null? 'empty-list '<= list?)

   ;; SRFI-1
   (register-predicate! circular-list? 'circular-list)
   (register-predicate! dotted-list? 'dotted-list)
   (register-predicate! not-pair? 'not-pair)))

;;; Registration of predicates defined earlier in the boot load, or
;;; needed before their packages are initialized.
(add-boot-init!
 (lambda ()
   ;; MIT/GNU Scheme: specialized arithmetic
   (register-predicate! exact-integer? 'exact-integer '<= integer?)
   (register-predicate! exact-nonnegative-integer? 'exact-nonnegative-integer
			'<= exact-integer?)
   (register-predicate! exact-positive-integer? 'exact-positive-integer
			'<= exact-integer?)
   (register-predicate! exact-rational? 'exact-rational '<= rational?)

   (register-predicate! fix:fixnum? 'fixnum '<= exact-integer?)
   (register-predicate! index-fixnum? 'index-fixnum
			'<= fix:fixnum?
			'<= exact-nonnegative-integer?)
   (register-predicate! negative-fixnum? 'negative-fixnum '<= fix:fixnum?)
   (register-predicate! positive-fixnum? 'positive-fixnum
			'<= fix:fixnum?
			'<= exact-positive-integer?)
   (register-predicate! non-negative-fixnum? 'non-negative-fixnum
			'<= fix:fixnum?
			'<= exact-nonnegative-integer?)
   (register-predicate! non-positive-fixnum? 'non-positive-fixnum
			'<= fix:fixnum?)
   (register-predicate! radix? 'radix '<= index-fixnum?)

   (register-predicate! flo:flonum? 'flonum '<= real?)

   ;; MIT/GNU Scheme: lists
   (register-predicate! alist? 'association-list '<= list?)
   (register-predicate! keyword-list? 'keyword-list '<= list?)
   (register-predicate! list-of-unique-symbols? 'list-of-unique-symbols
			'<= list?)
   (register-predicate! non-empty-list? 'non-empty-list
			'<= list?
			'<= pair?)
   (register-predicate! unique-keyword-list? 'unique-keyword-list
			'<= keyword-list?)

   ;; MIT/GNU Scheme: procedures
   (register-predicate! apply-hook? 'apply-hook '<= procedure?)
   (register-predicate! binary-procedure? 'binary-procedure '<= procedure?)
   (register-predicate! compiled-procedure? 'compiled-procedure '<= procedure?)
   (register-predicate! entity? 'entity '<= procedure?)
   (register-predicate! memoizer? 'memoizer '<= apply-hook?)
   (register-predicate! primitive-procedure? 'primitive-procedure
			'<= procedure?)
   (register-predicate! procedure-arity? 'procedure-arity)
   (register-predicate! thunk? 'thunk '<= procedure?)
   (register-predicate! unary-procedure? 'unary-procedure '<= procedure?)))

(add-boot-init!
 (lambda ()
   ;; MIT/GNU Scheme: misc
   (register-predicate! 8-bit-char? '8-bit-char '<= char?)
   (register-predicate! ascii-char? 'ascii-char '<= 8-bit-char?)
   (register-predicate! bit-string? 'bit-string)
   (register-predicate! bitless-char? 'bitless-char '<= char?)
   (register-predicate! cell? 'cell)
   (register-predicate! code-point-list? 'code-point-list '<= list?)
   (register-predicate! compiled-code-address? 'compiled-code-address)
   (register-predicate! compiled-code-block? 'compiled-code-block)
   (register-predicate! compiled-expression? 'compiled-expression)
   (register-predicate! compiled-return-address? 'compiled-return-address)
   (register-predicate! control-point? 'control-point)
   (register-predicate! ephemeron? 'ephemeron)
   (register-predicate! equality-predicate? 'equality-predicate
			'<= binary-procedure?)
   (register-predicate! interned-symbol? 'interned-symbol '<= symbol?)
   (register-predicate! keyword? 'keyword '<= symbol?)
   (register-predicate! lambda-tag? 'lambda-tag)
   (register-predicate! named-structure? 'named-structure)
   (register-predicate! named-list? 'named-list
			'<= non-empty-list?
			'<= named-structure?)
   (register-predicate! named-vector? 'named-vector
			'<= vector?
			'<= named-structure?)
   (register-predicate! population? 'population)
   (register-predicate! promise? 'promise)
   (register-predicate! record? 'record
			'<= %record?
			'<= named-structure?)
   (register-predicate! applicable-record? 'applicable-record '<= record?)
   (register-predicate! stack-address? 'stack-address)
   (register-predicate! thread-mutex? 'thread-mutex)
   (register-predicate! undefined-value? 'undefined-value)
   (register-predicate! unicode-code-point? 'unicode-code-point
			'<= index-fixnum?)
   (register-predicate! unicode-scalar-value? 'unicode-scalar-value
			'<= unicode-code-point?)
   (register-predicate! uninterned-symbol? 'uninterned-symbol '<= symbol?)
   (register-predicate! weak-list? 'weak-list)
   (register-predicate! weak-pair? 'weak-pair)

   (run-deferred-boot-actions 'predicate-registrations)
   (run-deferred-boot-actions 'predicate-relations)))
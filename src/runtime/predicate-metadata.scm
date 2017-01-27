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

;;;; Predicates: metadata
;;; package: (runtime predicate-metadata)

(declare (usual-integrations))

(define predicate?)
(define get-predicate-tag)
(define set-predicate-tag!)
(define delete-predicate-tag!)
(add-boot-init!
 (lambda ()
   (let ((table (make-hashed-metadata-table)))
     (set! predicate? (table 'has?))
     (set! get-predicate-tag (table 'get-if-available))
     (set! set-predicate-tag! (table 'put!))
     (set! delete-predicate-tag! (table 'delete!))
     unspecific)))

(define (register-predicate! predicate name . keylist)
  (guarantee keyword-list? keylist 'register-predicate!)
  (let ((tag
         (make-tag name
                   predicate
		   predicate-tagging-strategy:never
                   (get-keyword-value keylist 'extra)
                   (get-keyword-value keylist 'description))))
    (for-each (lambda (superset)
		(set-tag<=! tag (predicate->tag superset)))
	      (get-keyword-values keylist '<=))
    tag))

(define (predicate-name predicate)
  (tag-name (predicate->tag predicate 'predicate-name)))

(define (predicate-element-constructor predicate)
  (tag-element-constructor (predicate->tag predicate 'predicate-constructor)))

(define (predicate-element-accessor predicate)
  (tag-element-accessor (predicate->tag predicate 'predicate-accessor)))

(define (predicate-tagging-strategy predicate)
  (tag-tagging-strategy (predicate->tag predicate 'predicate-tagging-strategy)))

(define (predicate-description predicate)
  (let ((tag (get-predicate-tag predicate #f)))
    (if tag
        (tag-description tag)
        (string-append "object satisfying " (object->description predicate)))))

(define (set-predicate<=! predicate superset)
  (set-tag<=! (predicate->tag predicate 'set-predicate<=!)
              (predicate->tag superset 'set-predicate<=!)))

(define (unregister-predicate! predicate)
  (delete-tag! (predicate->tag predicate)))

(define (predicate->tag predicate #!optional caller)
  (let ((tag (get-predicate-tag predicate #f)))
    (if (not tag)
        (error:not-a predicate? predicate caller))
    tag))

(define (guarantee predicate object #!optional caller)
  (if (not (predicate object))
      (error:not-a predicate object caller))
  object)

(define (error:not-a predicate object #!optional caller)
  (error:wrong-type-argument object (predicate-description predicate) caller))

(define (guarantee-list-of predicate object #!optional caller)
  (if (not (list-of-type? object predicate))
      (error:not-a-list-of predicate object caller))
  object)

(define (error:not-a-list-of predicate object #!optional caller)
  (error:wrong-type-argument object
                             (string-append "list of "
                                            (predicate-description predicate))
                             caller))

(define (make-tag name datum-test tagging-strategy caller
		  #!optional extra description)
  (guarantee tag-name? name caller)
  (guarantee unary-procedure? datum-test caller)
  (if (not (default-object? description))
      (guarantee string? description caller))
  (tagging-strategy name datum-test
    (lambda (predicate constructor accessor)
      (let ((tag
	     (%make-tag name
			predicate
			constructor
			accessor
			(if (default-object? extra) #f extra)
			(if (default-object? description) #f description)
			tagging-strategy
			(make-strong-eq-hash-table)
			(make-strong-eq-hash-table))))
	(set-predicate-tag! predicate tag)
	tag))))

(define (tag-name? object)
  (or (symbol? object)
      (and (list? object)
           (every tag-name? object))))

(define-record-type <tag>
    (%make-tag name predicate constructor accessor extra description
	       tagging-strategy subsets supersets)
    tag?
  (name tag-name)
  (predicate tag->predicate)
  (constructor tag-element-constructor)
  (accessor tag-element-accessor)
  (extra tag-extra)
  (description %tag-description)
  (tagging-strategy tag-tagging-strategy)
  (subsets tag-subsets)
  (supersets tag-supersets))

(set-record-type-unparser-method! <tag>
  (simple-unparser-method 'tag
    (lambda (tag)
      (list (tag-name tag)))))

(define (tag-description tag)
  (or (%tag-description tag)
      (object->description (tag-name tag))))

(define (object->description object)
  (call-with-output-string
    (lambda (port)
      (write object port))))

(define (get-tag-subsets tag)
  (hash-table-keys (tag-subsets tag)))

(define (get-tag-supersets tag)
  (hash-table-keys (tag-supersets tag)))

(define (set-tag<=! tag superset)
  (event-distributor/invoke! event:predicate-metadata 'set-tag<=! tag superset)
  (%link! tag superset))

(define (delete-tag! tag)
  (event-distributor/invoke! event:predicate-metadata 'delete-tag! tag)
  ;; Directly link subsets to supersets.
  (for-each (lambda (subset)
	      (for-each (lambda (superset)
			  (%link! subset superset))
			(get-tag-supersets tag)))
	    (get-tag-subsets tag))
  ;; Delete this tag from subsets and supersets.
  (for-each (lambda (subset)
	      (hash-table-delete! (tag-supersets subset) tag))
	    (get-tag-subsets tag))
  (for-each (lambda (superset)
	      (hash-table-delete! (tag-subsets superset) tag))
	    (get-tag-supersets tag))
  (delete-predicate-tag! tag))

(define (%link! subset superset)
  (hash-table-set! (tag-subsets superset) subset subset)
  (hash-table-set! (tag-supersets subset) superset superset))

(define event:predicate-metadata (make-event-distributor))

(define the-top-tag)
(define the-bottom-tag)
(add-boot-init!
 (lambda ()
   (register-predicate! predicate? 'predicate)
   (register-predicate! tag-name? 'tag-name)
   (register-predicate! any-object? '(conjoin) 'description "any object")
   (register-predicate! no-object? '(disjoin) 'description "no object")

   (set! the-top-tag (predicate->tag any-object?))
   (set! the-bottom-tag (predicate->tag no-object?))
   unspecific))

(define (top-tag) the-top-tag)
(define (top-tag? object) (eqv? the-top-tag object))

(define (bottom-tag) the-bottom-tag)
(define (bottom-tag? object) (eqv? the-bottom-tag object))

(define (any-object? object) object #t)
(define (no-object? object) object #f)

(add-boot-init!
 (lambda ()
   (register-predicate! %record? '%record)
   (register-predicate! record? 'record '<= %record?)
   (cleanup-boot-time-record-predicates!)))

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

   (register-predicate! flo:flonum? 'flonum '<= real?)

   (register-mit-bytevector-predicates!)

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
   (register-predicate! generic-procedure? 'generic-procedure '<= procedure?)
   (register-predicate! memoizer? 'memoizer '<= apply-hook?)
   (register-predicate! primitive-procedure? 'primitive-procedure
			'<= procedure?)
   (register-predicate! procedure-arity? 'procedure-arity)
   (register-predicate! thunk? 'thunk '<= procedure?)
   (register-predicate! unary-procedure? 'unary-procedure '<= procedure?)
   (register-predicate! unparser-method? 'unparser-method '<= procedure?)))

(add-boot-init!
 (lambda ()
   ;; MIT/GNU Scheme: misc
   (register-predicate! 8-bit-char? '8-bit-char '<= char?)
   (register-predicate! bit-string? 'bit-string)
   (register-predicate! cell? 'cell)
   (register-predicate! compiled-code-address? 'compiled-code-address)
   (register-predicate! compiled-code-block? 'compiled-code-block)
   (register-predicate! compiled-expression? 'compiled-expression)
   (register-predicate! compiled-return-address? 'compiled-return-address)
   (register-predicate! dispatch-tag? 'dispatch-tag)
   (register-predicate! ephemeron? 'ephemeron)
   (register-predicate! environment? 'environment)
   (register-predicate! equality-predicate? 'equality-predicate
			'<= binary-procedure?)
   (register-predicate! interned-symbol? 'interned-symbol '<= symbol?)
   (register-predicate! keyword? 'keyword '<= symbol?)
   (register-predicate! lambda-tag? 'lambda-tag)
   (register-predicate! named-structure? 'named-structure)
   (register-predicate! population? 'population)
   (register-predicate! promise? 'promise)
   (register-predicate! record-type? 'record-type)
   (register-predicate! stack-address? 'stack-address)
   (register-predicate! thread-mutex? 'thread-mutex)
   (register-predicate! undefined-value? 'undefined-value)
   (register-predicate! unicode-char? 'unicode-char '<= char?)
   (register-predicate! unicode-scalar-value? 'unicode-scalar-value
			'<= index-fixnum?)
   (register-predicate! uninterned-symbol? 'uninterned-symbol '<= symbol?)
   (register-predicate! weak-list? 'weak-list)
   (register-predicate! weak-pair? 'weak-pair)

   (register-ustring-predicates!)))
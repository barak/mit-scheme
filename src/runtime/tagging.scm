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

;;;; Predicates: tagging
;;; package: (runtime tagging)

(declare (usual-integrations))

;;; TODO(cph): eliminate after 9.3 release:
(define-integrable tagged-object-type #x25)

(define (tagged-object? object)
  (fix:= tagged-object-type (object-type object)))

(define (object-tagger predicate)
  (let ((tag (predicate->tag predicate)))
    (lambda (datum)
      (make-tagged-object tag datum))))

(define (tag-object predicate datum)
  (make-tagged-object (predicate->tag predicate) datum))

(define (tagged-object-predicate object)
  (tag->predicate (tagged-object-tag object)))

(define-integrable (make-tagged-object tag datum)
  (system-pair-cons tagged-object-type tag datum))

(define (tagged-object-tag object)
  (guarantee tagged-object? object 'tagged-object-tag)
  (system-pair-car object))

(define (tagged-object-datum object)
  (guarantee tagged-object? object 'tagged-object-datum)
  (system-pair-cdr object))

(define unparser-methods)
(add-boot-init!
 (lambda ()
   (register-predicate! tagged-object? 'tagged-object)
   (set! unparser-methods (make-key-weak-eqv-hash-table))
   unspecific))

(define (get-tagged-object-unparser-method object)
  (hash-table-ref/default unparser-methods (tagged-object-tag object) #f))

(define (set-tagged-object-unparser-method! tag unparser)
  (if unparser
      (begin
	(guarantee unparser-method? unparser
		   'set-tagged-object-unparser-method!)
	(hash-table-set! unparser-methods tag unparser))
      (hash-table-delete! unparser-methods tag)))

(define (object->predicate object)
  (tag->predicate (object->tag object)))

(define (object->tag object)
  (let ((code (object-type object)))
    (or (vector-ref primitive-tags code)
	((vector-ref primitive-tag-methods code) object))))

(define (object->datum object)
  (cond ((tagged-object? object) (system-pair-cdr object))
        (else object)))

(define primitive-tags)
(define primitive-tag-methods)
(add-boot-init!
 (lambda ()
   (set! primitive-tags
	 (make-vector (microcode-type/code-limit)
		      (top-tag)))
   (set! primitive-tag-methods
	 (make-vector (microcode-type/code-limit) #f))
   unspecific))

(add-boot-init!
 (lambda ()
   (define (define-primitive-predicate type-name predicate)
     (vector-set! primitive-tags
		  (microcode-type/name->code type-name)
		  (predicate->tag predicate)))

   (define-primitive-predicate 'bignum exact-integer?)
   (define-primitive-predicate 'bytevector bytevector?)
   (define-primitive-predicate 'cell cell?)
   (define-primitive-predicate 'character char?)
   (define-primitive-predicate 'compiled-code-block compiled-code-block?)
   (define-primitive-predicate 'ephemeron ephemeron?)
   (define-primitive-predicate 'extended-procedure procedure?)
   (define-primitive-predicate 'false boolean?)
   (define-primitive-predicate 'fixnum fix:fixnum?)
   (define-primitive-predicate 'flonum flo:flonum?)
   (define-primitive-predicate 'interned-symbol interned-symbol?)
   (define-primitive-predicate 'pair pair?)
   (define-primitive-predicate 'primitive primitive-procedure?)
   (define-primitive-predicate 'procedure procedure?)
   (define-primitive-predicate 'promise promise?)
   (define-primitive-predicate 'ratnum exact-rational?)
   (define-primitive-predicate 'recnum number?)
   (define-primitive-predicate 'stack-environment stack-address?)
   (define-primitive-predicate 'string string?)
   (define-primitive-predicate 'uninterned-symbol uninterned-symbol?)
   (define-primitive-predicate 'vector vector?)
   (define-primitive-predicate 'vector-1b bit-string?)
   (define-primitive-predicate 'weak-cons weak-pair?)))

(add-boot-init!
 (lambda ()
   (define (define-primitive-predicate-method type-name method)
     (let ((type-code (microcode-type/name->code type-name)))
       (vector-set! primitive-tags type-code #f)
       (vector-set! primitive-tag-methods type-code method)))

   (define-primitive-predicate-method 'tagged-object
     system-pair-car)

   (define-primitive-predicate-method 'constant
     (let* ((constant-tags
	     (list->vector
	      (map predicate->tag
		   (list boolean?
			 undefined-value?
			 undefined-value?
			 lambda-tag?
			 lambda-tag?
			 lambda-tag?
			 eof-object?
			 default-object?
			 lambda-tag?
			 null?))))
	    (n-tags (vector-length constant-tags)))
       (lambda (object)
	 (let ((datum (object-datum object)))
	   (if (and (fix:fixnum? datum) (fix:< datum n-tags))
	       (vector-ref constant-tags datum)
	       (top-tag))))))

   (define-primitive-predicate-method 'entity
     (let ((apply-hook-tag (predicate->tag apply-hook?))
	   (entity-tag (predicate->tag entity?)))
       (lambda (object)
	 (if (%entity-is-apply-hook? object)
	     apply-hook-tag
	     entity-tag))))

   (define-primitive-predicate-method 'compiled-entry
     (let ((procedure-tag (predicate->tag compiled-procedure?))
	   (return-tag (predicate->tag compiled-return-address?))
	   (expression-tag (predicate->tag compiled-expression?))
	   (default-tag (predicate->tag compiled-code-address?)))
       (lambda (entry)
	 (case (system-hunk3-cxr0
		((ucode-primitive compiled-entry-kind 1) entry))
	   ((0) procedure-tag)
	   ((1) return-tag)
	   ((2) expression-tag)
	   (else default-tag)))))

   (define-primitive-predicate-method 'record
     (let ((default-tag (predicate->tag record?)))
       (lambda (object)
	 default-tag)))))
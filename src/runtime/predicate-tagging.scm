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

;;;; Predicates: tagging
;;; package: (runtime predicate-tagging)

(declare (usual-integrations))

(define (object->predicate object)
  (dispatch-tag->predicate (object->dispatch-tag object)))

(define (object->dispatch-tag object)
  (let ((code (object-type object)))
    (or (vector-ref primitive-tags code)
	((vector-ref primitive-tag-methods code) object)
	(error "Unknown type code:" code))))

(define (object->datum object)
  (if (%tagged-object? object)
      (%tagged-object-datum object)
      object))

(define (predicate-tagger predicate)
  (%tag-tagger (predicate->dispatch-tag predicate) predicate))

(define (dispatch-tag-tagger tag)
  (%tag-tagger tag (dispatch-tag->predicate tag)))

(define (%tag-tagger tag predicate)
  (lambda (datum #!optional tagger-name)
    (if (dispatch-tag<= (object->dispatch-tag datum) tag)
	datum
	(begin
	  (guarantee predicate datum tagger-name)
	  (%make-tagged-object tag datum)))))

(define primitive-tags)
(define primitive-tag-methods)
(add-boot-init!
 (lambda ()
   (set! primitive-tags
	 (make-vector (microcode-type/code-limit)
		      (top-dispatch-tag)))
   (set! primitive-tag-methods
	 (make-vector (microcode-type/code-limit) #f))
   unspecific))

(add-boot-init!
 (lambda ()
   (define (define-primitive-predicate type-name predicate)
     (vector-set! primitive-tags
		  (microcode-type/name->code type-name)
		  (predicate->dispatch-tag predicate)))

   (define-primitive-predicate 'assignment scode-assignment?)
   (define-primitive-predicate 'bignum exact-integer?)
   (define-primitive-predicate 'bytevector bytevector?)
   (define-primitive-predicate 'cell cell?)
   (define-primitive-predicate 'character char?)
   (define-primitive-predicate 'compiled-code-block compiled-code-block?)
   (define-primitive-predicate 'conditional scode-conditional?)
   (define-primitive-predicate 'control-point control-point?)
   (define-primitive-predicate 'definition scode-definition?)
   (define-primitive-predicate 'delay scode-delay?)
   (define-primitive-predicate 'disjunction scode-disjunction?)
   (define-primitive-predicate 'environment ic-environment?)
   (define-primitive-predicate 'ephemeron ephemeron?)
   (define-primitive-predicate 'extended-lambda scode-lambda?)
   (define-primitive-predicate 'extended-procedure procedure?)
   (define-primitive-predicate 'false boolean?)
   (define-primitive-predicate 'fixnum fix:fixnum?)
   (define-primitive-predicate 'flonum flo:flonum?)
   (define-primitive-predicate 'interned-symbol interned-symbol?)
   (define-primitive-predicate 'lambda scode-lambda?)
   (define-primitive-predicate 'pair pair?)
   (define-primitive-predicate 'primitive primitive-procedure?)
   (define-primitive-predicate 'procedure procedure?)
   (define-primitive-predicate 'promise promise?)
   (define-primitive-predicate 'quotation scode-quotation?)
   (define-primitive-predicate 'ratnum exact-rational?)
   (define-primitive-predicate 'recnum number?)
   (define-primitive-predicate 'stack-environment stack-address?)
   (define-primitive-predicate 'string string?)
   (define-primitive-predicate 'the-environment scode-the-environment?)
   (define-primitive-predicate 'unicode-string string?)
   (define-primitive-predicate 'uninterned-symbol uninterned-symbol?)
   (define-primitive-predicate 'variable scode-variable?)
   (define-primitive-predicate 'vector vector?)
   (define-primitive-predicate 'vector-1b bit-string?)
   (define-primitive-predicate 'weak-cons weak-pair?)
   ))

(add-boot-init!
 (lambda ()
   (define (define-primitive-predicate-method type-name method)
     (let ((type-code (microcode-type/name->code type-name)))
       (vector-set! primitive-tags type-code #f)
       (vector-set! primitive-tag-methods type-code method)))

   (define (simple-alternative primary alternative)
     (let ((primary-tag (predicate->dispatch-tag primary))
	   (alternative-tag (predicate->dispatch-tag alternative)))
       (lambda (object)
	 (if (alternative object)
	     alternative-tag
	     primary-tag))))

   (define-primitive-predicate-method 'access
     (simple-alternative scode-access? scode-absolute-reference?))

   (define-primitive-predicate-method 'combination
     (simple-alternative scode-combination? scode-unassigned??))

   (define-primitive-predicate-method 'comment
     (simple-alternative scode-comment? scode-declaration?))

   (define-primitive-predicate-method 'compiled-entry
     (let ((procedure-tag (predicate->dispatch-tag compiled-procedure?))
	   (return-tag (predicate->dispatch-tag compiled-return-address?))
	   (expression-tag (predicate->dispatch-tag compiled-expression?))
	   (default-tag (predicate->dispatch-tag compiled-code-address?)))
       (lambda (entry)
	 (case (system-hunk3-cxr0
		((ucode-primitive compiled-entry-kind 1) entry))
	   ((0) procedure-tag)
	   ((1) return-tag)
	   ((2) expression-tag)
	   (else default-tag)))))

   (define-primitive-predicate-method 'constant
     (let* ((constant-tags
	     (list->vector
	      (map predicate->dispatch-tag
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
	       (top-dispatch-tag))))))

   (define-primitive-predicate-method 'entity
     (let ((apply-hook-tag (predicate->dispatch-tag apply-hook?))
	   (entity-tag (predicate->dispatch-tag entity?)))
       (lambda (object)
	 (if (%entity-is-apply-hook? object)
	     apply-hook-tag
	     entity-tag))))

   (define-primitive-predicate-method 'record
     (let ((default-tag (predicate->dispatch-tag %record?)))
       (lambda (object)
	 (if (dispatch-tag? (%record-ref object 0))
	     (%record-ref object 0)
	     default-tag))))

   (define-primitive-predicate-method 'sequence
     (simple-alternative scode-sequence? scode-open-block?))

   (define-primitive-predicate-method 'tagged-object
     %tagged-object-tag)))
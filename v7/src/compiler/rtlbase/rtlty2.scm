#| -*-Scheme-*-

$Id: rtlty2.scm,v 4.14 2002/11/20 19:45:56 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; Register Transfer Language Type Definitions
;;; package: (compiler)

(declare (usual-integrations))

(define-integrable rtl:expression? pair?)
(define-integrable rtl:expression-type car)
(define-integrable rtl:address-register cadr)
(define-integrable rtl:address-number caddr)
(define-integrable rtl:test-expression cadr)
(define-integrable rtl:invocation-pushed cadr)
(define-integrable rtl:invocation-continuation caddr)

(define-integrable (rtl:set-invocation-continuation! rtl continuation)
  (set-car! (cddr rtl) continuation))

;;;; Locatives

;;; Locatives are used as an intermediate form by the code generator
;;; to build expressions.  Later, when the expressions are inserted
;;; into statements, any locatives they contain are eliminated by
;;; "simplifying" them into sequential instructions using pseudo
;;; registers.

(define-integrable register:environment
  'ENVIRONMENT)

(define-integrable register:stack-pointer
  'STACK-POINTER)

(define-integrable register:dynamic-link
  'DYNAMIC-LINK)

(define-integrable register:value
  'VALUE)

(define-integrable register:int-mask
  'INT-MASK)

(define-integrable register:memory-top
  'MEMORY-TOP)

(define-integrable register:free
  'FREE)

(define-integrable (rtl:interpreter-call-result:access)
  (rtl:make-fetch 'INTERPRETER-CALL-RESULT:ACCESS))

(define-integrable (rtl:interpreter-call-result:cache-reference)
  (rtl:make-fetch 'INTERPRETER-CALL-RESULT:CACHE-REFERENCE))

(define-integrable (rtl:interpreter-call-result:cache-unassigned?)
  (rtl:make-fetch 'INTERPRETER-CALL-RESULT:CACHE-UNASSIGNED?))

(define-integrable (rtl:interpreter-call-result:lookup)
  (rtl:make-fetch 'INTERPRETER-CALL-RESULT:LOOKUP))

(define-integrable (rtl:interpreter-call-result:unassigned?)
  (rtl:make-fetch 'INTERPRETER-CALL-RESULT:UNASSIGNED?))

(define-integrable (rtl:interpreter-call-result:unbound?)
  (rtl:make-fetch 'INTERPRETER-CALL-RESULT:UNBOUND?))

;;; "Pre-simplification" locative offsets

(define (rtl:locative-offset? locative)
  (and (pair? locative) (eq? (car locative) 'OFFSET)))

(define-integrable rtl:locative-offset-base cadr)
(define-integrable rtl:locative-offset-offset caddr)

#|
(define (rtl:locative-offset-granularity locative)
  ;; This is kludged up for backward compatibility
  (if (rtl:locative-offset? locative)
      (if (pair? (cdddr locative))
	  (cadddr locative)
	  'OBJECT)
      (error "Not a locative offset" locative)))
|#
(define-integrable rtl:locative-offset-granularity cadddr)

(define-integrable (rtl:locative-byte-offset? locative)
  (eq? (rtl:locative-offset-granularity locative) 'BYTE))

(define-integrable (rtl:locative-float-offset? locative)
  (eq? (rtl:locative-offset-granularity locative) 'FLOAT))

(define-integrable (rtl:locative-object-offset? locative)
  (eq? (rtl:locative-offset-granularity locative) 'OBJECT))

(define-integrable (rtl:locative-offset locative offset)
  (rtl:locative-object-offset locative offset))

(define (rtl:locative-byte-offset locative byte-offset)
  (cond ((rtl:locative-offset? locative)
	 `(OFFSET ,(rtl:locative-offset-base locative)
		  ,(back-end:+
		    byte-offset
		    (cond ((rtl:locative-byte-offset? locative)
			   (rtl:locative-offset-offset locative))
			  ((rtl:locative-object-offset? locative)
			   (back-end:*
			    (rtl:locative-offset-offset locative)
			    address-units-per-object))
			  (else
			   (back-end:*
			    (rtl:locative-offset-offset locative)
			    address-units-per-float))))
		  BYTE))
	((back-end:= byte-offset 0)
	 locative)
	(else
	 `(OFFSET ,locative ,byte-offset BYTE))))

(define (rtl:locative-float-offset locative float-offset)
  (let ((default
	  (lambda ()
	    `(OFFSET ,locative ,float-offset FLOAT))))
    (cond ((rtl:locative-offset? locative)
	   (if (rtl:locative-float-offset? locative)
	       `(OFFSET ,(rtl:locative-offset-base locative)
			,(back-end:+ (rtl:locative-offset-offset locative)
				     float-offset)
			FLOAT)
	       (default)))
	  (else
	   (default)))))

(define (rtl:locative-object-offset locative offset)
  (cond ((back-end:= offset 0) locative)
	((rtl:locative-offset? locative)
	 (if (not (rtl:locative-object-offset? locative))
	     (error "Can't add object offset to non-object offset"
		    locative offset)
	     `(OFFSET ,(rtl:locative-offset-base locative)
		      ,(back-end:+ (rtl:locative-offset-offset locative)
				   offset)
		      OBJECT)))
	(else
	 `(OFFSET ,locative ,offset OBJECT))))

(define (rtl:locative-index? locative)
  (and (pair? locative) (eq? (car locative) 'INDEX)))

(define-integrable rtl:locative-index-base cadr)
(define-integrable rtl:locative-index-offset caddr)
(define-integrable rtl:locative-index-granularity cadddr)

(define-integrable (rtl:locative-byte-index? locative)
  (eq? (rtl:locative-index-granularity locative) 'BYTE))

(define-integrable (rtl:locative-float-index? locative)
  (eq? (rtl:locative-index-granularity locative) 'FLOAT))

(define-integrable (rtl:locative-object-index? locative)
  (eq? (rtl:locative-index-granularity locative) 'OBJECT))

(define (rtl:locative-byte-index locative offset)
  `(INDEX ,locative ,offset BYTE))

(define (rtl:locative-float-index locative offset)
  `(INDEX ,locative ,offset FLOAT))

(define (rtl:locative-object-index locative offset)
  `(INDEX ,locative ,offset OBJECT))

;;; Expressions that are used in the intermediate form.

(define-integrable (rtl:make-address locative)
  `(ADDRESS ,locative))

(define-integrable (rtl:make-environment locative)
  `(ENVIRONMENT ,locative))

(define-integrable (rtl:make-cell-cons expression)
  `(CELL-CONS ,expression))

(define-integrable (rtl:make-fetch locative)
  `(FETCH ,locative))

(define-integrable (rtl:make-typed-cons:pair type car cdr)
  `(TYPED-CONS:PAIR ,type ,car ,cdr))

(define-integrable (rtl:make-typed-cons:vector type elements)
  `(TYPED-CONS:VECTOR ,type ,@elements))

(define-integrable (rtl:make-typed-cons:procedure entry)
  `(TYPED-CONS:PROCEDURE ,entry))

;;; Linearizer Support

(define-integrable (rtl:make-jump-statement label)
  `(JUMP ,label))

(define-integrable (rtl:make-jumpc-statement predicate label)
  `(JUMPC ,predicate ,label))

(define-integrable (rtl:make-label-statement label)
  `(LABEL ,label))

(define-integrable (rtl:negate-predicate expression)
  `(NOT ,expression))

;;; Stack

(define-integrable (stack-locative-offset locative offset)
  (rtl:locative-offset locative (stack->memory-offset offset)))

(define-integrable (stack-push-address)
  (rtl:make-pre-increment (interpreter-stack-pointer)
			  (stack->memory-offset -1)))

(define-integrable (stack-pop-address)
  (rtl:make-post-increment (interpreter-stack-pointer)
			   (stack->memory-offset 1)))
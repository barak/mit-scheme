;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/utabs.scm,v 13.42 1987/03/09 15:00:25 cph Exp $
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Microcode Table Interface

(declare (usual-integrations))

(define fixed-objects-vector-slot)

(define number-of-microcode-types)
(define microcode-type-name)
(define microcode-type)
(define microcode-type-predicate)
(define object-type)

(define number-of-microcode-returns)
(define microcode-return)
(define make-return-address)
(define return-address?)
(define return-address-code)
(define return-address-name)

(define number-of-microcode-errors)
(define microcode-error)

(define number-of-microcode-terminations)
(define microcode-termination)
(define microcode-termination-name)

(define make-primitive-procedure)
(define primitive-procedure?)
(define primitive-procedure-name)
(define implemented-primitive-procedure?)

(define microcode-identification-item)

(define future?)

(define microcode-system
  (make-environment

(define :name "Microcode")
(define :version)
(define :modification)
(define :identification)
(define :release)

(let-syntax ((define-primitive
	       (macro (name)
		 `(DEFINE ,name ,(make-primitive-procedure name)))))
  (define-primitive binary-fasload)
  (define-primitive microcode-identify)
  (define-primitive microcode-tables-filename)
  (define-primitive map-machine-address-to-code)
  (define-primitive map-code-to-machine-address)
  (define-primitive get-external-counts)
  (define-primitive get-external-number)
  (define-primitive get-external-name))

;;;; Fixed Objects Vector

(set! fixed-objects-vector-slot
(named-lambda (fixed-objects-vector-slot name)
  (or (microcode-table-search 15 name)
      (error "Unknown name" fixed-objects-vector-slot name))))

(define fixed-objects)

(define (microcode-table-search slot name)
  (let ((vector (vector-ref fixed-objects slot)))
    (let ((end (vector-length vector)))
      (define (loop i)
	(and (not (= i end))
	     (let ((entry (vector-ref vector i)))
	       (if (if (pair? entry)
		       (memq name entry)
		       (eq? name entry))
		   i
		   (loop (1+ i))))))
      (loop 0))))

(define (microcode-table-ref slot index)
  (let ((vector (vector-ref fixed-objects slot)))
    (and (< index (vector-length vector))
	 (let ((entry (vector-ref vector index)))
	   (if (pair? entry)
	       (car entry)
	       entry)))))

;;;; Microcode Type Codes

(define types-slot)

(define renamed-user-object-types
  '((FIXNUM . NUMBER) (BIG-FIXNUM . NUMBER) (BIG-FLONUM . NUMBER)
    (EXTENDED-FIXNUM . NUMBER)
    (EXTENDED-PROCEDURE . PROCEDURE)
    (LEXPR . LAMBDA) (EXTENDED-LAMBDA . LAMBDA)
    (COMBINATION-1 . COMBINATION) (COMBINATION-2 . COMBINATION)
    (PRIMITIVE-COMBINATION-0 . COMBINATION)
    (PRIMITIVE-COMBINATION-1 . COMBINATION)
    (PRIMITIVE-COMBINATION-2 . COMBINATION)
    (PRIMITIVE-COMBINATION-3 . COMBINATION)
    (SEQUENCE-2 . SEQUENCE) (SEQUENCE-3 . SEQUENCE)
    (INTERN-SYMBOL . SYMBOL)
    (PRIMITIVE . PRIMITIVE-PROCEDURE)))

(set! microcode-type-name
(named-lambda (microcode-type-name type)
  (microcode-table-ref types-slot type)))

(set! microcode-type
(named-lambda (microcode-type name)
  (or (microcode-table-search types-slot name)
      (error "Unknown name" microcode-type name))))

(set! microcode-type-predicate
(named-lambda (microcode-type-predicate name)
  (type-predicate (microcode-type name))))

(define ((type-predicate type) object)
  (primitive-type? type object))

(set! object-type
(named-lambda (object-type object)
  (let ((type (microcode-type-name (primitive-type object))))
    (let ((entry (assq type renamed-user-object-types)))
      (if (not (null? entry))
	  (cdr entry)
	  type)))))

;;;; Microcode Return Codes

(define returns-slot)
(define return-address-type)

(set! microcode-return
(named-lambda (microcode-return name)
  (microcode-table-search returns-slot name)))

(set! make-return-address
(named-lambda (make-return-address code)
  (map-code-to-machine-address return-address-type code)))

(set! return-address?
(named-lambda (return-address? object)
  (primitive-type? return-address-type object)))

(set! return-address-code
(named-lambda (return-address-code return-address)
  (map-machine-address-to-code return-address-type return-address)))

(set! return-address-name
(named-lambda (return-address-name return-address)
  (microcode-table-ref returns-slot (return-address-code return-address))))

;;;; Microcode Error Codes

(define errors-slot)

(set! microcode-error
(named-lambda (microcode-error name)
  (microcode-table-search errors-slot name)))

;;;; Microcode Termination Codes

(define termination-vector-slot)

(set! microcode-termination
(named-lambda (microcode-termination name)
  (microcode-table-search termination-vector-slot name)))

(set! microcode-termination-name
(named-lambda (microcode-termination-name type)
  (code->name termination-vector-slot type)))

(define identification-vector-slot)

(set! microcode-identification-item
  (lambda (name)
    (vector-ref :identification
		(or (microcode-table-search identification-vector-slot name)
		    (error "Unknown identification item" name)))))

;;;; Microcode Primitives

(define primitives-slot)
(define primitive-type-code)
(define external-type-code)

(set! primitive-procedure?
(named-lambda (primitive-procedure? object)
  (or (primitive-type? primitive-type-code object)
      (primitive-type? external-type-code object))))

(set! make-primitive-procedure
(named-lambda (make-primitive-procedure name #!optional force?)
  (let ((code (name->code primitives-slot 'PRIMITIVE name)))
    (if code
	(map-code-to-machine-address primitive-type-code code)
	(or (get-external-number name force?)
	    (error "Unknown name" make-primitive-procedure name))))))

(set! implemented-primitive-procedure?
(named-lambda (implemented-primitive-procedure? object)
  (cond ((primitive-type? primitive-type-code object) true)
	((primitive-type? external-type-code object)
	 (get-external-number (external-code->name (primitive-datum object))
			      false))
	(else
	 (error "Not a primitive procedure" implemented-primitive-procedure?
		object)))))

(set! primitive-procedure-name
(named-lambda (primitive-procedure-name primitive-procedure)
  (cond ((primitive-type? primitive-type-code primitive-procedure)
	 (code->name primitives-slot
		     'PRIMITIVE
		     (map-machine-address-to-code primitive-type-code
						  primitive-procedure)))
	((primitive-type? external-type-code primitive-procedure)
	 (external-code->name (primitive-datum primitive-procedure)))
	(else
	 (error "Not a primitive procedure" primitive-procedure-name
		primitive-procedure)))))

(define (name->code slot type name)
  (or (and (pair? name)
	   (eq? (car name) type)
	   (pair? (cdr name))
	   (let ((x (cdr name)))
	     (and (integer? (car x))
		  (not (negative? (car x)))
		  (null? (cdr x))
		  (car x))))
      (microcode-table-search slot name)))

(define (code->name slot type code)
  (or (and (not (negative? code))
	   (microcode-table-ref slot code))
      (list type code)))

(define (external-code->name code)
  (let ((current-counts (get-external-counts)))
    (cond ((< code (car current-counts)) (get-external-name code))
	  ((< code (+ (car current-counts) (cdr current-counts)))
	   (get-external-name code))	;Maybe should warn about undefined
	  (else
	   (error "Not an external procedure name" external-code->name
		  code)))))

;;;; Initialization

(define microcode-tables-identification)

(define (snarf-version)
  (set! :identification (microcode-identify))

  (set! microcode-tables-identification
	(scode-eval (binary-fasload (microcode-tables-filename))
		    system-global-environment))

  (set! fixed-objects (get-fixed-objects-vector))

  (set! types-slot (fixed-objects-vector-slot 'MICROCODE-TYPES-VECTOR))
  (set! number-of-microcode-types
	(vector-length (vector-ref fixed-objects types-slot)))

  (set! returns-slot (fixed-objects-vector-slot 'MICROCODE-RETURNS-VECTOR))
  (set! return-address-type (microcode-type 'RETURN-ADDRESS))
  (set! number-of-microcode-returns
	(vector-length (vector-ref fixed-objects returns-slot)))

  (set! errors-slot (fixed-objects-vector-slot 'MICROCODE-ERRORS-VECTOR))
  (set! number-of-microcode-errors
	(vector-length (vector-ref fixed-objects errors-slot)))

  (set! primitives-slot
	(fixed-objects-vector-slot 'MICROCODE-PRIMITIVES-VECTOR))
  (set! primitive-type-code (microcode-type 'PRIMITIVE))

  (set! external-type-code (microcode-type 'PRIMITIVE-EXTERNAL))

  (set! termination-vector-slot
	(fixed-objects-vector-slot 'MICROCODE-TERMINATIONS-VECTOR))
  (set! number-of-microcode-terminations
	(vector-length (vector-ref fixed-objects termination-vector-slot)))

  (set! identification-vector-slot
	(fixed-objects-vector-slot 'MICROCODE-IDENTIFICATION-VECTOR))
  (set! :release (microcode-identification-item 'SYSTEM-RELEASE-STRING))
  (set! :version (microcode-identification-item 'MICROCODE-VERSION))
  (set! :modification (microcode-identification-item 'MICROCODE-MODIFICATION))

  ;; Predicate to test if object is a future without touching it.
  (set! future? 
	(let ((primitive (make-primitive-procedure 'FUTURE? true)))
	  (if (implemented-primitive-procedure? primitive)
	      primitive
	      (lambda (object) false)))))

(snarf-version)

;;; end MICROCODE-SYSTEM.
))
))
#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v8/src/runtime/prgcop.scm,v 1.3 1991/09/20 03:58:39 cph Exp $

Copyright (c) 1990-91 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Program copier
;;; package: (runtime program-copier)

(declare (usual-integrations))

(define-primitives
  (object-new-type primitive-object-new-type 2))

(define (initialize-package!)
  (set! copier/scode-walker
	(make-scode-walker
	 copy-constant
	 `((ACCESS ,(%copy-pair (ucode-type ACCESS)))
	   (ASSIGNMENT ,(%copy-triple (ucode-type ASSIGNMENT)))
	   (COMBINATION ,copy-COMBINATION-object)
	   (COMMENT ,copy-COMMENT-object)
	   (CONDITIONAL ,(%copy-triple (ucode-type CONDITIONAL)))
	   (DEFINITION ,(%copy-triple (ucode-type DEFINITION)))
	   (DELAY ,(%copy-pair (ucode-type DELAY)))
	   (DISJUNCTION ,(%copy-pair (ucode-type DISJUNCTION)))
	   (IN-PACKAGE ,(%copy-pair (ucode-type IN-PACKAGE)))
	   (LAMBDA ,copy-LAMBDA-object)
	   (QUOTATION ,(%copy-pair (ucode-type QUOTATION)))
	   (SEQUENCE ,copy-SEQUENCE-object)
	   (THE-ENVIRONMENT ,copy-constant)
	   (VARIABLE ,copy-VARIABLE-object))))
  unspecific)

;;;; Top level

(define *default/copy-constants?* false)

(define *copy-constants?*)

(define *object-copies*)
(define copier/scode-walker)

(define-integrable (make-object-association-table)
  (list '*OBJECT-COPIES*))

(define-integrable (object-association object)
  (assq object (cdr *object-copies*)))

(define (add-association! object other)
  (let* ((table *object-copies*)
	 (place (assq object (cdr table))))
    (cond ((not place)
	   (set-cdr! table (cons (cons object other) (cdr table))))
	  ((not (eq? (cdr place) other))
	   (error "add-association!: Multiple associations" object other)))
    unspecific))

(define (copy-program exp #!optional copy-constants?)
  ;; There should be an option to unlink a linked program.
  ;; This is currently difficult because procedure caches
  ;; do not have enough information to determine what the
  ;; variable name was.  The original block can be used for
  ;; this, but it may as well be copied then.
  (fluid-let ((*copy-constants?*
	       (if (default-object? copy-constants?)
		   *default/copy-constants?*
		   copy-constants?))
	      (*object-copies*
	       (make-object-association-table)))
    (copy-object exp)))

(define (copy-object obj)
  (let ((association (object-association obj)))
    (if association
	(cdr association)
	((scode-walk copier/scode-walker obj) obj))))

(define (copy-constant obj)
  (cond ((compiled-code-address? obj)
	 (%copy-compiled-code-address obj))
	((compiled-code-block? obj)
	 (%copy-compiled-code-block obj))
	((not *copy-constants?*)
	 obj)
	(else
	 (%copy-constant obj))))

(define (%copy-constant obj)
  (cond ((or (number? obj)
	     (symbol? obj)
	     (boolean? obj)
	     (null? obj)
	     (char? obj)
	     (object-type? (ucode-type REFERENCE-TRAP) obj))
	 obj)
	((pair? obj)
	 (%%copy-pair (ucode-type PAIR) obj))
	((vector? obj)
	 (%%copy-vector (ucode-type VECTOR) obj))
	((string? obj)
	 (let ((copy (string-copy obj)))
	   (add-association! obj copy)
	   copy))
	((bit-string? obj)
	 (let ((copy (bit-string-copy obj)))
	   (add-association! obj copy)
	   copy))
	(else
	 ;; This includes procedures and environments.
	 (error "copy-constant: Can't handle" obj))))

(define (%copy-compiled-code-address obj)
  (let ((new-block (copy-compiled-code-block
		    (compiled-code-address->block obj))))
    (atomically
     (lambda ()
       (object-new-type
	(object-type obj)
	(+ (compiled-code-address->offset obj)
	   (object-datum new-block)))))))

(define (copy-compiled-code-block obj)
  (let ((association (object-association obj)))
    (if association
	(cdr association)
	(%copy-compiled-code-block obj))))

(define (%copy-compiled-code-block obj)
  (let* ((new (vector-copy (object-new-type (ucode-type VECTOR) obj)))
	 (typed (object-new-type (ucode-type compiled-code-block) new))
	 (len (vector-length new)))
    (add-association! obj typed)
    (do ((i (fix:+ (object-datum (vector-ref new 0)) 1) (fix:+ 1 i)))    
	((not (fix:< i len)))
      (vector-set! new i (copy-object (vector-ref new i))))
    typed))

(define-integrable (atomically thunk)
  (with-absolutely-no-interrupts thunk))  

(define ((%copy-pair type) obj)
  (%%copy-pair type obj))

(define (%%copy-pair type obj)
  (let ((the-car (system-pair-car obj))
	(the-cdr (system-pair-cdr obj)))
    (let* ((new (cons the-car the-cdr))
	   (typed (object-new-type type new)))
      (add-association! obj typed)
      (set-car! new (copy-object the-car))
      (set-cdr! new (copy-object the-cdr))
      typed)))

(define ((%copy-triple type) obj)
  (%%copy-triple type obj))

(define (%%copy-triple type obj)
  (let ((the-cxr0 (system-hunk3-cxr0 obj))
	(the-cxr1 (system-hunk3-cxr1 obj))
	(the-cxr2 (system-hunk3-cxr2 obj)))
    (let* ((new (hunk3-cons the-cxr0 the-cxr1 the-cxr2))
	   (typed (object-new-type type new)))
      (add-association! obj typed)
      (system-hunk3-set-cxr0! new (copy-object the-cxr0))
      (system-hunk3-set-cxr1! new (copy-object the-cxr1))
      (system-hunk3-set-cxr2! new (copy-object the-cxr2))
      typed)))

#|
(define ((%copy-quad type) obj)
  (%%copy-quad type obj))

(define (%%copy-quad type obj)
  (let ((the-cxr0 (system-hunk4-cxr0 obj))
	(the-cxr1 (system-hunk4-cxr1 obj))
	(the-cxr2 (system-hunk4-cxr2 obj))
	(the-cxr3 (system-hunk4-cxr3 obj)))
    (let* ((new (hunk4-cons the-cxr0 the-cxr1 the-cxr2 the-cxr3))
	   (typed (object-new-type type new)))
      (add-association! obj typed)
      (system-hunk4-set-cxr0! new (copy-object the-cxr0))
      (system-hunk4-set-cxr1! new (copy-object the-cxr1))
      (system-hunk4-set-cxr2! new (copy-object the-cxr2))
      (system-hunk4-set-cxr3! new (copy-object the-cxr3))
      typed)))
|#

(define (copy-vector vec)
  (let ((association (object-association vec)))
    (if association
	(cdr association)
	(%%copy-vector (ucode-type VECTOR) vec))))

(define ((%copy-vector type) obj)
  (%%copy-vector type obj))

(define (%%copy-vector type obj)
  (let* ((new (vector-copy
	       (object-new-type (ucode-type VECTOR) obj)))
	 (typed (object-new-type type new))
	 (len (vector-length new)))
    (add-association! obj typed)
    (do ((i 0 (fix:+ i 1)))
	((not (fix:< i len)))
      (vector-set! new i (copy-object (vector-ref new i))))
    typed))

(define (copy-SEQUENCE-object obj)
  (cond ((object-type? (ucode-type SEQUENCE-2) obj)
	 (%copy-pair (ucode-type SEQUENCE-2) obj))
	((object-type? (ucode-type SEQUENCE-3) obj)
	 (%copy-triple (ucode-type SEQUENCE-3) obj))
	(else
	 (error "copy-SEQUENCE-object: Unknown type" obj))))

(define (copy-COMBINATION-object obj)
  (cond ((object-type? (ucode-type combination) obj)
	 (%%copy-vector (ucode-type combination) obj))
	((object-type? (ucode-type combination-1) obj)
	 (%%copy-pair (ucode-type combination-1) obj))
	((object-type? (ucode-type combination-2) obj)
	 (%%copy-triple (ucode-type combination-2) obj))
	((object-type? (ucode-type primitive-combination-0) obj)
	 obj)					; Non-pointer
	((object-type? (ucode-type primitive-combination-1) obj)
	 (%%copy-pair (ucode-type primitive-combination-1) obj))
	((object-type? (ucode-type primitive-combination-2) obj)
	 (%%copy-triple (ucode-type primitive-combination-2) obj))
	((object-type? (ucode-type primitive-combination-3) obj)
	 (%%copy-vector (ucode-type primitive-combination-3) obj))
	(else
	 (error "copy-COMBINATION-object: Unknown type" obj))))

(define (copy-LAMBDA-object obj)
  (cond ((object-type? (ucode-type lambda) obj)
	 (%%copy-pair (ucode-type lambda) obj))
	((object-type? (ucode-type extended-lambda) obj)
	 (%%copy-triple (ucode-type extended-lambda) obj))
	((object-type? (ucode-type lexpr) obj)
	 (%%copy-pair (ucode-type lexpr) obj))
	(else
	 (error "COPY-LAMBDA-object: Unknown type" obj))))

(define (copy-VARIABLE-object obj)
  (let ((var (make-variable (variable-name obj))))
    (add-association! obj var)
    var))    

(define (copy-COMMENT-object obj)
  (let ((the-text (comment-text obj)))
    (if (not (dbg-info-vector? the-text))
	(%%copy-pair (ucode-type COMMENT) obj)
	(let ((the-car (system-pair-car obj))
	      (the-cdr (system-pair-cdr obj)))
	  (let* ((new (cons the-car the-cdr))
		 (typed (object-new-type (ucode-type COMMENT) new)))
	    (add-association! obj typed)
	    (let ((text-copy (copy-dbg-info-vector the-text)))
	      (set-car! new (if (eq? the-car the-text)
				text-copy
				(copy-object the-car)))
	      (set-cdr! new (if (eq? the-cdr the-text)
				text-copy
				(copy-object the-cdr)))
	      typed))))))

(define (copy-dbg-info-vector obj)
  (let ((association (object-association obj)))
    (cond (association
	   (cdr association))
	  ((vector? obj)
	   (%%copy-vector (ucode-type VECTOR) obj))
	  ((pair? obj)
	   ;; Guarantee that top-level vectors are copied.
	   (for-each (lambda (element)
		       (if (vector? element)
			   (copy-vector element)))
		     obj)
	   (copy-list obj))
	  (else
	   (error "copy-dbg-info-vector: Unknown type" obj)))))

(define (copy-list obj)
  (let ((association (object-association obj)))
    (cond (association
	   (cdr association))
	  ((not (pair? obj))
	   ((scode-walk copier/scode-walker obj) obj))
	  (else
	   (let ((the-car (car obj))
		 (the-cdr (cdr obj)))
	     (let ((new (cons the-car the-cdr)))
	       (add-association! obj new)
	       (set-car! new (copy-object the-car))
	       (set-cdr! new (copy-list the-cdr))
	       new))))))
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

;;;; Program copier
;;; package: (runtime program-copier)

(declare (usual-integrations))

(define-primitives
  (object-new-type primitive-object-new-type 2))

(define (initialize-package!)
  (set! *copy-constants?* (make-unsettable-parameter 'unbound))
  (set! *object-copies* (make-unsettable-parameter 'unbound))
  (set! copier/scode-walker
	(make-scode-walker
	 copy-constant
	 `((access ,(%copy-pair (ucode-type access)))
	   (assignment ,(%copy-triple (ucode-type assignment)))
	   (combination ,copy-combination-object)
	   (comment ,copy-comment-object)
	   (conditional ,(%copy-triple (ucode-type conditional)))
	   (definition ,(%copy-pair (ucode-type definition)))
	   (delay ,(%copy-pair (ucode-type delay)))
	   (disjunction ,(%copy-pair (ucode-type disjunction)))
	   (lambda ,copy-lambda-object)
	   (quotation ,(%copy-pair (ucode-type quotation)))
	   (sequence ,copy-sequence-object)
	   (the-environment ,copy-constant)
	   (variable ,copy-variable-object))))
  unspecific)

;;;; Top level

(define *default/copy-constants?* #f)

(define *copy-constants?*)

(define *object-copies*)
(define copier/scode-walker)

(define-integrable (make-object-association-table)
  (list '*object-copies*))

(define-integrable (object-association object)
  (assq object (cdr (*object-copies*))))

(define (add-association! object other)
  (let* ((table (*object-copies*))
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
  (parameterize ((*copy-constants?*
		  (if (default-object? copy-constants?)
		      *default/copy-constants?*
		      copy-constants?))
		 (*object-copies* (make-object-association-table)))
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
	((not (*copy-constants?*))
	 obj)
	(else
	 (%copy-constant obj))))

(define (%copy-constant obj)
  (cond ((or (number? obj)
	     (symbol? obj)
	     (boolean? obj)
	     (null? obj)
	     (char? obj)
	     (object-type? (ucode-type reference-trap) obj))
	 obj)
	((pair? obj)
	 (%%copy-pair (ucode-type pair) obj))
	((vector? obj)
	 (%%copy-vector (ucode-type vector) obj))
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
    (with-absolutely-no-interrupts
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
  (let* ((new (vector-copy (object-new-type (ucode-type vector) obj)))
	 (typed (object-new-type (ucode-type compiled-code-block) new))
	 (len (vector-length new)))
    ((ucode-primitive declare-compiled-code-block 1) typed)
    (add-association! obj typed)
    (do ((i (fix:+ (object-datum (vector-ref new 0)) 1) (fix:+ 1 i)))
	((not (fix:< i len)))
      (vector-set! new i (copy-object (vector-ref new i))))
    typed))

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
	(%%copy-vector (ucode-type vector) vec))))

(define ((%copy-vector type) obj)
  (%%copy-vector type obj))

(define (%%copy-vector type obj)
  (let* ((new (vector-copy
	       (object-new-type (ucode-type vector) obj)))
	 (typed (object-new-type type new))
	 (len (vector-length new)))
    (add-association! obj typed)
    (do ((i 0 (fix:+ i 1)))
	((not (fix:< i len)))
      (vector-set! new i (copy-object (vector-ref new i))))
    typed))

(define (copy-sequence-object obj)
  (if (object-type? (ucode-type sequence) obj)
      (%%copy-pair (ucode-type sequence) obj)
      (error "copy-sequence-object: Unknown type" obj)))

(define (copy-combination-object obj)
  (make-scode-combination
   (copy-object (scode-combination-operator obj))
   (map copy-object (scode-combination-operands obj))))

(define (copy-lambda-object obj)
  (cond ((object-type? (ucode-type lambda) obj)
	 (%%copy-pair (ucode-type lambda) obj))
	((object-type? (ucode-type extended-lambda) obj)
	 (%%copy-triple (ucode-type extended-lambda) obj))
	((object-type? (ucode-type lexpr) obj)
	 (%%copy-pair (ucode-type lexpr) obj))
	(else
	 (error "copy-lambda-object: Unknown type" obj))))

(define (copy-variable-object obj)
  (let ((var (make-scode-variable (scode-variable-name obj))))
    (add-association! obj var)
    var))

(define (copy-comment-object obj)
  (let ((the-text (scode-comment-text obj)))
    (if (not (dbg-info-vector? the-text))
	(%%copy-pair (ucode-type comment) obj)
	(let ((the-car (system-pair-car obj))
	      (the-cdr (system-pair-cdr obj)))
	  (let* ((new (cons the-car the-cdr))
		 (typed (object-new-type (ucode-type comment) new)))
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
	   (%%copy-vector (ucode-type vector) obj))
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
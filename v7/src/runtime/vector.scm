#| -*-Scheme-*-

$Id: vector.scm,v 14.6 1995/07/27 21:33:27 adams Exp $

Copyright (c) 1988-1995 Massachusetts Institute of Technology

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

;;;; Operations on Vectors
;;; package: ()

(declare (usual-integrations))

(define-primitives
 vector-length vector-ref vector-set!
 list->vector vector subvector->list
 subvector-move-right! subvector-move-left! subvector-fill!)

(define-integrable (vector? object)
  (object-type? (ucode-type vector) object))

(define (make-vector size #!optional fill)
  (guarantee-index/vector size 'make-vector)
  (let ((fill (if (default-object? fill) default-vector-fill-value fill)))
    (%make-vector size fill)))
		

(define-integrable default-vector-fill-value #F)

(define-integrable (%make-vector size fill)
  ((ucode-primitive vector-cons) size fill))

(define (vector->list vector)
  (guarantee-vector vector 'vector->list)
  (subvector->list vector 0 (vector-length vector)))

(define (vector-fill! vector value)
  (guarantee-vector vector 'vector-fill!)
  (subvector-fill! vector 0 (vector-length vector) value))

(define (subvector vector start end)
  ;; VECTOR, START and END checked by `-' and SUBVECTOR-MOVE-RIGHT!
  (let ((result (make-vector (- end start) #F)))
    (subvector-move-right! vector start end result 0)
    result))

(define-integrable (vector-head vector end)
  (subvector vector 0 end))

(define (vector-tail vector start)
  (guarantee-vector vector 'vector-tail)
  (subvector vector start (vector-length vector)))

(define (vector-copy vector)
  (guarantee-vector vector 'vector-copy)
  (let ((length (vector-length vector)))
    (let ((new-vector (%make-vector length #F)))
      (subvector-move-right! vector 0 length new-vector 0)
      new-vector)))

(define (%vector-append vectors)
  (let ((result
	 (%make-vector
	  (let loop ((vectors vectors) (length 0))
	    (if (null? vectors)
		length
		(begin
		  (guarantee-vector (car vectors) 'vector-append)
		  (loop (cdr vectors)
			(fix:+ (vector-length (car vectors)) length)))))
	  #F)))

    (let loop ((vectors vectors) (index 0))
      (if (null? vectors)
	  result
	  (let ((size (vector-length (car vectors))))
	    (subvector-move-right! (car vectors) 0 size result index)
	    (loop (cdr vectors) (fix:+ index size)))))))

(define (vector-append . vectors)
  (%vector-append vectors))

(define (vector-grow vector length)
  (guarantee-vector vector 'vector-grow)
  (let ((new-vector (make-vector length default-vector-fill-value)))
    (subvector-move-right! vector 0 (vector-length vector) new-vector 0)
    new-vector))

(define (make-initialized-vector length initialization)
  ;; LENGTH is checked by MAKE-VECTOR
  (let ((vector (make-vector length #F)))
    (let loop ((index 0))
      (if (fix:< index length)
	  (begin
	    (vector-set! vector index (initialization index))
	    (loop (fix:+ index 1)))))
    vector))

(define (vector-map vector procedure)
  (guarantee-vector vector 'vector-map)
  (let ((length (vector-length vector)))
    (if (fix:zero? length)
	vector
	(let ((result (%make-vector length #F)))
	  (let loop ((index 0))
	    (if (fix:< index length)
		(begin
		  (vector-set! result
			       index
			       (procedure (vector-ref vector index)))
		  (loop (fix:+ index 1)))))
	  result))))

(define (for-each-vector-element vector procedure)
  (guarantee-vector vector 'for-each-vector-element)
  (let ((length (vector-length vector)))
    (let loop ((index 0))
      (if (fix:< index length)
	  (begin
	    (procedure (vector-ref vector index))
	    (loop (fix:+ index 1)))))))

(define (subvector-find-next-element vector start end item)
  (guarantee-vector vector 'subvector-find-next-element)
  (guarantee-index/vector start 'subvector-find-next-element)
  (guarantee-vector-bound end vector 'subvector-find-next-element)
  (let loop ((index start))
    (and (fix:< index end)
	 (if (eqv? (vector-ref vector index) item)
	     index
	     (loop (fix:+ index 1))))))

(define (subvector-find-previous-element vector start end item)
  (guarantee-vector vector 'subvector-find-previous-element)
  (guarantee-index/vector start 'subvector-find-previous-element)
  (guarantee-vector-bound end vector 'subvector-find-previous-element)
  (let loop ((index (fix:- end 1)))
    (and (fix:<= start index)
	 (if (eqv? (vector-ref vector index) item)
	     index
	     (loop (fix:- index 1))))))

(define-integrable (vector-find-next-element vector item)
  (guarantee-vector vector 'vector-find-next-element)
  (subvector-find-next-element vector 0 (vector-length vector) item))

(define-integrable (vector-find-previous-element vector item)
  (guarantee-vector vector 'vector-find-previous-element)
  (subvector-find-previous-element vector 0 (vector-length vector) item))

(define (vector-binary-search vector key<? unwrap-key key)
  (guarantee-vector vector 'vector-binary-search)
  (let loop ((start 0) (end (vector-length vector)))
    (and (fix:< start end)
	 (let ((midpoint (fix:quotient (fix:+ start end) 2)))
	   (let ((item (vector-ref vector midpoint)))
	     (let ((key* (unwrap-key item)))
	       (cond ((key<? key key*) (loop start midpoint))
		     ((key<? key* key) (loop (fix:+ midpoint 1) end))
		     (else item))))))))

(define-integrable (safe-vector-ref vector index)
  (guarantee-vector vector 'safe-vector-ref)
  (guarantee-vector-index index vector 'safe-vector-ref)
  (vector-ref vector index))

(define-integrable (vector-first vector) (safe-vector-ref vector 0))
(define-integrable (vector-second vector) (safe-vector-ref vector 1))
(define-integrable (vector-third vector) (safe-vector-ref vector 2))
(define-integrable (vector-fourth vector) (safe-vector-ref vector 3))
(define-integrable (vector-fifth vector) (safe-vector-ref vector 4))
(define-integrable (vector-sixth vector) (safe-vector-ref vector 5))
(define-integrable (vector-seventh vector) (safe-vector-ref vector 6))
(define-integrable (vector-eighth vector) (safe-vector-ref vector 7))

(define-integrable (guarantee-vector object procedure)
  (if (not (vector? object))
      (error:wrong-type-argument object "vector" procedure)))

(define-integrable (guarantee-index/vector object procedure)
  (if (not (index-fixnum? object))
      (guarantee-index/vector/fail object procedure)))

(define (guarantee-index/vector/fail object procedure)
  (error:wrong-type-argument object "valid vector index"
			     procedure))

(define-integrable (guarantee-vector-index object vector procedure)
  (guarantee-index/vector object procedure)
  (if (not (fix:< object (vector-length vector)))
      (error:bad-range-argument object procedure)))

(define-integrable (guarantee-vector-bound object vector procedure)
  (guarantee-index/vector object procedure)
  (if (not (fix:<= object (vector-length vector)))
      (error:bad-range-argument object procedure)))


#| -*-Scheme-*-

$Id: vector.scm,v 14.7 1997/02/22 07:49:39 cph Exp $

Copyright (c) 1988-97 Massachusetts Institute of Technology

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

(define-integrable (guarantee-vector object procedure)
  (if (not (vector? object))
      (error:wrong-type-argument object "vector" procedure)))

(define-integrable (guarantee-subvector vector start end procedure)
  (guarantee-vector vector procedure)
  (if (not (index-fixnum? start))
      (error:wrong-type-argument start "valid vector index" procedure))
  (if (not (index-fixnum? end))
      (error:wrong-type-argument end "valid vector index" procedure))
  (if (not (fix:<= start end))
      (error:bad-range-argument start procedure))
  (if (not (fix:<= end (vector-length vector)))
      (error:bad-range-argument end procedure)))

(define-integrable (vector? object)
  (object-type? (ucode-type vector) object))

(define (make-vector size #!optional fill)
  (if (not (index-fixnum? size))
      (error:wrong-type-argument size "valid vector index" 'MAKE-VECTOR))
  ((ucode-primitive vector-cons) size (if (default-object? fill) #f fill)))

(define (vector->list vector)
  (guarantee-vector vector 'VECTOR->LIST)
  (subvector->list vector 0 (vector-length vector)))

(define (vector-fill! vector value)
  (guarantee-vector vector 'VECTOR-FILL!)
  (subvector-fill! vector 0 (vector-length vector) value))

(define (subvector vector start end)
  (guarantee-subvector vector start end 'SUBVECTOR)
  (let ((result (make-vector (fix:- end start))))
    (subvector-move-right! vector start end result 0)
    result))

(define-integrable (vector-head vector end)
  (subvector vector 0 end))

(define (vector-tail vector start)
  (guarantee-vector vector 'VECTOR-TAIL)
  (subvector vector start (vector-length vector)))

(define (vector-copy vector)
  (guarantee-vector vector 'VECTOR-COPY)
  (let ((length (vector-length vector)))
    (let ((new-vector (make-vector length)))
      (subvector-move-right! vector 0 length new-vector 0)
      new-vector)))

(define (vector-append . vectors)
  (let ((result
	 (make-vector
	  (let loop ((vectors vectors) (length 0))
	    (if (null? vectors)
		length
		(begin
		  (guarantee-vector (car vectors) 'VECTOR-APPEND)
		  (loop (cdr vectors)
			(fix:+ (vector-length (car vectors)) length))))))))
    (let loop ((vectors vectors) (index 0))
      (if (null? vectors)
	  result
	  (let ((size (vector-length (car vectors))))
	    (subvector-move-right! (car vectors) 0 size result index)
	    (loop (cdr vectors) (fix:+ index size)))))))

(define (vector-grow vector length)
  (guarantee-vector vector 'VECTOR-GROW)
  (let ((new-vector (make-vector length #f)))
    (subvector-move-right! vector 0 (vector-length vector) new-vector 0)
    new-vector))

(define (make-initialized-vector length initialization)
  ;; LENGTH is checked by MAKE-VECTOR
  (let ((vector (make-vector length)))
    (let loop ((index 0))
      (if (fix:< index length)
	  (begin
	    (vector-set! vector index (initialization index))
	    (loop (fix:+ index 1)))))
    vector))

(define (vector-map vector procedure)
  (guarantee-vector vector 'VECTOR-MAP)
  (let ((length (vector-length vector)))
    (if (fix:= 0 length)
	vector
	(let ((result (make-vector length)))
	  (let loop ((index 0))
	    (if (fix:< index length)
		(begin
		  (vector-set! result
			       index
			       (procedure (vector-ref vector index)))
		  (loop (fix:+ index 1)))))
	  result))))

(define (for-each-vector-element vector procedure)
  (guarantee-vector vector 'FOR-EACH-VECTOR-ELEMENT)
  (let ((length (vector-length vector)))
    (let loop ((index 0))
      (if (fix:< index length)
	  (begin
	    (procedure (vector-ref vector index))
	    (loop (fix:+ index 1)))))))

(define (subvector-find-next-element vector start end item)
  (guarantee-subvector vector start end 'SUBVECTOR-FIND-NEXT-ELEMENT)
  (let loop ((index start))
    (and (fix:< index end)
	 (if (eqv? (vector-ref vector index) item)
	     index
	     (loop (fix:+ index 1))))))

(define (subvector-find-next-element-not vector start end item)
  (guarantee-subvector vector start end 'SUBVECTOR-FIND-NEXT-ELEMENT-NOT)
  (let loop ((index start))
    (and (fix:< index end)
	 (if (eqv? (vector-ref vector index) item)
	     (loop (fix:+ index 1))
	     index))))

(define (subvector-find-previous-element vector start end item)
  (guarantee-subvector vector start end 'SUBVECTOR-FIND-PREVIOUS-ELEMENT)
  (let loop ((index (fix:- end 1)))
    (and (fix:<= start index)
	 (if (eqv? (vector-ref vector index) item)
	     index
	     (loop (fix:- index 1))))))

(define (subvector-find-previous-element-not vector start end item)
  (guarantee-subvector vector start end 'SUBVECTOR-FIND-PREVIOUS-ELEMENT-NOT)
  (let loop ((index (fix:- end 1)))
    (and (fix:<= start index)
	 (if (eqv? (vector-ref vector index) item)
	     (loop (fix:- index 1))
	     index))))

(define-integrable (vector-find-next-element vector item)
  (guarantee-vector vector 'VECTOR-FIND-NEXT-ELEMENT)
  (subvector-find-next-element vector 0 (vector-length vector) item))

(define-integrable (vector-find-previous-element vector item)
  (guarantee-vector vector 'VECTOR-FIND-PREVIOUS-ELEMENT)
  (subvector-find-previous-element vector 0 (vector-length vector) item))

(define (vector-binary-search vector key<? unwrap-key key)
  (guarantee-vector vector 'VECTOR-BINARY-SEARCH)
  (let loop ((start 0) (end (vector-length vector)))
    (and (fix:< start end)
	 (let ((midpoint (fix:quotient (fix:+ start end) 2)))
	   (let ((item (vector-ref vector midpoint)))
	     (let ((key* (unwrap-key item)))
	       (cond ((key<? key key*) (loop start midpoint))
		     ((key<? key* key) (loop (fix:+ midpoint 1) end))
		     (else item))))))))

(let-syntax
    ((iref
      (macro (name index)
	`(DEFINE-INTEGRABLE (,name VECTOR)
	   (GUARANTEE-VECTOR VECTOR 'SAFE-VECTOR-REF)
	   (VECTOR-REF VECTOR ,index)))))
  (iref vector-first 0)
  (iref vector-second 1)
  (iref vector-third 2)
  (iref vector-fourth 3)
  (iref vector-fifth 4)
  (iref vector-sixth 5)
  (iref vector-seventh 6)
  (iref vector-eighth 7))

(define (vector-move! v1 v2)
  (guarantee-vector v1 'VECTOR-MOVE!)
  (subvector-move-left! v1 0 (vector-length v1) v2 0))

(define (subvector-filled? vector start end element)
  (guarantee-subvector vector start end 'SUBVECTOR-FILLED?)
  (let loop ((index start))
    (or (fix:= index end)
	(and (eqv? (vector-ref v index) element)
	     (loop (fix:+ index 1))))))

(define (vector-filled? vector element)
  (guarantee-subvector vector 'VECTOR-FILLED?)
  (subvector-filled? vector 0 (vector-length vector) element))

(define (subvector-uniform? vector start end)
  (guarantee-subvector vector start end 'SUBVECTOR-UNIFORM?)
  (if (fix:< start end)
      (subvector-filled? vector (fix:+ start 1) end (vector-ref vector start))
      #t))

(define (vector-uniform? vector)
  (guarantee-subvector vector 'VECTOR-UNIFORM?)
  (subvector-uniform? vector 0 (vector-length vector)))
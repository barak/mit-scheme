#| -*-Scheme-*-

$Id: vector.scm,v 14.20 2002/02/03 03:38:57 cph Exp $

Copyright (c) 1988-2002 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.
|#

;;;; Operations on Vectors
;;; package: (runtime vector)

(declare (usual-integrations))

(define-primitives
  vector? vector-length vector-ref vector-set!
  list->vector vector subvector->list
  subvector-move-right! subvector-move-left! subvector-fill!)

(define-integrable (guarantee-vector object procedure)
  (if (not (vector? object))
      (error:wrong-type-argument object "vector" procedure)))

(define-integrable (guarantee-subvector vector start end procedure)
  (guarantee-vector vector procedure)
  (if (not (index-fixnum? start))
      (error:wrong-type-argument start "vector index" procedure))
  (if (not (index-fixnum? end))
      (error:wrong-type-argument end "vector index" procedure))
  (if (not (fix:<= start end))
      (error:bad-range-argument start procedure))
  (if (not (fix:<= end (vector-length vector)))
      (error:bad-range-argument end procedure)))

(define (make-vector size #!optional fill)
  (if (not (index-fixnum? size))
      (error:wrong-type-argument size "vector index" 'MAKE-VECTOR))
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
	    (if (pair? vectors)
		(begin
		  (guarantee-vector (car vectors) 'VECTOR-APPEND)
		  (loop (cdr vectors)
			(fix:+ (vector-length (car vectors)) length)))
		length)))))
    (let loop ((vectors vectors) (index 0))
      (if (pair? vectors)
	  (let ((size (vector-length (car vectors))))
	    (subvector-move-right! (car vectors) 0 size result index)
	    (loop (cdr vectors) (fix:+ index size)))
	  result))))

(define (vector-grow vector length #!optional value)
  (guarantee-vector vector 'VECTOR-GROW)
  (if (not (index-fixnum? length))
      (error:wrong-type-argument length "vector length" 'VECTOR-GROW))
  (if (fix:< length (vector-length vector))
      (error:bad-range-argument length 'VECTOR-GROW))
  (let ((vector* (make-vector length (if (default-object? value) #f value))))
    (subvector-move-right! vector 0 (vector-length vector) vector* 0)
    vector*))

(define (make-initialized-vector length initialization)
  ;; LENGTH is checked by MAKE-VECTOR
  (let ((vector (make-vector length)))
    (let loop ((index 0))
      (if (fix:< index length)
	  (begin
	    (vector-set! vector index (initialization index))
	    (loop (fix:+ index 1)))))
    vector))

(define (vector-map procedure vector)
  (if (vector? procedure)
      ;; KLUDGE: accept arguments in old order.
      (vector-map vector procedure)
      (begin
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
		result))))))

(define (for-each-vector-element vector procedure)
  (guarantee-vector vector 'FOR-EACH-VECTOR-ELEMENT)
  (let ((length (vector-length vector)))
    (let loop ((index 0))
      (if (fix:< index length)
	  (begin
	    (procedure (vector-ref vector index))
	    (loop (fix:+ index 1)))))))

(define (vector-of-type? vector predicate)
  (and (vector? vector)
       (let ((n (vector-length vector)))
	 (let loop ((i 0))
	   (or (fix:= i n)
	       (and (predicate (vector-ref vector i))
		    (loop (fix:+ i 1))))))))

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
      (sc-macro-transformer
       (lambda (form environment)
	 `(DEFINE-INTEGRABLE (,(close-syntax (cadr form) environment) VECTOR)
	    (GUARANTEE-VECTOR VECTOR 'SAFE-VECTOR-REF)
	    (VECTOR-REF VECTOR ,(caddr form)))))))
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
	(and (eqv? (vector-ref vector index) element)
	     (loop (fix:+ index 1))))))

(define (vector-filled? vector element)
  (guarantee-vector vector 'VECTOR-FILLED?)
  (subvector-filled? vector 0 (vector-length vector) element))

(define (subvector-uniform? vector start end)
  (guarantee-subvector vector start end 'SUBVECTOR-UNIFORM?)
  (if (fix:< start end)
      (subvector-filled? vector (fix:+ start 1) end (vector-ref vector start))
      #t))

(define (vector-uniform? vector)
  (guarantee-vector vector 'VECTOR-UNIFORM?)
  (subvector-uniform? vector 0 (vector-length vector)))
#| -*-Scheme-*-

$Id: vector.scm,v 14.29 2008/02/10 06:14:19 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; Operations on Vectors
;;; package: (runtime vector)

(declare (usual-integrations))

(define-integrable vector
  (ucode-primitive vector))

(define-integrable (vector? object)
  ((ucode-primitive vector?) object))

(define-integrable (vector-length v)
  ((ucode-primitive vector-length) v))

(define-integrable (vector-ref v i)
  ((ucode-primitive vector-ref) v i))

(define-integrable (vector-set! v i x)
  ((ucode-primitive vector-set!) v i x))

(define-integrable (list->vector list)
  ((ucode-primitive list->vector) list))

(define-integrable (subvector->list v s e)
  ((ucode-primitive subvector->list) v s e))

(define-integrable (subvector-fill! v s e x)
  ((ucode-primitive subvector-fill!) v s e x))

(define-integrable (subvector-move-left! v1 s1 e1 v2 s2)
  ((ucode-primitive subvector-move-left!) v1 s1 e1 v2 s2))

(define-integrable (subvector-move-right! v1 s1 e1 v2 s2)
  ((ucode-primitive subvector-move-right!) v1 s1 e1 v2 s2))

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

(define (vector-map procedure vector . vectors)
  (guarantee-vector vector 'VECTOR-MAP)
  (for-each (lambda (v) (guarantee-vector v 'VECTOR-MAP)) vectors)
  (let ((n (vector-length vector)))
    (for-each (lambda (v)
		(if (not (fix:= (vector-length v) n))
		    (error:bad-range-argument v 'VECTOR-MAP)))
	      vectors)
    (let ((result (make-vector n)))
      (do ((i 0 (fix:+ i 1)))
	  ((not (fix:< i n)))
	(vector-set! result
		     i
		     (apply procedure
			    (vector-ref vector i)
			    (map (lambda (v) (vector-ref v i)) vectors))))
      result)))

(define (vector-for-each procedure vector . vectors)
  (guarantee-vector vector 'VECTOR-FOR-EACH)
  (for-each (lambda (v) (guarantee-vector v 'VECTOR-FOR-EACH)) vectors)
  (let ((n (vector-length vector)))
    (for-each (lambda (v)
		(if (not (fix:= (vector-length v) n))
		    (error:bad-range-argument v 'VECTOR-FOR-EACH)))
	      vectors)
    (do ((i 0 (fix:+ i 1)))
	((not (fix:< i n)) unspecific)
      (apply procedure
	     (vector-ref vector i)
	     (map (lambda (v) (vector-ref v i)) vectors)))))

(define (for-each-vector-element vector procedure)
  (vector-for-each procedure vector))

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
	 `(DEFINE-INTEGRABLE (,(cadr form) VECTOR)
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

(define (vector-of-type? object predicate)
  (and (vector? object)
       (let ((n (vector-length object)))
	 (let loop ((i 0))
	   (if (fix:< i n)
	       (and (predicate (vector-ref object i))
		    (loop (fix:+ i 1)))
	       #t)))))

(define (guarantee-vector-of-type object predicate description caller)
  (if (not (vector-of-type? object predicate))
      (error:wrong-type-argument object description caller)))

(define (vector-of-unique-symbols? object)
  (and (vector? object)
       (let ((n (vector-length object)))
	 (let loop ((i 0))
	   (if (fix:< i n)
	       (let ((elt (vector-ref object i)))
		 (and (symbol? elt)
		      (let find-dup ((i (fix:+ i 1)))
			(if (fix:< i n)
			    (and (not (eq? (vector-ref object i) elt))
				 (find-dup (fix:+ i 1)))
			    #t))
		      (loop (fix:+ i 1))))
	       #t)))))

(define-guarantee vector-of-unique-symbols "vector of unique symbols")
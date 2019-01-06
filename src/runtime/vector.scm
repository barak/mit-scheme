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

;;;; Operations on Vectors
;;; package: (runtime vector)

(declare (usual-integrations))

(define-primitives
  (list->vector 1)
  (primitive-make-object 2)
  (primitive-object-set! 3)
  (subvector->list 3)
  (subvector-fill! 4)
  (subvector-move-left! 5)
  (subvector-move-right! 5)
  (vector -1)
  (vector-length 1)
  (vector-ref 2)
  (vector-set! 3)
  (vector? 1))

(define-integrable (guarantee-subvector vector start end procedure)
  (guarantee vector? vector procedure)
  (if (not (index-fixnum? start))
      (error:wrong-type-argument start "vector index" procedure))
  (if (not (index-fixnum? end))
      (error:wrong-type-argument end "vector index" procedure))
  (if (not (fix:<= start end))
      (error:bad-range-argument start procedure))
  (if (not (fix:<= end (vector-length vector)))
      (error:bad-range-argument end procedure)))

(define (make-vector length #!optional fill)
  (vector-cons length (if (default-object? fill) #f fill)))

(define (vector-cons length fill)
  (let-syntax
      ((expand-cases
	(sc-macro-transformer
	 (lambda (form use-env)
	   (declare (ignore use-env))
	   (let ((limit (cadr form))	;must be a power of 2
		 (gen-accessor
		  (lambda (i)
		    `(vector ,@(make-list i 'fill)))))
	     `(if (and (index-fixnum? length)
		       (fix:< length ,limit))
		  ,(let loop ((low 0) (high limit))
		     (if (> (- high low) 1)
			 (let ((mid (quotient (+ high low) 2)))
			   `(if (fix:< length ,mid)
				,(loop low mid)
				,(loop mid high)))
			 (gen-accessor low)))
		  ((ucode-primitive vector-cons) length fill)))))))
    (expand-cases 16)))

(define (vector-builder #!optional buffer-length)
  (make-sequence-builder any-object? vector? make-vector vector-length
			 vector-set! vector-copy!
    (if (default-object? buffer-length)
	16
	(begin
	  (guarantee positive-fixnum? buffer-length 'vector-builder)
	  buffer-length))))

(define (vector->list vector #!optional start end)
  (subvector->list vector
		   (if (default-object? start) 0 start)
		   (if (default-object? end) (vector-length vector) end)))

(define (vector-fill! vector value #!optional start end)
  (subvector-fill! vector
		   (if (default-object? start) 0 start)
		   (if (default-object? end) (vector-length vector) end)
		   value))

(define (subvector vector start end)
  (guarantee-subvector vector start end 'subvector)
  (let ((result (make-vector (fix:- end start))))
    (subvector-move-right! vector start end result 0)
    result))

(define-integrable (vector-head vector end)
  (subvector vector 0 end))

(define (vector-head! vector end)
  (guarantee-subvector vector 0 end 'vector-head!)
  (if (fix:< end (vector-length vector))
      (primitive-object-set! vector 0
			     (primitive-make-object (ucode-type false)
						    end)))
  vector)

(define (vector-tail vector start)
  (guarantee vector? vector 'vector-tail)
  (subvector vector start (vector-length vector)))

(define (vector-copy vector #!optional start end)
  (let ((start (if (default-object? start) 0 start))
	(end (if (default-object? end) (vector-length vector) end)))
    (guarantee-subvector vector start end 'vector-copy)
    (let ((result (make-vector (fix:- end start))))
      (subvector-move-right! vector start end result 0)
      result)))

(define (vector-append . vectors)
  (let ((result
	 (make-vector
	  (let loop ((vectors vectors) (length 0))
	    (if (pair? vectors)
		(begin
		  (guarantee vector? (car vectors) 'vector-append)
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
  (guarantee vector? vector 'vector-grow)
  (if (not (index-fixnum? length))
      (error:wrong-type-argument length "vector length" 'vector-grow))
  (if (fix:< length (vector-length vector))
      (error:bad-range-argument length 'vector-grow))
  (let ((vector* (make-vector length value)))
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
  (guarantee vector? vector 'vector-map)
  (for-each (lambda (v) (guarantee vector? v 'vector-map)) vectors)
  (let ((n (vector-length vector)))
    (for-each (lambda (v)
		(if (not (fix:= (vector-length v) n))
		    (error:bad-range-argument v 'vector-map)))
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
  (guarantee vector? vector 'vector-for-each)
  (for-each (lambda (v) (guarantee vector? v 'vector-for-each)) vectors)
  (let ((n (vector-length vector)))
    (for-each (lambda (v)
		(if (not (fix:= (vector-length v) n))
		    (error:bad-range-argument v 'vector-for-each)))
	      vectors)
    (do ((i 0 (fix:+ i 1)))
	((not (fix:< i n)) unspecific)
      (apply procedure
	     (vector-ref vector i)
	     (map (lambda (v) (vector-ref v i)) vectors)))))

(define (for-each-vector-element vector procedure)
  (vector-for-each procedure vector))

(define (vector-any procedure vector . vectors)
  (let ((n (vector-length vector)))
    (if (pair? vectors)
	(let ((n
	       (fold-left (lambda (n v)
			    (fix:min (vector-length v) n))
			  n
			  vectors)))
	  (let loop ((i 0))
	    (if (fix:< i n)
		(or (apply procedure
			   (vector-ref vector i)
			   (map (lambda (vector*)
				  (vector-ref vector* i))
				vectors))
		    (loop (fix:+ i 1)))
		#f)))
	(let loop ((i 0))
	  (if (fix:< i n)
	      (or (procedure (vector-ref vector i))
		  (loop (fix:+ i 1)))
	      #f)))))

(define (vector-every procedure vector . vectors)
  (let ((n (vector-length vector)))
    (if (pair? vectors)
	(let ((n
	       (fold-left (lambda (n v)
			    (fix:min (vector-length v) n))
			  n
			  vectors)))
	  (let loop ((i 0))
	    (if (fix:< i n)
		(and (apply procedure
			    (vector-ref vector i)
			    (map (lambda (vector*)
				   (vector-ref vector* i))
				 vectors))
		     (loop (fix:+ i 1)))
		#t)))
	(let loop ((i 0))
	  (if (fix:< i n)
	      (and (procedure (vector-ref vector i))
		   (loop (fix:+ i 1)))
	      #t)))))

(define (subvector-find-next-element vector start end item)
  (guarantee-subvector vector start end 'subvector-find-next-element)
  (let loop ((index start))
    (and (fix:< index end)
	 (if (eqv? (vector-ref vector index) item)
	     index
	     (loop (fix:+ index 1))))))

(define (subvector-find-next-element-not vector start end item)
  (guarantee-subvector vector start end 'subvector-find-next-element-not)
  (let loop ((index start))
    (and (fix:< index end)
	 (if (eqv? (vector-ref vector index) item)
	     (loop (fix:+ index 1))
	     index))))

(define (subvector-find-previous-element vector start end item)
  (guarantee-subvector vector start end 'subvector-find-previous-element)
  (let loop ((index (fix:- end 1)))
    (and (fix:<= start index)
	 (if (eqv? (vector-ref vector index) item)
	     index
	     (loop (fix:- index 1))))))

(define (subvector-find-previous-element-not vector start end item)
  (guarantee-subvector vector start end 'subvector-find-previous-element-not)
  (let loop ((index (fix:- end 1)))
    (and (fix:<= start index)
	 (if (eqv? (vector-ref vector index) item)
	     (loop (fix:- index 1))
	     index))))

(define-integrable (vector-find-next-element vector item)
  (guarantee vector? vector 'vector-find-next-element)
  (subvector-find-next-element vector 0 (vector-length vector) item))

(define-integrable (vector-find-previous-element vector item)
  (guarantee vector? vector 'vector-find-previous-element)
  (subvector-find-previous-element vector 0 (vector-length vector) item))

(define (vector-binary-search vector key<? unwrap-key key)
  (guarantee vector? vector 'vector-binary-search)
  (let loop ((start 0) (end (vector-length vector)))
    (and (fix:< start end)
	 (let ((midpoint (fix:quotient (fix:+ start end) 2)))
	   (let ((item (vector-ref vector midpoint)))
	     (let ((key* (unwrap-key item)))
	       (cond ((key<? key key*) (loop start midpoint))
		     ((key<? key* key) (loop (fix:+ midpoint 1) end))
		     (else item))))))))

(define-integrable (vector-first vector) (vector-ref vector 0))
(define-integrable (vector-second vector) (vector-ref vector 1))
(define-integrable (vector-third vector) (vector-ref vector 2))
(define-integrable (vector-fourth vector) (vector-ref vector 3))
(define-integrable (vector-fifth vector) (vector-ref vector 4))
(define-integrable (vector-sixth vector) (vector-ref vector 5))
(define-integrable (vector-seventh vector) (vector-ref vector 6))
(define-integrable (vector-eighth vector) (vector-ref vector 7))

(define (vector-move! v1 v2)
  (vector-copy! v2 0 v1))

(define (vector-copy! to at from #!optional start end)
  (let ((start (if (default-object? start) 0 start))
	(end (if (default-object? end) (vector-length from) end)))
    (cond ((or (not (eq? to from)) (fix:< to start))
	   (subvector-move-left! from start end to at))
	  ((fix:> to start)
	   (subvector-move-right! from start end to at)))))

(define (subvector-filled? vector start end element)
  (guarantee-subvector vector start end 'subvector-filled?)
  (let loop ((index start))
    (or (fix:= index end)
	(and (eqv? (vector-ref vector index) element)
	     (loop (fix:+ index 1))))))

(define (vector-filled? vector element)
  (guarantee vector? vector 'vector-filled?)
  (subvector-filled? vector 0 (vector-length vector) element))

(define (subvector-uniform? vector start end)
  (guarantee-subvector vector start end 'subvector-uniform?)
  (if (fix:< start end)
      (subvector-filled? vector (fix:+ start 1) end (vector-ref vector start))
      #t))

(define (vector-uniform? vector)
  (guarantee vector? vector 'vector-uniform?)
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
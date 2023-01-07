#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

;;;; Generators (compatible with SRFI 158)
;;; package: (runtime generator)

(declare (usual-integrations))

;;;; Generator Constructors

(define (generator . args)
  (lambda ()
    (if (pair? args)
        (let ((next (car args)))
          (set! args (cdr args))
          next)
        (eof-object))))

(define (circular-generator . args)
  (let ((base-args args))
    (lambda ()
      (when (null? args)
        (set! args base-args))
      (let ((next (car args)))
        (set! args (cdr args))
        next))))

(define (make-iota-generator count #!optional start step)
  (guarantee exact-nonnegative-integer? count 'make-iota-generator)
  (let ((start (if (default-object? start) 0 start))
	(step (if (default-object? step) 1 step)))
    (if (and (exact? start) (inexact? step))
	(set! start (inexact start)))
    (lambda ()
      (if (> count 0)
	  (let ((result start))
	   (set! count (- count 1))
	   (set! start (+ start step))
	   result)
	  (eof-object)))))

(define (make-range-generator start #!optional end step)
  (let ((step (if (default-object? step) 1 step)))
    (if (default-object? end)
	(lambda ()
	  (let ((result start))
	   (set! start (+ start step))
	   result))
	(begin
	  (if (and (exact? start) (inexact? step))
	      (set! start (inexact start)))
	  (lambda ()
	    (if (< start end)
		(let ((v start))
		  (set! start (+ start step))
		  v)
		(eof-object)))))))

(define (list->generator items)
  (lambda ()
    (if (null-list? items 'list->generator)
	(eof-object)
	(let ((item (car items)))
	  (set! items (cdr items))
	  item))))

(define (vector->generator v #!optional start end)
  (let* ((end (fix:end-index end (vector-length v) 'vector->generator))
	 (start (fix:start-index start end 'vector->generator))
	 (index start))
    (lambda ()
      (if (fix:< index end)
	  (let ((next (vector-ref v index)))
	    (set! index (fix:+ index 1))
	    next)
	  (eof-object)))))

(define (reverse-vector->generator v #!optional start end)
  (let* ((end (fix:end-index end (vector-length v) 'reverse-vector->generator))
	 (start (fix:start-index start end 'reverse-vector->generator))
	 (index (fix:- end 1)))
    (lambda ()
      (if (fix:>= index start)
	  (let ((next (vector-ref v index)))
	    (set! index (fix:- index 1))
	    next)
	  (eof-object)))))

(define (string->generator s #!optional start end)
  (let* ((end (fix:end-index end (string-length s) 'string->generator))
	 (start (fix:start-index start end 'string->generator))
	 (index start))
    (lambda ()
      (if (fix:< index end)
	  (let ((next (string-ref s index)))
	    (set! index (fix:+ index 1))
	    next)
	  (eof-object)))))

(define (bytevector->generator bv #!optional start end)
  (let* ((end (fix:end-index end (bytevector-length bv) 'bytevector->generator))
	 (start (fix:start-index start end 'bytevector->generator))
	 (index start))
    (lambda ()
      (if (fix:< index end)
	  (let ((next (bytevector-u8-ref bv index)))
	    (set! index (fix:+ index 1))
	    next)
	  (eof-object)))))

(define (make-coroutine-generator proc)
  (let ((return #f)
	(resume #f))

    (define (yield v)
      (call/cc
	(lambda (k)
	  (set! resume k)
	  (return v))))

    (lambda ()
      (call/cc
	(lambda (cc)
	  (set! return cc)
	  (if resume
	      (resume unspecific)
	      (begin
		(proc yield)
		(set! resume
		      (lambda (v)
			(declare (ignore v))
			(return (eof-object))))
		(return (eof-object)))))))))

(define (make-for-each-generator for-each object)
  (make-coroutine-generator
   (lambda (yield)
     (for-each yield object))))

(define (make-unfold-generator stop? mapper successor seed)
  (make-coroutine-generator
   (lambda (yield)
     (let loop ((s seed))
       (if (not (stop? s))
           (begin
	     (yield (mapper s))
             (loop (successor s))))))))

;;;; Generator Operations

(define (gcons* . args)
  (if (pair? args)
      (let ((first (car args))
	    (rest (cdr args)))
	(lambda ()
	  (if (pair? rest)
	      (let ((item first))
		(set! first (car rest))
		(set! rest (cdr rest))
		item)
	      (first))))
      eof-object))

(define (gappend . generators)
  (cond ((null? generators)
	 eof-object)
	((null? (cdr generators))
	 (car generators))
	(else
	 (let ((first (car generators))
	       (rest (cdr generators)))
	   (define (gappend-generator)
	     (if first
		 (let loop ()
		   (let ((next (first)))
		     (cond ((not (eof-object? next))
			    next)
			   ((pair? rest)
			    (set! first (car rest))
			    (set! rest (cdr rest))
			    (loop))
			   (else
			    (set! first #f)
			    (eof-object)))))
		 (eof-object)))
	   gappend-generator))))

(define (gflatten generator)
  (let ((state '()))
    (define (gflatten-generator)
      (cond ((eof-object? state)
	     state)
	    ((null-list? state)
	     (set! state (generator))
	     (gflatten-generator))
	    (else
	     (let ((next (car state)))
	       (set! state (cdr state))
	       next))))
    gflatten-generator))

(define (ggroup gen k #!optional padding)
  (guarantee exact-nonnegative-integer? k 'ggroup)
  (named-lambda (ggroup-generator)
    (let loop ((i 0) (result '()))
      (if (< i k)
	  (let ((item (gen)))
	    (if (eof-object? item)
		(if (> i 0)
		    (let ((result (reverse result)))
		      (if (default-object? padding)
			  result
			  (append result (make-list (- k i) padding))))
		    item)
		(loop (+ i 1) (cons item result))))
	  (reverse result)))))

(define (gmerge < gen . gens)

  (define (restart gens)
    (cond ((null? (cdr gens)) (car gens))
	  ((null? (cddr gens)) (case-2 (car gens) (cadr gens)))
	  (else (case-n gens))))


  (define (case-2 gen-left gen-right)
    (let ((left (gen-left))
	  (right (gen-right)))
      (lambda ()
	(cond ((and (eof-object? left)
		    (eof-object? right))
	       left)
	      ((eof-object? left)
	       (let ((obj right))
		 (set! right (gen-right))
		 obj))
	      ((eof-object? right)
	       (let ((obj left))
		 (set! left (gen-left))
		 obj))
	      ((< right left)
	       (let ((obj right))
		 (set! right (gen-right))
		 obj))
	      (else
	       (let ((obj left))
		 (set! left (gen-left))
		 obj))))))

  (define (case-n gens)
    (restart
     (let loop ((gens gens) (paired '()))
       (cond ((null? gens)
	      (reverse paired))
	     ((null? (cdr gens))
	      (reverse (cons (car gens) paired)))
	     (else
	      (loop (cddr gens)
		    (cons (case-2 (car gens) (cadr gens))
			  paired)))))))

  (restart (cons gen gens)))

(define (gmap procedure gen . gens)
  (cond ((null? gens)
	 (named-lambda (gmap-generator-1)
	   (let ((v (gen)))
	     (if (eof-object? v)
		 v
		 (procedure v)))))
	((null? (cdr gens))
	 (let ((gen2 (car gens)))
	   (named-lambda (gmap-generator-2)
	     (let ((v1 (gen))
		   (v2 (gen2)))
	       (if (or (eof-object? v1) (eof-object? v2))
		   (eof-object)
		   (procedure v1 v2))))))
	(else
	 (let ((gens (cons gen gens)))
	   (named-lambda (gmap-generator-n)
	     (let ((vs (map (lambda (gen) (gen)) gens)))
	       (if (any eof-object? vs)
		   (eof-object)
		   (apply procedure vs))))))))

(define (gcombine procedure seed gen . gens)
  (define (gcombine-generator)
    (let ((items (map (lambda (x) (x)) (cons gen gens))))
      (if (any eof-object? items)
	  (eof-object)
	  (let-values (((value newseed)
			(apply procedure (append items (list seed)))))
	    (set! seed newseed)
	    value))))
  gcombine-generator)

(define (gfilter predicate gen)
  (define (gfilter-generator)
    (let ((v (gen)))
      (if (or (eof-object? v)
	      (predicate v))
	  v
	  (gfilter-generator))))
  gfilter-generator)

(define (gremove predicate gen)
  (define (gremove-generator)
    (let ((v (gen)))
      (if (or (eof-object? v)
	      (not (predicate v)))
	  v
	  (gremove-generator))))
  gremove-generator)

(define (gstate-filter procedure seed gen)
  (let ((state seed))
    (define (gstate-filter-generator)
      (let ((item (gen)))
        (if (eof-object? item)
            item
            (let-values (((yes newstate) (procedure item state)))
              (set! state newstate)
              (if yes
		  item
		  (gstate-filter-generator))))))
    gstate-filter-generator))

(define (gtake gen k #!optional padding)
  (guarantee exact-nonnegative-integer? k 'gtake)
  (if (default-object? padding)
      (named-lambda (gtake-generator)
	(if (> k 0)
	    (begin
	      (set! k (- k 1))
	      (gen))
	    (eof-object)))
      (named-lambda (gtake-with-padding-generator)
	(if (> k 0)
	    (begin
	      (set! k (- k 1))
	      (let ((v (gen)))
		(if (eof-object? v)
		    padding
		    v)))
	    (eof-object)))))

(define (gdrop gen k)
  (guarantee exact-nonnegative-integer? k 'gdrop)
  (define (gdrop-generator)
    (if (> k 0)
	(begin
	  (set! k (- k 1))
	  (gen)
	  (gdrop-generator))
	(gen)))
  gdrop-generator)

(define (gtake-while predicate gen)
  (let ((found #f))
    (define (gtake-while-generator)
      (if found
	  (eof-object)
	  (let ((v (gen)))
	    (if (or (eof-object? v)
		    (not (predicate v)))
		(begin
		  (set! found #t)
		  (eof-object))
		v))))
    gtake-while-generator))

(define (gdrop-while predicate gen)
  (let ((found #f))
    (define (gdrop-while-generator)
      (if found
	  (gen)
	  (let loop ()
	    (let ((v (gen)))
	      (if (or (eof-object? v)
		      (not (predicate v)))
		  (begin
		    (set! found #t)
		    v)
		  (loop))))))
    gdrop-while-generator))

(define (gdelete item gen #!optional =)
  (gremove (let ((= (if (default-object? =) equal? =)))
	     (lambda (val)
	       (= item val)))
	   gen))

(define (gdelete-neighbor-dups gen #!optional =)
  (let ((first-time? #t)
	(prev #f)
	(= (if (default-object? =) equal? =)))
    (define (gdelete-neighbor-dups-generator)
      (if first-time?
	  (begin
	    (set! first-time? #f)
	    (set! prev (gen))
	    prev)
	  (let loop ((v (gen)))
	    (cond ((eof-object? v)
		   v)
		  ((= prev v)
		   (loop (gen)))
		  (else
		   (set! prev v)
		   v)))))
    gdelete-neighbor-dups-generator))

(define (gindex value-gen index-gen)
  (let ((done? #f)
	(count 0))
    (define (gindex-generator)
      (if done?
	  (eof-object)
	  (let loop
	      ((value (value-gen))
	       (index (index-gen)))
            (cond ((or (eof-object? value) (eof-object? index))
		   (set! done? #t)
		   (eof-object))
		  ((= index count)
		   (set! count (+ count 1))
		   value)
		  (else
		   (set! count (+ count 1))
		   (loop (value-gen) index))))))
    gindex-generator))

(define (gselect value-gen truth-gen)
  (let ((done? #f))
    (define (gselect-generator)
      (if done?
	  (eof-object)
	  (let loop
	      ((value (value-gen))
	       (truth (truth-gen)))
            (cond ((or (eof-object? value) (eof-object? truth))
		   (set! done? #t)
		   (eof-object))
		  (truth value)
		  (else (loop (value-gen) (truth-gen)))))))
    gselect-generator))

(define (gpeeker gen)
  (let ((next #f))

    (define (object-ready?)
      (if (not next)
	  (set! next (gen)))
      (not (eof-object? next)))

    ;; Assumes that (object-ready?) is #t.
    (define (peek-object)
      next)

    ;; Assumes that (object-ready?) is #t.
    (define (read-object)
      (let ((object next))
        (set! next #f)
        object))

    (values object-ready? peek-object read-object)))

;;;; Consuming Generator Values

(define (generator->list gen #!optional n)
  (generator-fold-right cons '()
			(if (default-object? n)
			    gen
			    (gtake gen n))))

(define (generator->reverse-list gen #!optional n)
  (generator-fold cons '()
		  (if (default-object? n)
		      gen
		      (gtake gen n))))

(define (generator->vector gen #!optional n)
  (list->vector (generator->list gen n)))

(define (generator->vector! vector at gen)
  (let* ((end (vector-length vector))
	 (start (fix:start-index at end 'generator->vector!))
	 (n (fix:- end start)))
      (let loop ((value (gen)) (i 0))
	(if (or (eof-object? value)
		(not (fix:< i n)))
	    i
	    (begin
	      (vector-set! vector (fix:+ start i) value)
	      (loop (gen) (fix:+ i 1)))))))

(define (generator->string gen #!optional n)
  (list->string (generator->list gen n)))

(define (generator-fold kons knil gen . gens)
  (cond ((null? gens)
	 (let loop ((acc knil))
	   (let ((v (gen)))
	     (if (eof-object? v)
		 acc
		 (loop (kons v acc))))))
	((null? (cdr gens))
	 (let ((gen2 (car gens)))
	   (let loop ((acc knil))
	     (let ((v1 (gen))
		   (v2 (gen2)))
	       (if (or (eof-object? v1) (eof-object? v2))
		   acc
		   (loop (kons v1 v2 acc)))))))
	(else
	 (let ((gens (cons gen gens)))
	   (let loop ((acc knil))
	     (let ((vs (map (lambda (gen) (gen)) gens)))
	       (if (any eof-object? vs)
		   acc
		   (loop (apply kons (append vs (list acc)))))))))))

(define (generator-fold-map kons knil procedure gen . gens)
  (cond ((null? gens)
	 (let loop ((acc knil))
	   (let ((v (gen)))
	     (if (eof-object? v)
		 acc
		 (loop (kons (procedure v) acc))))))
	((null? (cdr gens))
	 (let ((gen2 (car gens)))
	   (let loop ((acc knil))
	     (let ((v1 (gen))
		   (v2 (gen2)))
	       (if (or (eof-object? v1) (eof-object? v2))
		   acc
		   (loop (kons (procedure v1 v2) acc)))))))
	(else
	 (let ((gens (cons gen gens)))
	   (let loop ((acc knil))
	     (let ((vs (map (lambda (gen) (gen)) gens)))
	       (if (any eof-object? vs)
		   acc
		   (loop (kons (apply procedure vs) acc)))))))))

(define (generator-fold-right kons knil gen . gens)
  (cond ((null? gens)
	 (let loop ()
	   (let ((v (gen)))
	     (if (eof-object? v)
		 knil
		 (kons v (loop))))))
	((null? (cdr gens))
	 (let ((gen2 (car gens)))
	   (let loop ()
	     (let ((v1 (gen))
		   (v2 (gen2)))
	       (if (or (eof-object? v1) (eof-object? v2))
		   knil
		   (kons v1 v2 (loop)))))))
	(else
	 (let ((gens (cons gen gens)))
	   (let loop ()
	     (let ((vs (map (lambda (gen) (gen)) gens)))
	       (if (any eof-object? vs)
		   knil
		   (apply kons (append vs (list (loop)))))))))))

(define (generator-fold-right-map kons knil procedure gen . gens)
  (cond ((null? gens)
	 (let loop ()
	   (let ((v (gen)))
	     (if (eof-object? v)
		 knil
		 (kons (procedure v) (loop))))))
	((null? (cdr gens))
	 (let ((gen2 (car gens)))
	   (let loop ()
	     (let ((v1 (gen))
		   (v2 (gen2)))
	       (if (or (eof-object? v1) (eof-object? v2))
		   knil
		   (kons (procedure v1 v2) (loop)))))))
	(else
	 (let ((gens (cons gen gens)))
	   (let loop ()
	     (let ((vs (map (lambda (gen) (gen)) gens)))
	       (if (any eof-object? vs)
		   knil
		   (kons (apply procedure vs) (loop)))))))))

(define (generator-for-each procedure gen . gens)
  (cond ((null? gens)
	 (let loop ()
	   (let ((v (gen)))
	     (if (not (eof-object? v))
		 (begin
		   (procedure v)
		   (loop))))))
	((null? (cdr gens))
	 (let ((gen2 (car gens)))
	   (let loop ()
	     (let ((v1 (gen))
		   (v2 (gen2)))
	       (if (not (or (eof-object? v1) (eof-object? v2)))
		   (begin
		     (procedure v1 v2)
		     (loop)))))))
	(else
	 (let ((gens (cons gen gens)))
	   (let loop ()
	     (let ((vs (map (lambda (gen) (gen)) gens)))
	       (if (not (any eof-object? vs))
		   (begin
		     (apply procedure vs)
		     (loop)))))))))

(define (generator-map->list procedure gen . gens)
  (apply generator-fold-right-map cons '() procedure gen gens))

(define (generator-find predicate gen)
  (let loop ()
    (let ((v (gen)))
      (cond ((eof-object? v) #f)
	    ((predicate v) v)
	    (else (loop))))))

(define (generator-count predicate gen)
  (let loop ((n 0))
    (let ((v (gen)))
      (if (eof-object? v)
	  n
	  (loop (if (predicate v) (+ n 1) n))))))

(define (generator-any predicate gen)
  (let loop ()
    (let ((v (gen)))
      (if (eof-object? v)
	  #f
	  (or (predicate v)
	      (loop))))))

(define (generator-every predicate gen)
  (let loop ((prev #t))
    (let ((v (gen)))
      (if (eof-object? v)
	  prev
	  (let ((this (predicate v)))
	    (and this
		 (loop this)))))))

(define (generator-unfold gen unfold . args)
  (apply unfold
	 eof-object?
	 (lambda (x) x)
	 (lambda (x) (declare (ignore x)) (gen))
	 (gen)
	 args))

;;;; Accumulator Constructors

(define (make-accumulator kons knil finalize)
  (let ((state knil))
    (define (accumulator-proc obj)
      (if (eof-object? obj)
          (finalize state)
          (begin
	    (set! state (kons obj state))
	    unspecific)))
    accumulator-proc))

(define (count-accumulator)
  (make-accumulator (lambda (obj state)
		      (declare (ignore obj))
		      (+ state 1))
		    0
		    (lambda (x) x)))

(define (list-accumulator)
  (make-accumulator cons '() reverse))

(define (reverse-list-accumulator)
  (make-accumulator cons '() (lambda (x) x)))

(define (vector-accumulator)
  (make-accumulator cons '()
		    (lambda (x) (list->vector (reverse x)))))

(define (reverse-vector-accumulator)
  (make-accumulator cons '() list->vector))

(define (vector-accumulator! vector at)
  (let* ((end (vector-length vector))
	 (i (fix:start-index at end 'vector-accumulator!)))
    (define (vector-accumulator!-proc obj)
      (if (eof-object? obj)
	  vector
	  (begin
	    (if (not (fix:< i end))
		(error "Trying to accumulate past end of vector"))
	    (vector-set! vector i obj)
	    (set! i (fix:+ i 1))
	    unspecific)))
    vector-accumulator!-proc))

(define (string-accumulator)
  (make-accumulator cons '() (lambda (x) (list->string (reverse x)))))

(define (bytevector-accumulator)
  (make-accumulator cons '() (lambda (x) (list->bytevector (reverse x)))))

(define (bytevector-accumulator! bytes at)
  (let* ((end (bytevector-length bytes))
	 (i (fix:start-index at end 'bytevector-accumulator!)))
    (define (bytevector-accumulator!-proc obj)
      (if (eof-object? obj)
	  bytes
	  (begin
	    (if (not (fix:< i end))
		(error "Trying to accumulate past end of bytevector"))
	    (bytevector-u8-set! bytes i obj)
	    (set! i (fix:+ i 1))
	    unspecific)))
    bytevector-accumulator!-proc))

(define (sum-accumulator)
  (make-accumulator + 0 (lambda (x) x)))

(define (product-accumulator)
  (make-accumulator * 1 (lambda (x) x)))
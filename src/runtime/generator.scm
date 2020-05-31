#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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
	  (let ((next (bytevector-u8-ref string index)))
	    (set! index (fix:+ index 1))
	    next)
	  (eof-object)))))

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

(define (generator-unfold gen unfold . args)
  (apply unfold
	 eof-object?
	 (lambda (x) x)
	 (lambda (x) (declare (ignore x)) (gen))
	 (gen)
	 args))

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

(define (generator-map->list procedure gen . gens)
  (apply generator-fold-right-map cons '() procedure gen gens))

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

(define (generator-find predicate gen)
  (let loop ()
    (let ((v (gen)))
      (if (or (eof-object? v) (predicate v))
	  v
	  (loop)))))

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

(define (gtake gen k #!optional padding)
  (guarantee exact-nonnegative-integer? k 'gtake)
  (if (eof-object? padding)
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
    (lambda ()
      (if found
	  (eof-object)
	  (let ((v (gen)))
	    (if (or (eof-object? v)
		    (not (predicate v)))
		(begin
		  (set! found #t)
		  (eof-object))
		v))))))

(define (gdrop-while predicate gen)
  (let ((found #f))
    (lambda ()
      (if found
	  (gen)
	  (let loop ()
	    (let ((v (gen)))
	      (if (or (eof-object? v)
		      (not (predicate v)))
		  (begin
		    (set! found #t)
		    v)
		  (loop))))))))

(define (gdelete item gen #!optional =)
  (gremove (let ((= (if (default-object? =) equal? =)))
	     (lambda (val)
	       (= item val)))
	   gen))
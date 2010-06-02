#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; Test of character-set abstraction

(declare (usual-integrations))

(define-test 'interesting-svl-round-trip
  (lambda ()
    (map (lambda (svl)
	   (run-sub-test
	    (lambda ()
	      (assert-equal-canonical-svls (named-call 'SVL-ROUND-TRIP
						       svl-round-trip svl)
					   svl))))
	 interesting-svls)))

(define (svl-round-trip svl)
  (char-set->scalar-values (scalar-values->char-set svl)))

(define-test 'random-svl-round-trip
  (lambda ()
    (map (lambda (svl)
	   (run-sub-test
	    (lambda ()
	      (guarantee-well-formed-scalar-value-list svl)
	      (assert-equal-canonical-svls
	       (named-call '%CANONICALIZE-SCALAR-VALUE-LIST
			   %canonicalize-scalar-value-list
			   svl)
	       (svl-round-trip svl)))))
	 (append! (append-map! (lambda (i)
				 (make-random-svls i 100))
			       (iota 4 1))
		  (make-random-svls 100 100)))))

(define (make-random-svls n-ranges n-iter)
  (map (lambda (i)
	 i
	 (make-random-svl n-ranges))
       (iota n-iter)))

(define (make-random-svl n-ranges)
  (let ((modulus (* %low-limit 2)))
    (make-initialized-list n-ranges
      (lambda (i)
	(let loop ()
	  (let ((n (random (- char-code-limit modulus))))
	    (let ((m (random modulus)))
	      (if (= m 0)
		  n
		  (cons n (+ n m 1))))))))))

(define-test 'membership
  (lambda ()
    (map (lambda (svl)
	   (map (lambda (value)
		  (run-sub-test
		   (lambda ()
		     (with-test-properties
			 (lambda ()
			   (assert-boolean-=
			    (char-set-member? (scalar-values->char-set svl)
					      (integer->char value))
			    (named-call 'SVL-MEMBER? svl-member? svl value)))
		       'EXPRESSION `(CHAR-SET-MEMBER? ,svl ,value)))))
		(enumerate-test-values)))
	 interesting-svls)))

(define (enumerate-test-values)
  (append (iota (+ %low-limit 8))
	  (iota 8 (- char-code-limit 8))))

(define (svl-member? svl value)
  (let loop ((svl svl))
    (if (pair? svl)
	(if (and (<= (segment-start (car svl)) value)
		 (< value (segment-end (car svl))))
	    #t
	    (loop (cdr svl)))
	#f)))

(define-test 'invert
  (lambda ()
    (map (lambda (svl)
	   (run-sub-test
	    (lambda ()
	      (assert-equal (named-call 'SVL-INVERT-THRU
					svl-invert-thru svl)
			    (named-call 'SVL-INVERT-DIRECT
					svl-invert-DIRECT svl)))))
	 interesting-svls)))

(define (svl-invert-thru svl)
  (char-set->scalar-values (char-set-invert (scalar-values->char-set svl))))

(define (svl-invert-direct svl)

  (define (go svl prev-end)
    (if (pair? svl)
	(cons (make-segment prev-end
			    (segment-start (car svl)))
	      (go (cdr svl)
		  (segment-end (car svl))))
	(if (< prev-end char-code-limit)
	    (list (make-segment prev-end char-code-limit))
	    '())))

  (if (and (pair? svl)
	   (= (segment-start (car svl)) 0))
      (go (cdr svl)
	  (segment-end (car svl)))
      (go svl 0)))

(define (make-binary-test name operation svl-direct)
  (lambda ()
    (map (lambda (svl1)
	   (map (lambda (svl2)
		  (run-sub-test
		   (lambda ()
		     (with-test-properties
			 (lambda ()
			   (assert-equal
			    (char-set->scalar-values
			     (operation (scalar-values->char-set svl1)
					(scalar-values->char-set svl2)))
			    (svl-direct svl1 svl2)))
		       'EXPRESSION `(,name ,svl1 ,svl2)))))
		interesting-svls))
	 interesting-svls)))

(define-test 'union
  (make-binary-test 'CHAR-SET-UNION
		    char-set-union
		    (lambda (svl1 svl2)
		      (named-call 'SVL-UNION svl-union svl1 svl2))))

(define (svl-union svl1 svl2)
  (if (pair? svl1)
      (if (pair? svl2)
	  (let ((s1 (segment-start (car svl1)))
		(e1 (segment-end (car svl1)))
		(s2 (segment-start (car svl2)))
		(e2 (segment-end (car svl2))))
	    (cond ((< e1 s2)
		   (cons (car svl1)
			 (svl-union (cdr svl1) svl2)))
		  ((< e2 s1)
		   (cons (car svl2)
			 (svl-union svl1 (cdr svl2))))
		  (else
		   (let ((s3 (min s1 s2)))
		     (receive (e3 svl1 svl2)
			 (union:find-end (max e1 e2)
					 (cdr svl1)
					 (cdr svl2))
		       (cons (make-segment s3 e3)
			     (svl-union svl1 svl2)))))))
	  svl1)
      svl2))

(define (union:find-end e0 svl1 svl2)
  (let ((s1
	 (if (pair? svl1)
	     (segment-start (car svl1))
	     #f))
	(s2
	 (if (pair? svl2)
	     (segment-start (car svl2))
	     #f)))
    (if (or (and (not s1) (not s2))
	    (< e0
	       (cond ((not s1) s2)
		     ((not s2) s1)
		     (else (min s1 s2)))))
	(values e0 svl1 svl2)
	(if (and s1
		 (or (not s2)
		     (< s1 s2)))
	    (union:find-end (max e0 (segment-end (car svl1)))
			    (cdr svl1)
			    svl2)
	    (union:find-end (max e0 (segment-end (car svl2)))
			    svl1
			    (cdr svl2))))))

(define-test 'intersection
  (make-binary-test 'CHAR-SET-INTERSECTION
		    char-set-intersection
		    (lambda (svl1 svl2)
		      (named-call 'SVL-INTERSECTION
				  svl-intersection svl1 svl2))))

(define (svl-intersection svl1 svl2)
  (let loop ((svl1 svl1) (svl2 svl2))
    (if (and (pair? svl1) (pair? svl2))
	(let ((s1 (segment-start (car svl1)))
	      (e1 (segment-end (car svl1)))
	      (s2 (segment-start (car svl2)))
	      (e2 (segment-end (car svl2))))
	  (cond ((<= e1 s2) (loop (cdr svl1) svl2))
		((<= e2 s1) (loop svl1 (cdr svl2)))
		(else
		 (cons (make-segment (max s1 s2) (min e1 e2))
		       (cond ((< e1 e2)
			      (loop (cdr svl1) svl2))
			     ((> e1 e2)
			      (loop svl1 (cdr svl2)))
			     (else
			      (loop (cdr svl1) (cdr svl2))))))))
	'())))

(define-test 'difference
  (make-binary-test 'CHAR-SET-DIFFERENCE
		    char-set-difference
		    (lambda (svl1 svl2)
		      (named-call 'SVL-DIFFERENCE svl-difference svl1 svl2))))

(define (svl-difference svl1 svl2)
  (let loop ((svl1 svl1) (svl2 svl2))
    (if (pair? svl1)
	(if (pair? svl2)
	    (let ((s1 (segment-start (car svl1)))
		  (e1 (segment-end (car svl1)))
		  (s2 (segment-start (car svl2)))
		  (e2 (segment-end (car svl2))))
	      (cond ((<= e1 s2)
		     (cons (car svl1)
			   (loop (cdr svl1) svl2)))
		    ((<= e2 s1)
		     (loop svl1 (cdr svl2)))
		    (else
		     (let ((tail
			    (cond ((< e1 e2)
				   (loop (cdr svl1)
					 (cons (make-segment e1 e2)
					       (cdr svl2))))
				  ((= e1 e2)
				   (loop (cdr svl1) (cdr svl2)))
				  (else
				   (loop (cons (make-segment e2 e1)
					       (cdr svl1))
					 (cdr svl2))))))
		       (if (< s1 s2)
			   (cons (make-segment s1 s2) tail)
			   tail)))))
	    svl1)
	'())))

(define (assert-equal-canonical-svls svl1 svl2)
  (list (assert-canonical-svl svl1)
	(assert-canonical-svl svl2)
	(assert-equal svl1 svl2)))

(define (assert-canonical-svl svl)
  (assert-true `(CANONICAL-SVL? ,svl)
	       (canonical-svl? svl)))

(define (named-call name operation . args)
  (with-test-properties (lambda () (apply operation args))
    'EXPRESSION (cons name args)))

(define (canonical-svl? items)
  (and (list-of-type? items
	 (lambda (item)
	   (if (pair? item)
	       (and (exact-nonnegative-integer? (car item))
		    (exact-nonnegative-integer? (cdr item))
		    (< (car item) (cdr item))
		    (<= (cdr item) char-code-limit))
	       (and (exact-nonnegative-integer? item)
		    (< item char-code-limit)))))
       (every-tail (lambda (tail)
		     (if (and (pair? tail)
			      (pair? (cdr tail)))
			 (< (segment-end (car tail))
			    (segment-start (cadr tail)))
			 #t))
		   items)))

(define (make-segment start end)
  (if (= (- end start) 1)
      start
      (cons start end)))

(define (segment-start segment)
  (if (pair? segment)
      (car segment)
      segment))

(define (segment-end segment)
  (if (pair? segment)
      (cdr segment)
      (+ segment 1)))

(define (append-map-tail! procedure items)
  (if (pair? items)
      (append! (procedure items)
	       (append-map-tail! procedure (cdr items)))
      '()))

(define (every-tail pred items)
  (if (pair? items)
      (and (pred items)
	   (every-tail pred (cdr items)))
      (pred items)))

(define interesting-points
  (list 0
	1
	(- %low-limit 1)
	%low-limit
	(+ %low-limit 1)
	(- char-code-limit 1)
	char-code-limit))

(define (mapper->generator mapper)
  (lambda (points)
    (let loop ((points points))
      (if (pair? points)
	  (append! (mapper (car points) points)
		   (loop (cdr points)))
	  '()))))

(define 1-generator
  (mapper->generator
   (lambda (start ends)
     (map (lambda (end)
	    (list (make-segment start end)))
	  ends))))

(define (n+1-generator n-generator)
  (mapper->generator
   (lambda (start tails)
     (append-map-tail! (lambda (tail)
			 (let ((segment (make-segment start (car tail))))
			   (map (lambda (segments)
				  (cons segment segments))
				(n-generator (cdr tail)))))
		       tails))))

(define 2-generator
  (n+1-generator 1-generator))

(define 3-generator
  (n+1-generator 2-generator))

(define interesting-svls
  (cons (list)
	(append! (1-generator interesting-points)
		 (2-generator interesting-points)
		 (3-generator interesting-points))))
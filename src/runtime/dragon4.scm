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

;;;; Floating point number printer
;;; package: (runtime number)

#|

The Dragon4 algorithm is described in "How to print floating point
numbers accurately" by Guy L Steele Jr and Jon L White in ACM SIGPLAN
Conference on Programming Language Design and Implementation 1990
(PLDI '90).

Burger & Dybvig ("Printing Floating-Point Numbers Quickly and
Accurately" by Robert G Burger and R Kent Dybvig, PLDI '96) describe a
variant of the Dragon4 algorithm that addresses some of the efficiency
issues.  It is much faster for very large or very small numbers, but
not much different to numbers within a few orders of magnitude of 1.

|#

(declare (usual-integrations))

(define flonum-printer-hook #f)
(define flonum-unparser-cutoff #!default)
(define param:flonum-printer-cutoff)
(define expt-radix)

(define (initialize-dragon4!)
  (set! param:flonum-printer-cutoff
	(make-settable-parameter 'normal
				 (lambda (cutoff)
				   (guarantee-cutoff-spec cutoff)
				   cutoff)))
  (set! expt-radix
	(let ((v (make-initialized-vector 310 (lambda (i) (expt 10 i)))))
	  (lambda (base exponent)
	    (if (and (= base 10)
		     (>= exponent 0)
		     (< exponent (vector-length v)))
		(vector-ref v exponent)
		(rat:expt base exponent)))))
  unspecific)

(define (flo:->string x radix)
  (let ((x>0
	 (lambda (x)
	   (let ((p flo:significand-digits-base-2))
	     (call-with-values (lambda () (dragon4-normalize x p))
	       (lambda (f e)
		 (call-with-values flonum-printer-cutoff-args
		   (lambda (cutoff-mode cutoff display-procedure)
		     (dragon4 f e p radix cutoff-mode cutoff
		       (lambda (u k generate)
			 (let ((digits
				(list->string
				 (let loop ((u u) (k k) (generate generate))
				   k	;ignore
				   (if (negative? u)
				       '()
				       (cons (digit->char u radix)
					     (generate loop)))))))
			   (display-procedure digits k radix))))))))))))
    (or (and flonum-printer-hook
	     (flonum-printer-hook x radix))
	(cond ((flo:nan? x)
	       (string-copy "+nan.0"))
	      ((flo:positive? x)
	       (if (flo:infinite? x)
		   (string-copy "+inf.0")
		   (x>0 x)))
	      ((flo:negative? x)
	       (let ((x (flo:negate x)))
		 (if (flo:infinite? x)
		     (string-copy "-inf.0")
		     (string-append "-" (x>0 x)))))
	      ((flo:zero? x)
	       (string-copy (if (flo:safe-negative? x) "-0." "0.")))
	      (else
	       (string-copy "+nan.0"))))))

(define (flonum-printer:normal-output digits k radix)
  (let ((k+1 (+ k 1)))
    (let ((k+1-l (- k+1 (string-length digits)))
	  (n (flo:significand-digits radix)))
      (cond ((zero? (string-length digits))
	     (string-copy "0."))
	    ((< k+1-l (- n))
	     (scientific-output digits k radix 0))
	    ((negative? k)
	     (string-append "." (make-string (- k+1) #\0) digits))
	    ((negative? k+1-l)
	     (string-append (string-head digits k+1)
			    "."
			    (string-tail digits k+1)))
	    ((<= k n)
	     (string-append digits (make-string k+1-l #\0) "."))
	    (else
	     (scientific-output digits k radix 0))))))

(define (flonum-printer:scientific-output digits k radix)
  (scientific-output digits k radix 0))

(define (flonum-printer:engineering-output digits k radix)
  (scientific-output digits k radix (modulo k 3)))

(define (scientific-output digits k radix kr)
  (let ((l (string-length digits))
	(i (+ kr 1))
	(exponent (int:->string (- k kr) radix)))
    (cond ((= l 0)
	   (string-append "0e" exponent))
	  ((< l i)
	   (string-append digits (make-string (- i l) #\0) "e" exponent))
	  ((= l i)
	   (string-append digits "e" exponent))
	  (else
	   (string-append (string-head digits i)
			  "."
			  (string-tail digits i)
			  "e"
			  exponent)))))

(define (flonum-printer-cutoff-args)
  (let ((cutoff
	 (if (default-object? flonum-unparser-cutoff)
	     (param:flonum-printer-cutoff)
	     flonum-unparser-cutoff)))
    (cond ((eq? 'normal cutoff)
	   (values 'normal 0 flonum-printer:normal-output))
	  ((compound-cutoff-spec? cutoff)
	   (values (car cutoff)
		   (- (cadr cutoff))
		   (if (null? (cddr cutoff))
		       flonum-printer:normal-output
		       (lookup-symbolic-display-mode
			(caddr cutoff)))))
	  (else
	   (warn "illegal flonum printer cutoff parameter" cutoff)
	   (values 'normal 0 flonum-printer:normal-output)))))

(define (cutoff-spec? cutoff)
  (or (eq? 'normal cutoff)
      (compound-cutoff-spec? cutoff)))

(define (compound-cutoff-spec? cutoff)
  (and (pair? cutoff)
       (pair? (cdr cutoff))
       (let ((mode (car cutoff))
	     (place (cadr cutoff)))
	 (and (memq mode '(absolute relative normal))
	      (exact-integer? place)
	      (or (not (eq? 'relative mode))
		  (positive? place))))
       (or (null? (cddr cutoff))
	   (and (pair? (cddr cutoff))
		(null? (cdddr cutoff))
		(let ((mode (caddr cutoff)))
		  (or (memq mode '(normal scientific engineering))
		      (and (procedure? mode)
			   (procedure-arity-valid? mode 3))))))))

(define-guarantee cutoff-spec "flonum printer cutoff spec")

(define (lookup-symbolic-display-mode mode)
  (case mode
    ((engineering) flonum-printer:engineering-output)
    ((scientific) flonum-printer:scientific-output)
    ((normal) flonum-printer:normal-output)
    (else mode)))

(define (dragon4-normalize x precision)
  (call-with-values (lambda () (flo:normalize x))
    (lambda (f e-p)
      (values (flo:->integer (flo:denormalize f precision))
	      (- e-p precision)))))

(define (dragon4 f e p radix cutoff-mode cutoff format)
  (call-with-values
      (lambda ()
	(cond ((positive? e)
	       (let ((shift (int:expt 2 e)))
		 (dragon4-fixup f e p radix cutoff-mode cutoff
				(int:* f shift) 1 shift)))
	      ((negative? e)
	       (dragon4-fixup f e p radix cutoff-mode cutoff
			      f (int:expt 2 (- e)) 1))
	      (else
	       (dragon4-fixup f e p radix cutoff-mode cutoff f 1 1))))
    (lambda (k r s m- m+ cutoff round-up?)
      (if (<= k cutoff)
	  ((dragon4-fill (- k 1)) format)
	  (let ((2s (int:* 2 s)))
	    (let loop ((r r) (m- m-) (m+ m+) (k k) (format format))
	      (let ((qr (integer-divide (int:* r radix) s)))
		(let ((k (- k 1))
		      (u (integer-divide-quotient qr))
		      (r (integer-divide-remainder qr))
		      (m- (int:* m- radix))
		      (m+ (int:* m+ radix)))
		  (let ((2r (int:* 2 r)))
		    (let ((high?
			   (if round-up?
			       (int:>= 2r (int:- 2s m+))
			       (int:> 2r (int:- 2s m+))))
			  (round
			   (lambda ()
			     (dragon4-done format
					   (if (int:<= 2r s) u (+ u 1))
					   k))))
		      (cond ((int:< 2r m-)
			     (if high? (round) (dragon4-done format u k)))
			    (high?
			     (dragon4-done format (+ u 1) k))
			    ((= k cutoff)
			     (round))
			    (else
			     (format u k
				     (lambda (format)
				       (loop r m- m+ k format)))))))))))))))

(define (dragon4-done format u k)
  (format u k (dragon4-fill (- k 1))))

(define (dragon4-fill k)
  (lambda (format)
    (format -1 k (dragon4-fill (- k 1)))))

(define (dragon4-fixup f e p radix cutoff-mode cutoff r s m-)

  (define (adjust k r s m- m+)
    (let ((2r (int:* 2 r)))
      (let loop ((k k) (s s) (m- m-) (m+ m+) (round-up? #f))

	(define (adjust-for-mode s k)
	  (define (cutoff-adjust cutoff)
	    (let ((a (- cutoff k)))
	      (let ((y (ceiling (* s (expt-radix radix a)))))
		(let ((m- (int:max y m-))
		      (m+ (int:max y m+)))
		  (let ((round-up? (or (int:= m+ y) round-up?)))
		    (if (int:<= (int:* 2 s) (int:+ 2r m+))
			(loop k s m- m+ round-up?)
			(values k r s m- m+ cutoff round-up?)))))))
	  (case cutoff-mode
	    ((normal)
	     (values k r s m- m+
		     (- k (flo:significand-digits radix) 2) ; i.e. ignore cutoff
		     round-up?))
	    ((absolute) (cutoff-adjust cutoff))
	    ((relative) (cutoff-adjust (+ k cutoff)))
	    (else (error:wrong-type-datum cutoff-mode #f))))

	(let ((2r+m+ (int:+ 2r m+)))
	  (let loop ((s s) (k k))
	    (if (int:<= (int:* 2 s) 2r+m+)
		(loop (int:* s radix) (+ k 1))
		(adjust-for-mode s k)))))))

  (define (scale r s m+)
    (let ((est-k
	   (ceiling->exact (- (* (+ e p -1) (/ (flo:log 2.) (log radix)))
			      1e-9))))	; fudge factor ensures K never too big
      (if (< est-k 0)
	  (let ((factor (expt-radix radix (- est-k))))
	    (let loop ((k est-k)
		       (r (int:* r factor))
		       (m- (int:* m- factor))
		       (m+ (int:* m+ factor)))
	      (if (int:< (int:* r radix) s)
		  (loop (- k 1)
			(int:* r radix)
			(int:* m- radix)
			(int:* m+ radix))
		  (adjust k r s m- m+))))
	  (adjust est-k r (int:* s (expt-radix radix est-k)) m- m+))))

  (if (int:= f (int:expt 2 (- p 1)))
      (scale (int:* 2 r) (int:* 2 s) (int:* 2 m-))
      (scale r s m-)))
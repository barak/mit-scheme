#| -*-Scheme-*-

$Id: dragon4.scm,v 1.12 1997/07/26 07:39:07 cph Exp $

Copyright (c) 1989-97 Massachusetts Institute of Technology

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

;;;; Floating Point Number Unparser
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

(define (flo:->string x radix)
  (let ((inf?
	 (lambda (x)
	   (and (flo:> x 1.)
		(flo:= x (flo:/ x 2.)))))
	(x>0
	 (lambda (x)
	   (let ((p flo:significand-digits-base-2))
	     (call-with-values (lambda () (dragon4-normalize x p))
	       (lambda (f e)
		 (call-with-values flonum-unparser-cutoff-args
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
    (or (and flonum-unparser-hook
	     (flonum-unparser-hook x radix))
	(cond ((flo:positive? x)
	       (if (inf? x)
		   (string-copy "#[+inf]")
		   (x>0 x)))
	      ((flo:negative? x)
	       (let ((x (flo:negate x)))
		 (if (inf? x)
		     (string-copy "#[-inf]")
		     (string-append "-" (x>0 x)))))
	      ((flo:zero? x)
	       (string-copy "0."))
	      (else
	       (string-copy "#[NaN]"))))))

(define (flonum-unparser:normal-output digits k radix)
  (let ((k+1 (+ k 1)))
    (let ((k+1-l (- k+1 (string-length digits)))
	  (n (flo:significand-digits radix)))
      (cond ((< k+1-l (- n))
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

(define (flonum-unparser:scientific-output digits k radix)
  (scientific-output digits k radix 0))

(define (flonum-unparser:engineering-output digits k radix)
  (scientific-output digits k radix (modulo k 3)))

(define (scientific-output digits k radix kr)
  (let ((l (string-length digits))
	(i (+ kr 1))
	(exponent (int:->string (- k kr) radix)))
    (cond ((< l i)
	   (string-append digits (make-string (- i l) #\0) "e" exponent))
	  ((= l i)
	   (string-append digits "e" exponent))
	  (else
	   (string-append (string-head digits i)
			  "."
			  (string-tail digits i)
			  "e"
			  exponent)))))

(define (flonum-unparser-cutoff-args)
  (cond ((eq? 'NORMAL flonum-unparser-cutoff)
	 (values 'NORMAL 0 flonum-unparser:normal-output))
	((and (pair? flonum-unparser-cutoff)
	      (pair? (cdr flonum-unparser-cutoff))
	      (let ((mode (car flonum-unparser-cutoff))
		    (place (cadr flonum-unparser-cutoff)))
		(and (memq mode '(ABSOLUTE RELATIVE NORMAL))
		     (exact-integer? place)
		     (or (not (eq? 'RELATIVE mode))
			 (positive? place))))
	      (or (null? (cddr flonum-unparser-cutoff))
		  (and (pair? (cddr flonum-unparser-cutoff))
		       (null? (cdddr flonum-unparser-cutoff))
		       (let ((mode (caddr flonum-unparser-cutoff)))
			 (or (memq mode '(NORMAL SCIENTIFIC ENGINEERING))
			     (and (procedure? mode)
				  (procedure-arity-valid? mode 3)))))))
	 (values (car flonum-unparser-cutoff)
		 (- (cadr flonum-unparser-cutoff))
		 (if (null? (cddr flonum-unparser-cutoff))
		     flonum-unparser:normal-output
		     (lookup-symbolic-display-mode
		      (caddr flonum-unparser-cutoff)))))
	(else
	 (warn "illegal flonum unparser cutoff parameter"
	       flonum-unparser-cutoff)
	 (values 'NORMAL 0 flonum-unparser:normal-output))))

(define (lookup-symbolic-display-mode mode)
  (case mode
    ((ENGINEERING) flonum-unparser:engineering-output)
    ((SCIENTIFIC) flonum-unparser:scientific-output)
    ((NORMAL) flonum-unparser:normal-output)
    (else mode)))

(define flonum-unparser-hook #f)
(define flonum-unparser-cutoff 'NORMAL)

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
	    ((NORMAL)   (values k r s m- m+ k round-up?))
	    ((ABSOLUTE) (cutoff-adjust cutoff))
	    ((RELATIVE) (cutoff-adjust (+ k cutoff)))
	    (else (error:wrong-type-datum cutoff-mode #f))))

	(let ((2r+m+ (int:+ 2r m+)))
	  (let loop ((s s) (k k))
	    (if (int:<= (int:* 2 s) 2r+m+)
		(loop (int:* s radix) (+ k 1))
		(adjust-for-mode s k)))))))

  (define (scale r s m+)
    (let ((est-k
	   (ceiling->exact (- (* (+ e p -1) (/ (flo:log 2.) (log radix)))
			      1e-9))))	; fudge factor ensures K bever too big
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


(define expt-radix
  (let ((v (make-initialized-vector 310 (lambda (i) (expt 10 i)))))
    (lambda (base exponent)
      (if (and (= base 10)
	       (>= exponent 0)
	       (< exponent (vector-length v)))
	  (vector-ref v exponent)
	  (rat:expt base exponent)))))
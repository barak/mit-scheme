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

;;;; Scheme Arithmetic
;;; package: (runtime number)

(declare (usual-integrations))

;;;; Utilities

(define-syntax copy
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(identifier) (cdr form))
	 (let ((identifier (close-syntax (cadr form) environment)))
	   `(local-declare ((integrate ,identifier)) ,identifier))
	 (ill-formed-syntax form)))))

;;;; Primitives

(define-primitives
  (listify-bignum 2)
  (integer->flonum 2)
  (fixnum->flonum 1)
  (flo:denormalize flonum-denormalize 2)
  (integer-length-in-bits 1)
  (integer-shift-left 2))

(define-integrable (int:bignum? object)
  (object-type? (ucode-type big-fixnum) object))

(define-integrable (make-ratnum n d)
  (system-pair-cons (ucode-type ratnum) n d))

(define-integrable (ratnum? object)
  (object-type? (ucode-type ratnum) object))

(define-integrable (ratnum-numerator ratnum)
  (system-pair-car ratnum))

(define-integrable (ratnum-denominator ratnum)
  (system-pair-cdr ratnum))

(define-integrable (flonum? object)
  (object-type? (ucode-type big-flonum) object))

(define (flo:normalize x)
  (let ((r (%flo:normalize x)))
    (values (car r) (cdr r))))

(define-integrable (%flo:normalize x)
  ((ucode-primitive flonum-normalize 1) x))

(define-integrable flo:->integer
  flo:truncate->exact)

(define-integrable (recnum? object)
  (object-type? (ucode-type recnum) object))

(define-integrable (make-recnum real imag)
  (system-pair-cons (ucode-type recnum) real imag))

(define-integrable (rec:real-part recnum)
  (system-pair-car recnum))

(define-integrable (rec:imag-part recnum)
  (system-pair-cdr recnum))

;;;; Constants

(define-integrable flo:0 0.)
(define-integrable flo:1 1.)
(define rec:pi/2 (flo:* 2. (flo:atan2 1. 1.)))
(define rec:pi (flo:* 2. rec:pi/2))

(define flo:radix 2)
(define flo:ulp-of-one)
(define flo:error-bound)
(define flo:log-error-bound)
(define flo:normal-exponent-max-base-2)
(define flo:normal-exponent-min-base-2)
(define flo:normal-exponent-max-base-e)
(define flo:normal-exponent-min-base-e)
(define flo:normal-exponent-max-base-10)
(define flo:normal-exponent-min-base-10)
(define flo:subnormal-exponent-min-base-2)
(define flo:subnormal-exponent-min-base-e)
(define flo:subnormal-exponent-min-base-10)
(define flo:significand-digits-base-2)
(define flo:significand-digits-base-10)
(define int:flonum-integer-limit)

(define (initialize-microcode-dependencies!)
  (let ((p microcode-id/floating-mantissa-bits))
    (set! flo:significand-digits-base-2 p)
    ;; Add two here because first and last digits may be
    ;; "partial" in the sense that each represents less than the
    ;; `flo:log10/log2' bits.  This is a kludge, but doing the
    ;; "right thing" seems hard.  See Steele&White for a discussion of
    ;; this phenomenon.
    (set! flo:significand-digits-base-10
	  (int:+ 2
		 (flo:floor->exact
		  (flo:/ (int:->flonum p)
			 (flo:/ (flo:log 10.) (flo:log 2.))))))
    (set! int:flonum-integer-limit (int:expt 2 p)))
  (set! flo:ulp-of-one microcode-id/floating-epsilon)
  (set! flo:error-bound (flo:/ flo:ulp-of-one 2.))
  (set! flo:log-error-bound (flo:log flo:error-bound))
  (set! flo:normal-exponent-max-base-2 microcode-id/floating-exponent-max)
  (set! flo:normal-exponent-min-base-2 microcode-id/floating-exponent-min)
  (set! flo:subnormal-exponent-min-base-2
	(int:- flo:normal-exponent-min-base-2
	       (int:- flo:significand-digits-base-2 1)))
  (set! flo:normal-exponent-max-base-e
	(flo:log (flo:expt 2. (int:->flonum flo:normal-exponent-max-base-2))))
  (set! flo:normal-exponent-min-base-e
	(flo:log (flo:expt 2. (int:->flonum flo:normal-exponent-min-base-2))))
  (set! flo:subnormal-exponent-min-base-e
	(flo:+
	 flo:normal-exponent-min-base-e
	 (flo:log
	  (flo:expt 2.
		    (flo:- 0. (int:->flonum flo:significand-digits-base-2))))))
  (set! flo:normal-exponent-max-base-10
	(flo:/ flo:normal-exponent-max-base-e (flo:log 10.)))
  (set! flo:normal-exponent-min-base-10
	(flo:/ flo:normal-exponent-min-base-e (flo:log 10.)))
  (set! flo:subnormal-exponent-min-base-10
	(flo:/ flo:subnormal-exponent-min-base-e (flo:log 10.)))
  unspecific)

(define (initialize-package!)
  (initialize-microcode-dependencies!)
  (add-event-receiver! event:after-restore initialize-microcode-dependencies!)
  (initialize-*maximum-fixnum-radix-powers*!)
  (set-fixed-objects-item! 'generic-trampoline-zero? complex:zero?)
  (set-fixed-objects-item! 'generic-trampoline-positive? complex:positive?)
  (set-fixed-objects-item! 'generic-trampoline-negative? complex:negative?)
  (set-fixed-objects-item! 'generic-trampoline-add-1 complex:1+)
  (set-fixed-objects-item! 'generic-trampoline-subtract-1 complex:-1+)
  (set-fixed-objects-item! 'generic-trampoline-equal? complex:=)
  (set-fixed-objects-item! 'generic-trampoline-less? complex:<)
  (set-fixed-objects-item! 'generic-trampoline-greater? complex:>)
  (set-fixed-objects-item! 'generic-trampoline-add complex:+)
  (set-fixed-objects-item! 'generic-trampoline-subtract complex:-)
  (set-fixed-objects-item! 'generic-trampoline-multiply complex:*)
  (set-fixed-objects-item! 'generic-trampoline-divide complex:/)
  (set-fixed-objects-item! 'generic-trampoline-quotient complex:quotient)
  (set-fixed-objects-item! 'generic-trampoline-remainder complex:remainder)
  (set-fixed-objects-item! 'generic-trampoline-modulo complex:modulo)

  ;; The binary cases for the following operators rely on the fact that the
  ;; &<mumble> operators, either interpreted or open-coded by the
  ;; compiler, calls the GENERIC-TRAMPOLINE version above, are set to
  ;; the appropriate binary procedures when this package is
  ;; initialized.  We could have just replaced (ucode-primitive &+)
  ;; with + etc and relied on + being integrated, but that is not
  ;; very clear.

  (let-syntax
      ((commutative
	(sc-macro-transformer
	 (lambda (form environment)
	   (let ((name (list-ref form 1))
		 (identity (close-syntax (list-ref form 3) environment)))
	     `(set! ,(close-syntax name environment)
		    (make-arity-dispatched-procedure
		     (named-lambda (,name self . zs)
		       self		; ignored
		       (reduce ,(close-syntax (list-ref form 2) environment)
			       ,identity
			       zs))
		     (named-lambda (,(symbol 'nullary- name))
		       ,identity)
		     (named-lambda (,(symbol 'unary- name) z)
		       (if (not (complex:complex? z))
			   (error:wrong-type-argument z "number" ',name))
		       z)
		     (named-lambda (,(symbol 'binary- name) z1 z2)
		       ((ucode-primitive ,(list-ref form 4)) z1 z2)))))))))
    (commutative + complex:+ 0 &+)
    (commutative * complex:* 1 &*))

  (let-syntax
      ((non-commutative
	(sc-macro-transformer
	 (lambda (form environment)
	   (let ((name (list-ref form 1)))
	     `(set! ,(close-syntax name environment)
		    (make-arity-dispatched-procedure
		     (named-lambda (,name self z1 . zs)
		       self		; ignored
		       (,(close-syntax (list-ref form 3) environment)
			z1
			(reduce ,(close-syntax (list-ref form 4) environment)
				,(close-syntax (list-ref form 5) environment)
				zs)))
		     #f
		     ,(close-syntax (list-ref form 2) environment)
		     (named-lambda (,(symbol 'binary- name) z1 z2)
		       ((ucode-primitive ,(list-ref form 6)) z1 z2)))))))))
    (non-commutative - complex:negate complex:- complex:+ 0 &-)
    (non-commutative / complex:invert complex:/ complex:* 1 &/))

  (let-syntax
      ((relational
	(sc-macro-transformer
	 (lambda (form environment)
	   (let ((name (list-ref form 1))
		 (type (list-ref form 4)))
	     `(set! ,(close-syntax name environment)
		    (make-arity-dispatched-procedure
		     (named-lambda (,name self . zs)
		       self		; ignored
		       (reduce-comparator
			,(close-syntax (list-ref form 2) environment)
			zs ',name))
		     (named-lambda (,(symbol 'nullary- name)) #t)
		     (named-lambda (,(symbol 'unary- name) z)
		       (if (not (,(intern (string-append "complex:" type "?"))
				 z))
			   (error:wrong-type-argument
			    z ,(string-append type " number") ',name))
		       #t)
		     (named-lambda (,(symbol 'binary- name) z1 z2)
		       ,(let ((p
			       `((ucode-primitive ,(list-ref form 3)) z1 z2)))
			  (if (list-ref form 5)
			      `(not ,p)
			      p))))))))))
    (relational = complex:= &= "complex" #f)
    (relational < complex:< &< "real" #f)
    (relational > complex:> &> "real" #f)
    (relational <= (lambda (x y) (not (complex:< y x))) &> "real" #t)
    (relational >= (lambda (x y) (not (complex:< x y))) &< "real" #t))

  (let-syntax
      ((max/min
	(sc-macro-transformer
	 (lambda (form environment)
	   (let ((name (list-ref form 1))
		 (generic-binary (close-syntax (list-ref form 2) environment)))
	     `(set! ,(close-syntax name environment)
		    (make-arity-dispatched-procedure
		     (named-lambda (,name self x . xs)
		       self		; ignored
		       (reduce-max/min ,generic-binary x xs ',name))
		     #f
		     (named-lambda (,(symbol 'unary- name) x)
		       (if (not (complex:real? x))
			   (error:wrong-type-argument x "real number" ',name))
		       x)
		     ,generic-binary)))))))
    (max/min max complex:max)
    (max/min min complex:min))

  unspecific)

(define (int:max n m)
  (if (int:< n m) m n))

(define (int:min n m)
  (if (int:< n m) n m))

(define (int:abs n)
  (if (int:negative? n) (int:negate n) n))

(define (int:even? n)
  (int:zero? (int:remainder n 2)))

(define (int:gcd n m)
  (let loop ((n n) (m m))
    (cond ((not (int:zero? m)) (loop m (int:remainder n m)))
	  ((int:negative? n) (int:negate n))
	  (else n))))

(define (int:lcm n m)
  (if (or (int:zero? n) (int:zero? m))
      0
      (int:quotient (let ((n (int:* n m)))
		      (if (int:negative? n)
			  (int:negate n)
			  n))
		    (int:gcd n m))))

(define (int:floor n d)
  (let ((qr (int:divide n d)))
    (let ((q (integer-divide-quotient qr)))
      (if (or (int:zero? (integer-divide-remainder qr))
	      (if (int:negative? n)
		  (int:negative? d)
		  (not (int:negative? d))))
	  q
	  (int:-1+ q)))))

(define (int:ceiling n d)
  (let ((qr (int:divide n d)))
    (let ((q (integer-divide-quotient qr)))
      (if (or (int:zero? (integer-divide-remainder qr))
	      (if (int:negative? n)
		  (not (int:negative? d))
		  (int:negative? d)))
	  q
	  (int:1+ q)))))

(define (int:round n d)
  (let ((positive-case
	 (lambda (n d)
	   (let ((qr (int:divide n d)))
	     (let ((q (integer-divide-quotient qr))
		   (2r (int:* 2 (integer-divide-remainder qr))))
	       (if (or (int:> 2r d)
		       (and (int:= 2r d)
			    (not (fix:zero? (int:remainder q 2)))))
		   (int:1+ q)
		   q))))))
    (if (int:negative? n)
	(if (int:negative? d)
	    (positive-case (int:negate n) (int:negate d))
	    (int:negate (positive-case (int:negate n) d)))
	(if (int:negative? d)
	    (int:negate (positive-case n (int:negate d)))
	    (positive-case n d)))))

(define (int:expt b e)
  (cond ((int:positive? e)
	 (cond ((or (int:= 1 e)
		    (int:zero? b)
		    (int:= 1 b))
		b)
	       ((int:= 2 b)
		(integer-shift-left 1 e))
	       (else
		(let loop ((b b) (e e) (answer 1))
		  (let ((qr (int:divide e 2)))
		    (let ((b (int:* b b))
			  (e (integer-divide-quotient qr))
			  (answer
			   (if (fix:= 0 (integer-divide-remainder qr))
			       answer
			       (int:* answer b))))
		      (if (int:= 1 e)
			  (int:* answer b)
			  (loop b e answer))))))))
	((int:zero? e) 1)
	(else (error:bad-range-argument e 'expt))))

;; A vector indexed by radix of pairs of the form (N . (expt RADIX N))
;; where N is the maximum value for which the cdr is a fixnum.  Used
;; to quickly determine how many digits to process at a time to
;; optimize the use of fixnum arithmetic.

(define *maximum-fixnum-radix-powers*)

(define (initialize-*maximum-fixnum-radix-powers*!)
  (set! *maximum-fixnum-radix-powers*
	(make-initialized-vector 37
	  (lambda (radix)
	    (and (fix:>= radix 2)
		 (let loop ((digits 0) (factor 1))
		   (let ((nf (int:* factor radix)))
		     (if (fix:fixnum? nf)
			 (loop (int:+ digits 1) nf)
			 (cons digits factor)))))))))

;; INT:->STRING chooses between 3 strategies for generating the digits:
;;
;;  PRINT-FIXNUM exploits fast fixnum arithmetic
;;  PRINT-MEDIUM chops off groups of digits that can be printed by PRINT-FIXNUM
;;  PRINT-LARGE works by dividing the problem into approximately equal sizes,
;;   which is asympotically faster but requires more operations for moderate
;;   values.

(define (int:->string number radix)
  ;; Pre: (and (exact-integer? NUMBER) (fixnum? radix) (<= 2 radix 36))

  (define-integrable (digit->char digit radix)
    radix ; ignored
    (string-ref "0123456789abcdefghijklmnopqrstuvwxyz" digit))

  (define (print-fixnum n min-digits tail)
    (let loop ((n n) (n-digits 0) (tail tail))
      (cond ((not (fix:zero? n))
	     (loop (fix:quotient n radix)
		   (fix:+ n-digits 1)
		   (cons (digit->char (fix:remainder n radix)
				      radix)
			 tail)))
	    ((fix:< n-digits min-digits)
	     (loop n (fix:+ n-digits 1) (cons #\0 tail)))
	    (else
	     tail))))

  (define (print-medium value split-factor split-digits)
    (let loop ((n value) (tail '()))
      (if (fix:fixnum? n)
	  (print-fixnum n 0 tail)
	  (let ((qr (integer-divide n split-factor)))
	    (loop (integer-divide-quotient qr)
		  (print-fixnum (integer-divide-remainder qr)
				split-digits
				tail))))))

  (define (fast-test-to-avoid-ultimate-multiply quantum value)
    ;; Uses the number of bignum `digits' or words to test if
    ;; QUANTUM^2>VALUE (the `-1' skips the bignum internal header).
    ;; Since an N digit multiply takes time O(N^2), the benefit of
    ;; avoiding the last squaring is detectable for VALUE>10^100,
    ;; increases with VALUE, but is limited to about 40-50% overall
    ;; improvement by the division operations.
    (define-integrable (bignum-digits n) (fix:+ -1 (system-vector-length n)))
    (and (not (fixnum? quantum))	; i.e. bignum
	 (fix:> (fix:- (fix:* (bignum-digits quantum) 2) 1)
		(bignum-digits value))))

  (define (make-power-stack value quantum stack n-digits)
    (cond ((> quantum value)
	   (use-power-stack value stack n-digits))
	  ((fast-test-to-avoid-ultimate-multiply quantum value)
	   (use-power-stack value (cons quantum stack) (* 2 n-digits)))
	  (else
	   (make-power-stack value
			     (* quantum quantum)
			     (cons quantum stack)
			     (* 2 n-digits)))))

  (define (use-power-stack value stack digits)
    ;; Test at [1] could be (null? stack), but (fixnum? value) is true
    ;; in this case by construction and accelerates printing of numbers
    ;; with a large number of zero digits.
    (define (separate leftmost? value stack n-digits tail)
      (if (fix:fixnum? value)		; [1]
	  (print-fixnum value (if leftmost? 0 n-digits) tail)
	  (let ((split (integer-divide value (car stack)))
		(rest (cdr stack)))
	    (let ((next-left (integer-divide-quotient split))
		  (n-digits/2 (fix:quotient n-digits 2)))
	      (if (and leftmost? (zero? next-left))
		  (separate #t
			    (integer-divide-remainder split)
			    rest
			    n-digits/2
			    tail)
		  (separate leftmost?
			    next-left
			    rest
			    n-digits/2
			    (separate #f
				      (integer-divide-remainder split)
				      rest
				      n-digits/2
				      tail)))))))

    (separate #t value stack digits '()))

  (define (n>0 value)
    (if (fix:fixnum? value)
	(print-fixnum value 1 '())
	(let* ((split-info (vector-ref *maximum-fixnum-radix-powers* radix))
	       (split-digits (car split-info))
	       (split-factor (cdr split-info))
	       (sl (system-vector-length value)))
	  (if (< sl 10)
	      (print-medium value split-factor split-digits)
	      (make-power-stack value split-factor '() split-digits)))))

  (cond ((not (int:integer? number))
	 (error:wrong-type-argument number #f 'number->string))
	((int:negative? number)
	 (list->string (cons #\- (n>0 (int:negate number)))))
	(else
	 (list->string (n>0 number)))))

(declare (integrate-operator rat:rational?))
(define (rat:rational? object)
  (or (ratnum? object)
      (int:integer? object)))

(define (rat:integer? object)
  (and (not (ratnum? object))
       (int:integer? object)))

(define (rat:= q r)
  (if (ratnum? q)
      (if (ratnum? r)
	  (and (int:= (ratnum-numerator q) (ratnum-numerator r))
	       (int:= (ratnum-denominator q) (ratnum-denominator r)))
	  (if (int:integer? r)
	      #f
	      (error:wrong-type-argument r #f '=)))
      (if (ratnum? r)
	  (if (int:integer? q)
	      #f
	      (error:wrong-type-argument q #f '=))
	  (int:= q r))))

(define (rat:< q r)
  (if (ratnum? q)
      (if (ratnum? r)
	  (int:< (int:* (ratnum-numerator q) (ratnum-denominator r))
		 (int:* (ratnum-numerator r) (ratnum-denominator q)))
	  (int:< (ratnum-numerator q) (int:* r (ratnum-denominator q))))
      (if (ratnum? r)
	  (int:< (int:* q (ratnum-denominator r)) (ratnum-numerator r))
	  (int:< q r))))

(define (rat:zero? q)
  (and (not (ratnum? q))
       (int:zero? q)))

(define (rat:negative? q)
  (if (ratnum? q)
      (int:negative? (ratnum-numerator q))
      (int:negative? q)))

(define (rat:positive? q)
  (if (ratnum? q)
      (int:positive? (ratnum-numerator q))
      (int:positive? q)))

(define (rat:max m n)
  (if (rat:< m n) n m))

(define (rat:min m n)
  (if (rat:< m n) m n))

;;; The notation here is from Knuth (p. 291).
;;; In various places we take the gcd of two numbers and then call
;;; quotient to reduce those numbers.  We could check for 1 here, but
;;; this is generally important only for bignums, and the bignum
;;; quotient already performs that check.

(define-syntax define-addition-operator
  (sc-macro-transformer
   (lambda (form environment)
     (let ((name (list-ref form 1))
	   (int:op (close-syntax (list-ref form 2) environment)))
       `(define (,name u/u* v/v*)
	  (rat:binary-operator u/u* v/v*
	    ,int:op
	    (lambda (u v v*)
	      (make-rational (,int:op (int:* u v*) v) v*))
	    (lambda (u u* v)
	      (make-rational (,int:op u (int:* v u*)) u*))
	    (lambda (u u* v v*)
	      (let ((d1 (int:gcd u* v*)))
		(if (int:= d1 1)
		    (make-rational (,int:op (int:* u v*) (int:* v u*))
				   (int:* u* v*))
		    (let* ((u*/d1 (int:quotient u* d1))
			   (t
			    (,int:op (int:* u (int:quotient v* d1))
				     (int:* v u*/d1))))
		      (if (int:zero? t)
			  0	;(make-rational 0 1)
			  (let ((d2 (int:gcd t d1)))
			    (make-rational
			     (int:quotient t d2)
			     (int:* u*/d1
				    (int:quotient v* d2)))))))))))))))

(define-addition-operator rat:+ int:+)
(define-addition-operator rat:- int:-)

(define (rat:1+ v/v*)
  (if (ratnum? v/v*)
      (let ((v* (ratnum-denominator v/v*)))
	(make-ratnum (int:+ (ratnum-numerator v/v*) v*) v*))
      (int:1+ v/v*)))

(define (rat:-1+ v/v*)
  (if (ratnum? v/v*)
      (let ((v* (ratnum-denominator v/v*)))
	(make-ratnum (int:- (ratnum-numerator v/v*) v*) v*))
      (int:-1+ v/v*)))

(define (rat:negate v/v*)
  (if (ratnum? v/v*)
      (make-ratnum (int:negate (ratnum-numerator v/v*))
		   (ratnum-denominator v/v*))
      (int:negate v/v*)))

(define (rat:* u/u* v/v*)
  (rat:binary-operator u/u* v/v*
    int:*
    (lambda (u v v*)
      (let ((d (int:gcd u v*)))
	(make-rational (int:* (int:quotient u d) v)
		       (int:quotient v* d))))
    (lambda (u u* v)
      (let ((d (int:gcd v u*)))
	(make-rational (int:* u (int:quotient v d))
		       (int:quotient u* d))))
    (lambda (u u* v v*)
      (let ((d1 (int:gcd u v*))
	    (d2 (int:gcd v u*)))
	(make-rational (int:* (int:quotient u d1) (int:quotient v d2))
		       (int:* (int:quotient u* d2) (int:quotient v* d1)))))))

(define (rat:square q)
  (if (ratnum? q)
      (make-ratnum (let ((n (ratnum-numerator q))) (int:* n n))
		   (let ((d (ratnum-denominator q))) (int:* d d)))
      (int:* q q)))

(define (rat:cube q)
  (if (ratnum? q)
      (make-ratnum (let ((n (ratnum-numerator q))) (int:* n (int:* n n)))
		   (let ((d (ratnum-denominator q))) (int:* d (int:* d d))))
      (int:* q (int:* q q))))

(define (rat:/ u/u* v/v*)
  (declare (integrate-operator rat:sign-correction))
  (define (rat:sign-correction u v cont)
    (declare (integrate u v))
    (if (int:negative? v)
	(cont (int:negate u) (int:negate v))
	(cont u v)))
  (rat:binary-operator u/u* v/v*
    (lambda (u v)
      (if (int:zero? v)
	  (error:divide-by-zero '/ (list u/u* v/v*))
	  (rat:sign-correction u v
	    (lambda (u v)
	      (let ((d (int:gcd u v)))
		(make-rational (int:quotient u d)
			       (int:quotient v d)))))))
    (lambda (u v v*)
      (rat:sign-correction u v
	(lambda (u v)
	  (let ((d (int:gcd u v)))
	    (make-rational (int:* (int:quotient u d) v*)
			   (int:quotient v d))))))
    (lambda (u u* v)
      (if (int:zero? v)
	  (error:divide-by-zero '/ (list u/u* v/v*))
	  (rat:sign-correction u v
	    (lambda (u v)
	      (let ((d (int:gcd u v)))
		(make-rational (int:quotient u d)
			       (int:* u* (int:quotient v d))))))))
    (lambda (u u* v v*)
      (let ((d1 (int:gcd u v))
	    (d2
	     (let ((d2 (int:gcd v* u*)))
	       (if (int:negative? v)
		   (int:negate d2)
		   d2))))
	(make-rational (int:* (int:quotient u d1) (int:quotient v* d2))
		       (int:* (int:quotient u* d2) (int:quotient v d1)))))))

(define (rat:invert v/v*)
  (if (ratnum? v/v*)
      (let ((v (ratnum-numerator v/v*))
	    (v* (ratnum-denominator v/v*)))
	(cond ((int:positive? v)
	       (make-rational v* v))
	      ((int:negative? v)
	       (make-rational (int:negate v*) (int:negate v)))
	      (else
	       (error:divide-by-zero '/ (list 1 v/v*)))))
      (cond ((int:positive? v/v*) (make-rational 1 v/v*))
	    ((int:negative? v/v*) (make-rational -1 (int:negate v/v*)))
	    (else (error:divide-by-zero '/ (list 1 v/v*))))))

(define-integrable (rat:binary-operator u/u* v/v*
					int*int int*rat rat*int rat*rat)
  (if (ratnum? u/u*)
      (if (ratnum? v/v*)
	  (rat*rat (ratnum-numerator u/u*)
		   (ratnum-denominator u/u*)
		   (ratnum-numerator v/v*)
		   (ratnum-denominator v/v*))
	  (rat*int (ratnum-numerator u/u*)
		   (ratnum-denominator u/u*)
		   v/v*))
      (if (ratnum? v/v*)
	  (int*rat u/u*
		  (ratnum-numerator v/v*)
		  (ratnum-denominator v/v*))
	  (int*int u/u* v/v*))))

(define (rat:abs q)
  (cond ((ratnum? q)
	 (let ((numerator (ratnum-numerator q)))
	   (if (int:negative? numerator)
	       (make-ratnum (int:negate numerator) (ratnum-denominator q))
	       q)))
	((int:negative? q) (int:negate q))
	(else q)))

(define (rat:numerator q)
  (cond ((ratnum? q) (ratnum-numerator q))
	((int:integer? q) q)
	(else (error:wrong-type-argument q #f 'numerator))))

(define (rat:denominator q)
  (cond ((ratnum? q) (ratnum-denominator q))
	((int:integer? q) 1)
	(else (error:wrong-type-argument q #f 'denominator))))

(define-syntax define-integer-coercion
  (sc-macro-transformer
   (lambda (form environment)
     `(define (,(list-ref form 1) q)
	(cond ((ratnum? q)
	       (,(close-syntax (list-ref form 3) environment)
		(ratnum-numerator q)
		(ratnum-denominator q)))
	      ((int:integer? q) q)
	      (else
	       (error:wrong-type-argument q
					  "real number"
					  ',(list-ref form 2))))))))

(define-integer-coercion rat:floor floor int:floor)
(define-integer-coercion rat:ceiling ceiling int:ceiling)
(define-integer-coercion rat:truncate truncate int:quotient)
(define-integer-coercion rat:round round int:round)

(define (rat:rationalize q e)
  (rat:simplest-rational (rat:- q e) (rat:+ q e)))

(define (rat:simplest-rational x y)
  ;; Courtesy of Alan Bawden.
  ;; Produces the simplest rational between X and Y inclusive.
  ;; (In the comments that follow, [x] means (rat:floor x).)
  (let ((x<y
	 (lambda (x y)
	   (define (loop x y)
	     (if (int:integer? x)
		 x
		 (let ((fx (rat:floor x)) ; [X] <= X < [X]+1
		       (fy (rat:floor y))) ; [Y] <= Y < [Y]+1, also [X] <= [Y]
		   (if (rat:= fx fy)
		       ;; [Y] = [X] < X < Y so expand the next term in
		       ;; the continued fraction:
		       (rat:+ fx
			      (rat:invert (loop (rat:invert (rat:- y fy))
						(rat:invert (rat:- x fx)))))
		       ;; [X] < X < [X]+1 <= [Y] <= Y so [X]+1 is the answer:
		       (rat:1+ fx)))))
	   (cond ((rat:positive? x)
		  ;; 0 < X < Y
		  (loop x y))
		 ((rat:negative? y)
		  ;; X < Y < 0 so 0 < -Y < -X and we negate the answer:
		  (rat:negate (loop (rat:negate y) (rat:negate x))))
		 (else
		  ;; X <= 0 <= Y so zero is the answer:
		  0)))))
    (cond ((rat:< x y) (x<y x y))
	  ((rat:< y x) (x<y y x))
	  (else x))))

(define (rat:expt b e)
  (if (int:integer? e)
      (if (int:integer? b)
	  (if (int:negative? e)
	      (rat:invert (int:expt b (int:negate e)))
	      (int:expt b e))
	  (let ((exact-method
		 (lambda (e)
		   (if (int:= 1 e)
		       b
		       (let loop ((b b) (e e) (answer 1))
			 (let ((qr (int:divide e 2)))
			   (let ((b (rat:* b b))
				 (e (integer-divide-quotient qr))
				 (answer
				  (if (int:zero? (integer-divide-remainder qr))
				      answer
				      (rat:* answer b))))
			     (if (int:= 1 e)
				 (rat:* answer b)
				 (loop b e answer)))))))))
	    (cond ((int:negative? e)
		   (rat:invert (exact-method (int:negate e))))
		  ((int:positive? e)
		   (exact-method e))
		  (else 1))))
      (error:bad-range-argument e 'expt)))

(define (rat:->string q radix)
  (if (ratnum? q)
      (string-append (int:->string (ratnum-numerator q) radix)
		     "/"
		     (int:->string (ratnum-denominator q) radix))
      (int:->string q radix)))

(define (make-rational n d)
  (if (or (int:zero? n) (int:= 1 d))
      n
      (make-ratnum n d)))

(define (rat:->inexact q)
  (if (ratnum? q)
      (ratio->flonum (ratnum-numerator q) (ratnum-denominator q))
      (int:->inexact q)))

(define (ratio->flonum n d)
  (define (n>0 n d)
    (let ((k (int:- (integer-length-in-bits n)
		    (integer-length-in-bits d)))
	  (p flo:significand-digits-base-2))
      (letrec
	  ((step1
	    (lambda (n d)
	      ;; (assert (< (expt 2 (- k 1)) (/ n d) (expt 2 (+ k 1))))
	      (if (int:negative? k)
		  (step2 (integer-shift-left n (int:negate k)) d)
		  (step2 n (integer-shift-left d k)))))
	   (step2
	    (lambda (n d)
	      ;; (assert (< 1/2 (/ n d) 2))
	      (if (int:< n d)
		  (step3 n d (int:- k p))
		  (step3 n (int:* 2 d) (int:- (int:1+ k) p)))))
	   (step3
	    (lambda (n d e)
	      ;; (assert (and (<= 1/2 (/ n d)) (< (/ n d) 1)))
	      (let ((n (int:round (integer-shift-left n p) d)))
		(if (int:= n int:flonum-integer-limit)
		    (step4 (int:quotient n 2) (int:1+ e))
		    (step4 n e)))))
	   (step4
	    (lambda (n e)
	      (flo:denormalize (integer->flonum n #b11) e))))
	(step1 n d))))

  (define (slow-method n d)
    (if (int:positive? n)
	(n>0 n d)
	(flo:negate (n>0 (int:negate n) d))))

  (cond ((eq? n 0) flo:0)
	((integer->flonum n #b01)
	 => (lambda (n-exact-flonum)
	      (cond ((integer->flonum d #b01)
		     => (lambda (d-exact-flonum)
			  (flo:/ n-exact-flonum d-exact-flonum)))
		    (else (slow-method n d)))))
	(else (slow-method n d))))

(define (int:->inexact n)
  (if (fixnum? n)
      (fixnum->flonum n) ;; 8.0 compiler open-codes when is N fixnum (by test)
      (integer->flonum n #b10)))

(define (flo:significand-digits radix)
  (cond ((int:= radix 10)
	 flo:significand-digits-base-10)
	((int:= radix 2)
	 flo:significand-digits-base-2)
	(else
	 (int:+ 2
		(flo:floor->exact
		 (flo:/ (int:->flonum flo:significand-digits-base-2)
			(flo:/ (flo:log (int:->flonum radix))
			       (flo:log 2.))))))))

(declare (integrate flo:integer?))
(define (flo:integer? x)
  (and (flo:finite? x)
       (flo:= x (flo:round x))))

(define (flo:rationalize x e)
  (flo:simplest-rational (flo:- x e) (flo:+ x e)))

(define (flo:simplest-rational x y)
  ;; See comments at `rat:simplest-rational'.
  (let ((x<y
	 (lambda (x y)
	   (define (loop x y)
	     (let ((fx (flo:floor x))
		   (fy (flo:floor y)))
	       (cond ((not (flo:< fx x)) fx)
		     ((flo:= fx fy)
		      (flo:+ fx
			     (flo:/ flo:1
				    (loop (flo:/ flo:1 (flo:- y fy))
					  (flo:/ flo:1 (flo:- x fx))))))
		     (else (flo:+ fx flo:1)))))
	   (cond ((flo:positive? x) (loop x y))
		 ((flo:negative? y)
		  (flo:negate (loop (flo:negate y) (flo:negate x))))
		 (else flo:0)))))
    (cond ((flo:< x y) (x<y x y))
	  ((flo:< y x) (x<y y x))
	  (else x))))

(define (flo:rationalize->exact x e)
  (flo:simplest-exact-rational (flo:- x e) (flo:+ x e)))

(define (flo:simplest-exact-rational x y)
  ;; See comments at `rat:simplest-rational'.
  (let ((x<y
	 (lambda (x y)
	   (define (loop x y)
	     (let ((fx (flo:floor x))
		   (fy (flo:floor y)))
	       (cond ((not (flo:< fx x))
		      (flo:->integer fx))
		     ((flo:= fx fy)
		      (rat:+ (flo:->integer fx)
			     (rat:invert (loop (flo:/ flo:1 (flo:- y fy))
					       (flo:/ flo:1 (flo:- x fx))))))
		     (else
		      (rat:1+ (flo:->integer fx))))))
	   (cond ((flo:positive? x) (loop x y))
		 ((flo:negative? y)
		  (rat:negate (loop (flo:negate y) (flo:negate x))))
		 (else 0)))))
    (cond ((flo:< x y) (x<y x y))
	  ((flo:< y x) (x<y y x))
	  (else (flo:->rational x)))))

(define (flo:->rational x)
;;; Don't use multiple-values here because this gets called before they are
;;; defined.
  (let ((p (%flo:normalize x)))
    (let ((f (car p))
	  (e-p (cdr p)))
      (let ((p flo:significand-digits-base-2))
	(rat:* (flo:->integer (flo:denormalize f p))
	       (rat:expt 2 (int:- e-p p)))))))

(define (real:real? object)
  (or (flonum? object)
      (rat:rational? object)))

(define-integrable (real:0 exact?)
  (if exact? 0 0.0))

(define (real:exact1= x)
  (and (real:exact? x)
       (real:= 1 x)))

(define (real:rational? x)
  (if (flonum? x)
      (not (or (flo:nan? x) (flo:infinite? x)))
      (rat:rational? x)))

(define (real:integer? x)
  (if (flonum? x) (flo:integer? x) ((copy rat:integer?) x)))

(define (real:exact? x)
  (and (not (flonum? x))
       (or (rat:rational? x)
	   (error:wrong-type-argument x #f 'exact?))))

(define (real:finite? x)
  (if (flonum? x) (flo:finite? x) #t))

(define (real:infinite? x)
  (if (flonum? x) (flo:infinite? x) #f))

(define (real:nan? x)
  (if (flonum? x) (flo:nan? x) #f))

(define (real:zero? x)
  (if (flonum? x) (flo:zero? x) ((copy rat:zero?) x)))

(define (real:exact0= x)
  (if (flonum? x) #f ((copy rat:zero?) x)))

(define (real:negative? x)
  (if (flonum? x) (flo:negative? x) ((copy rat:negative?) x)))

(define (real:positive? x)
  (if (flonum? x) (flo:positive? x) ((copy rat:positive?) x)))

(define-syntax define-standard-unary
  (sc-macro-transformer
   (lambda (form environment)
     `(define (,(list-ref form 1) x)
	(if (flonum? x)
	    (,(close-syntax (list-ref form 2) environment) x)
	    (,(close-syntax (list-ref form 3) environment) x))))))

(define-standard-unary real:1+ (lambda (x) (flo:+ x flo:1)) (copy rat:1+))
(define-standard-unary real:-1+ (lambda (x) (flo:- x flo:1)) (copy rat:-1+))
(define-standard-unary real:negate flo:negate (copy rat:negate))
(define-standard-unary real:invert (lambda (x) (flo:/ flo:1 x)) rat:invert)
(define-standard-unary real:abs flo:abs rat:abs)
(define-standard-unary real:square (lambda (x) (flo:* x x)) rat:square)
(define-standard-unary real:cube (lambda (x) (flo:* x (flo:* x x))) rat:cube)
(define-standard-unary real:floor flo:floor rat:floor)
(define-standard-unary real:ceiling flo:ceiling rat:ceiling)
(define-standard-unary real:truncate flo:truncate rat:truncate)
(define-standard-unary real:round flo:round rat:round)
(define-standard-unary real:floor->exact flo:floor->exact rat:floor)
(define-standard-unary real:ceiling->exact flo:ceiling->exact rat:ceiling)
(define-standard-unary real:truncate->exact flo:truncate->exact rat:truncate)
(define-standard-unary real:round->exact flo:round->exact rat:round)
(define-standard-unary real:exact->inexact (lambda (x) x) rat:->inexact)
(define-standard-unary real:inexact->exact flo:->rational
  (lambda (q)
    (if (rat:rational? q)
	q
	(error:wrong-type-argument q #f 'inexact->exact))))

(define-syntax define-standard-binary
  (sc-macro-transformer
   (lambda (form environment)
     (let ((flo:op (close-syntax (list-ref form 2) environment))
	   (rat:op (close-syntax (list-ref form 3) environment)))
       `(define (,(list-ref form 1) x y)
	  (if (flonum? x)
	      (if (flonum? y)
		  (,flo:op x y)
		  (,flo:op x (rat:->inexact y)))
	      (if (flonum? y)
		  (,flo:op (rat:->inexact x) y)
		  (,rat:op x y))))))))

(define-standard-binary real:+ flo:+ (copy rat:+))
(define-standard-binary real:- flo:- (copy rat:-))
(define-standard-binary real:rationalize
  flo:rationalize
  rat:rationalize)
(define-standard-binary real:rationalize->exact
  flo:rationalize->exact
  rat:rationalize)
(define-standard-binary real:simplest-rational
  flo:simplest-rational
  rat:simplest-rational)
(define-standard-binary real:simplest-exact-rational
  flo:simplest-exact-rational
  rat:simplest-rational)

(define (real:* x y)
  (cond ((flonum? x)
	 (cond ((flonum? y) (flo:* x y))
	       ((rat:zero? y) (if (flo:nan? x) x y))
	       (else (flo:* x (rat:->inexact y)))))
	((rat:zero? x) (if (and (flonum? y) (flo:nan? y)) y x))
	((flonum? y) (flo:* (rat:->inexact x) y))
	(else ((copy rat:*) x y))))

(define (real:/ x y)
  (cond ((flonum? x) (flo:/ x (if (flonum? y) y (rat:->inexact y))))
	((flonum? y)
	 (if (and (rat:zero? x) (not (flo:nan? y)))
	     x
	     (flo:/ (rat:->inexact x) y)))
	(else ((copy rat:/) x y))))

(define (real:eqv? x y)
  (if (flonum? x)
      (and (flonum? y)
	   (flo:eqv? x y))
      (and (not (flonum? y))
	   ;; Both are exact, so RAT:= will DTRT.
	   ((copy rat:=) x y))))

(define (real:= x y)
  (if (flonum? x)
      (if (flonum? y)
	  (flo:= x y)
	  (flo=rat? x y))
      (if (flonum? y)
	  (rat=flo? x y)
	  ((copy rat:=) x y))))

(define-integrable (flo=rat? x y)
  (and (flo:finite? x)
       (rat:= (flo:->rational x) y)))

(define-integrable (rat=flo? x y)
  (flo=rat? y x))

(define (real:< x y)
  (if (flonum? x)
      (if (flonum? y)
	  (flo:< x y)
	  (flo<rat? x y #f))
      (if (flonum? y)
	  (rat<flo? x y #f)
	  ((copy rat:<) x y))))

(define (real:max x y)
  (if (flonum? x)
      (if (flonum? y)
	  (cond ((flo:<= x y) y)
		((flo:<= y x) x)
		((flo:nan? x) y)
		(else x))
	  (if (flo<rat? x y #t)
	      (rat:->inexact y)
	      x))
      (if (flonum? y)
	  (if (rat<flo? x y #f)
	      y
	      (rat:->inexact x))
	  (if (rat:< x y) y x))))

(define (real:min x y)
  (if (flonum? x)
      (if (flonum? y)
	  (cond ((flo:<= x y) x)
		((flo:<= y x) y)
		((flo:nan? y) x)
		(else y))
	  (if (flo<rat? x y #f)
	      x
	      (rat:->inexact y)))
      (if (flonum? y)
	  (if (rat<flo? x y #t)
	      (rat:->inexact x)
	      y)
	  (if (rat:< x y) x y))))

(define-integrable (flo<rat? x y if-nan)
  (cond ((flo:finite? x) (rat:< (flo:->rational x) y))
	((flo:nan? x) if-nan)
	(else (flo:< x 0.))))

(define-integrable (rat<flo? x y if-nan)
  (cond ((flo:finite? y) (rat:< x (flo:->rational y)))
	((flo:nan? y) if-nan)
	(else (flo:< 0. y))))

(define (real:even? n)
  ((copy int:even?)
   (if (flonum? n)
       (if (flo:integer? n)
	   (flo:->integer n)
	   (error:wrong-type-argument n #f 'even?))
       n)))

(define-syntax define-integer-binary
  (sc-macro-transformer
   (lambda (form environment)
     (let ((operator (close-syntax (list-ref form 3) environment))
	   (flo->int
	    (lambda (n)
	      `(if (flo:integer? ,n)
		   (flo:->integer ,n)
		   (error:wrong-type-argument ,n "integer"
					      ',(list-ref form 2))))))
       `(define (,(list-ref form 1) n m)
	  (if (flonum? n)
	      (int:->inexact
	       (,operator ,(flo->int 'n)
			  (if (flonum? m)
			      ,(flo->int 'm)
			      m)))
	      (if (flonum? m)
		  (int:->inexact (,operator n ,(flo->int 'm)))
		  (,operator n m))))))))

(define-integer-binary real:quotient quotient int:quotient)
(define-integer-binary real:remainder remainder int:remainder)
(define-integer-binary real:modulo modulo int:modulo)
(define-integer-binary real:integer-floor integer-floor int:floor)
(define-integer-binary real:integer-ceiling integer-ceiling int:ceiling)
(define-integer-binary real:integer-round integer-round int:round)
(define-integer-binary real:divide integer-divide int:divide)
(define-integer-binary real:gcd gcd int:gcd)
(define-integer-binary real:lcm lcm int:lcm)

(define-syntax define-rational-unary
  (sc-macro-transformer
   (lambda (form environment)
     (let ((operator (close-syntax (list-ref form 2) environment)))
       `(define (,(list-ref form 1) q)
	  (if (flonum? q)
	      (rat:->inexact (,operator (flo:->rational q)))
	      (,operator q)))))))

(define-rational-unary real:numerator rat:numerator)
(define-rational-unary real:denominator rat:denominator)

(define-syntax define-rational-exact-unary
  (sc-macro-transformer
   (lambda (form environment)
     (let ((operator (close-syntax (list-ref form 2) environment)))
       `(define (,(list-ref form 1) q)
	  (if (flonum? q)
	      (,operator (flo:->rational q))
	      (,operator q)))))))

(define-rational-exact-unary real:numerator->exact rat:numerator)
(define-rational-exact-unary real:denominator->exact rat:denominator)

(define-syntax define-transcendental-unary
  (sc-macro-transformer
   (lambda (form environment)
     `(define (,(list-ref form 1) x)
	(if (,(close-syntax (list-ref form 2) environment) x)
	    ,(close-syntax (list-ref form 3) environment)
	    (,(close-syntax (list-ref form 4) environment)
	     (real:->inexact x)))))))

(define-transcendental-unary real:exp real:exact0= 1 flo:exp)
(define-transcendental-unary real:log real:exact1= 0 flo:log)
(define-transcendental-unary real:expm1 real:exact0= 0 flo:expm1-guarded)
(define-transcendental-unary real:log1p real:exact0= 0 flo:log1p-guarded)
(define-transcendental-unary real:sin real:exact0= 0 flo:sin)
(define-transcendental-unary real:cos real:exact0= 1 flo:cos)
(define-transcendental-unary real:tan real:exact0= 0 flo:tan)
(define-transcendental-unary real:asin real:exact0= 0 flo:asin)
(define-transcendental-unary real:acos real:exact1= 0 flo:acos)
(define-transcendental-unary real:atan real:exact0= 0 flo:atan)

(define-integrable flo:log2 (flo:log 2.))
(define-integrable flo:1-sqrt1/2 (flo:- 1. (flo:sqrt 0.5)))

(declare (integrate flo:expm1-guarded))
(define (flo:expm1-guarded x)
  (if (flo:< (flo:abs x) flo:log2)
      (flo:expm1 x)
      (flo:- (flo:exp x) 1.)))

(declare (integrate flo:log1p-guarded))
(define (flo:log1p-guarded x)
  (if (flo:< (flo:abs x) flo:1-sqrt1/2)
      (flo:log1p x)
      (flo:log (flo:+ 1. x))))

(define (real:atan2 y x)
  (if (and (real:exact0= y)
	   (real:exact? x))
      (if (real:negative? x) rec:pi 0)
      (flo:atan2 (real:->inexact y) (real:->inexact x))))

(define (rat:sqrt x)
  (let ((guess (flo:sqrt (rat:->inexact x))))
    (if (int:integer? x)
	(let ((n (flo:round->exact guess)))
	  (if (int:= x (int:* n n))
	      n
	      guess))
	(let ((q (flo:->rational guess)))
	  (if (rat:= x (rat:square q))
	      q
	      guess)))))

(define (real:sqrt x)
  (if (flonum? x) (flo:sqrt x) (rat:sqrt x)))

(define (real:->inexact x)
  (if (flonum? x)
      x
      (rat:->inexact x)))

(define (real:->string x radix)
  (if (flonum? x)
      (flo:->string x radix)
      (rat:->string x radix)))

(define (real:expt x y)
  (let ((general-case
	 (lambda (x y)
	   (cond ((flo:zero? y) flo:1)
		 ((flo:zero? x)
		  (if (flo:positive? y)
		      x
		      (error:divide-by-zero 'expt (list x y))))
		 ((and (flo:negative? x)
		       (not (flo:integer? y)))
		  (error:bad-range-argument x 'expt))
		 (else
		  (flo:expt x y))))))
    (if (flonum? x)
	(cond ((flonum? y)
	       (general-case x y))
	      ((int:integer? y)
	       (let ((exact-method
		      (lambda (y)
			(if (int:= 1 y)
			    x
			    (let loop ((x x) (y y) (answer flo:1))
			      (let ((qr (int:divide y 2)))
				(let ((x (flo:* x x))
				      (y (integer-divide-quotient qr))
				      (answer
				       (if (int:zero?
					    (integer-divide-remainder qr))
					   answer
					   (flo:* answer x))))
				  (if (int:= 1 y)
				      (flo:* answer x)
				      (loop x y answer)))))))))
		 (cond ((int:positive? y) (exact-method y))
		       ((int:negative? y)
			(if (flo:< (flo:abs x) flo:1)
			    (flo:/ flo:1 (exact-method (int:negate y)))
			    ;; For any base above 1 and sufficiently large
			    ;; exponents, or for any negative exponent and
			    ;; sufficiently large base, the division above
			    ;; would overflow into infinite and then yield
			    ;; 0 when it should yield a subnormal.  So use
			    ;; the general case.
			    (general-case x (int:->flonum y))))
		       (else flo:1))))
	      (else
	       (general-case x (rat:->inexact y))))
	(cond ((flonum? y)
	       (general-case (rat:->inexact x) y))
	      ((int:integer? y)
	       (rat:expt x y))
	      ((and (rat:positive? x)
		    (int:= 1 (rat:numerator y)))
	       (let ((d (rat:denominator y)))
		 (if (int:= 2 d)
		     (rat:sqrt x)
		     (let ((guess
			    (flo:expt (rat:->inexact x) (rat:->inexact y))))
		       (let ((q
			      (if (int:integer? x)
				  (flo:round->exact guess)
				  (flo:->rational guess))))
			 (if (rat:= x (rat:expt q d))
			     q
			     guess))))))
	      (else
	       (general-case (rat:->inexact x) (rat:->inexact y)))))))

(define (complex:complex? object)
  (or (recnum? object) ((copy real:real?) object)))

(define (complex:real? object)
  (if (recnum? object)
      (real:zero? (rec:imag-part object))
      ((copy real:real?) object)))

(define (complex:rational? object)
  (if (recnum? object)
      (and (real:zero? (rec:imag-part object))
	   (real:rational? (rec:real-part object)))
      ((copy real:rational?) object)))

(define (complex:integer? object)
  (if (recnum? object)
      (and (real:zero? (rec:imag-part object))
	   (real:integer? (rec:real-part object)))
      ((copy real:integer?) object)))

(define (complex:exact? z)
  (if (recnum? z)
      ((copy rec:exact?) z)
      ((copy real:exact?) z)))

(define (rec:exact? z)
  (and (real:exact? (rec:real-part z))
       (real:exact? (rec:imag-part z))))

(define (complex:finite? z)
  (if (recnum? z)
      ((copy rec:finite?) z)
      ((copy real:finite?) z)))

(define (rec:finite? z)
  (and (real:finite? (rec:real-part z))
       (real:finite? (rec:imag-part z))))

(define (complex:infinite? z)
  (if (recnum? z)
      ((copy rec:infinite?) z)
      ((copy real:infinite?) z)))

(define (rec:infinite? z)
  (or (real:infinite? (rec:real-part z))
      (real:infinite? (rec:imag-part z))))

(define (complex:nan? z)
  (if (recnum? z)
      ((copy rec:nan?) z)
      ((copy real:nan?) z)))

(define (rec:nan? z)
  (or (real:nan? (rec:real-part z))
      (real:nan? (rec:imag-part z))))

(define (complex:real-arg name x)
  (if (recnum? x) (rec:real-arg name x) x))

(define (rec:real-arg name x)
  (if (not (real:zero? (rec:imag-part x)))
      (error:wrong-type-argument x #f name))
  (rec:real-part x))

(define (complex:eqv? z1 z2)
  (if (recnum? z1)
      (if (recnum? z2)
	  (and (real:eqv? (rec:real-part z1) (rec:real-part z2))
	       (real:eqv? (rec:imag-part z1) (rec:imag-part z2)))
	  (and (real:exact0= (rec:imag-part z1))
	       (real:eqv? (rec:real-part z1) z2)))
      (if (recnum? z2)
	  (and (real:exact0= (rec:imag-part z2))
	       (real:eqv? z1 (rec:imag-part z2)))
	  ((copy real:eqv?) z1 z2))))

(define (complex:= z1 z2)
  (if (recnum? z1)
      (if (recnum? z2)
	  (and (real:= (rec:real-part z1) (rec:real-part z2))
	       (real:= (rec:imag-part z1) (rec:imag-part z2)))
	  (and (real:zero? (rec:imag-part z1))
	       (real:= (rec:real-part z1) z2)))
      (if (recnum? z2)
	  (and (real:zero? (rec:imag-part z2))
	       (real:= z1 (rec:real-part z2)))
	  ((copy real:=) z1 z2))))

(define (complex:< x y)
  (if (recnum? x)
      (if (recnum? y)
	  (real:< (rec:real-arg '< x) (rec:real-arg '< y))
	  (real:< (rec:real-arg '< x) y))
      (if (recnum? y)
	  (real:< x (rec:real-arg '< y))
	  ((copy real:<) x y))))

(define (complex:> x y)
  (complex:< y x))

(define (complex:zero? z)
  (if (recnum? z)
      (and (real:zero? (rec:real-part z))
	   (real:zero? (rec:imag-part z)))
      ((copy real:zero?) z)))

(define (complex:positive? x)
  (if (recnum? x)
      (real:positive? (rec:real-arg 'positive? x))
      ((copy real:positive?) x)))

(define (complex:negative? x)
  (if (recnum? x)
      (real:negative? (rec:real-arg 'negative? x))
      ((copy real:negative?) x)))

(define (complex:even? x)
  (if (recnum? x) (real:even? (rec:real-arg 'even? x)) ((copy real:even?) x)))

(define (complex:max x y)
  (if (recnum? x)
      (if (recnum? y)
	  (real:max (rec:real-arg 'max x) (rec:real-arg 'max y))
	  (real:max (rec:real-arg 'max x) y))
      (if (recnum? y)
	  (real:max x (rec:real-arg 'max y))
	  ((copy real:max) x y))))

(define (complex:min x y)
  (if (recnum? x)
      (if (recnum? y)
	  (real:min (rec:real-arg 'min x) (rec:real-arg 'min y))
	  (real:min (rec:real-arg 'min x) y))
      (if (recnum? y)
	  (real:min x (rec:real-arg 'min y))
	  ((copy real:min) x y))))

(define (complex:+ z1 z2)
  (if (recnum? z1)
      (if (recnum? z2)
	  (complex:%make-rectangular
	   (real:+ (rec:real-part z1) (rec:real-part z2))
	   (real:+ (rec:imag-part z1) (rec:imag-part z2)))
	  (make-recnum (real:+ (rec:real-part z1) z2)
		       (rec:imag-part z1)))
      (if (recnum? z2)
	  (make-recnum (real:+ z1 (rec:real-part z2))
		       (rec:imag-part z2))
	  ((copy real:+) z1 z2))))

(define (complex:1+ z)
  (if (recnum? z)
      (make-recnum (real:1+ (rec:real-part z)) (rec:imag-part z))
      ((copy real:1+) z)))

(define (complex:-1+ z)
  (if (recnum? z)
      (make-recnum (real:-1+ (rec:real-part z)) (rec:imag-part z))
      ((copy real:-1+) z)))

(define (complex:* z1 z2)
  (if (recnum? z1)
      (if (recnum? z2)
	  (let ((z1r (rec:real-part z1))
		(z1i (rec:imag-part z1))
		(z2r (rec:real-part z2))
		(z2i (rec:imag-part z2)))
	    (complex:%make-rectangular
	     (real:- (real:* z1r z2r) (real:* z1i z2i))
	     (real:+ (real:* z1r z2i) (real:* z1i z2r))))
	  (complex:%make-rectangular (real:* (rec:real-part z1) z2)
				     (real:* (rec:imag-part z1) z2)))
      (if (recnum? z2)
	  (complex:%make-rectangular (real:* z1 (rec:real-part z2))
				     (real:* z1 (rec:imag-part z2)))
	  ((copy real:*) z1 z2))))

(define (complex:+i* z)
  (if (recnum? z)
      (complex:%make-rectangular (real:negate (rec:imag-part z))
				 (rec:real-part z))
      (complex:%make-rectangular 0 z)))

(define (complex:-i* z)
  (if (recnum? z)
      (complex:%make-rectangular (rec:imag-part z)
				 (real:negate (rec:real-part z)))
      (complex:%make-rectangular 0 (real:negate z))))

(define (complex:- z1 z2)
  (if (recnum? z1)
      (if (recnum? z2)
	  (complex:%make-rectangular
	   (real:- (rec:real-part z1) (rec:real-part z2))
	   (real:- (rec:imag-part z1) (rec:imag-part z2)))
	  (make-recnum (real:- (rec:real-part z1) z2)
		       (rec:imag-part z1)))
      (if (recnum? z2)
	  (make-recnum (real:- z1 (rec:real-part z2))
		       (real:negate (rec:imag-part z2)))
	  ((copy real:-) z1 z2))))

(define (complex:negate z)
  (if (recnum? z)
      (make-recnum (real:negate (rec:real-part z))
		   (real:negate (rec:imag-part z)))
      ((copy real:negate) z)))

(define (complex:conjugate z)
  (cond ((recnum? z)
	 (make-recnum (rec:real-part z)
		      (real:negate (rec:imag-part z))))
	((real:real? z)
	 z)
	(else
	 (error:wrong-type-argument z #f 'conjugate))))

(define (complex:/ z1 z2)
  (if (recnum? z1)
      (if (recnum? z2)
	  (let ((z1r (rec:real-part z1))
		(z1i (rec:imag-part z1))
		(z2r (rec:real-part z2))
		(z2i (rec:imag-part z2)))
	    (let ((d (real:+ (real:square z2r) (real:square z2i))))
	      (complex:%make-rectangular
	       (real:/ (real:+ (real:* z1r z2r) (real:* z1i z2i)) d)
	       (real:/ (real:- (real:* z1i z2r) (real:* z1r z2i)) d))))
	  (make-recnum (real:/ (rec:real-part z1) z2)
		       (real:/ (rec:imag-part z1) z2)))
      (if (recnum? z2)
	  (let ((z2r (rec:real-part z2))
		(z2i (rec:imag-part z2)))
	    (let ((d (real:+ (real:square z2r) (real:square z2i))))
	      (complex:%make-rectangular
	       (real:/ (real:* z1 z2r) d)
	       (real:/ (real:negate (real:* z1 z2i)) d))))
	  ((copy real:/) z1 z2))))

(define (complex:invert z)
  (if (recnum? z)
      (let ((zr (rec:real-part z))
	    (zi (rec:imag-part z)))
	(let ((d (real:+ (real:square zr) (real:square zi))))
	  (make-recnum (real:/ zr d)
		       (real:/ (real:negate zi) d))))
      ((copy real:invert) z)))

(define (complex:abs x)
  (if (recnum? x) (real:abs (rec:real-arg 'abs x)) ((copy real:abs) x)))

(define (complex:quotient n d)
  (real:quotient (complex:real-arg 'quotient n)
		 (complex:real-arg 'quotient d)))

(define (complex:remainder n d)
  (real:remainder (complex:real-arg 'remainder n)
		  (complex:real-arg 'remainder d)))

(define (complex:modulo n d)
  (real:modulo (complex:real-arg 'modulo n)
	       (complex:real-arg 'modulo d)))

(define (complex:integer-floor n d)
  (real:integer-floor (complex:real-arg 'integer-floor n)
		      (complex:real-arg 'integer-floor d)))

(define (complex:integer-ceiling n d)
  (real:integer-ceiling (complex:real-arg 'integer-ceiling n)
			(complex:real-arg 'integer-ceiling d)))

(define (complex:integer-round n d)
  (real:integer-round (complex:real-arg 'integer-round n)
		      (complex:real-arg 'integer-round d)))

(define (complex:divide n d)
  (real:divide (complex:real-arg 'divide n)
	       (complex:real-arg 'divide d)))

(define (complex:gcd n m)
  (real:gcd (complex:real-arg 'gcd n)
	    (complex:real-arg 'gcd m)))

(define (complex:lcm n m)
  (real:lcm (complex:real-arg 'lcm n)
	    (complex:real-arg 'lcm m)))

(define (complex:numerator q)
  (real:numerator (complex:real-arg 'numerator q)))

(define (complex:denominator q)
  (real:denominator (complex:real-arg 'denominator q)))

(define (complex:numerator->exact q)
  (real:numerator->exact (complex:real-arg 'numerator->exact q)))

(define (complex:denominator->exact q)
  (real:denominator->exact (complex:real-arg 'denominator->exact q)))

(define (complex:floor x)
  (if (recnum? x)
      (real:floor (rec:real-arg 'floor x))
      ((copy real:floor) x)))

(define (complex:ceiling x)
  (if (recnum? x)
      (real:ceiling (rec:real-arg 'ceiling x))
      ((copy real:ceiling) x)))

(define (complex:truncate x)
  (if (recnum? x)
      (real:truncate (rec:real-arg 'truncate x))
      ((copy real:truncate) x)))

(define (complex:round x)
  (if (recnum? x)
      (real:round (rec:real-arg 'round x))
      ((copy real:round) x)))

(define (complex:floor->exact x)
  (if (recnum? x)
      (real:floor->exact (rec:real-arg 'floor->exact x))
      ((copy real:floor->exact) x)))

(define (complex:ceiling->exact x)
  (if (recnum? x)
      (real:ceiling->exact (rec:real-arg 'ceiling->exact x))
      ((copy real:ceiling->exact) x)))

(define (complex:truncate->exact x)
  (if (recnum? x)
      (real:truncate->exact (rec:real-arg 'truncate->exact x))
      ((copy real:truncate->exact) x)))

(define (complex:round->exact x)
  (if (recnum? x)
      (real:round->exact (rec:real-arg 'round->exact x))
      ((copy real:round->exact) x)))

(define (complex:rationalize x e)
  (real:rationalize (complex:real-arg 'rationalize x)
		    (complex:real-arg 'rationalize e)))

(define (complex:rationalize->exact x e)
  (real:rationalize->exact (complex:real-arg 'rationalize x)
			   (complex:real-arg 'rationalize e)))

(define (complex:simplest-rational x y)
  (real:simplest-rational (complex:real-arg 'simplest-rational x)
			  (complex:real-arg 'simplest-rational y)))

(define (complex:simplest-exact-rational x y)
  (real:simplest-exact-rational (complex:real-arg 'simplest-rational x)
				(complex:real-arg 'simplest-rational y)))

(define (complex:exp z)
  (if (recnum? z)
      (complex:%make-polar (real:exp (rec:real-part z))
			   (rec:imag-part z))
      ((copy real:exp) z)))

(define (complex:log z)
  (cond ((recnum? z)
	 (complex:%make-rectangular (real:log (complex:magnitude z))
				    (complex:angle z)))
	((real:negative? z)
	 (make-recnum (real:log (real:negate z)) rec:pi))
	(else
	 ((copy real:log) z))))

(define (complex:expm1 z)
  (if (recnum? z)
      (complex:- (complex:exp z) 1)	;XXX
      ((copy real:expm1) z)))

(define (complex:log1p z)
  (if (recnum? z)
      (complex:log (complex:+ z 1))	;XXX
      ((copy real:log1p) z)))

(define (complex:sin z)
  (if (recnum? z)
      (complex:/ (let ((iz (complex:+i* z)))
		   (complex:- (complex:exp iz)
			      (complex:exp (complex:negate iz))))
		 +2i)
      ((copy real:sin) z)))

(define (complex:cos z)
  (if (recnum? z)
      (complex:/ (let ((iz (complex:+i* z)))
		   (complex:+ (complex:exp iz)
			      (complex:exp (complex:negate iz))))
		 2)
      ((copy real:cos) z)))

(define (complex:tan z)
  (if (recnum? z)
      (complex:-i*
       (let ((iz (complex:+i* z)))
	 (let ((e+iz (complex:exp iz))
	       (e-iz (complex:exp (complex:negate iz))))
	   (complex:/ (complex:- e+iz e-iz)
		      (complex:+ e+iz e-iz)))))
      ((copy real:tan) z)))

;;; Complex arguments -- ASIN
;;;   The danger in the complex case happens for large y when
;;;     z = iy.  In this case iz + sqrt(1-z^2) --> -y + y.
;;;   A clever way out of this difficulty uses symmetry to always
;;;     take the benevolent branch of the square root.
;;;   That is, make iz and sqrt(1-z^2) always end up in the same
;;;     quadrant so catastrophic cancellation cannot occur.
;;;  This is ensured if z is in quadrants III or IV.

(define (complex:asin z)
  (let ((safe-case
	 (lambda (z)
	   (complex:-i*
	    (complex:log
	     (complex:+ (complex:+i* z)
			(complex:sqrt (complex:- 1 (complex:* z z)))))))))
    (let ((unsafe-case
	   (lambda (z)
	     (complex:negate (safe-case (complex:negate z))))))
      (cond ((recnum? z)
	     (if (let ((imag (rec:imag-part z)))
		   (or (real:positive? imag)	;get out of Q I and II
		       (and (real:zero? imag)	;and stay off negative reals
			    (real:negative? (rec:real-part z)))))
		 (unsafe-case z)
		 (safe-case z)))
	    ((real:< z -1)
	     (unsafe-case z))
	    ((real:< 1 z)
	     (safe-case z))
	    (else
	     ((copy real:asin) z))))))

(define (complex:acos z)
  (if (or (recnum? z)
	  (real:< z -1)
	  (real:< 1 z))
      (complex:-i*
       (complex:log
	(complex:+ z
		   (complex:+i*
		    (complex:sqrt (complex:- 1 (complex:* z z)))))))
      ((copy real:acos) z)))

(define (complex:atan z)
  (if (recnum? z)
      (rec:atan z)
      ((copy real:atan) z)))

(define (complex:atan2 y x)
  (let ((rec-case
	 (lambda (y x)
	   (rec:atan (make-recnum (real:exact->inexact x)
				  (real:exact->inexact y))))))
    (cond ((recnum? y)
	   (rec-case (rec:real-arg 'atan y) (complex:real-arg 'atan x)))
	  ((recnum? x)
	   (rec-case y (rec:real-arg 'atan x)))
	  (else
	   ((copy real:atan2) y x)))))

(define (rec:atan z)
  (complex:/ (let ((iz (complex:+i* z)))
	       (complex:- (complex:log (complex:1+ iz))
			  (complex:log (complex:- 1 iz))))
	     +2i))

(define (complex:angle z)
  (cond ((recnum? z)
	 (if (and (real:zero? (rec:real-part z))
		  (real:zero? (rec:imag-part z)))
	     (real:0 (complex:exact? z))
	     (real:atan2 (rec:imag-part z) (rec:real-part z))))
	((real:negative? z) rec:pi)
	(else (real:0 (real:exact? z)))))

(define (complex:magnitude z)
  (if (recnum? z)
      (let ((ar (real:abs (rec:real-part z)))
	    (ai (real:abs (rec:imag-part z))))
	(let ((v (real:max ar ai))
	      (w (real:min ar ai)))
	  (if (real:zero? v)
	      v
	      (real:* v (real:sqrt (real:1+ (real:square (real:/ w v))))))))
      (real:abs z)))

(define (complex:sqrt z)
  (cond ((recnum? z)
	 (complex:%make-polar (real:sqrt (complex:magnitude z))
			      (real:/ (complex:angle z) 2)))
	((real:negative? z)
	 (complex:%make-rectangular 0 (real:sqrt (real:negate z))))
	(else
	 ((copy real:sqrt) z))))

(define (complex:expt z1 z2)
  (cond ((complex:zero? z1)
	 (cond ((eqv? z2 0)
		1)
	       ((real:positive? (complex:real-part z2))
		(real:0 (complex:exact? z1)))
	       ((real:zero? (complex:real-part z2))
		(error:bad-range-argument z2 'expt))
	       (else
		(error:divide-by-zero 'expt (list z1 z2)))))
	((and (recnum? z1)
	      (int:integer? z2))
	 (let ((exact-method
		(lambda (z2)
		  (if (int:= 1 z2)
		      z1
		      (let loop ((z1 z1) (z2 z2) (answer 1))
			(let ((qr (int:divide z2 2)))
			  (let ((z1 (complex:* z1 z1))
				(z2 (integer-divide-quotient qr))
				(answer
				 (if (int:zero?
				      (integer-divide-remainder qr))
				     answer
				     (complex:* answer z1))))
			    (if (int:= 1 z2)
				(complex:* answer z1)
				(loop z1 z2 answer)))))))))
	   (cond ((int:positive? z2) (exact-method z2))
		 ((int:negative? z2)
		  (complex:/ 1 (exact-method (int:negate z2))))
		 (else 1))))
	((or (recnum? z1)
	     (recnum? z2)
	     (and (real:negative? z1)
		  (not (real:integer? z2))))
	 (complex:exp (complex:* (complex:log z1) z2)))
	(else
	 (real:expt z1 z2))))

(define (complex:make-rectangular real imag)
  (let ((check-arg
	 (lambda (x)
	   (if (recnum? x)
	       (rec:real-arg 'make-rectangular x)
	       (begin
		 (if (not (real:real? x))
		     (error:wrong-type-argument x #f 'make-rectangular))
		 x)))))
    ((copy complex:%make-rectangular) (check-arg real) (check-arg imag))))

(define (complex:make-polar real imag)
  ((copy complex:%make-polar) (complex:real-arg 'make-polar real)
			      (complex:real-arg 'make-polar imag)))

(define (complex:%make-rectangular real imag)
  (if (real:exact0= imag)
      real
      (make-recnum real imag)))

(define (complex:%make-polar magnitude angle)
  (complex:%make-rectangular (real:* magnitude (real:cos angle))
			     (real:* magnitude (real:sin angle))))

(define (complex:real-part z)
  (cond ((recnum? z) (rec:real-part z))
	((real:real? z) z)
	(else (error:wrong-type-argument z #f 'real-part))))

(define (complex:imag-part z)
  (cond ((recnum? z) (rec:imag-part z))
	((real:real? z) 0)
	(else (error:wrong-type-argument z #f 'imag-part))))

(define (complex:exact->inexact z)
  (if (recnum? z)
      (complex:%make-rectangular (real:exact->inexact (rec:real-part z))
				 (real:exact->inexact (rec:imag-part z)))
      ((copy real:exact->inexact) z)))

(define (complex:inexact->exact z)
  (if (recnum? z)
      (complex:%make-rectangular (real:inexact->exact (rec:real-part z))
				 (real:inexact->exact (rec:imag-part z)))
      ((copy real:inexact->exact) z)))

(define (complex:->string z radix)
  (if (recnum? z)
      (string-append
       (let ((r (rec:real-part z)))
	 (if (real:exact0= r)
	     ""
	     (real:->string r radix)))
       (let ((i (rec:imag-part z))
	     (positive-case
	      (lambda (i)
		(if (real:exact1= i)
		    ""
		    (real:->string i radix)))))
	 (if (real:negative? i)
	     (string-append "-" (positive-case (real:negate i)))
	     (string-append "+" (positive-case i))))
       (if imaginary-unit-j? "j" "i"))
      (real:->string z radix)))

(define imaginary-unit-j? #f)

(define (inexact? z)
  (not (complex:exact? z)))

(define (exact-nonnegative-integer? object)
  (and (int:integer? object)
       (not (int:negative? object))))

(define (exact-positive-integer? object)
  (and (int:integer? object)
       (int:positive? object)))

(define-guarantee number "number")
(define-guarantee complex "complex number")
(define-guarantee real "real number")
(define-guarantee rational "rational number")
(define-guarantee integer "integer")
(define-guarantee exact "exact number")
(define-guarantee exact-rational "exact rational number")
(define-guarantee exact-integer "exact integer")
(define-guarantee inexact "inexact number")
(define-guarantee exact-nonnegative-integer "exact non-negative integer")
(define-guarantee exact-positive-integer "exact positive integer")

(define (non-negative? object)
  (not (negative? object)))

(define (non-positive? object)
  (not (positive? object)))

(define-guarantee positive "positive number")
(define-guarantee negative "negative number")
(define-guarantee non-positive "non-positive number")
(define-guarantee non-negative "non-negative number")

;;; The following three procedures were originally just renamings of
;;; their COMPLEX: equivalents.  They have been rewritten this way to
;;; cause the compiler to generate better code for them.

(define (quotient n d)
  ((ucode-primitive quotient 2) n d))

(define (remainder n d)
  ((ucode-primitive remainder 2) n d))

(define (modulo n d)
  (let ((r ((ucode-primitive remainder 2) n d)))
    (if (or (zero? r)
	    (if (negative? n)
		(negative? d)
		(not (negative? d))))
	r
	(+ r d))))

(define-integrable integer-divide-quotient car)
(define-integrable integer-divide-remainder cdr)

(define (gcd . integers)
  (fold-left complex:gcd 0 integers))

(define (lcm . integers)
  (fold-left complex:lcm 1 integers))

(define (atan z #!optional x)
  (if (default-object? x)
      (complex:atan z)
      (complex:atan2 z x)))

(define (square z)
  (complex:* z z))

(define (cube z)
  (complex:* z (complex:* z z)))

;;; log(1 - e^x), defined only on negative x

(define (log1mexp x)
  (guarantee-real x 'log1mexp)
  (guarantee-negative x 'log1mexp)
  (if (< (- flo:log2) x)
      (log (- (expm1 x)))
      (log1p (- (exp x)))))

;;; log(1 + e^x)

(define (log1pexp x)
  (guarantee-real x 'log1pexp)
  (cond ((<= x flo:subnormal-exponent-min-base-e) 0.)
	((<= x flo:log-error-bound) (exp x))
	((<= x 18) (log1p (exp x)))
	((<= x 33.3) (+ x (exp (- x))))
	(else (exact->inexact x))))

;;; Some lemmas for the bounds below.
;;;
;;; Lemma 1.  If |d| < 1/2, then 1/(1 + d) <= 2.
;;;
;;; Proof.  If 0 <= d <= 1/2, then 1 + d >= 1, so that 1/(1 + d) <= 1.
;;; If -1/2 <= d <= 0, then 1 + d >= 1/2, so that 1/(1 + d) <= 2.  QED.
;;;
;;; Lemma 2. If b = a*(1 + d)/(1 + d') for |d'| < 1/2 and nonzero a, b,
;;; then b = a*(1 + e) for |e| <= 2|d' - d|.
;;;
;;; Proof.  |a - b|/|a|
;;;		= |a - a*(1 + d)/(1 + d')|/|a|
;;;		= |1 - (1 + d)/(1 + d')|
;;;		= |(1 + d' - 1 - d)/(1 + d')|
;;;		= |(d' - d)/(1 + d')|
;;;	       <= 2|d' - d|, by Lemma 1,
;;;
;;; QED.
;;;
;;; Lemma 3.  For |d|, |d'| < 1/4,
;;;
;;;	|log((1 + d)/(1 + d'))| <= 4|d - d'|.
;;;
;;; Proof.  Write
;;;
;;;	log((1 + d)/(1 + d'))
;;;	 = log(1 + (1 + d)/(1 + d') - 1)
;;;	 = log(1 + (1 + d - 1 - d')/(1 + d')
;;;	 = log(1 + (d - d')/(1 + d')).
;;;
;;; By Lemma 1, |(d - d')/(1 + d')| < 2|d' - d| < 1, so the Taylor
;;; series of log(1 + x) converges absolutely for (d - d')/(1 + d'),
;;; and thus we have
;;;
;;;	|log(1 + (d - d')/(1 + d'))|
;;;	 = |\sum_{n=1}^\infty ((d - d')/(1 + d'))^n/n|
;;;	<= \sum_{n=1}^\infty |(d - d')/(1 + d')|^n/n
;;;	<= \sum_{n=1}^\infty |2(d' - d)|^n/n
;;;	<= \sum_{n=1}^\infty |2(d' - d)|^n
;;;	 = 1/(1 - |2(d' - d)|)
;;;	<= 4|d' - d|,
;;;
;;; QED.
;;;
;;; Lemma 4.  If 1/e <= 1 + x <= e, then
;;;
;;;	log(1 + (1 + d) x) = (1 + d') log(1 + x)
;;;
;;; for |d'| < 8|d|.
;;;
;;; Proof.  Write
;;;
;;;	log(1 + (1 + d) x)
;;;	= log(1 + x + x*d)
;;;	= log((1 + x) (1 + x + x*d)/(1 + x))
;;;	= log(1 + x) + log((1 + x + x*d)/(1 + x))
;;;	= log(1 + x) (1 + log((1 + x + x*d)/(1 + x))/log(1 + x)).
;;;
;;; The relative error is bounded by
;;;
;;;	|log((1 + x + x*d)/(1 + x))/log(1 + x)|
;;;	<= 4|x + x*d - x|/|log(1 + x)|, by Lemma 3,
;;;	 = 4|x*d|/|log(1 + x)|
;;;	 < 8|d|,
;;;
;;; since in this range 0 < 1 - 1/e < x/log(1 + x) <= e - 1 < 2.  QED.

;;; Logistic function: 1/(1 + e^{-x}) = e^x/(1 + e^x).	Maps a
;;; log-odds-space probability in [-\infty, +\infty] into a
;;; direct-space probability in [0,1].	Inverse of logit.
;;;
;;; Ill-conditioned for large x; the identity logistic(-x) = 1 -
;;; logistic(x) and the function (logistic-1/2 x) = (- (logistic x)
;;; 1/2) may help to rearrange a computation.
;;;
;;; This implementation gives relative error bounded by 7 eps.

(define (logistic x)
  (guarantee-real x 'logistic)
  (cond ((<= x flo:subnormal-exponent-min-base-e)
	 ;; e^x/(1 + e^x) < e^x < smallest positive float.  (XXX Should
	 ;; raise inexact and underflow here.)
	 0.)
	((<= x flo:log-error-bound)
	 ;; e^x < eps, so
	 ;;
	 ;;	|e^x - e^x/(1 + e^x)|/|e^x/(1 + e^x)|
	 ;;	<= |1 - 1/(1 + e^x)|*|1 + e^x|
	 ;;	 = |(1 + e^x - 1)/(1 + e^x)|*|1 + e^x|
	 ;;	 = |e^x/(1 + e^x)|*|1 + e^x|
	 ;;	 = |e^x|
	 ;;	 < eps.
	 ;;
	 (exp x))
	((<= x (- flo:log-error-bound))
	 ;; e^{-x} > 0, so 1 + e^{-x} > 1, and 0 < 1/(1 + e^{-x}) < 1;
	 ;; further, since e^{-x} < 1 + e^{-x}, we also have 0 <
	 ;; e^{-x}/(1 + e^{-x}) < 1.  Thus, if exp has relative error
	 ;; d0, + has relative error d1, and / has relative error d2,
	 ;; then we get
	 ;;
	 ;;	(1 + d2)/[(1 + (1 + d0) e^{-x})(1 + d1)]
	 ;;	= (1 + d2)/[1 + e^{-x} + d0 e^{-x}
	 ;;			+ d1 + d1 e^{-x} + d0 d1 e^{-x}]
	 ;;	= (1 + d2)/[(1 + e^{-x})(1 + d0 e^{-x}/(1 + e^{-x})
	 ;;				    + d1/(1 + e^{-x})
	 ;;				    + d0 d1 e^{-x}/(1 + e^{-x}))].
	 ;;	= (1 + d2)/[(1 + e^{-x})(1 + d')]
	 ;;	= [1/(1 + e^{-x})] (1 + d2)/(1 + d')
	 ;;
	 ;; where
	 ;;
	 ;;	d' = d0 e^{-x}/(1 + e^{-x})
	 ;;	     + d1/(1 + e^{-x})
	 ;;	     + d0 d1 e^{-x}/(1 + e^{-x}).
	 ;;
	 ;; By Lemma 2 this relative error is bounded by
	 ;;
	 ;;	2|d2 - d'|
	 ;;	 = 2|d2 - d0 e^{-x}/(1 + e^{-x})
	 ;;		- d1/(1 + e^{-x})
	 ;;		- d0 d1 e^{-x}/(1 + e^{-x})|
	 ;;	<= 2|d2| + 2|d0 e^{-x}/(1 + e^{-x})|
	 ;;		+ 2|d1/(1 + e^{-x})|
	 ;;		+ 2|d0 d1 e^{-x}/(1 + e^{-x})|
	 ;;	<= 2|d2| + 2|d0| + 2|d1| + 2|d0 d1|
	 ;;	<= 6 eps + 2 eps^2.
	 ;;
	 (/ 1 (+ 1 (exp (- x)))))
	(else
	 ;; If x > -log eps, then e^{-x} < eps, so the relative error
	 ;; of 1 from 1/(1 + e^{-x}) is
	 ;;
	 ;;	|1/(1 + e^{-x}) - 1|/|1/(1 + e^{-x})|
	 ;;	 = |e^{-x}/(1 + e^{-x})|/|1/(1 + e^{-x})|
	 ;;	 = |e^{-x}|
	 ;;	<= eps.
	 ;;
	 1.)))

;;; Logistic function, translated in output by 1/2: logistic(x) - 1/2 =
;;; 1/(1 + e^{-x}) - 1/2. Well-conditioned on the entire real plane,
;;; with maximum condition number 1 at 0.
;;;
;;; This implementation gives relative error bounded by 5 eps.

(define (logistic-1/2 x)
  ;; Suppose exp has error d0, + has error d1, expm1 has error d2, and
  ;; / has error d3, so we evaluate
  ;;
  ;;	-(1 + d2) (1 + d3) (e^{-x} - 1)
  ;;	  / [2 (1 + d1) (1 + (1 + d0) e^{-x})].
  ;;
  ;; In the denominator,
  ;;
  ;;	1 + (1 + d0) e^{-x}
  ;;	= 1 + e^{-x} + d0 e^{-x}
  ;;	= (1 + e^{-x}) (1 + d0 e^{-x}/(1 + e^{-x})),
  ;;
  ;; so the relative error of the numerator is
  ;;
  ;;	d' = d2 + d3 + d2 d3,
  ;; and of the denominator,
  ;;	d'' = d1 + d0 e^{-x}/(1 + e^{-x}) + d0 d1 e^{-x}/(1 + e^{-x})
  ;;	    = d1 + d0 L(-x) + d0 d1 L(-x),
  ;;
  ;; where L(-x) is logistic(-x).  By Lemma 1 the relative error of the
  ;; quotient is bounded by
  ;;
  ;;	2|d2 + d3 + d2 d3 - d1 - d0 L(x) + d0 d1 L(x)|,
  ;;
  ;; Since 0 < L(x) < 1, this is bounded by
  ;;
  ;;	2|d2| + 2|d3| + 2|d2 d3| + 2|d1| + 2|d0| + 2|d0 d1|
  ;;	<= 4 eps + 2 eps^2.
  ;;
  (- (/ (expm1 (- x)) (* 2 (+ 1 (exp (- x)))))))

(define-integrable logit-boundary-lo	;logistic(-1)
  (flo:/ (flo:exp -1.) (flo:+ 1. (flo:exp -1.))))
(define-integrable logit-boundary-hi	;logistic(+1)
  (flo:/ 1. (flo:+ 1. (flo:exp -1.))))

;;; Logit function: log p/(1 - p).  Defined on [0,1].  Maps a
;;; direct-space probability in [0,1] to a log-odds-space probability
;;; in [-\infty, +\infty].  Inverse of logistic.
;;;
;;; Ill-conditioned near 1/2 and 1; the identity logit(1 - p) =
;;; -logit(p) and the function (logit1/2+ p0) = (logit (+ 1/2 p0)) may
;;; help to rearrange a computation for p in [1/(1 + e), 1 - 1/(1 +
;;; e)].
;;;
;;; This implementation gives relative error bounded by 10 eps.

(define (logit p)
  (guarantee-real p 'logit)
  (if (not (<= 0 p 1))
      (error:bad-range-argument p 'logit))
  ;; For small p, 1 - p is so close to 1 that log(p) is essentially
  ;; log(p/(1 - p)).  For p near 1/2, the quotient is close to 1 so we
  ;; want to use log1p with the identity
  ;;
  ;;	log(p/(1 - p)) = -log((1 - p)/p)
  ;;	  = -log(1 + (1 - p)/p - 1)
  ;;	  = -log(1 + (1 - p - p)/p)
  ;;	  = -log(1 + (1 - 2p)/p).
  ;;
  ;; to get an intermediate quotient near zero.
  ;;
  (cond ((<= logit-boundary-lo p logit-boundary-hi)
	 ;; Since p = 2p/2 <= 1 <= 2*2p = 4p, the floating-point
	 ;; evaluation of 1 - 2p is exact; the only error arises from
	 ;; division and log1p.	 First, note that if logistic(-1) <= p
	 ;; <= logistic(+1), (1 - 2p)/p lies in the bounds of Lemma 4.
	 ;;
	 ;; If division has relative error d0 and log1p has relative
	 ;; error d1, the outcome is
	 ;;
	 ;;	-(1 + d1) log(1 + (1 - 2p) (1 + d0)/p)
	 ;;	= -(1 + d1) (1 + d') log(1 + (1 - 2p)/p)
	 ;;	= -(1 + d1 + d' + d1 d') log(1 + (1 - 2p)/p).
	 ;;
	 ;; where |d'| < 8|d0| by Lemma 4.  The relative error is then
	 ;; bounded by
	 ;;
	 ;;	|d1 + d' + d1 d'|
	 ;;	<= |d1| + 8|d0| + 8|d1 d0|
	 ;;	<= 9 eps + 8 eps^2.
	 ;;
	 (- (log1p (/ (- 1 (* 2 p)) p))))
	(else
	 ;; If - has relative error d0, / has relative error d1, and
	 ;; log has relative error d2, then
	 ;;
	 ;;	(1 + d2) log((1 + d0) p/[(1 - p)(1 + d1)])
	 ;;	= (1 + d2) [log(p/(1 - p)) + log((1 + d0)/(1 + d1))]
	 ;;	= log(p/(1 - p)) + d2 log(p/(1 - p))
	 ;;	  + (1 + d2) log((1 + d0)/(1 + d1))
	 ;;	= log(p/(1 - p))*[1 + d2 +
	 ;;	    + (1 + d2) log((1 + d0)/(1 + d1))/log(p/(1 - p))]
	 ;;
	 ;; Since 0 <= p < logistic(-1) or logistic(+1) < p <= 1, we
	 ;; have |log(p/(1 - p))| > 1.	Hence this error is bounded by
	 ;;
	 ;;	|d2 + (1 + d2) log((1 + d0)/(1 + d1))/log(p/(1 - p))|
	 ;;	<= |d2| + |(1 + d2) log((1 + d0)/(1 + d1))/log(p/(1 - p))|
	 ;;	<= |d2| + |(1 + d2) log((1 + d0)/(1 + d1))|
	 ;;	<= |d2| + 4|(1 + d2) (d0 - d1)|, by Lemma 3,
	 ;;	<= |d2| + 4|d0 - d1 + d2 d0 - d1 d0|
	 ;;	<= |d2| + 4|d0| + 4|d1| + 4|d2 d0| + 4|d1 d0|
	 ;;	<= 9 eps + 8 eps^2.
	 ;;
	 (log (/ p (- 1 p))))))

;;; Logit function, translated in input by 1/2: (logit1/2+ p-1/2) =
;;; (logit (+ 1/2 p-1/2)).  Defined on [-1/2, 1/2].  Inverse of
;;; logistic-1/2.
;;;
;;; Ill-conditioned near +/-1/2.  If |p0| > 1/2 - 1/(1 + e), it may be
;;; better to compute 1/2 + p0 or -1/2 - p0 and to use logit instead.
;;; This implementation gives relative error bounded by 10 eps.

(define (logit1/2+ p-1/2)
  (cond ((<= (abs p-1/2) (- 1/2 (/ 1 (+ 1 (exp 1)))))
	 ;; If p' = p - 1/2, then p = 1/2 + p', so we compute:
	 ;;
	 ;; log(p/(1 - p))
	 ;; = log((1/2 + p')/(1 - (1/2 + p')))
	 ;; = log((1/2 + p')/(1/2 - p'))
	 ;; = log(1 + (1/2 + p')/(1/2 - p') - 1)
	 ;; = log(1 + (1/2 + p' - (1/2 - p'))/(1/2 - p'))
	 ;; = log(1 + (1/2 + p' - 1/2 + p')/(1/2 - p'))
	 ;; = log(1 + 2 p'/(1/2 - p'))
	 ;;
	 ;; Note that since p0/2 <= 1/2 <= 2 p0, 1/2 - p0 is
	 ;; computed exactly without error; the only error
	 ;; arises from division and log1p.  If the error of
	 ;; division is d0 and the error of log1p is d1, then
	 ;; what we compute is
	 ;;
	 ;;	(1 + d1) log(1 + (1 + d0) 2 p0/(1/2 - p0))
	 ;;	= (1 + d1) (1 + d') log(1 + 2 p0/(1/2 - p0))
	 ;;	= (1 + d1 + d' + d1 d') log(1 + 2 p0/(1/2 - p0)).
	 ;;
	 ;; where |d'| < 8|d0| by Lemma 4, since
	 ;;
	 ;;	1/e <= 1 + 2*p0/(1/2 - p0) <= e
	 ;;
	 ;; when |p0| <= 1/2 - 1/(1 + e).  Hence the relative
	 ;; error is bounded by
	 ;;
	 ;;	|d1 + d' + d1 d'|
	 ;;	<= |d1| + |d'| + |d1 d'|
	 ;;	<= |d1| + 8 |d0| + 8 |d1 d0|
	 ;;	<= 9 eps + 8 eps^2.
	 ;;
	 (log1p (/ (* 2 p-1/2) (- 1/2 p-1/2))))
	(else
	 ;; We have a choice of computing logit(1/2 + p0) or -logit(1 -
	 ;; (1/2 + p0)) = -logit(1/2 - p0).  It doesn't matter which
	 ;; way we do this: either way, since 1/2 p0 <= 1/2 <= 2 p0,
	 ;; the sum and difference are computed exactly.  So let's do
	 ;; the one that skips the final negation.
	 ;;
	 ;; Again, the only error arises from division and log.  So the
	 ;; result is
	 ;;
	 ;;	(1 + d1) log((1 + d0) (1/2 + p0)/(1/2 - p0))
	 ;;	= (1 + d1) (1 + log(1 + d0)/log((1/2 + p0)/(1/2 - p0)))
	 ;;	  * log((1/2 + p0)/(1/2 - p0))
	 ;;	= (1 + d') log((1/2 + p0)/(1/2 - p0))
	 ;;
	 ;; where
	 ;;
	 ;;	d' = d1 + log(1 + d0)/log((1/2 + p0)/(1/2 - p0))
	 ;;	     + d1 log(1 + d0)/log((1/2 + p0)/(1/2 - p0)).
	 ;;
	 ;; For |p| > 1/2 - 1/(1 + e), logit(1/2 + p0) > 1.  For |d0| <
	 ;; 1/2, |log(1 + d0)| < 2|d0|.  Hence this is bounded by
	 ;;
	 ;;	|d'| <= |d1| + 2|d0| + 2|d0 d1|
	 ;;	     <= 3 eps + 2 eps^2.
	 ;;
	 (log (/ (+ 1/2 p-1/2) (- 1/2 p-1/2))))))

;;; log logistic(x) = -log (1 + e^{-x})

(define (log-logistic x)
  (guarantee-real x 'log-logistic)
  (- (log1pexp (- x))))

(define logit-exp-boundary-lo		;log logistic(-1)
  (flo:- 0. (flo:log (flo:+ 1. (flo:exp +1.)))))
(define logit-exp-boundary-hi		;log logistic(+1)
  (flo:- 0. (flo:log (flo:+ 1. (flo:exp -1.)))))

;;; log e^t/(1 - e^t) = logit(e^t)

(define (logit-exp t)
  (guarantee-real t 'logit-exp)
  (cond ((<= t flo:log-error-bound)
	 ;; e^t < eps, so since log(e^t/(1 - e^t)) = t - log(1 - e^t),
	 ;; and |log(1 - e^t)| < 1 < |t|, we have
	 ;;
	 ;;	|t - log(e^t/(1 - e^t))|/|log(e^t/(1 - e^t))|
	 ;;	 = |log(1 - e^t)|/|t - log(1 - e^t)|
	 ;;	<= |log(1 - e^t)|
	 ;;	<= 1/(1 - e^t)
	 ;;	<= 2|e^t|
	 ;;	<= 2 eps.
	 ;;
	 t)
	((<= logit-exp-boundary-lo t logit-exp-boundary-hi)
	 ;; We can use the identity
	 ;;
	 ;;	log(e^t/(1 - e^t))
	 ;;	= -log((1 - e^t)/e^t)
	 ;;	= -log(1 + (1 - e^t)/e^t - 1)
	 ;;	= -log(1 + (1 - e^t - e^t)/e^t)
	 ;;	= -log(1 + (1 - 2 e^t)/e^t)
	 ;;
	 ;; to compute this with log1p.
	 ;;
	 ;; Since e^t = 2 e^t/2 <= 1 < 2*2 e^t = 4 e^t, 1 - 2 e^t is
	 ;; without additional error beyond that in e^t.  Further,
	 ;; |e^t/(1 - 2 e^t)| <= 2.  The intermediate division is
	 ;;
	 ;;	(1 - 2 (1 + d0) e^t) (1 + d1)/[(1 + d0) e^t]
	 ;;	= (1 - 2 e^t - 2 d0 e^t) (1 + d1)/[(1 + d0) e^t]
	 ;;	= (1 - 2 e^t) (1 - 2 d0 e^t/(1 - 2 e^t)) (1 + d1)
	 ;;	    / [(1 + d0) e^t]
	 ;;	= [(1 - 2 e^t)/e^t]
	 ;;	  * (1 - 2 d0 e^t/(1 - 2 e^t)) (1 + d1)/(1 + d0)
	 ;;	= [(1 - 2 e^t)/e^t]
	 ;;	  * (1 + d1 - (1 + d1) 2 d0 e^t/(1 - 2 e^t))
	 ;;	  / (1 + d0).
	 ;;
	 ;; By Lemma 2, the relative error d' of the intermediate division
	 ;; is bounded by
	 ;;
	 ;;	2|d0 - d1 + (1 + d1) 2 d0 e^t/(1 - 2 e^t)|
	 ;;	<= 2|d0| + 2|d1| + 2|d0 (1 + d1) e^t/(1 - 2 e^t)|
	 ;;	<= 2|d0| + 2|d1| + 4|d0 (1 + d1)|
	 ;;	 = 2|d0| + 2|d1| + 4|d0 + d0 d1)|
	 ;;	<= 2|d0| + 2|d1| + 4|d0| + 4|d0 d1|
	 ;;	<= 8 eps + 4 eps^2.
	 ;;
	 ;; By Lemma 4, the relative error of using log1p is compounded
	 ;; by no more than 8|d'|, so the relative error of the result
	 ;; is bounded by
	 ;;
	 ;;	|d2| + |d'| + |d2 d'|
	 ;;	<= eps + 8 eps + 4 eps^2 + eps*(6 eps + 4 eps^2)
	 ;;	 = 9 eps + 10 eps^2 + 4 eps^3.
	 ;;
	 (let ((e^t (exp t)))
	   (- (log1p (/ (- 1 (* 2 e^t)) e^t)))))
	(else
	 ;; We use the identity
	 ;;
	 ;;	log(e^t/(1 - e^t))
	 ;;	= -log((1 - e^t)/e^t)
	 ;;	= -log(e^{-t} - 1)
	 ;;
	 ;; to compute this with expm1.
	 ;;
	 ;;	-(1 + d0) log((1 + d1) (e^{-t} - 1))
	 ;;	= -(1 + d0) [log(e^{-t} - 1) + log(1 + d1)]
	 ;;	= -[(1 + d0) log(e^{-t} - 1) + (1 + d0) log(1 + d1)]
	 ;;	= -[log(e^{-t} - 1) + d0 log(e^{-t} - 1)
	 ;;		+ (1 + d0) log(1 + d1) log(e^{-t} - 1)/log(e^{-t} - 1)]
	 ;;	= -log(e^{-t} - 1)
	 ;;	  * (1 + d0 + (1 + d0) log(1 + d1)/log(e^{-t} - 1))
	 ;;
	 ;; If t <= -log(1 + e), then log(e^{-t} - 1) >= 1; similarly,
	 ;; if t >= -log(1 + 1/e), then log(e^{-t} - 1) <= -1.	Hence,
	 ;; in both cases, |log(e^{-t} - 1)| >= 1, so that
	 ;;
	 ;;	|d0 + (1 + d0) log(1 + d1)/log(e^{-t} - 1)|
	 ;;	<= |d0| + |(1 + d0) log(1 + d1)/log(e^{-t} - 1)|
	 ;;	<= |d0| + |(1 + d0) log(1 + d1)|
	 ;;	<= |d0| + |log(1 + d1)| + |d0 log(1 + d1)|
	 ;;	<= |d0| + |1/(1 - |d1|)| + |d0/(1 - d1)|
	 ;;	<= |d0| + 2|d1| + 2|d0 d1|
	 ;;	<= 3 eps + 2 eps^2.
	 ;;
	 (- (log (expm1 (- t)))))))

;;; Replaced with arity-dispatched version in INITIALIZE-PACKAGE!.

(define =)
(define <)
(define >)
(define <=)
(define >=)

(define (reduce-comparator binary-comparator numbers procedure)
  (if (pair? numbers)
      (if (pair? (cdr numbers))
	  (let loop
	      ((x (car numbers))
	       (y (cadr numbers))
	       (rest (cddr numbers)))
	    (and (binary-comparator x y)
		 (if (pair? rest)
		     (loop y (car rest) (cdr rest))
		     #t)))
	  (begin
	    (if (not (complex:complex? (car numbers)))
		(error:wrong-type-argument (car numbers) #f procedure))
	    #t))
      #t))

(define (odd? n)
  (not (complex:even? n)))

;;; Replaced with arity-dispatched version in INITIALIZE-PACKAGE!.

(define +)
(define *)
(define -)
(define /)

(define max)
(define min)

(define (reduce-max/min max/min x1 xs procedure)
  (if (pair? xs)
      (let loop ((x1 x1) (xs xs))
	(let ((x1 (max/min x1 (car xs)))
	      (xs (cdr xs)))
	  (if (pair? xs)
	      (loop x1 xs)
	      x1)))
      (begin
	(if (not (complex:complex? x1))
	    (error:wrong-type-argument x1 #f procedure))
	x1)))

(define (number->string z #!optional radix)
  (complex:->string
   z
   (cond ((default-object? radix)
	  10)
	 ((and (exact-integer? radix)
	       (<= 2 radix 36))
	  radix)
	 ((and (pair? radix)
	       (eq? (car radix) 'heur)
	       (list? radix))
	  (parse-format-tail (cdr radix)))
	 (else
	  (error:bad-range-argument radix 'number->string)))))

(define (parse-format-tail tail)
  (let loop
      ((tail tail)
       (exactness-expressed #f)
       (radix #f)
       (radix-expressed #f))
    (if (pair? tail)
	(let ((modifier (car tail))
	      (tail (cdr tail)))
	  (let ((specify-modifier
		 (lambda (old)
		   (if old
		       (error "Respecification of format modifier"
			      (cadr modifier)))
		   (cadr modifier))))
	    (cond ((and (pair? modifier)
			(eq? (car modifier) 'exactness)
			(pair? (cdr modifier))
			(memq (cadr modifier) '(e s))
			(null? (cddr modifier)))
		   (if (eq? (cadr modifier) 'e)
		       (warn "NUMBER->STRING: ignoring exactness modifier"
			     modifier))
		   (loop tail
			 (specify-modifier exactness-expressed)
			 radix
			 radix-expressed))
		  ((and (pair? modifier)
			(eq? (car modifier) 'radix)
			(pair? (cdr modifier))
			(memq (cadr modifier) '(b o d x))
			(or (null? (cddr modifier))
			    (and (pair? (cddr modifier))
				 (memq (caddr modifier) '(e s))
				 (null? (cdddr modifier)))))
		   (if (and (pair? (cddr modifier))
			    (eq? (caddr modifier) 'e))
		       (warn
			"NUMBER->STRING: ignoring radix expression modifier"
			modifier))
		   (loop tail
			 exactness-expressed
			 (specify-modifier radix)
			 (if (pair? (cddr modifier)) (caddr modifier) 'e)))
		  (else
		   (error "Illegal format modifier" modifier)))))
	(case radix
	  ((b) 2)
	  ((o) 8)
	  ((d #f) 10)
	  ((x) 16)))))
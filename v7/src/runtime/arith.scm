#| -*-Scheme-*-

$Id: arith.scm,v 1.50 2002/02/09 06:09:39 cph Exp $

Copyright (c) 1989-1999, 2001, 2002 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;;; Scheme Arithmetic
;;; package: (runtime number)

(declare (usual-integrations))

;;;; Utilities

(define-syntax copy
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(IDENTIFIER) (cdr form))
	 (let ((identifier (close-syntax (cadr form) environment)))
	   `(LOCAL-DECLARE ((INTEGRATE ,identifier)) ,identifier))
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
  (let ((r ((ucode-primitive flonum-normalize 1) x)))
    (values (car r) (cdr r))))

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
  unspecific)

(define (initialize-package!)
  (initialize-microcode-dependencies!)
  (add-event-receiver! event:after-restore initialize-microcode-dependencies!)
  (initialize-*maximum-fixnum-radix-powers*!)
  (let ((fixed-objects-vector (get-fixed-objects-vector)))
    (let ((set-trampoline!
	   (lambda (slot operator)
	     (vector-set! fixed-objects-vector
			  (fixed-objects-vector-slot slot)
			  operator))))
      (set-trampoline! 'GENERIC-TRAMPOLINE-ZERO? complex:zero?)
      (set-trampoline! 'GENERIC-TRAMPOLINE-POSITIVE? complex:positive?)
      (set-trampoline! 'GENERIC-TRAMPOLINE-NEGATIVE? complex:negative?)
      (set-trampoline! 'GENERIC-TRAMPOLINE-ADD-1 complex:1+)
      (set-trampoline! 'GENERIC-TRAMPOLINE-SUBTRACT-1 complex:-1+)
      (set-trampoline! 'GENERIC-TRAMPOLINE-EQUAL? complex:=)
      (set-trampoline! 'GENERIC-TRAMPOLINE-LESS? complex:<)
      (set-trampoline! 'GENERIC-TRAMPOLINE-GREATER? complex:>)
      (set-trampoline! 'GENERIC-TRAMPOLINE-ADD complex:+)
      (set-trampoline! 'GENERIC-TRAMPOLINE-SUBTRACT complex:-)
      (set-trampoline! 'GENERIC-TRAMPOLINE-MULTIPLY complex:*)
      (set-trampoline! 'GENERIC-TRAMPOLINE-DIVIDE complex:/)
      (set-trampoline! 'GENERIC-TRAMPOLINE-QUOTIENT complex:quotient)
      (set-trampoline! 'GENERIC-TRAMPOLINE-REMAINDER complex:remainder)
      (set-trampoline! 'GENERIC-TRAMPOLINE-MODULO complex:modulo)))

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
	     `(SET! ,(close-syntax name environment)
		    (MAKE-ENTITY
		     (NAMED-LAMBDA (,name SELF . ZS)
		       SELF		; ignored
		       (REDUCE ,(close-syntax (list-ref form 2) environment)
			       ,identity
			       ZS))
		     (VECTOR
		      (FIXED-OBJECTS-ITEM 'arity-dispatcher-tag)
		      (NAMED-LAMBDA (,(symbol-append 'NULLARY- name))
			,identity)
		      (NAMED-LAMBDA (,(symbol-append 'UNARY- name) Z)
			(IF (NOT (COMPLEX:COMPLEX? Z))
			    (ERROR:WRONG-TYPE-ARGUMENT Z "number" ',name))
			Z)
		      (NAMED-LAMBDA (,(symbol-append 'BINARY- name) Z1 Z2)
			((UCODE-PRIMITIVE ,(list-ref form 4)) Z1 Z2))))))))))
    (commutative + complex:+ 0 &+)
    (commutative * complex:* 1 &*))

  (let-syntax
      ((non-commutative
	(sc-macro-transformer
	 (lambda (form environment)
	   (let ((name (list-ref form 1)))
	     `(SET! ,(close-syntax name environment)
		    (MAKE-ENTITY
		     (NAMED-LAMBDA (,name SELF Z1 . ZS)
		       SELF		; ignored
		       (,(close-syntax (list-ref form 3) environment)
			Z1
			(REDUCE ,(close-syntax (list-ref form 4) environment)
				,(close-syntax (list-ref form 5) environment)
				ZS)))
		     (VECTOR
		      (FIXED-OBJECTS-ITEM 'arity-dispatcher-tag)
		      #F
		      ,(close-syntax (list-ref form 2) environment)
		      (NAMED-LAMBDA (,(symbol-append 'BINARY- name) Z1 Z2)
			((UCODE-PRIMITIVE ,(list-ref form 6)) Z1 Z2))))))))))
    (non-commutative - complex:negate complex:- complex:+ 0 &-)
    (non-commutative / complex:invert complex:/ complex:* 1 &/))

  (let-syntax
      ((relational
	(sc-macro-transformer
	 (lambda (form environment)
	   (let ((name (list-ref form 1))
		 (type (list-ref form 4)))
	     `(SET! ,(close-syntax name environment)
		    (MAKE-ENTITY
		     (NAMED-LAMBDA (,name SELF . ZS)
		       SELF		; ignored
		       (REDUCE-COMPARATOR
			,(close-syntax (list-ref form 2) environment)
			ZS ',name))
		     (VECTOR
		      (FIXED-OBJECTS-ITEM 'arity-dispatcher-tag)
		      (NAMED-LAMBDA (,(symbol-append 'NULLARY- name)) #T)
		      (NAMED-LAMBDA (,(symbol-append 'UNARY- name) Z)
			(IF (NOT (,(intern (string-append "complex:" type "?"))
				  Z))
			    (ERROR:WRONG-TYPE-ARGUMENT
			     Z ,(string-append type " number") ',name))
			#T)
		      (NAMED-LAMBDA (,(symbol-append 'BINARY- name) Z1 Z2)
			,(let ((p
				`((UCODE-PRIMITIVE ,(list-ref form 3)) Z1 Z2)))
			   (if (list-ref form 5)
			       `(NOT ,p)
			       p)))))))))))
    (relational = complex:= &= "complex" #F)
    (relational < complex:< &< "real" #F)
    (relational > complex:> &> "real" #F)
    (relational <= (lambda (x y) (not (complex:< y x))) &> "real" #T)
    (relational >= (lambda (x y) (not (complex:< x y))) &< "real" #T))

  (let-syntax
      ((max/min
	(sc-macro-transformer
	 (lambda (form environment)
	   (let ((name (list-ref form 1))
		 (generic-binary (close-syntax (list-ref form 2) environment)))
	     `(SET! ,(close-syntax name environment)
		    (MAKE-ENTITY
		     (NAMED-LAMBDA (,name SELF X . XS)
		       SELF		; ignored
		       (REDUCE-MAX/MIN ,generic-binary X XS ',name))
		     (VECTOR
		      (FIXED-OBJECTS-ITEM 'arity-dispatcher-tag)
		      #F
		      (NAMED-LAMBDA (,(symbol-append 'UNARY- name) X)
			(IF (NOT (COMPLEX:REAL? X))
			    (ERROR:WRONG-TYPE-ARGUMENT X "real number" ',name))
			X)
		      ,generic-binary))))))))
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

(define (int:modulo n d)
  (let ((r (int:remainder n d)))
    (if (or (int:zero? r)
	    (if (int:negative? n)
		(int:negative? d)
		(not (int:negative? d))))
	r
	(int:+ r d))))

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
	(else (error:bad-range-argument e 'EXPT))))

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
    (string-ref "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" digit))

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
		  (separate true
			    (integer-divide-remainder split)
			    rest
			    n-digits/2
			    tail)
		  (separate leftmost?
			    next-left
			    rest
			    n-digits/2
			    (separate false
				      (integer-divide-remainder split)
				      rest
				      n-digits/2
				      tail)))))))

    (separate true value stack digits '()))

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
	 (error:wrong-type-argument number false 'NUMBER->STRING))
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
	      (error:wrong-type-argument r false '=)))
      (if (ratnum? r)
	  (if (int:integer? q)
	      #f
	      (error:wrong-type-argument q false '=))
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

(let-syntax
    ((define-addition-operator
       (sc-macro-transformer
	(lambda (form environment)
	  (let ((name (list-ref form 1))
		(int:op (close-syntax (list-ref form 2) environment)))
	    `(DEFINE (,name U/U* V/V*)
	       (RAT:BINARY-OPERATOR U/U* V/V*
		 ,int:op
		 (LAMBDA (U V V*)
		   (MAKE-RATIONAL (,int:op (INT:* U V*) V) V*))
		 (LAMBDA (U U* V)
		   (MAKE-RATIONAL (,int:op U (INT:* V U*)) U*))
		 (LAMBDA (U U* V V*)
		   (LET ((D1 (INT:GCD U* V*)))
		     (IF (INT:= D1 1)
			 (MAKE-RATIONAL (,int:op (INT:* U V*) (INT:* V U*))
					(INT:* U* V*))
			 (LET* ((U*/D1 (INT:QUOTIENT U* D1))
				(T
				 (,int:op (INT:* U (INT:QUOTIENT V* D1))
					  (INT:* V U*/D1))))
			   (IF (INT:ZERO? T)
			       0	;(MAKE-RATIONAL 0 1)
			       (LET ((D2 (INT:GCD T D1)))
				 (MAKE-RATIONAL
				  (INT:QUOTIENT T D2)
				  (INT:* U*/D1
					 (INT:QUOTIENT V* D2))))))))))))))))
  (define-addition-operator rat:+ int:+)
  (define-addition-operator rat:- int:-))

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
	(else (error:wrong-type-argument q false 'NUMERATOR))))

(define (rat:denominator q)
  (cond ((ratnum? q) (ratnum-denominator q))
	((int:integer? q) 1)
	(else (error:wrong-type-argument q false 'DENOMINATOR))))

(let-syntax
    ((define-integer-coercion
       (sc-macro-transformer
	(lambda (form environment)
	  `(DEFINE (,(list-ref form 1) Q)
	     (COND ((RATNUM? Q)
		    (,(close-syntax (list-ref form 3) environment)
		     (RATNUM-NUMERATOR Q)
		     (RATNUM-DENOMINATOR Q)))
		   ((INT:INTEGER? Q) Q)
		   (ELSE
		    (ERROR:WRONG-TYPE-ARGUMENT Q
					       "real number"
					       ',(list-ref form 2)))))))))
  (define-integer-coercion rat:floor floor int:floor)
  (define-integer-coercion rat:ceiling ceiling int:ceiling)
  (define-integer-coercion rat:truncate truncate int:quotient)
  (define-integer-coercion rat:round round int:round))

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
      (error:bad-range-argument e 'EXPT)))

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
  (flo:= x (flo:round x)))

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
  (with-values (lambda () (flo:normalize x))
    (lambda (f e-p)
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
  (or (flonum? x) (rat:rational? x)))

(define (real:integer? x)
  (if (flonum? x) (flo:integer? x) ((copy rat:integer?) x)))

(define (real:exact? x)
  (and (not (flonum? x))
       (or (rat:rational? x)
	   (error:wrong-type-argument x false 'EXACT?))))

(define (real:zero? x)
  (if (flonum? x) (flo:zero? x) ((copy rat:zero?) x)))

(define (real:exact0= x)
  (and (not (flonum? x)) ((copy rat:zero?) x)))

(define (real:negative? x)
  (if (flonum? x) (flo:negative? x) ((copy rat:negative?) x)))

(define (real:positive? x)
  (if (flonum? x) (flo:positive? x) ((copy rat:positive?) x)))

(let-syntax
    ((define-standard-unary
       (sc-macro-transformer
	(lambda (form environment)
	  `(DEFINE (,(list-ref form 1) X)
	     (IF (FLONUM? X)
		 (,(close-syntax (list-ref form 2) environment) X)
		 (,(close-syntax (list-ref form 3) environment) X)))))))
  (define-standard-unary real:1+ (lambda (x) (flo:+ x flo:1)) (copy rat:1+))
  (define-standard-unary real:-1+ (lambda (x) (flo:- x flo:1)) (copy rat:-1+))
  (define-standard-unary real:negate flo:negate (copy rat:negate))
  (define-standard-unary real:invert (lambda (x) (flo:/ flo:1 x)) rat:invert)
  (define-standard-unary real:abs flo:abs rat:abs)
  (define-standard-unary real:square (lambda (x) (flo:* x x)) rat:square)
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
	  (error:wrong-type-argument q false 'INEXACT->EXACT)))))

(let-syntax
    ((define-standard-binary
       (sc-macro-transformer
	(lambda (form environment)
	  (let ((flo:op (close-syntax (list-ref form 2) environment))
		(rat:op (close-syntax (list-ref form 3) environment)))
	    `(DEFINE (,(list-ref form 1) X Y)
	       (IF (FLONUM? X)
		   (IF (FLONUM? Y)
		       (,flo:op X Y)
		       (,flo:op X (RAT:->INEXACT Y)))
		   (IF (FLONUM? Y)
		       (,flo:op (RAT:->INEXACT X) Y)
		       (,rat:op X Y)))))))))
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
    rat:simplest-rational))

(define (real:= x y)
  (if (flonum? x)
      (if (flonum? y)
	  (flo:= x y)
	  (rat:= (flo:->rational x) y))
      (if (flonum? y)
	  (rat:= x (flo:->rational y))
	  ((copy rat:=) x y))))

(define (real:< x y)
  (if (flonum? x)
      (if (flonum? y)
	  (flo:< x y)
	  (rat:< (flo:->rational x) y))
      (if (flonum? y)
	  (rat:< x (flo:->rational y))
	  ((copy rat:<) x y))))

(define (real:max x y)
  (if (flonum? x)
      (if (flonum? y)
	  (if (flo:< x y) y x)
	  (if (rat:< (flo:->rational x) y) (rat:->inexact y) x))
      (if (flonum? y)
	  (if (rat:< x (flo:->rational y)) y (rat:->inexact x))
	  (if (rat:< x y) y x))))

(define (real:min x y)
  (if (flonum? x)
      (if (flonum? y)
	  (if (flo:< x y) x y)
	  (if (rat:< (flo:->rational x) y) x (rat:->inexact y)))
      (if (flonum? y)
	  (if (rat:< x (flo:->rational y)) (rat:->inexact x) y)
	  (if (rat:< x y) x y))))

(define (real:* x y)
  (cond ((flonum? x)
	 (cond ((flonum? y) (flo:* x y))
	       ((rat:zero? y) y)
	       (else (flo:* x (rat:->inexact y)))))
	((rat:zero? x) x)
	((flonum? y) (flo:* (rat:->inexact x) y))
	(else ((copy rat:*) x y))))

(define (real:/ x y)
  (cond ((flonum? x) (flo:/ x (if (flonum? y) y (rat:->inexact y))))
	((flonum? y) (if (rat:zero? x) x (flo:/ (rat:->inexact x) y)))
	(else ((copy rat:/) x y))))

(define (real:even? n)
  ((copy int:even?)
   (if (flonum? n)
       (if (flo:integer? n)
	   (flo:->integer n)
	   (error:wrong-type-argument n false 'EVEN?))
       n)))

(let-syntax
    ((define-integer-binary
      (sc-macro-transformer
       (lambda (form environment)
	 (let ((operator (close-syntax (list-ref form 3) environment))
	       (flo->int
		(lambda (n)
		  `(IF (FLO:INTEGER? ,n)
		       (FLO:->INTEGER ,n)
		       (ERROR:WRONG-TYPE-ARGUMENT ,n "integer"
						  ',(list-ref form 2))))))
	   `(DEFINE (,(list-ref form 1) N M)
	      (IF (FLONUM? N)
		  (INT:->INEXACT
		   (,operator ,(flo->int 'N)
			      (IF (FLONUM? M)
				  ,(flo->int 'M)
				  M)))
		  (IF (FLONUM? M)
		      (INT:->INEXACT (,operator N ,(flo->int 'M)))
		      (,operator N M)))))))))
  (define-integer-binary real:quotient quotient int:quotient)
  (define-integer-binary real:remainder remainder int:remainder)
  (define-integer-binary real:modulo modulo int:modulo)
  (define-integer-binary real:integer-floor integer-floor int:floor)
  (define-integer-binary real:integer-ceiling integer-ceiling int:ceiling)
  (define-integer-binary real:integer-round integer-round int:round)
  (define-integer-binary real:divide integer-divide int:divide)
  (define-integer-binary real:gcd gcd int:gcd)
  (define-integer-binary real:lcm lcm int:lcm))

(let-syntax
    ((define-rational-unary
      (sc-macro-transformer
       (lambda (form environment)
	 (let ((operator (close-syntax (list-ref form 2) environment)))
	   `(DEFINE (,(list-ref form 1) Q)
	      (IF (FLONUM? Q)
		  (RAT:->INEXACT (,operator (FLO:->RATIONAL Q)))
		  (,operator Q))))))))
  (define-rational-unary real:numerator rat:numerator)
  (define-rational-unary real:denominator rat:denominator))

(let-syntax
    ((define-transcendental-unary
      (sc-macro-transformer
       (lambda (form environment)
	 `(DEFINE (,(list-ref form 1) X)
	    (IF (,(close-syntax (list-ref form 2) environment) X)
		,(close-syntax (list-ref form 3) environment)
		(,(close-syntax (list-ref form 4) environment)
		 (REAL:->INEXACT X))))))))
  (define-transcendental-unary real:exp real:exact0= 1 flo:exp)
  (define-transcendental-unary real:log real:exact1= 0 flo:log)
  (define-transcendental-unary real:sin real:exact0= 0 flo:sin)
  (define-transcendental-unary real:cos real:exact0= 1 flo:cos)
  (define-transcendental-unary real:tan real:exact0= 0 flo:tan)
  (define-transcendental-unary real:asin real:exact0= 0 flo:asin)
  (define-transcendental-unary real:acos real:exact1= 0 flo:acos)
  (define-transcendental-unary real:atan real:exact0= 0 flo:atan))

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
		      (error:divide-by-zero 'EXPT (list x y))))
		 ((and (flo:negative? x)
		       (not (flo:integer? y)))
		  (error:bad-range-argument x 'EXPT))
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
			(flo:/ flo:1 (exact-method (int:negate y))))
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

(define (complex:real-arg name x)
  (if (recnum? x) (rec:real-arg name x) x))

(define (rec:real-arg name x)
  (if (not (real:zero? (rec:imag-part x)))
      (error:wrong-type-argument x false name))
  (rec:real-part x))

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
      (real:positive? (rec:real-arg 'POSITIVE? x))
      ((copy real:positive?) x)))

(define (complex:negative? x)
  (if (recnum? x)
      (real:negative? (rec:real-arg 'NEGATIVE? x))
      ((copy real:negative?) x)))

(define (complex:even? x)
  (if (recnum? x) (real:even? (rec:real-arg 'EVEN? x)) ((copy real:even?) x)))

(define (complex:max x y)
  (if (recnum? x)
      (if (recnum? y)
	  (real:max (rec:real-arg 'MAX x) (rec:real-arg 'MAX y))
	  (real:max (rec:real-arg 'MAX x) y))
      (if (recnum? y)
	  (real:max x (rec:real-arg 'MAX y))
	  ((copy real:max) x y))))

(define (complex:min x y)
  (if (recnum? x)
      (if (recnum? y)
	  (real:min (rec:real-arg 'MIN x) (rec:real-arg 'MIN y))
	  (real:min (rec:real-arg 'MIN x) y))
      (if (recnum? y)
	  (real:min x (rec:real-arg 'MIN y))
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
	 (error:wrong-type-argument z false 'CONJUGATE))))

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
  (if (recnum? x) (real:abs (rec:real-arg 'ABS x)) ((copy real:abs) x)))

(define (complex:quotient n d)
  (real:quotient (complex:real-arg 'QUOTIENT n)
		 (complex:real-arg 'QUOTIENT d)))

(define (complex:remainder n d)
  (real:remainder (complex:real-arg 'REMAINDER n)
		  (complex:real-arg 'REMAINDER d)))

(define (complex:modulo n d)
  (real:modulo (complex:real-arg 'MODULO n)
	       (complex:real-arg 'MODULO d)))

(define (complex:integer-floor n d)
  (real:integer-floor (complex:real-arg 'INTEGER-FLOOR n)
		      (complex:real-arg 'INTEGER-FLOOR d)))

(define (complex:integer-ceiling n d)
  (real:integer-ceiling (complex:real-arg 'INTEGER-CEILING n)
			(complex:real-arg 'INTEGER-CEILING d)))

(define (complex:integer-round n d)
  (real:integer-round (complex:real-arg 'INTEGER-ROUND n)
		      (complex:real-arg 'INTEGER-ROUND d)))

(define (complex:divide n d)
  (real:divide (complex:real-arg 'DIVIDE n)
	       (complex:real-arg 'DIVIDE d)))

(define (complex:gcd n m)
  (real:gcd (complex:real-arg 'GCD n)
	    (complex:real-arg 'GCD m)))

(define (complex:lcm n m)
  (real:lcm (complex:real-arg 'LCM n)
	    (complex:real-arg 'LCM m)))

(define (complex:numerator q)
  (real:numerator (complex:real-arg 'NUMERATOR q)))

(define (complex:denominator q)
  (real:denominator (complex:real-arg 'DENOMINATOR q)))

(define (complex:floor x)
  (if (recnum? x)
      (real:floor (rec:real-arg 'FLOOR x))
      ((copy real:floor) x)))

(define (complex:ceiling x)
  (if (recnum? x)
      (real:ceiling (rec:real-arg 'CEILING x))
      ((copy real:ceiling) x)))

(define (complex:truncate x)
  (if (recnum? x)
      (real:truncate (rec:real-arg 'TRUNCATE x))
      ((copy real:truncate) x)))

(define (complex:round x)
  (if (recnum? x)
      (real:round (rec:real-arg 'ROUND x))
      ((copy real:round) x)))

(define (complex:floor->exact x)
  (if (recnum? x)
      (real:floor->exact (rec:real-arg 'FLOOR->EXACT x))
      ((copy real:floor->exact) x)))

(define (complex:ceiling->exact x)
  (if (recnum? x)
      (real:ceiling->exact (rec:real-arg 'CEILING->EXACT x))
      ((copy real:ceiling->exact) x)))

(define (complex:truncate->exact x)
  (if (recnum? x)
      (real:truncate->exact (rec:real-arg 'TRUNCATE->EXACT x))
      ((copy real:truncate->exact) x)))

(define (complex:round->exact x)
  (if (recnum? x)
      (real:round->exact (rec:real-arg 'ROUND->EXACT x))
      ((copy real:round->exact) x)))

(define (complex:rationalize x e)
  (real:rationalize (complex:real-arg 'RATIONALIZE x)
		    (complex:real-arg 'RATIONALIZE e)))

(define (complex:rationalize->exact x e)
  (real:rationalize->exact (complex:real-arg 'RATIONALIZE x)
			   (complex:real-arg 'RATIONALIZE e)))

(define (complex:simplest-rational x y)
  (real:simplest-rational (complex:real-arg 'SIMPLEST-RATIONAL x)
			  (complex:real-arg 'SIMPLEST-RATIONAL y)))

(define (complex:simplest-exact-rational x y)
  (real:simplest-exact-rational (complex:real-arg 'SIMPLEST-RATIONAL x)
				(complex:real-arg 'SIMPLEST-RATIONAL y)))

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
	   (rec-case (rec:real-arg 'ATAN y) (complex:real-arg 'ATAN x)))
	  ((recnum? x)
	   (rec-case y (rec:real-arg 'ATAN x)))
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
  (let ((general-case
	 (lambda ()
	   (complex:exp (complex:* (complex:log z1) z2)))))
    (cond ((recnum? z1)
	   (if (and (rec:exact? z1)
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
		       (else 1)))
	       (general-case)))
	  ((or (recnum? z2)
	       (and (real:negative? z1)
		    (not (real:integer? z2))))
	   (general-case))
	  (else
	   (real:expt z1 z2)))))

(define (complex:make-rectangular real imag)
  (let ((check-arg
	 (lambda (x)
	   (if (recnum? x)
	       (rec:real-arg 'MAKE-RECTANGULAR x)
	       (begin
		 (if (not (real:real? x))
		     (error:wrong-type-argument x false 'MAKE-RECTANGULAR))
		 x)))))
    ((copy complex:%make-rectangular) (check-arg real) (check-arg imag))))

(define (complex:make-polar real imag)
  ((copy complex:%make-polar) (complex:real-arg 'MAKE-POLAR real)
			      (complex:real-arg 'MAKE-POLAR imag)))

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
	(else (error:wrong-type-argument z false 'REAL-PART))))

(define (complex:imag-part z)
  (cond ((recnum? z) (rec:imag-part z))
	((real:real? z) 0)
	(else (error:wrong-type-argument z false 'IMAG-PART))))

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

(define number? complex:complex?)
(define complex? complex:complex?)
(define real? complex:real?)
(define rational? complex:rational?)
(define integer? complex:integer?)
(define exact? complex:exact?)
(define exact-rational? rat:rational?)
(define exact-integer? int:integer?)

(define (exact-nonnegative-integer? object)
  (and (int:integer? object)
       (not (int:negative? object))))

(define (inexact? z)
  (not (complex:exact? z)))

;; Replaced with arity-dispatched version in INITIALIZE-PACKAGE!

(define =)
(define <)
(define >)
(define <=)
(define >=)

(define (reduce-comparator binary-comparator numbers procedure)
  (cond ((null? numbers)
	 true)
	((null? (cdr numbers))
	 (if (not (complex:complex? (car numbers)))
	     (error:wrong-type-argument (car numbers) false procedure))
	 true)
	(else
	 (let loop ((x (car numbers)) (rest (cdr numbers)))
	   (or (null? rest)
	       (let ((y (car rest)))
		 (and (binary-comparator x y)
		      (loop y (cdr rest)))))))))

(define zero? complex:zero?)
(define positive? complex:positive?)
(define negative? complex:negative?)

(define (odd? n)
  (not (complex:even? n)))

(define even? complex:even?)


;; Replaced with arity-dispatched version in INITIALIZE-PACKAGE!

(define +)
(define *)
(define -)
(define /)

(define max)
(define min)

(define (reduce-max/min max/min x1 xs procedure)
  (if (null? xs)
      (begin
	(if (not (complex:complex? x1))
	    (error:wrong-type-argument x1 false procedure))
	x1)
      (let loop ((x1 x1) (xs xs))
	(let ((x1 (max/min x1 (car xs)))
	      (xs (cdr xs)))
	  (if (null? xs)
	      x1
	      (loop x1 xs))))))

(define 1+ complex:1+)
(define -1+ complex:-1+)

(define conjugate complex:conjugate)

(define abs complex:abs)

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

(define integer-floor complex:integer-floor)
(define integer-ceiling complex:integer-ceiling)
(define integer-truncate complex:quotient)
(define integer-round complex:integer-round)
(define integer-divide complex:divide)
(define-integrable integer-divide-quotient car)
(define-integrable integer-divide-remainder cdr)

(define (gcd . integers)
  (fold-left complex:gcd 0 integers))

(define (lcm . integers)
  (fold-left complex:lcm 1 integers))

(define numerator complex:numerator)
(define denominator complex:denominator)
(define floor complex:floor)
(define ceiling complex:ceiling)
(define truncate complex:truncate)
(define round complex:round)
(define floor->exact complex:floor->exact)
(define ceiling->exact complex:ceiling->exact)
(define truncate->exact complex:truncate->exact)
(define round->exact complex:round->exact)
(define rationalize complex:rationalize)
(define rationalize->exact complex:rationalize->exact)
(define simplest-rational complex:simplest-rational)
(define simplest-exact-rational complex:simplest-exact-rational)
(define exp complex:exp)
(define log complex:log)
(define sin complex:sin)
(define cos complex:cos)
(define tan complex:tan)
(define asin complex:asin)
(define acos complex:acos)

(define (atan z #!optional x)
  (if (default-object? x)
      (complex:atan z)
      (complex:atan2 z x)))

(define sqrt complex:sqrt)
(define expt complex:expt)
(define make-rectangular complex:make-rectangular)
(define make-polar complex:make-polar)
(define real-part complex:real-part)
(define imag-part complex:imag-part)
(define magnitude complex:magnitude)
(define angle complex:angle)
(define exact->inexact complex:exact->inexact)
(define inexact->exact complex:inexact->exact)

(define (square z)
  (complex:* z z))

(define (number->string z #!optional radix)
  (complex:->string
   z
   (cond ((default-object? radix)
	  10)
	 ((and (exact-integer? radix)
	       (<= 2 radix 36))
	  radix)
	 ((and (pair? radix)
	       (eq? (car radix) 'HEUR)
	       (list? radix))
	  (parse-format-tail (cdr radix)))
	 (else
	  (error:bad-range-argument radix 'NUMBER->STRING)))))

(define (parse-format-tail tail)
  (let loop
      ((tail tail)
       (exactness-expressed false)
       (radix false)
       (radix-expressed false))
    (if (null? tail)
	(case radix ((B) 2) ((O) 8) ((#F D) 10) ((X) 16))
	(let ((modifier (car tail))
	      (tail (cdr tail)))
	  (let ((specify-modifier
		 (lambda (old)
		   (if old
		       (error "Respecification of format modifier"
			      (cadr modifier)))
		   (cadr modifier))))
	    (cond ((and (pair? modifier)
			(eq? (car modifier) 'EXACTNESS)
			(pair? (cdr modifier))
			(memq (cadr modifier) '(E S))
			(null? (cddr modifier)))
		   (if (eq? (cadr modifier) 'E)
		       (warn "NUMBER->STRING: ignoring exactness modifier"
			     modifier))
		   (loop tail
			 (specify-modifier exactness-expressed)
			 radix
			 radix-expressed))
		  ((and (pair? modifier)
			(eq? (car modifier) 'RADIX)
			(pair? (cdr modifier))
			(memq (cadr modifier) '(B O D X))
			(or (null? (cddr modifier))
			    (and (pair? (cddr modifier))
				 (memq (caddr modifier) '(E S))
				 (null? (cdddr modifier)))))
		   (if (and (pair? (cddr modifier))
			    (eq? (caddr modifier) 'E))
		       (warn
			"NUMBER->STRING: ignoring radix expression modifier"
			modifier))
		   (loop tail
			 exactness-expressed
			 (specify-modifier radix)
			 (if (null? (cddr modifier)) 'E (caddr modifier))))
		  (else
		   (error "Illegal format modifier" modifier))))))))
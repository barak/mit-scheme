#| -*-Scheme-*-

$Id: numpar.scm,v 14.22 2007/01/05 21:19:28 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

;;;; Number Parser
;;; package: (runtime number-parser)

(declare (usual-integrations))

(define (string->number string #!optional radix error?)
  (if (not (string? string))
      (error:wrong-type-argument string "string" 'STRING->NUMBER))
  (parse-number string 0 (string-length string) radix error? 'STRING->NUMBER))

(define (substring->number string start end #!optional radix error?)
  (if (not (string? string))
      (error:wrong-type-argument string "string" 'SUBSTRING->NUMBER))
  (if (not (index-fixnum? start))
      (error:wrong-type-argument start "string index" 'SUBSTRING->NUMBER))
  (if (not (index-fixnum? end))
      (error:wrong-type-argument end "string index" 'SUBSTRING->NUMBER))
  (if (not (fix:<= end (string-length string)))
      (error:bad-range-argument end 'SUBSTRING->NUMBER))
  (if (not (fix:<= start end))
      (error:bad-range-argument start 'SUBSTRING->NUMBER))
  (parse-number string start end radix error? 'SUBSTRING->NUMBER))

(define (parse-number string start end radix error? caller)
  (let ((z
	 (parse-number-1 string start end
			 (if (default-object? radix) #f radix)
			 caller)))
    (if (and (not z) (if (default-object? error?) #f error?))
	(error:bad-range-argument string caller))
    z))

(define (parse-number-1 string start end default-radix name)
  (if (not (or (eq? #f default-radix) (eq? 2 default-radix)
	       (eq? 8 default-radix) (eq? 10 default-radix)
	       (eq? 16 default-radix)))
      (error:bad-range-argument default-radix name))
  (let loop ((start start) (exactness #f) (radix #f))
    (and (fix:< start end)
	 (if (char=? #\# (string-ref string start))
	     (let ((start (fix:+ start 1)))
	       (and (fix:< start end)
		    (let ((char (string-ref string start))
			  (start (fix:+ start 1)))
		      (let ((do-radix
			     (lambda (r)
			       (and (not radix) (loop start exactness r))))
			    (do-exactness
			     (lambda (e)
			       (and (not exactness) (loop start e radix)))))
			(cond ((or (char=? #\b char) (char=? #\B char))
			       (do-radix 2))
			      ((or (char=? #\o char) (char=? #\O char))
			       (do-radix 8))
			      ((or (char=? #\d char) (char=? #\D char))
			       (do-radix 10))
			      ((or (char=? #\x char) (char=? #\X char))
			       (do-radix 16))
			      ((or (char=? #\e char) (char=? #\E char))
			       (do-exactness 'EXACT))
			      ((or (char=? #\i char) (char=? #\I char))
			       (do-exactness 'INEXACT))
			      (else #f))))))
	     (parse-top-level string start end exactness
			      (or radix default-radix))))))

(define (parse-top-level string start end exactness radix)
  (and (fix:< start end)
       (let ((char (string-ref string start))
	     (start (fix:+ start 1)))
	 (cond ((sign? char)
		(find-leader string start end
			     exactness (or radix 10)
			     char))
	       ((char=? #\. char)
		(and (or (not radix) (fix:= 10 radix))
		     (parse-decimal-1 string start end
				      (or exactness 'IMPLICIT-INEXACT) #f)))
	       ((char->digit char (or radix 10))
		=> (lambda (digit)
		     (parse-integer string start end digit
				    exactness (or radix 10) #f)))
	       (else #f)))))

(define (find-leader string start end exactness radix sign)
  ;; State: leading sign has been seen.
  (and (fix:< start end)
       (let ((char (string-ref string start))
	     (start (fix:+ start 1)))
	 (cond ((char->digit char radix)
		=> (lambda (digit)
		     (parse-integer string start end digit
				    exactness radix sign)))
	       ((char=? #\. char)
		(and (fix:= 10 radix)
		     (parse-decimal-1 string start end
				      (or exactness 'IMPLICIT-INEXACT) sign)))
	       ((i? char)
		(and (fix:= start end)
		     (if (eq? #\- sign) -i +i)))
	       (else #f)))))

(define (parse-integer string start end integer exactness radix sign)
  ;; State: at least one digit has been seen.
  (parse-digits string start end integer exactness radix
    (lambda (start integer exactness sharp?)
      (if (fix:< start end)
	  (let ((char (string-ref string start))
		(start+1 (fix:+ start 1)))
	    (cond ((char=? #\/ char)
		   (parse-denominator-1 string start+1 end
					integer exactness radix sign))
		  ((char=? #\. char)
		   (and (fix:= radix 10)
			(if sharp?
			    (parse-decimal-3 string start+1 end
					     integer 0 exactness sign)
			    (parse-decimal-2 string start+1 end
					     integer 0
					     (or exactness 'IMPLICIT-INEXACT)
					     sign))))
		  ((exponent-marker? char)
		   (and (fix:= radix 10)
			(parse-exponent-1 string start+1 end
					  integer 0
					  (or exactness 'IMPLICIT-INEXACT)
					  sign)))
		  (else
		   (parse-complex string start end
				  (finish-integer integer exactness sign)
				  exactness radix sign))))
	  (finish-integer integer exactness sign)))))

(define (parse-digits string start end integer exactness radix k)
  (let loop ((start start) (integer integer))
    (if (fix:< start end)
	(let ((char (string-ref string start)))
	  (cond ((char->digit char radix)
		 => (lambda (digit)
		      (loop (fix:+ start 1)
			    (+ (* integer radix) digit))))
		((char=? #\# char)
		 (do ((start (fix:+ start 1) (fix:+ start 1))
		      (integer (* integer radix) (* integer radix)))
		     ((not (and (fix:< start end)
				(char=? #\# (string-ref string start))))
		      (k start integer (or exactness 'IMPLICIT-INEXACT) #t))))
		(else
		 (k start integer exactness #f))))
	(k start integer exactness #f))))

(define (parse-denominator-1 string start end numerator exactness radix sign)
  ;; State: numerator parsed, / seen.
  (let ((finish
	 (lambda (denominator exactness sign)
	   (finish-rational numerator denominator exactness sign))))
    (parse-digits string start end 0 exactness radix
      (lambda (start* integer exactness sharp?)
	sharp?
	(and (> start* start) ; >0 denominator digits 
	     (parse-complex string start* end
			    (finish integer exactness sign)
			    exactness radix sign))))))

(define (parse-decimal-1 string start end exactness sign)
  ;; State: radix is 10, leading dot seen.
  (and (fix:< start end)
       (let ((digit (char->digit (string-ref string start) 10))
	     (start (fix:+ start 1)))
	 (and digit
	      (parse-decimal-2 string start end digit -1 exactness sign)))))

(define (parse-decimal-2 string start end integer exponent exactness sign)
  ;; State: radix is 10, dot seen.
  (let loop ((start start) (integer integer) (exponent exponent))
    (if (fix:< start end)
	(let ((char (string-ref string start))
	      (start+1 (fix:+ start 1)))
	  (cond ((char->digit char 10)
		 => (lambda (digit)
		      (loop start+1
			    (+ (* integer 10) digit)
			    (- exponent 1))))
		((char=? #\# char)
		 (parse-decimal-3 string start+1 end
				  integer exponent exactness sign))
		(else
		 (parse-decimal-4 string start end
				  integer exponent exactness sign))))
	(finish-real integer exponent exactness sign))))

(define (parse-decimal-3 string start end integer exponent exactness sign)
  ;; State: radix is 10, dot and # seen.
  (let loop ((start start))
    (if (fix:< start end)
	(let ((char (string-ref string start))
	      (start+1 (fix:+ start 1)))
	  (if (char=? #\# char)
	      (loop start+1)
	      (parse-decimal-4 string start end
			       integer exponent exactness sign)))
	(finish-real integer exponent exactness sign))))

(define (parse-decimal-4 string start end integer exponent exactness sign)
  (if (exponent-marker? (string-ref string start))
      (parse-exponent-1 string (fix:+ start 1) end
			integer exponent exactness sign)
      (parse-decimal-5 string start end integer exponent exactness sign)))

(define (parse-exponent-1 string start end integer exponent exactness sign)
  ;; State: radix is 10, exponent seen.
  (define (get-digits start esign)
    (and (fix:< start end)
	 (let ((digit (char->digit (string-ref string start) 10)))
	   (and digit
		(let loop ((start (fix:+ start 1)) (eint digit))
		  (if (fix:< start end)
		      (let ((digit
			     (char->digit (string-ref string start) 10)))
			(if digit
			    (loop (fix:+ start 1)
				  (+ (* eint 10) digit))
			    (continue start eint esign)))
		      (continue start eint esign)))))))

  (define (continue start eint esign)
    (let ((exponent (+ exponent (if (eq? #\- esign) (- eint) eint))))
      (if (fix:= start end)
	  (finish-real integer exponent exactness sign)
	  (parse-decimal-5 string start end
			   integer exponent exactness sign))))
			   
  
  (and (fix:< start end)
       (let ((esign (string-ref string start)))
	 (if (sign? esign)
	     (get-digits (fix:+ start 1) esign)
	     (get-digits start #f)))))

(define (parse-decimal-5 string start end integer exponent exactness sign)
  (parse-complex string start end
		 (finish-real integer exponent exactness sign)
		 exactness 10 sign))

(define (parse-complex string start end real exactness radix sign)
  (if (fix:< start end)
      (let ((char (string-ref string start))
	    (start+1 (fix:+ start 1))
	    (exactness (if (eq? 'IMPLICIT-INEXACT exactness) #f exactness)))
	(cond ((sign? char)
	       (let ((imaginary
		      (parse-top-level string start end exactness radix)))
		 (and (complex? imaginary)
		      (= 0 (real-part imaginary))
		      (+ real imaginary))))
	      ((char=? #\@ char)
	       (let ((angle
		      (parse-top-level string start+1 end exactness radix)))
		 (and (real? angle)
		      (make-polar real angle))))
	      ((i? char)
	       (and sign
		    (fix:= start+1 end)
		    (make-rectangular 0 real)))
	      (else #f)))
      real))

(define (finish-integer integer exactness sign)
  ;; State: result is integer, apply exactness and sign.
  (finish integer exactness sign))

(define (finish-rational numerator denominator exactness sign)
  ;; State: result is rational, apply exactness and sign.
  (finish (/ numerator denominator) exactness sign))

;; (finish-real integer exponent exactness sign)
;;
;;    magnitude is (* INTEGER (EXPT 10 EXPONENT))
;;
;; In the general case for an inexact result, to obtain a correctly
;; rounded result, it is necessary to work with exact or high
;; precision numbers and convert to the rounded result at the last
;; moment.
;;
;; Sometimes flonum arithmetic is sufficient to obtain a correct result.
;; This is true when all the operations are known, by properties of
;; the numbers they operate on, to give exact results, except possibly
;; for the final operation which must then round correctly.
;;
;; Certain integers can be represented exactly by floating point numbers,
;; for example, IEEE 64 bit fp numbers can represent the integers 0
;; through 9007199254740991 (lets call these floating point integers),
;; and powers of 10 from 10^0 up to 10^22 (because 5^22 =
;; 2384185791015625 < 9007199254740991).
;;
;; This means that all 15 and fewer digit numbers and 90% of 16 digit
;; numbers with relatively small exponents can be converted correctly
;; using flonum arithmetic.
;;
;; (INTEGER->FLONUM N #b01) acts as both a conversion and a predicate for
;; integers that are also floating point integers.  (It might be
;; useful to have an extra flag that tests for N being a floating
;; point integer scaled by a power of two, e.g. 10^20.)
;;
;; Reciprocals of powers of 10 cannot be represented exactly as floating
;; point numbers because 1/10 is a continued fraction in binary.
;; Instead of
;;    (* INTEGER (EXPT 10 EXPONENT))
;; we compute
;;    (/ INTEGER (EXPT 10 (- EXPONENT)))
;; This method also benfits accuracy when FLONUM-PARSER-FAST? is true and
;; the reciprocal is exact.

(define exact-flonum-powers-of-10)	; a vector, i -> 10.^i

(define (finish-real integer exponent exactness sign)
  ;; State: result is integer, apply exactness and sign.

  (define (high-precision-method)
    (apply-exactness exactness
		     (* (apply-sign sign integer)
			(expt 10 exponent))))
   
  (if (or (eq? 'INEXACT exactness) (eq? 'IMPLICIT-INEXACT exactness))
      (let ((abs-exponent (if (< exponent 0) (- exponent) exponent))
	    (powers-of-10 exact-flonum-powers-of-10))
	(define-integrable (finish-flonum x power-of-10)
	  (if (eq? #\- sign)
	      (if (eq? exponent abs-exponent)
		  (flo:- 0. (flo:* x power-of-10))
		  (flo:- 0. (flo:/ x power-of-10)))
	      (if (eq? exponent abs-exponent)
		  (flo:* x power-of-10)
		  (flo:/ x power-of-10))))
	(cond ((and flonum-parser-fast?
		    (<= abs-exponent 308)) ; this aught to be defined somewhere
	       (if (< abs-exponent (vector-length powers-of-10))
		   (finish-flonum (int:->flonum integer)
				  (vector-ref powers-of-10 abs-exponent))
		   (finish-flonum (int:->flonum integer)
				  (flo:expt 10. (int:->flonum abs-exponent)))))
	      ((and (< abs-exponent (vector-length powers-of-10))
		    ((ucode-primitive integer->flonum 2) integer #b1))
	       => (lambda (exact-flonum-integer)
		    (finish-flonum exact-flonum-integer
				   (vector-ref powers-of-10 abs-exponent))))
	      (else (high-precision-method))))
      (high-precision-method)))

(define flonum-parser-fast?
  #f)

(define (finish number exactness sign)
  (apply-sign sign (apply-exactness exactness number)))

(define (apply-sign sign number)
  (if (eq? #\- sign)
      (- number)
      number))

(define (apply-exactness exactness number)
  (if (or (eq? 'INEXACT exactness) (eq? 'IMPLICIT-INEXACT exactness))
      (exact->inexact number)
      number))

(define-integrable (exponent-marker? char)
  (or (char=? #\e char) (char=? #\E char)
      (char=? #\s char) (char=? #\S char)
      (char=? #\f char) (char=? #\F char)
      (char=? #\d char) (char=? #\D char)
      (char=? #\l char) (char=? #\L char)))

(define-integrable (sign? char)
  (or (char=? #\+ char) (char=? #\- char)))

(define-integrable (i? char)
  (or (char=? #\i char) (char=? #\I char)))

(define (initialize-package!)
  (set! exact-flonum-powers-of-10
	(let loop ((i 0) (power 1) (powers '()))
	  (if (= (inexact->exact (exact->inexact power)) power)
	      (loop (+ i 1) (* power 10) (cons (exact->inexact power) powers))
	      (list->vector (reverse! powers)))))
  unspecific)

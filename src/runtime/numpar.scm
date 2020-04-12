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

;;;; Number Parser
;;; package: (runtime number-parser)

(declare (usual-integrations))

(add-boot-deps! '(runtime number))

(define (string->number string #!optional radix error? start end)
  (let* ((caller 'string->number)
	 (end (fix:end-index end (string-length string) caller))
	 (start (fix:start-index start end caller))
	 (z
	  (parse-number string start end
			(if (default-object? radix) #f radix)
			caller)))
    (if (and (not z) (if (default-object? error?) #f error?))
	(error:bad-range-argument string caller))
    z))

(define (substring->number string start end #!optional radix error?)
  (string->number string radix error? start end))

(define (parse-number string start end default-radix name)
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
			       (do-exactness 'exact))
			      ((or (char=? #\i char) (char=? #\I char))
			       (do-exactness 'inexact))
			      (else #f))))))
	     (let ((radix (or radix default-radix))
		   (comp 'real))
	       (parse-top-level string start end exactness radix comp))))))

(define (parse-top-level string start end exactness radix comp)
  (and (fix:< start end)
       (let ((char (string-ref string start))
	     (start (fix:+ start 1)))
	 (if (sign? char)
	     (let ((sign char))
	       (find-leader string start end
			    exactness (or radix 10)
			    sign comp))
	     (let ((sign #f))
	       (cond ((char=? #\. char)
		      (and (or (not radix) (fix:= 10 radix) (fix:= #x10 radix))
			   (parse-dotted-1 string start end
					   (or exactness 'implicit-inexact)
					   (or radix 10)
					   sign comp)))
		     ((char->digit char (or radix 10))
		      => (lambda (digit)
			   (parse-integer string start end digit
					  exactness (or radix 10) sign comp)))
		     (else #f)))))))

(define (find-leader string start end exactness radix sign comp)
  ;; State: leading sign has been seen.
  (and (fix:< start end)
       (let ((char (string-ref string start))
	     (start (fix:+ start 1)))
	 (cond ((char->digit char radix)
		=> (lambda (digit)
		     (parse-integer string start end digit
				    exactness radix sign comp)))
	       ((char=? #\. char)
		(parse-dotted-1 string start end
				(or exactness 'implicit-inexact)
				radix sign comp))
	       ((and (char-ci=? #\i char)
		     (string-prefix-ci? "nf.0" string start end))
		(and (not (eq? exactness 'exact))
		     (parse-complex string (+ start 4) end
				    (if (eq? #\- sign)
					(flo:-inf.0)
					(flo:+inf.0))
				    exactness radix sign comp)))
	       ((and (char-ci=? #\n char)
		     (string-prefix-ci? "an." string start end))
                (parse-nan-payload string (+ start 3) end exactness radix
                                   #t sign comp))
	       ((and (char-ci=? #\s char)
		     (string-prefix-ci? "nan." string start end))
		(parse-nan-payload string (+ start 4) end exactness radix
				   #f sign comp))
	       ((i? char)
		(and (fix:= start end)
		     (let ((v (if (eq? #\- sign) -1 1)))
		       (case comp
			 ((real) (make-rectangular 0 v))
			 ((imag) v)
			 ((angle) #f)
			 (else (error "Invalid complex state:" comp))))))
	       (else #f)))))

(define (parse-integer string start end integer exactness radix sign comp)
  ;; State: at least one digit has been seen.
  (parse-digits string start end integer exactness radix
    (lambda (start integer exactness sharp?)
      (if (fix:< start end)
	  (let ((char (string-ref string start))
		(start+1 (fix:+ start 1)))
	    (cond ((char=? #\/ char)
		   (parse-denominator-1 string start+1 end
					integer exactness radix sign comp))
		  ((char=? #\. char)
		   (if sharp?
		       (parse-dotted-3 string start+1 end
					integer 0 exactness radix sign comp)
		       (parse-dotted-2 string start+1 end
				       integer 0
				       (or exactness 'implicit-inexact)
				       radix sign comp)))
		  ((exponent-marker? char)
		   ;; XXX Necessary to limit this to radix 10?
		   (and (fix:= radix 10)
			(parse-exponent-1 string start+1 end
					  integer 0
					  (or exactness 'implicit-inexact)
					  radix sign 10 comp)))
		  ((or (char=? #\p char) (char=? #\P char))
		   (parse-exponent-1 string start+1 end
				     integer 0
				     (or exactness 'implicit-inexact)
				     radix sign 2 comp))
		  (else
		   (parse-complex string start end
				  (finish-integer integer exactness sign)
				  exactness radix sign comp))))
	  (and (not (eq? comp 'imag))
	       (finish-integer integer exactness sign))))))

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
		      (k start integer (or exactness 'implicit-inexact) #t))))
		(else
		 (k start integer exactness #f))))
	(k start integer exactness #f))))

(define (parse-denominator-1 string start end numerator exactness radix sign
			     comp)
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
			    exactness radix sign comp))))))

(define (parse-dotted-1 string start end exactness radix sign comp)
  ;; State: leading dot seen.
  (and (fix:< start end)
       (let ((digit (char->digit (string-ref string start) radix))
	     (start (fix:+ start 1)))
	 (and digit
	      (parse-dotted-2 string start end digit -1 exactness radix
			      sign comp)))))

(define (parse-dotted-2 string start end integer rexponent exactness radix
			sign comp)
  ;; State: dot seen.
  (let loop ((start start) (integer integer) (rexponent rexponent))
    (if (fix:< start end)
	(let ((char (string-ref string start))
	      (start+1 (fix:+ start 1)))
	  (cond ((char->digit char radix)
		 => (lambda (digit)
		      (loop start+1
			    (+ (* integer radix) digit)
			    (- rexponent 1))))
		((char=? #\# char)
		 (parse-dotted-3 string start+1 end
				 integer rexponent exactness radix sign comp))
		(else
		 (parse-dotted-4 string start end
				 integer rexponent
				 exactness radix sign comp))))
	(and (not (eq? comp 'imag))
	     (finish-real integer rexponent exactness radix sign 10 0)))))

(define (parse-dotted-3 string start end integer rexponent exactness radix
			sign comp)
  ;; State: dot and # seen.
  (let loop ((start start))
    (if (fix:< start end)
	(let ((char (string-ref string start))
	      (start+1 (fix:+ start 1)))
	  (if (char=? #\# char)
	      (loop start+1)
	      (parse-dotted-4 string start end
			      integer rexponent exactness radix sign comp)))
	(and (not (eq? comp 'imag))
	     (finish-real integer rexponent exactness radix sign radix 0)))))

(define (parse-dotted-4 string start end integer rexponent exactness radix
			sign comp)
  (cond ((exponent-marker? (string-ref string start))
	 (and (fix:= radix 10)
	      (parse-exponent-1 string (fix:+ start 1) end
				integer rexponent
				exactness radix sign 10 comp)))
	((or (char=? #\p (string-ref string start))
	     (char=? #\P (string-ref string start)))
	 (and (fix:= radix #x10)
	      (parse-exponent-1 string (fix:+ start 1) end
				integer rexponent
				exactness radix sign 2 comp)))
	(else
	 (parse-dotted-5 string start end integer rexponent exactness radix
			 sign 10 0 comp))))

(define (parse-exponent-1 string start end integer rexponent exactness radix
			  sign base comp)
  ;; State: exponent seen.
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
    (let ((bexponent (if (eq? #\- esign) (- eint) eint)))
      (if (fix:= start end)
	  (and (not (eq? comp 'imag))
	       (finish-real integer rexponent exactness radix sign
			    base bexponent))
	  (parse-dotted-5 string start end integer rexponent exactness radix
			  sign base bexponent comp))))

  (and (fix:< start end)
       (let ((esign (string-ref string start)))
	 (if (sign? esign)
	     (get-digits (fix:+ start 1) esign)
	     (get-digits start #f)))))

(define (parse-dotted-5 string start end integer rexponent exactness radix
			sign base bexponent comp)
  (parse-complex string start end
		 (finish-real integer rexponent exactness radix sign
			      base bexponent)
		 exactness radix sign comp))

(define (parse-complex string start end real exactness radix sign comp)
  (if (fix:< start end)
      (let ((char (string-ref string start))
	    (start+1 (fix:+ start 1))
	    (exactness (if (eq? 'implicit-inexact exactness) #f exactness)))
	(cond ((i? char)
	       (and sign
		    (fix:= start+1 end)
		    (case comp
		      ((real) (make-rectangular 0 real))
		      ((imag) real)
		      ((angle) #f)
		      (else (error "Invalid complex state:" comp)))))
	      ((not (eq? comp 'real))
	       #f)
	      ((sign? char)
	       (let ((imaginary
		      (parse-top-level string start end exactness radix
				       'imag)))
		 (and imaginary
		      (begin
			(assert (real? imaginary))
			(make-rectangular real imaginary)))))
	      ((char=? #\@ char)
	       (let ((angle
		      (parse-top-level string start+1 end exactness radix
				       'angle)))
		 (and angle
		      (begin
			(assert (real? angle))
			(make-polar real angle)))))
	      (else #f)))
      (and (not (eq? comp 'imag)) real)))

(define (parse-nan-payload string start end exactness radix quiet? sign comp)
  (let loop ((payload 0) (start start))
    (define (finish-nan)
      (and (or quiet? (not (zero? payload)))
	   (not (eq? exactness 'exact))
	   (flo:make-nan (if (eq? sign #\-) #t #f) quiet? payload)))
    (if (fix:< start end)
        (let ((char (string-ref string start)))
          (cond ((char->digit char radix)
                 => (lambda (digit)
                      (loop (+ (* payload radix) digit) (fix:+ start 1))))
                ((finish-nan)
		 => (lambda (nan)
		      (parse-complex string start end nan
				     exactness radix sign comp)))
		(else #f)))
        (finish-nan))))

(define (finish-integer integer exactness sign)
  ;; State: result is integer, apply exactness and sign.
  (finish integer exactness sign))

(define (finish-rational numerator denominator exactness sign)
  ;; State: result is rational, apply exactness and sign.
  (finish (/ numerator denominator) exactness sign))

;; (finish-real integer rexponent exactness radix sign base bexponent)
;;
;;    magnitude is (* INTEGER (EXPT RADIX REXPONENT) (EXPT BASE BEXPONENT))
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

;; a vector, i -> 10.^i
(define-deferred exact-flonum-powers-of-10
  (let loop ((i 0) (power 1) (powers '()))
    (if (= (inexact->exact (exact->inexact power)) power)
	(loop (+ i 1) (* power 10) (cons (exact->inexact power) powers))
	(list->vector (reverse! powers)))))

(define (finish-real integer rexponent exactness radix sign base bexponent)
  ;; State: result is integer, apply exactness and sign.

  (define (high-precision-method)
    (apply-sign sign
		(apply-exactness exactness
				 (* integer
				    (expt radix rexponent)
				    (expt base bexponent)))))

  (if (and (fix:= radix 10)
	   (fix:= base 10)
	   (or (eq? 'inexact exactness) (eq? 'implicit-inexact exactness)))
      (let* ((exponent (+ rexponent bexponent))
	     (abs-exponent (if (< exponent 0) (- exponent) exponent))
	     (powers-of-10 exact-flonum-powers-of-10))
	(define-integrable (finish-flonum x power-of-10)
	  (if (eq? #\- sign)
	      (if (eq? exponent abs-exponent)
		  (flo:negate (flo:* x power-of-10))
		  (flo:negate (flo:/ x power-of-10)))
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
      (if (and (fix:power-of-two? radix)
	       (fix:power-of-two? base)
	       (or (eq? 'inexact exactness) (eq? 'implicit-inexact exactness)))
	  ;; x * r^re * b^be
	  ;; = x * 2^{log_2 r^re} * 2^{log_2 b^be}
	  ;; = x * 2^{re log_2 r + be log_2 b}
	  (let* ((log2r (fix:- (integer-length radix) 1))
		 (log2b (fix:- (integer-length base) 1)))
	    (let* ((e (fix:+ (fix:* rexponent log2r) (fix:* bexponent log2b)))
		   (x (flo:ldexp (int:->flonum integer) e)))
	      (if (eq? #\- sign)
		  (flo:negate x)
		  x)))
	  (high-precision-method))))

(define-integrable (fix:power-of-two? x)
  (fix:= 0 (fix:and x (fix:- x 1))))

(define flonum-parser-fast?
  #f)

(define (finish number exactness sign)
  (apply-sign sign (apply-exactness exactness number)))

(define (apply-sign sign number)
  (if (eq? #\- sign)
      ;; Kludge to work around miscompilation of (- number).
      (cond ((flo:flonum? number)
	     (flo:negate number))
	    ((and (complex? number) (not (real? number)))
	     (make-rectangular (apply-sign sign (real-part number))
			       (apply-sign sign (imag-part number))))
	    (else
	     (- number)))
      number))

#;
(define (apply-sign sign number)
  (if (eq? #\- sign)
      (- number)
      number))

(define (apply-exactness exactness number)
  (if (or (eq? 'inexact exactness) (eq? 'implicit-inexact exactness))
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
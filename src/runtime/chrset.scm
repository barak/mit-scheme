#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

;;;; Character Sets
;;; package: (runtime character-set)

(declare (usual-integrations))

;;; The character set is stored in two parts.  The LOW part is a bit-vector
;;; encoding of the code points below a limit.  The HIGH part is a sequence
;;; of code-point ranges, each of which has an inclusive START and an
;;; exclusive END.  The ranges in the sequence are all disjoint from one
;;; another, and no two ranges are adjacent.  These ranges are sorted so that
;;; their STARTs are in order.
;;;
;;; The HIGH range sequence is implemented as a u32 bytevector of alternating
;;; START and END points.  The vector always has an even number of points.

(define-record-type <char-set>
    (%make-char-set low high)
    char-set?
  (low %char-set-low)
  (high %char-set-high))

(define-integrable %low-cps-per-byte 8)

(define (%make-low low-limit)
  (make-bytevector (fix:quotient low-limit %low-cps-per-byte) 0))

(define (%low-limit low)
  (fix:lsh (bytevector-length low) 3))

(define (%low-ref low cp)
  (not (fix:= (fix:and (bytevector-u8-ref low (fix:lsh cp -3))
		       (fix:lsh 1 (fix:and cp 7)))
	      0)))

(define (%low-set! low cp)
  (bytevector-u8-set! low
		      (fix:lsh cp -3)
		      (fix:or (bytevector-u8-ref low (fix:lsh cp -3))
			      (fix:lsh 1 (fix:and cp 7)))))

(define-integrable %high-bytes-per-cp 3)

(define (%make-high n-cps)
  (make-bytevector (fix:* n-cps %high-bytes-per-cp)))

(define (%high-length high)
  (fix:quotient (bytevector-length high) %high-bytes-per-cp))

(define (%high-ref high index)
  (let ((i (fix:* index %high-bytes-per-cp)))
    (fix:+ (bytevector-u8-ref high i)
	   (fix:+ (fix:lsh (bytevector-u8-ref high (fix:+ i 1)) 8)
		  (fix:lsh (bytevector-u8-ref high (fix:+ i 2)) 16)))))

(define (%high-set! high index cp)
  (let ((i (fix:* index %high-bytes-per-cp)))
    (bytevector-u8-set! high i (fix:and cp #xFF))
    (bytevector-u8-set! high (fix:+ i 1) (fix:and (fix:lsh cp -8) #xFF))
    (bytevector-u8-set! high (fix:+ i 2) (fix:lsh cp -16))))

;;;; Signal codecs

;;; A signal is a list of integers in the range 0 <= N <= #x110000  The
;;; list has an even number of elements, and each element is strictly less than
;;; the succeeding element.  This is exactly the same format used for the HIGH
;;; vector, except in a list.

;;; All char-sets are constructed by %SIGNAL->CHAR-SET.
(define (%signal->char-set signal)
  (let ((low-limit (%choose-low-limit signal)))
    (%make-char-set (%signal->low signal low-limit)
		    (%signal->high signal low-limit))))

(define (%choose-low-limit signal)
  (let ((max-low-bytes (fix:quotient #x110000 %high-bytes-per-cp)))
    (let loop
	((low-bytes 1)
	 (best-low-bytes 0)
	 (best-total-bytes (%estimate-size 0 signal)))
      (if (fix:< low-bytes max-low-bytes)
	  (let ((total-bytes (%estimate-size low-bytes signal)))
	    (if (fix:< total-bytes best-total-bytes)
		(loop (fix:lsh low-bytes 1) low-bytes total-bytes)
		(loop (fix:lsh low-bytes 1) best-low-bytes best-total-bytes)))
	  (fix:* best-low-bytes %low-cps-per-byte)))))

(define (%estimate-size low-bytes signal)
  (fix:+ low-bytes
	 (let ((min-cp (fix:* low-bytes %low-cps-per-byte)))
	   (let loop ((signal signal))
	     (if (pair? signal)
		 (if (fix:< (cadr signal) min-cp)
		     (loop (cddr signal))
		     (fix:* (length signal) %high-bytes-per-cp))
		 0)))))

(define (%signal->low signal low-limit)
  (let ((low (%make-low low-limit)))

    (define (loop signal)
      (if (pair? signal)
	  (let ((start (car signal))
		(end (cadr signal)))
	    (cond ((fix:<= end low-limit)
		   (set-range! start end)
		   (loop (cddr signal)))
		  ((fix:< start low-limit)
		   (set-range! start low-limit))))))

    (define (set-range! start end)
      (do ((i start (fix:+ i 1)))
	  ((not (fix:< i end)))
	(%low-set! low i)))

    (loop signal)
    low))

(define (%signal->high signal low-limit)

  (define (skip-low signal)
    (cond ((not (pair? signal)) '())
	  ((fix:<= (cadr signal) low-limit) (skip-low (cddr signal)))
	  ((fix:< (car signal) low-limit) (cons low-limit (cdr signal)))
	  (else signal)))

  (let ((signal (skip-low signal)))
    (let ((high (%make-high (length signal))))
      (do ((signal signal (cdr signal))
	   (i 0 (fix:+ i 1)))
	  ((not (pair? signal)))
	(%high-set! high i (car signal)))
      high)))

(define (%char-set->signal char-set)
  (reverse!
   (%high->signal (%char-set-high char-set)
		  (%low->signal (%char-set-low char-set)))))

(define (%low->signal low)
  (let ((low-limit (%low-limit low)))

    (define (find-start i result)
      (if (fix:< i low-limit)
	  (if (%low-ref low i)
	      (find-end i result)
	      (find-start (fix:+ i 1) result))
	  result))

    (define (find-end start result)
      (let loop ((i (fix:+ start 1)))
	(if (fix:< i low-limit)
	    (if (%low-ref low i)
		(loop (fix:+ i 1))
		(find-start i (rcons start i result)))
	    (rcons start low-limit result))))

    (find-start 0 '())))

(define (%high->signal high result)
  (let ((n (%high-length high)))

    (define (loop i result)
      (if (fix:< i n)
	  (loop (fix:+ i 1)
		(cons (%high-ref high i) result))
	  result))

    (if (and (fix:> n 0)
	     (pair? result)
	     (fix:= (%high-ref high 0) (car result)))
	(loop 1 (cdr result))
	(loop 0 result))))

(define-integrable (scons start end signal)
  (cons start (cons end signal)))

(define-integrable (rcons start end signal)
  (cons end (cons start signal)))

(define (make-signal-combiner combine)

  (define (loop v start sig1 sig2 result)
    (cond ((not (pair? sig1)) (tail v 2 start sig2 result))
	  ((not (pair? sig2)) (tail v 1 start sig1 result))
	  (else
	   (let ((end (fix:min (car sig1) (car sig2))))
	     (let ((result* (process v start end result)))
	       (cond ((fix:> (car sig2) end)
		      (loop (fix:xor v 1)
			    end
			    (cdr sig1)
			    sig2
			    result*))
		     ((fix:> (car sig1) end)
		      (loop (fix:xor v 2)
			    end
			    sig1
			    (cdr sig2)
			    result*))
		     (else
		      (loop (fix:xor v 3)
			    end
			    (cdr sig1)
			    (cdr sig2)
			    result*))))))))

  (define (tail v vi start signal result)
    (if (pair? signal)
	(let ((end (car signal)))
	  (tail (fix:xor v vi)
		vi
		end
		(cdr signal)
		(process v start end result)))
	(reverse!
	 (if (fix:< start #x110000)
	     (process v start #x110000 result)
	     result))))

  (define (process v start end result)
    (if (and (fix:< start end)
	     (combine (fix:= 1 (fix:and v 1))
		      (fix:= 2 (fix:and v 2))))
	(if (and (pair? result)
		 (fix:= start (car result)))
	    (rcons (cadr result) end (cddr result))
	    (rcons start end result))
	result))

  (lambda (sig1 sig2)
    (loop 0 0 sig1 sig2 '())))

;;;; Constructors

(define (char-set . chars)
  (char-set* chars))

(define (char-set* cpl)
  (guarantee-list-of cpl-element? cpl 'char-set*)
  (char-set-union* (%cpl->char-sets cpl)))

(define (%cpl->char-sets cpl)
  (let loop ((cpl cpl) (ranges '()) (char-sets '()))
    (cond ((not (pair? cpl))
	   (cons (%ranges->char-set (%normalize-ranges ranges))
		 char-sets))
	  ((%cpl-element->ranges (car cpl))
	   => (lambda (ranges*)
		(loop (cdr cpl)
		      (append ranges* ranges)
		      char-sets)))
	  ((char-set? (car cpl))
	   (loop (cdr cpl)
		 ranges
		 (cons (car cpl) char-sets)))
	  (else
	   (error:not-a cpl-element? (car cpl))))))

(define (%cpl-element->ranges elt)
  (cond ((%range? elt) (list elt))
	((base-char? elt) (list (char->integer elt)))
	((ustring? elt) (map char->integer (ustring->list elt)))
	(else #f)))

(define (%normalize-ranges ranges)
  (let ((ranges
	 (filter! (lambda (range)
		    (fix:< (%range-start range)
			   (%range-end range)))
		  (sort ranges %range<?))))
    (if (pair? ranges)
	(let loop ((ranges ranges))
	  (if (pair? (cdr ranges))
	      (let ((s1 (%range-start (car ranges)))
		    (e1 (%range-end (car ranges)))
		    (s2 (%range-start (cadr ranges)))
		    (e2 (%range-end (cadr ranges))))
		(if (fix:< e1 s2)
		    (loop (cdr ranges))
		    (begin
		      (set-car! ranges (%make-range s1 (fix:max e1 e2)))
		      (set-cdr! ranges (cddr ranges))
		      (loop ranges)))))))
    ranges))

(define (compute-char-set procedure)

  (define (find-start cp end signal)
    (if (fix:< cp end)
	(if (procedure cp)
	    (find-end (fix:+ cp 1) end cp signal)
	    (find-start (fix:+ cp 1) end signal))
	signal))

  (define (find-end cp end start signal)
    (if (fix:< cp end)
	(if (procedure cp)
	    (find-end (fix:+ cp 1) end start signal)
	    (find-start (fix:+ cp 1) end (scons cp start signal)))
	(scons end start signal)))

  (%signal->char-set
   (reverse! (find-start #xE000 #x110000
			 (find-start 0 #xD800 '())))))

;;;; Code-point lists

(define (code-point-list? object)
  (list-of-type? object cpl-element?))

(define (cpl-element? object)
  (or (%range? object)
      (base-char? object)
      (ustring? object)
      (char-set? object)))

(define (%range? object)
  (or (and (pair? object)
	   (index-fixnum? (car object))
	   (index-fixnum? (cdr object))
           (fix:<= (cdr object) #x110000)
	   (fix:<= (car object) (cdr object)))
      (unicode-code-point? object)))

(define (%make-range start end)
  (if (fix:= (fix:- end start) 1)
      start
      (cons start end)))

(define (%range-start range)
  (if (pair? range)
      (car range)
      range))

(define (%range-end range)
  (if (pair? range)
      (cdr range)
      (fix:+ range 1)))

(define (%range<? range1 range2)
  (or (fix:< (%range-start range1)
	     (%range-start range2))
      (and (fix:= (%range-start range1)
		  (%range-start range2))
	   (fix:< (%range-end range1)
		  (%range-end range2)))))

(define (%ranges->char-set ranges)
  (let loop ((ranges ranges) (signal '()))
    (if (pair? ranges)
	(loop (cdr ranges)
	      (rcons (%range-start (car ranges))
		     (%range-end (car ranges))
		     signal))
	(%signal->char-set (reverse! signal)))))

;;;; Accessors

(define (char-in-set? char char-set)
  (guarantee base-char? char 'char-in-set?)
  (%code-point-in-char-set? (char->integer char) char-set))

(define (code-point-in-char-set? cp char-set)
  (guarantee unicode-code-point? cp 'code-point-in-char-set?)
  (%code-point-in-char-set? cp char-set))

(define (%code-point-in-char-set? cp char-set)
  (if (fix:< cp (%low-limit (%char-set-low char-set)))
      (%low-ref (%char-set-low char-set) cp)
      (let ((high (%char-set-high char-set)))
	(let loop ((lower 0) (upper (%high-length high)))
	  (if (fix:< lower upper)
	      (let ((i (fix:* 2 (fix:quotient (fix:+ lower upper) 4))))
		(cond ((fix:< cp (%high-ref high i))
		       (loop lower i))
		      ((fix:>= cp (%high-ref high (fix:+ i 1)))
		       (loop (fix:+ i 2) upper))
		      (else #t)))
	      #f)))))

(define (char-set-predicate char-set)
  (guarantee char-set? char-set 'CHAR-SET-PREDICATE)
  (lambda (char)
    (char-set-member? char-set char)))

(define (char-set=? char-set . char-sets)
  (every (lambda (char-set*)
	   (and (bytevector=? (%char-set-low char-set*)
			      (%char-set-low char-set))
		(bytevector=? (%char-set-high char-set*)
			      (%char-set-high char-set))))
	 char-sets))

(define (char-set->code-points char-set)
  (let loop ((signal (%char-set->signal char-set)) (ranges '()))
    (if (pair? signal)
	(loop (cddr signal)
	      (cons (%make-range (car signal) (cadr signal))
		    ranges))
	(reverse! ranges))))

;;;; Combinations

(define (char-set-invert char-set)
  (%signal->char-set (signal-invert (%char-set->signal char-set))))

(define (signal-invert signal)

  (define (loop start signal inverse)
    (if (pair? signal)
	(loop (cadr signal)
	      (cddr signal)
	      (rcons start (car signal) inverse))
	(reverse!
	 (if (fix:< start #x110000)
	     (rcons start #x110000 inverse)
	     inverse))))

  (if (pair? signal)
      (if (fix:< 0 (car signal))
	  (loop 0 signal '())
	  (loop (cadr signal) (cddr signal) '()))
      '()))

(define (char-set-union . char-sets)
  (char-set-union* char-sets))

(define (char-set-union* char-sets)
  (guarantee list? char-sets 'char-set-union*)
  (%signal->char-set
   (reduce signal-union
	   '()
	   (map %char-set->signal char-sets))))

(define (char-set-intersection . char-sets)
  (char-set-intersection* char-sets))

(define (char-set-intersection* char-sets)
  (guarantee list? char-sets 'char-set-intersection*)
  (%signal->char-set
   (reduce signal-intersection
	   '(0 #x110000)
	   (map %char-set->signal char-sets))))

(define (char-set-difference char-set . char-sets)
  (guarantee list? char-sets 'char-set-difference)
  (%signal->char-set
   (fold-left signal-difference
	      (%char-set->signal char-set)
	      (map %char-set->signal char-sets))))

(define signal-union
  (make-signal-combiner (lambda (a b) (or a b))))

(define signal-intersection
  (make-signal-combiner (lambda (a b) (and a b))))

(define signal-difference
  (make-signal-combiner (lambda (a b) (and a (not b)))))

;;;; Miscellaneous character sets

(define char-ctl?)
(define char-graphic?)
(define char-set:ascii)
(define char-set:ctls)
(define char-set:graphic)
(define char-set:newline)
(define char-set:not-graphic)
(define char-set:not-standard)
(define char-set:standard)
(define char-set:unicode)
(define char-set:wsp)
(define char-standard?)
(define char-wsp?)
(add-boot-init!
 (lambda ()
   (set! char-set:unicode (compute-char-set unicode-char-code?))

   (set! char-set:graphic (%signal->char-set '(#x20 #x7F #xA0 #x100)))
   (set! char-set:not-graphic (char-set-invert char-set:graphic))
   (set! char-graphic? (char-set-predicate char-set:graphic))

   (set! char-set:standard
	 (char-set-union char-set:graphic (char-set #\newline)))
   (set! char-set:not-standard (char-set-invert char-set:standard))
   (set! char-standard? (char-set-predicate char-set:standard))

   (set! char-set:newline (char-set #\newline))

   ;; Used in RFCs:

   (set! char-set:ascii (%signal->char-set '(#x00 #x80)))

   (set! char-set:ctls (%signal->char-set '(#x00 #x20 #x7F #x80)))
   (set! char-ctl? (char-set-predicate char-set:ctls))

   (set! char-set:wsp (char-set #\space #\tab))
   (set! char-wsp? (char-set-predicate char-set:wsp))

   unspecific))

;;;; Backwards compatibility

(define (char-set-member? char-set char)
  (char-in-set? char char-set))

(define (string->char-set string)
  (char-set* (map char->integer (string->list string))))

;; Returns ASCII string:
(define (char-set->string char-set)
  (list->string (char-set-members char-set)))

;; Returns only ASCII members:
(define (char-set-members char-set)
  (let loop ((cp 0))
    (if (fix:< cp #x80)
	(if (%code-point-in-char-set? cp char-set)
	    (cons (integer->char cp)
		  (loop (fix:+ cp 1)))
	    (loop (fix:+ cp 1)))
	'())))

(define (ascii-range->char-set start end)
  (if (not (index-fixnum? start))
      (error:wrong-type-argument start "index fixnum" 'ASCII-RANGE->CHAR-SET))
  (if (not (index-fixnum? end))
      (error:wrong-type-argument end "index fixnum" 'ASCII-RANGE->CHAR-SET))
  (if (not (fix:<= start end))
      (error:bad-range-argument start 'ASCII-RANGE->CHAR-SET))
  (if (not (fix:<= end #x100))
      (error:bad-range-argument end 'ASCII-RANGE->CHAR-SET))
  (char-set (cons start end)))

(define (%char-set-table char-set)
  (let ((table (make-vector-8b #x100)))
    (do ((cp 0 (fix:+ cp 1)))
	((not (fix:< cp #x100)))
      (vector-8b-set! table cp
		      (if (%code-point-in-char-set? cp char-set) 1 0)))
    table))

(define (8-bit-char-set? char-set)
  (and (char-set? char-set)
       (let ((high (%char-set-high char-set)))
	 (let ((he (%high-length high)))
	   (if (fix:> he 0)
	       (fix:<= (%high-ref high (fix:- he 1)) #x100)
	       (let ((low (%char-set-low char-set)))
		 (let ((le (bytevector-length low)))
		   (let loop ((i #x20))
		     (or (not (fix:< i le))
			 (and (fix:= 0 (bytevector-u8-ref low i))
			      (loop (fix:+ i 1))))))))))))

(define-guarantee 8-bit-char-set "an 8-bit char-set")
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
;;;
;;; For simplicity, character sets are allowed to contain any code point.
;;; However, CHAR-SET-MEMBER? only accepts scalar values.

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
(define-integrable %high-bytes-per-range 6)

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

;;;; Code-point lists

(define (code-point-list? object)
  (list-of-type? object cpl-element?))

(define (cpl-element? object)
  (or (%range? object)
      (unicode-char? object)
      (ustring? object)
      (char-set? object)))

(define (%range? object)
  (or (and (pair? object)
	   (index-fixnum? (car object))
	   (index-fixnum? (cdr object))
           (fix:<= (cdr object) char-code-limit)
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

;;;; Convert char-set to code-point list

(define (char-set->code-points char-set)
  (guarantee char-set? char-set 'char-set->code-points)
  (reverse!
   (%high->code-points (%char-set-high char-set)
		       (%low->code-points (%char-set-low char-set)))))

(define (%low->code-points low)
  (let ((low-limit (fix:* 8 (bytevector-length low))))

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
		(find-start i (cons (%make-range start i) result)))
	    (cons (%make-range start i) result))))

    (find-start 0 '())))

(define (%high->code-points high result)
  (let ((n (%high-length high)))
    (define (loop i result)
      (if (fix:< i n)
	  (loop (fix:+ i 2)
		(cons (%make-range (%high-ref high i)
				   (%high-ref high (fix:+ i 1)))
		      result))
	  result))

    (if (and (fix:> n 0)
	     (pair? result)
	     (fix:= (%high-ref high 0)
		    (%range-end (car result))))
	(loop 2
	      (cons (%make-range (%range-start (car result))
				 (%high-ref high 1))
		    (cdr result)))
	(loop 0 result))))

;;;; General char-set constructor

(define (char-set . chars)
  (char-set* chars))

(define (char-set* cpl)
  (guarantee-list-of cpl-element? cpl 'char-set*)
  (char-set-union* (%cpl->char-sets cpl)))

(define (%cpl->char-sets cpl)
  (let loop ((cpl cpl) (ranges '()) (char-sets '()))
    (cond ((not (pair? cpl))
	   (cons (%ranges->char-set (%canonicalize-ranges ranges))
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
	((unicode-char? elt) (list (char->integer elt)))
	((ustring? elt) (map char->integer (ustring->list elt)))
	(else #f)))

(define (%canonicalize-ranges ranges)
  ;; Sorts ranges in order, deletes empty ranges, then merges adjacent ranges.
  (let ((ranges
	 (filter! (lambda (range)
		    (fix:< (%range-start range)
			   (%range-end range)))
		  (sort ranges %range<?))))
    (if (pair? ranges)
	(let loop
	    ((start1 (%range-start (car ranges)))
	     (end1 (%range-end (car ranges)))
	     (ranges (cdr ranges))
	     (result '()))
	  (if (pair? ranges)
	      (let ((start2 (%range-start (car ranges)))
		    (end2 (%range-end (car ranges)))
		    (ranges (cdr ranges)))
		(if (fix:< end1 start2)
		    (loop start2
			  end2
			  ranges
			  (cons (%make-range start1 end1)
				result))
		    (loop start1
			  (fix:max end1 end2)
			  ranges
			  result)))
	      (reverse!
	       (cons (%make-range start1 end1)
		     result))))
	ranges)))

(define (%range<? range1 range2)
  (or (fix:< (%range-start range1)
	     (%range-start range2))
      (and (fix:= (%range-start range1)
		  (%range-start range2))
	   (fix:< (%range-end range1)
		  (%range-end range2)))))

(define (%ranges->char-set ranges)
  (let ((low-limit (%choose-low-limit ranges)))
    (%make-char-set (%ranges->low ranges low-limit)
		    (%ranges->high ranges low-limit))))

(define (%choose-low-limit ranges)
  (let ((max-low-bytes (fix:quotient char-code-limit %high-bytes-per-cp)))
    (let loop
	((low-bytes 1)
	 (best-low-bytes 0)
	 (best-total-bytes (%estimate-size 0 ranges)))
      (if (fix:< low-bytes max-low-bytes)
	  (let ((total-bytes (%estimate-size low-bytes ranges)))
	    (if (fix:< total-bytes best-total-bytes)
		(loop (fix:lsh low-bytes 1) low-bytes total-bytes)
		(loop (fix:lsh low-bytes 1) best-low-bytes best-total-bytes)))
	  (fix:* best-low-bytes 8)))))

(define (%estimate-size low-bytes ranges)
  (fix:+ low-bytes
	 (let ((min-cp (fix:* 8 low-bytes)))
	   (let loop ((ranges ranges))
	     (if (pair? ranges)
		 (let ((range (car ranges)))
		   (if (fix:< (%range-end range) min-cp)
		       (loop (cdr ranges))
		       (fix:* (length ranges) %high-bytes-per-range)))
		 0)))))

(define (%ranges->low ranges low-limit)
  (let ((low (%make-low low-limit)))

    (define (loop ranges)
      (if (pair? ranges)
	  (let ((start (%range-start (car ranges)))
		(end (%range-end (car ranges))))
	    (cond ((fix:<= end low-limit)
		   (set-range! start end)
		   (loop (cdr ranges)))
		  ((fix:< start low-limit)
		   (set-range! start low-limit))))))

    (define (set-range! start end)
      (do ((i start (fix:+ i 1)))
	  ((not (fix:< i end)))
	(%low-set! low i)))

    (loop ranges)
    low))

(define (%ranges->high ranges low-limit)

  (define (skip-low ranges)
    (if (pair? ranges)
	(let ((start (%range-start (car ranges)))
	      (end (%range-end (car ranges))))
	  (cond ((fix:<= end low-limit)
		 (skip-low (cdr ranges)))
		((fix:< start low-limit)
		 (cons (%make-range low-limit end) (cdr ranges)))
		(else
		 ranges)))
	'()))

  (let ((ranges (skip-low ranges)))
    (let ((high (%make-high (fix:* 2 (length ranges)))))
      (do ((ranges ranges (cdr ranges))
	   (i 0 (fix:+ i 2)))
	  ((not (pair? ranges)))
	(%high-set! high i (%range-start (car ranges)))
	(%high-set! high (fix:+ i 1) (%range-end (car ranges))))
      high)))

(define char-set:empty
  (%ranges->char-set '()))

(define char-set:full
  (%ranges->char-set (list (cons 0 char-code-limit))))

(define (compute-char-set procedure)
  (%ranges->char-set (%compute-ranges procedure)))

(define (%compute-ranges procedure)
  (append! (%compute-ranges-1 0 #xD800 procedure)
	   (%compute-ranges-1 #xE000 char-code-limit procedure)))

(define (%compute-ranges-1 start end procedure)

  (define (find-start cp ranges)
    (if (fix:< cp end)
	(if (procedure cp)
	    (find-end (fix:+ cp 1) cp ranges)
	    (find-start (fix:+ cp 1) ranges))
	(done ranges)))

  (define (find-end cp start ranges)
    (if (fix:< cp end)
	(if (procedure cp)
	    (find-end (fix:+ cp 1) start ranges)
	    (find-start (fix:+ cp 1)
			(cons (%make-range start cp) ranges)))
	(done (cons (%make-range start end) ranges))))

  (define (done ranges)
    (reverse! ranges))

  (find-start start '()))

;;;; Predicates

(define (char-in-set? char char-set)
  (guarantee unicode-char? char 'char-in-set?)
  (guarantee char-set? char-set 'char-in-set?)
  (%scalar-value-in-char-set? (char->integer char) char-set))

(define (scalar-value-in-char-set? sv char-set)
  (guarantee unicode-scalar-value? sv 'scalar-value-in-char-set?)
  (guarantee char-set? char-set 'scalar-value-in-char-set?)
  (%scalar-value-in-char-set? sv char-set))

(define (%scalar-value-in-char-set? sv char-set)
  (if (fix:< sv (%low-limit (%char-set-low char-set)))
      (%low-ref (%char-set-low char-set) sv)
      (let ((high (%char-set-high char-set)))
	(let loop ((lower 0) (upper (%high-length high)))
	  (if (fix:< lower upper)
	      (let ((i (fix:* 2 (fix:quotient (fix:+ lower upper) 4))))
		(cond ((fix:< sv (%high-ref high i))
		       (loop lower i))
		      ((fix:>= sv (%high-ref high (fix:+ i 1)))
		       (loop (fix:+ i 2) upper))
		      (else #t)))
	      #f)))))

(define (char-set-predicate char-set)
  (guarantee char-set? char-set 'CHAR-SET-PREDICATE)
  (lambda (char)
    (char-set-member? char-set char)))

(define (char-set=? char-set . char-sets)
  (guarantee char-set? char-set 'CHAR-SET=?)
  (guarantee-list-of char-set? char-sets 'CHAR-SET=?)
  (every (lambda (char-set*)
	   (%=? char-set* char-set))
	 char-sets))

(define (%=? c1 c2)
  (and (bytevector=? (%char-set-low c1) (%char-set-low c2))
       (bytevector=? (%char-set-high c1) (%char-set-high c2))))

;;;; Combinations

(define (char-set-invert char-set)
  (%ranges->char-set
   (let loop ((start 0) (rs (char-set->code-points char-set)))
     (if (pair? rs)
	 (cons (%make-range start (%range-start (car rs)))
	       (loop (%range-end (car rs)) (cdr rs)))
	 (if (fix:< start char-code-limit)
	     (list (%make-range start char-code-limit))
	     '())))))

(define (char-set-union . char-sets)
  (char-set-union* char-sets))

(define (char-set-union* char-sets)
  (guarantee list? char-sets 'char-set-union*)
  (%ranges->char-set
   (reduce ranges-union
	   char-set:empty
	   (map char-set->code-points char-sets))))

(define (char-set-intersection . char-sets)
  (char-set-intersection* char-sets))

(define (char-set-intersection* char-sets)
  (guarantee list? char-sets 'char-set-intersection*)
  (%ranges->char-set
   (reduce ranges-intersection
	   char-set:full
	   (map char-set->code-points char-sets))))

(define (char-set-difference char-set . char-sets)
  (guarantee list? char-sets 'char-set-difference)
  (%ranges->char-set
   (fold-left ranges-difference
	      (char-set->code-points char-set)
	      (map char-set->code-points char-sets))))

(define (make-ranges-combiner p1 p2 p12)

  (define (loop rs1 rs2)
    (cond ((null? rs1) (tail p2 rs2))
	  ((null? rs2) (tail p1 rs1))
	  (else
	   (let ((s1 (%range-start (car rs1)))
		 (e1 (%range-end (car rs1)))
		 (s2 (%range-start (car rs2)))
		 (e2 (%range-end (car rs2))))
	     (cond ((fix:<= e1 s2)
		    (p1 s1 e1 (loop (cdr rs1) rs2)))
		   ((fix:<= e2 s1)
		    (p2 s2 e2 (loop rs1 (cdr rs2))))
		   (else
		    (let ((s (fix:max s1 s2))
			  (e (fix:min e1 e2)))
		      (let ((k
			     (lambda ()
			       (p12 s e
				    (loop (maybe-push e e1 (cdr rs1))
					  (maybe-push e e2 (cdr rs2)))))))
			(cond ((fix:< s1 s) (p1 s1 s (k)))
			      ((fix:< s2 s) (p2 s2 s (k)))
			      (else (k)))))))))))

  (define (tail p rs)
    (if (null? rs)
	'()
	(p (%range-start (car rs))
	   (%range-end (car rs))
	   (tail p (cdr rs)))))

  (define (maybe-push s e rs)
    (if (fix:< s e)
	(cons (%make-range s e) rs)
	rs))

  loop)

(define ranges-union)
(define ranges-intersection)
(define ranges-difference)
(let ()

  (define (keep s e rs)
    (cons (%make-range s e) rs))

  (define (drop s e rs)
    (declare (ignore s e))
    rs)

  (define (join s e rs)
    (if (and (pair? rs) (fix:= e (%range-start (car rs))))
	(keep s (%range-end (car rs)) (cdr rs))
	(keep s e rs)))

  (set! ranges-union
	(make-ranges-combiner join join join))
  (set! ranges-intersection
	(make-ranges-combiner drop drop keep))
  (set! ranges-difference
	(make-ranges-combiner keep drop drop))
  unspecific)

;;;; Non-Unicode character sets

(define-deferred char-set:unicode
  (compute-char-set unicode-char-code?))

(define-deferred char-set:graphic
  (char-set* '((#x20 . #x7F) (#xA0 . #x100))))
(define-deferred char-set:not-graphic (char-set-invert char-set:graphic))
(define-deferred char-graphic? (char-set-predicate char-set:graphic))

(define-deferred char-set:standard
  (char-set-union char-set:graphic (char-set #\newline)))
(define-deferred char-set:not-standard (char-set-invert char-set:standard))
(define-deferred char-standard? (char-set-predicate char-set:standard))

(define-deferred char-set:newline
  (char-set #\newline))

;;; Used in RFCs:

(define-deferred char-set:ascii (char-set* '((#x00 . #x80))))

(define-deferred char-set:ctls (char-set* '((#x00 . #x20) #x7F)))
(define-deferred char-ctl? (char-set-predicate char-set:ctls))

(define-deferred char-set:wsp (char-set #\space #\tab))
(define-deferred char-wsp? (char-set-predicate char-set:wsp))

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
	(if (%scalar-value-in-char-set? cp char-set)
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
  (let ((table (make-vector-8b #x100))
	(low (%char-set-low char-set)))
    (do ((cp 0 (fix:+ cp 1)))
	((not (fix:< cp #x100)))
      (vector-8b-set! table cp
		      (if (%scalar-value-in-char-set? cp char-set) 1 0)))
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
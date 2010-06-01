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

;;;; Character Sets
;;; package: (runtime character-set)

(declare (usual-integrations))

;;; The character set is stored in two parts.  The LOW part is a
;;; bit-vector encoding of the scalar values below %LOW-LIMIT.  The
;;; HIGH part is a sequence of scalar-value ranges, each of which has
;;; an inclusive START and an exclusive END.  The ranges in the
;;; sequence are all disjoint from one another, and no two ranges are
;;; adjacent.  These ranges are sorted so that their STARTs are in
;;; order.
;;;
;;; The HIGH range sequence is implemented as a vector of alternating
;;; START and END points.  The vector always has an even number of
;;; points.
;;;
;;; For simplicity, character sets are allowed to contain ranges that
;;; contain illegal scalar values.  However, CHAR-SET-MEMBER? doesn't
;;; accept illegal characters.

(define-structure (char-set (type-descriptor <char-set>)
			    (constructor %%make-char-set)
			    (conc-name %char-set-))
  (low #f read-only #t)
  (high #f read-only #t)
  ;; Backwards compatibility:
  (table #f read-only #t))

(define-guarantee char-set "character set")

(define (guarantee-char-sets char-sets #!optional caller)
  (for-each (lambda (char-set) (guarantee-char-set char-set caller))
	    char-sets))

(define (%make-char-set low high)
  (%%make-char-set low high
		   (let ((table (make-vector-8b #x100)))
		     (do ((i 0 (fix:+ i 1)))
			 ((not (fix:< i #x100)))
		       (vector-8b-set! table i (if (%low-ref low i) 1 0)))
		     table)))

(define-integrable %low-length #x100)
(define-integrable %low-limit #x800)

(define (%make-low #!optional fill-value)
  (make-vector-8b %low-length fill-value))

(define (%low-ref low scalar-value)
  (not (fix:= (fix:and (vector-8b-ref low (fix:lsh scalar-value -3))
		       (fix:lsh 1 (fix:and scalar-value 7)))
	      0)))

(define (%low-set! low scalar-value)
  (vector-8b-set! low
		  (fix:lsh scalar-value -3)
		  (fix:or (vector-8b-ref low (fix:lsh scalar-value -3))
			  (fix:lsh 1 (fix:and scalar-value 7)))))

(define %null-char-set
  (%make-char-set (%make-low 0) '#()))

(define (8-bit-char-set? char-set)
  (and (char-set? char-set)
       (fix:= (vector-length (%char-set-high char-set)) 0)
       (let ((low (%char-set-low char-set)))
	 (let loop ((i #x20))
	   (or (fix:= i %low-length)
	       (and (fix:= (vector-8b-ref low i) 0)
		    (loop (fix:+ i 1))))))))

(define-guarantee 8-bit-char-set "an 8-bit char-set")

;;;; Conversion to and from scalar-values list

(define (well-formed-scalar-value-list? ranges)
  (list-of-type? ranges well-formed-scalar-value-range?))

(define (well-formed-scalar-value-range? range)
  (if (pair? range)
      (and (index-fixnum? (car range))
	   (index-fixnum? (cdr range))
	   (fix:<= (car range) (cdr range))
	   (fix:<= (cdr range) char-code-limit))
      (and (index-fixnum? range)
	   (fix:< range char-code-limit))))

(define-guarantee well-formed-scalar-value-list "a Unicode scalar-value list")
(define-guarantee well-formed-scalar-value-range "a Unicode scalar-value range")

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

(define (char-set->scalar-values char-set)
  (guarantee-char-set char-set 'CHAR-SET->SCALAR-VALUES)
  (reverse!
   (%high->scalar-values (%char-set-high char-set)
			 (%low->scalar-values (%char-set-low char-set)))))

(define (%low->scalar-values low)

  (define (find-start i result)
    (if (fix:< i %low-limit)
	(if (%low-ref low i)
	    (find-end i result)
	    (find-start (fix:+ i 1) result))
	result))

  (define (find-end start result)
    (let loop ((i (fix:+ start 1)))
      (if (fix:< i %low-limit)
	  (if (%low-ref low i)
	      (loop (fix:+ i 1))
	      (find-start i (cons (%make-range start i) result)))
	  (cons (%make-range start i) result))))

  (find-start 0 '()))

(define (%high->scalar-values high result)
  (let ((n (vector-length high)))
    (define (loop i result)
      (if (fix:< i n)
	  (loop (fix:+ i 2)
		(cons (%make-range (vector-ref high i)
				   (vector-ref high (fix:+ i 1)))
		      result))
	  result))

    (if (and (fix:> n 0)
	     (pair? result)
	     (fix:= (vector-ref high 0)
		    (%range-end (car result))))
	(loop 2
	      (cons (%make-range (%range-start (car result))
				 (vector-ref high 1))
		    (cdr result)))
	(loop 0 result))))

(define (scalar-values->char-set ranges)
  (guarantee-well-formed-scalar-value-list ranges 'SCALAR-VALUES->CHAR-SET)
  (%scalar-values->char-set ranges))

(define (%scalar-values->char-set ranges)
  (receive (low-ranges high-ranges)
      (%split-ranges (%canonicalize-scalar-value-list ranges))
    (%make-char-set (%scalar-values->low low-ranges)
		    (%scalar-values->high high-ranges))))

(define (%scalar-values->low ranges)
  (let ((low (%make-low 0)))
    (for-each (lambda (range)
		(let ((end (%range-end range)))
		  (do ((i (%range-start range) (fix:+ i 1)))
		      ((not (fix:< i end)))
		    (%low-set! low i))))
	      ranges)
    low))

(define (%scalar-values->high ranges)
  (let ((high (make-vector (fix:* 2 (length ranges)))))
    (do ((ranges ranges (cdr ranges))
	 (i 0 (fix:+ i 2)))
	((not (pair? ranges)))
      (vector-set! high i (%range-start (car ranges)))
      (vector-set! high (fix:+ i 1) (%range-end (car ranges))))
    high))

(define (%canonicalize-scalar-value-list ranges)
  ;; Sort ranges in order, then merge adjacent ranges.
  (if (pair? ranges)
      (let ((ranges (sort ranges %range<?)))
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
		     result)))))
      ranges))

(define (%range<? range1 range2)
  (or (fix:< (%range-start range1)
	     (%range-start range2))
      (and (fix:= (%range-start range1)
		  (%range-start range2))
	   (fix:< (%range-end range1)
		  (%range-end range2)))))

(define (%split-ranges ranges)
  ;; Caller doesn't care about order of LOW results, so don't reverse
  ;; on return.
  (let loop ((ranges ranges) (low '()))
    (if (pair? ranges)
	(let ((range (car ranges)))
	  (cond ((fix:<= (%range-end range) %low-limit)
		 (loop (cdr ranges) (cons range low)))
		((fix:>= (%range-start range) %low-limit)
		 (values low ranges))
		(else
		 (values (cons (%make-range (%range-start range) %low-limit)
			       low)
			 (cons (%make-range %low-limit (%range-end range))
			       (cdr ranges))))))
	(values low '()))))

;;;; Predicates

(define (char-set-member? char-set char)
  (guarantee-char-set char-set 'CHAR-SET-MEMBER?)
  (guarantee-unicode-char char 'CHAR-SET-MEMBER?)
  (%scalar-value-in-char-set? (char-code char) char-set))

(define (scalar-value-in-char-set? scalar-value char-set)
  (guarantee-unicode-scalar-value scalar-value 'SCALAR-VALUE-IN-CHAR-SET?)
  (guarantee-char-set char-set 'SCALAR-VALUE-IN-CHAR-SET?)
  (%scalar-value-in-char-set? scalar-value char-set))

(define (%scalar-value-in-char-set? value char-set)
  (if (fix:< value %low-limit)
      (%low-ref (%char-set-low char-set) value)
      (let ((high (%char-set-high char-set)))
	(let loop ((lower 0) (upper (vector-length high)))
	  (if (fix:< lower upper)
	      (let ((i (fix:* 2 (fix:quotient (fix:+ lower upper) 4))))
		(cond ((fix:< value (vector-ref high i))
		       (loop lower i))
		      ((fix:>= value (vector-ref high (fix:+ i 1)))
		       (loop (fix:+ i 2) upper))
		      (else #t)))
	      #f)))))

(define (char-set-predicate char-set)
  (guarantee-char-set char-set 'CHAR-SET-PREDICATE)
  (lambda (char)
    (char-set-member? char-set char)))

(define (char-set=? char-set . char-sets)
  (guarantee-char-set char-set 'CHAR-SET=?)
  (guarantee-char-sets char-sets 'CHAR-SET=?)
  (every (lambda (char-set*)
	   (%=? char-set* char-set))
	 char-sets))

(define (%=? c1 c2)
  (and (%=?-low (%char-set-low c1) (%char-set-low c2))
       (%=?-high (%char-set-high c1) (%char-set-high c2))))

(define (%=?-low l1 l2)
  (let loop ((i 0))
    (if (fix:< i %low-length)
	(and (fix:= (vector-8b-ref l1 i) (vector-8b-ref l2 i))
	     (loop (fix:+ i 1)))
	#t)))

(define (%=?-high h1 h2)
  (let ((end (vector-length h1)))
    (and (fix:= end (vector-length h2))
	 (let loop ((i 0))
	   (if (fix:< i end)
	       (and (fix:= (vector-ref h1 i) (vector-ref h2 i))
		    (loop (fix:+ i 1)))
	       #t)))))

;;;; Mapping operations

(define (char-set-invert char-set)
  (guarantee-char-set char-set 'CHAR-SET-INVERT)
  (%invert char-set))

(define (%invert cs1)
  (%make-char-set (%low-invert (%char-set-low cs1))
		  (%high-invert (%char-set-high cs1))))

(define (%low-invert low1)
  (let ((low (%make-low)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i %low-length))
      (vector-8b-set! low i
		      (fix:and (fix:not (vector-8b-ref low1 i))
			       #xff)))
    low))

(define (%high-invert high1)
  (let ((n1 (vector-length high1)))
    (if (fix:> n1 0)
	(let ((leading-flush?
	       (fix:= (vector-ref high1 0) %low-limit))
	      (trailing-flush?
	       (fix:= (vector-ref high1 (fix:- n1 1)) char-code-limit)))
	  (receive (start1 start)
	      (if leading-flush?
		  (values 1 0)
		  (values 0 1))
	    (let ((m (fix:+ start (fix:- n1 start1))))
	      (receive (end1 n)
		  (if trailing-flush?
		      (values (fix:- n1 1) (fix:- m 1))
		      (values n1 (fix:+ m 1)))
		(let ((high (make-vector n)))
		  (if (not leading-flush?)
		      (vector-set! high 0 %low-limit))
		  (subvector-move-left! high1 start1 end1 high start)
		  (if (not trailing-flush?)
		      (vector-set! high (fix:- n 1) char-code-limit))
		  high)))))
	(vector %low-limit char-code-limit))))

(define (char-set-union . char-sets)
  (guarantee-char-sets char-sets 'CHAR-SET-UNION)
  (reduce %union %null-char-set char-sets))

(define (%union cs1 cs2)
  (%binary fix:or
	   (lambda (a b) (or a b))
	   cs1
	   cs2))

(define (char-set-intersection . char-sets)
  (guarantee-char-sets char-sets 'CHAR-SET-INTERSECTION)
  (reduce %intersection %null-char-set char-sets))

(define (%intersection cs1 cs2)
  (%binary fix:and
	   (lambda (a b) (and a b))
	   cs1
	   cs2))

(define (char-set-difference char-set . char-sets)
  (guarantee-char-set char-set 'CHAR-SET-DIFFERENCE)
  (guarantee-char-sets char-sets 'CHAR-SET-DIFFERENCE)
  (fold-left %difference char-set char-sets))

(define (%difference cs1 cs2)
  (%binary fix:andc
	   (lambda (a b) (and a (not b)))
	   cs1
	   cs2))

(define (%binary low-operation high-operation cs1 cs2)
  (%make-char-set (%low-binary low-operation
			       (%char-set-low cs1)
			       (%char-set-low cs2))
		  (%high-binary high-operation
				(%char-set-high cs1)
				(%char-set-high cs2))))

(define (%low-binary operation low1 low2)
  (let ((low (%make-low)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i %low-length))
      (vector-8b-set! low i
		      (operation (vector-8b-ref low1 i)
				 (vector-8b-ref low2 i))))
    low))

(define (%high-binary operation high1 high2)
  (let ((n1 (vector-length high1))
	(n2 (vector-length high2)))
    (let ((high (make-vector (fix:+ n1 n2))))

      (define (loop i1 state1 i2 state2 last-state i)
	(cond ((not (fix:< i1 n1))
	       (let loop2
		   ((i2 i2)
		    (state2 state2)
		    (last-state last-state)
		    (i i))
		 (if (fix:< i2 n2)
		     (let ((this-point (vector-ref high2 i2))
			   (state2 (not state2)))
		       (let ((this-state (operation state1 state2)))
			 (loop2 (fix:+ i2 1) state2 this-state
				(accum this-point this-state last-state i))))
		     (finish last-state i))))
	      ((not (fix:< i2 n2))
	       (let loop1
		   ((i1 i1)
		    (state1 state1)
		    (last-state last-state)
		    (i i))
		 (if (fix:< i1 n1)
		     (let ((this-point (vector-ref high1 i1))
			   (state1 (not state1)))
		       (let ((this-state (operation state1 state2)))
			 (loop1 (fix:+ i1 1) state1 this-state
				(accum this-point this-state last-state i))))
		     (finish last-state i))))
	      (else
	       (let ((point1 (vector-ref high1 i1))
		     (point2 (vector-ref high2 i2)))
		 (receive (this-point i1 state1 i2 state2)
		     (cond ((fix:< point1 point2)
			    (values point1
				    (fix:+ i1 1) (not state1)
				    i2 state2))
			   ((fix:< point2 point1)
			    (values point2
				    i1 state1
				    (fix:+ i2 1) (not state2)))
			   (else
			    (values point1
				    (fix:+ i1 1) (not state1)
				    (fix:+ i2 1) (not state2))))
		   (let ((this-state (operation state1 state2)))
		     (loop i1 state1
			   i2 state2
			   this-state
			   (accum this-point this-state last-state i))))))))

      (define (accum this-point this-state last-state i)
	(if (boolean=? this-state last-state)
	    i
	    (begin
	      (vector-set! high i this-point)
	      (fix:+ i 1))))

      (define (finish last-state i)
	(vector-head! high
		      (if last-state
			  (if (fix:< (vector-ref high (fix:- i 1))
				     char-code-limit)
			      (begin
				(vector-set! high i char-code-limit)
				(fix:+ i 1))
			      (fix:- i 1))
			  i)))

      (loop 0 #f 0 #f #f 0))))

;;;; Standard character sets

(define-deferred char-set:upper-case
  (scalar-values->char-set '((#x41 . #x5B) (#xC0 . #xD7) (#xD8 . #xDE))))
(define-deferred char-set:not-upper-case (char-set-invert char-set:upper-case))
(define-deferred char-upper-case? (char-set-predicate char-set:upper-case))

(define-deferred char-set:lower-case
  (scalar-values->char-set '((#x61 . #x7B) (#xE0 . #xF7) (#xF8 . #xFF))))
(define-deferred char-set:not-lower-case (char-set-invert char-set:lower-case))
(define-deferred char-lower-case? (char-set-predicate char-set:lower-case))

(define-deferred char-set:numeric (scalar-values->char-set '((#x30 . #x3A))))
(define-deferred char-set:not-numeric (char-set-invert char-set:numeric))
(define-deferred char-numeric? (char-set-predicate char-set:numeric))

(define-deferred char-set:graphic
  (scalar-values->char-set '((#x20 . #x7F) (#xA0 . #x100))))
(define-deferred char-set:not-graphic (char-set-invert char-set:graphic))
(define-deferred char-graphic? (char-set-predicate char-set:graphic))

(define-deferred char-set:whitespace
  (char-set #\newline #\tab #\linefeed #\page #\return #\space
	    (integer->char #xA0)))
(define-deferred char-set:not-whitespace (char-set-invert char-set:whitespace))
(define-deferred char-whitespace? (char-set-predicate char-set:whitespace))

(define-deferred char-set:alphabetic
  (char-set-union char-set:upper-case char-set:lower-case))
(define-deferred char-set:not-alphabetic (char-set-invert char-set:alphabetic))
(define-deferred char-alphabetic? (char-set-predicate char-set:alphabetic))

(define-deferred char-set:alphanumeric
  (char-set-union char-set:alphabetic char-set:numeric))
(define-deferred char-set:not-alphanumeric
  (char-set-invert char-set:alphanumeric))
(define-deferred char-alphanumeric? (char-set-predicate char-set:alphanumeric))

(define-deferred char-set:standard
  (char-set-union char-set:graphic (char-set #\newline)))
(define-deferred char-set:not-standard (char-set-invert char-set:standard))
(define-deferred char-standard? (char-set-predicate char-set:standard))

(define-deferred char-set:newline
  (char-set #\newline))

;;; Used in RFCs:

(define-deferred char-set:ascii
  (scalar-values->char-set '((#x00 . #x80))))

(define-deferred char-set:ctls
  (scalar-values->char-set '((#x00 . #x20) #x7F)))
(define-deferred char-ctl? (char-set-predicate char-set:ctls))

(define-deferred char-set:wsp (char-set #\space #\tab))
(define-deferred char-wsp? (char-set-predicate char-set:wsp))

;;;; Backwards compatibility

(define (string->char-set string)
  (scalar-values->char-set (map char->integer (string->list string))))

(define (char-set->string char-set)
  (list->string (map integer->char (char-set-members char-set))))

(define (char-set-members char-set)
  (guarantee-char-set char-set 'CHAR-SET-MEMBERS)
  (let ((low (%char-set-low char-set)))
    (let loop ((code 0))
      (if (fix:< code #x100)
	  (if (%low-ref low code)
	      (cons (integer->char code)
		    (loop (fix:+ code 1)))
	      (loop (fix:+ code 1)))
	  '()))))

(define (char-set . chars)
  (for-each (lambda (char)
	      (guarantee-unicode-char char 'CHAR-SET))
	    chars)
  (%scalar-values->char-set (map char->integer chars)))

(define (chars->char-set chars)
  (guarantee-list-of-type chars unicode-char? "character" 'CHARS->CHAR-SET)
  (%scalar-values->char-set (map char->integer chars)))

(define (ascii-range->char-set start end)
  (if (not (index-fixnum? start))
      (error:wrong-type-argument start "index fixnum" 'ASCII-RANGE->CHAR-SET))
  (if (not (index-fixnum? end))
      (error:wrong-type-argument end "index fixnum" 'ASCII-RANGE->CHAR-SET))
  (if (not (fix:<= start end))
      (error:bad-range-argument start 'ASCII-RANGE->CHAR-SET))
  (if (not (fix:<= end #x100))
      (error:bad-range-argument end 'ASCII-RANGE->CHAR-SET))
  (%scalar-values->char-set (list (cons start (fix:- end 1)))))

(define (alphabet->char-set char-set)
  char-set)

(define (char-set->alphabet char-set)
  char-set)

(define (char-in-alphabet? char char-set)
  (char-set-member? char-set char))

(define (alphabet->scalar-values char-set)
  (map (lambda (range)
	 (if (pair? range)
	     (cons (car range)
		   (fix:- (cdr range) 1))
	     range))
       (char-set->scalar-values char-set)))

(define (scalar-values->alphabet ranges)
  (guarantee-well-formed-scalar-value-list ranges 'SCALAR-VALUES->ALPHABET)
  (%scalar-values->char-set
   (map (lambda (range)
	  (if (pair? range)
	      (cons (car range)
		    (if (fix:< (cdr range) char-code-limit)
			(fix:+ (cdr range) 1)
			(error:bad-range-argument (cdr range)
						  'SCALAR-VALUES->ALPHABET)))
	      range))
	ranges)))
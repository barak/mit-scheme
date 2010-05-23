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
;;; The HIGH range sequence is implemented as a pair of vectors, one
;;; for the STARTs and one for the ENDs.  The two vectors have the
;;; same length.
;;;
;;; For simplicity, character sets are allowed to contain ranges that
;;; contain illegal scalar values.  However, CHAR-SET-MEMBER? doesn't
;;; accept illegal characters.

(define-structure (char-set (type-descriptor <char-set>)
			    (constructor %%make-char-set))
  (low #f read-only #t)
  (high-starts #f read-only #t)
  (high-ends #f read-only #t)
  ;; Backwards compatibility:
  (table #f read-only #t))

(define-guarantee char-set "character set")

(define (guarantee-char-sets char-sets #!optional caller)
  (for-each (lambda (char-set) (guarantee-char-set char-set caller))
	    char-sets))

(define (%make-char-set low high-starts high-ends)
  (%%make-char-set low high-starts high-ends
		   (let ((table (make-vector-8b #x100)))
		     (do ((i 0 (fix:+ i 1)))
			 ((not (fix:< i #x100)))
		       (vector-8b-set! table i (%low-ref low char-set)))
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
  (%make-char-set (%make-low 0) '#() '#()))

;;;; Conversion to and from scalar-values list

(define (well-formed-scalar-value-list? ranges)
  (list-of-type? ranges well-formed-scalar-value-range?))

(define (well-formed-scalar-value-range? range)
  (if (pair? range)
      (and (index-fixnum? (car range))
	   (index-fixnum? (cdr range))
	   (fix:< (car range) (cdr range))
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
      (car range)
      (fix:+ range 1)))

(define (char-set->scalar-values char-set)
  (guarantee-char-set char-set 'CHAR-SET->SCALAR-VALUES)
  (reverse!
   (%high->scalar-values (char-set-high-starts char-set)
			 (char-set-high-ends char-set)
			 (%low->scalar-values (char-set-low char-set) '()))))

(define (%low->scalar-values low result)

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

  (find-start 0 result))

(define (%high->scalar-values starts ends result)
  (let ((n (vector-length starts)))
    (let loop ((i 0) (result result))
      (if (fix:< i n)
	  (loop (fix:+ i 1)
		(cons (%make-range (vector-ref starts i)
				   (vector-ref ends i))
		      result))
	  result))))

(define (scalar-values->char-set ranges)
  (guarantee-well-formed-scalar-value-list ranges 'SCALAR-VALUES->CHAR-SET)
  (%scalar-values->char-set ranges))

(define (%scalar-values->char-set ranges)
  (receive (low-ranges high-ranges)
      (%canonicalize-scalar-value-list ranges)
    (receive (high-starts high-ends)
	(%scalar-values->high high-ranges)
      (%make-char-set (%scalar-values->low low-ranges)
		      high-starts
		      high-ends))))

(define (%scalar-values->low ranges)
  (let ((low (%make-low 0)))
    (for-each (lambda (range)
		(let ((end (%range-end range)))
		  (do ((i (%range-start range) (fix:+ i 1)))
		      ((fix:> i end))
		    (%low-set! low i))))
	      ranges)
    low))

(define (%scalar-values->high ranges)
  (let ((n-high (length ranges)))
    (let ((high-starts (make-vector n-high))
	  (high-ends (make-vector n-high)))
      (do ((ranges ranges (cdr ranges))
	   (i 0 (fix:+ i 1)))
	  ((not (pair? ranges)))
	(vector-set! high-starts i (%range-start (car ranges)))
	(vector-set! high-ends i (%range-end (car ranges))))
      (values high-starts high-ends))))

(define (%canonicalize-scalar-value-list ranges)
  ;; Sort ranges in order, then merge adjacent ranges.
  (%split-ranges
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
       ranges)))

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

(define (%scalar-value-in-char-set? value char-set)
  (if (fix:< value %low-limit)
      (%low-ref (char-set-low char-set) value)
      (let ((high-starts (char-set-high-starts char-set))
	    (high-ends (char-set-high-ends char-set)))
	(let loop ((lower 0) (upper (vector-length high-starts)))
	  (if (fix:< lower upper)
	      (let ((index (fix:quotient (fix:+ lower upper) 2)))
		(cond ((fix:< value (vector-ref high-starts index))
		       (loop lower index))
		      ((fix:>= value (vector-ref high-ends index))
		       (loop (fix:+ index 1) upper))
		      (else #t)))
	      #f)))))

(define (char-set=? char-set . char-sets)
  (guarantee-char-set char-set 'CHAR-SET=?)
  (guarantee-char-sets char-sets 'CHAR-SET=?)
  (every (lambda (char-set*)
	   (%=? char-set* char-set))
	 char-sets))

(define (%=? c1 c2)
  (and (%=?-low (char-set-low c1) (char-set-low c2))
       (%=?-high (char-set-high-starts c1) (char-set-high-starts c2))
       (%=?-high (char-set-high-ends c1) (char-set-high-ends c2))))

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

;;;; 8-bit character sets

(define (8-bit-char-set? char-set)
  (and (char-set? char-set)
       (fix:= (vector-length (char-set-high-starts char-set)) 0)
       (let ((low (char-set-low char-set)))
	 (let loop ((i #x20))
	   (or (fix:= i %low-length)
	       (and (fix:= (vector-8b-ref low i) 0)
		    (loop (fix:+ i 1))))))))

(define-guarantee 8-bit-char-set "an 8-bit char-set")

;;;; Mapping operations

(define (char-set-invert char-set)
  (guarantee-char-set char-set 'CHAR-SET-INVERT)
  (%invert char-set))

(define-deferred %invert
  (%split-map-1 (%low-unary fix:not)
		%invert-high))

(define (%invert-high starts1 ends1)
  (let ((n1 (vector-length starts1)))

    (define (go n i1 prev-end)
      (let ((starts (make-vector n))
	    (ends (make-vector n)))
	(let loop ((i1 i1) (i 0) (prev-end prev-end))
	  (if (fix:< i1 n1)
	      (loop (fix:+ i1 1)
		    (%high-copy-1 (vector-ref starts1 i1)
				  (vector-ref ends1 i1)
				  starts ends i))
	      (%high-copy-1 prev-end char-code-limit
			    starts ends i)))
	(values starts ends)))

    (if (and (fix:> n1 0)
	     (fix:= (vector-ref starts1 0) %low-limit))
	(go n1 1 (vector-ref ends1 0))
	(go (fix:+ n1 1) 0 %low-limit))))

(define (char-set-union . char-sets)
  (guarantee-char-sets char-sets 'CHAR-SET-UNION)
  (reduce %union %null-char-set char-sets))

(define-deferred %union
  (%split-map-2 (%low-binary fix:or)
		(%high-binary %high-copy-n %high-copy-n
			      %high-copy-1 %high-copy-1
			      (lambda (start1 end1 start2 end2 starts ends i)
				(%high-copy-1 (fix:min start1 start2)
					      (fix:max end1 end2)
					      starts ends i)))))

(define (char-set-intersection . char-sets)
  (guarantee-char-sets char-sets 'CHAR-SET-INTERSECTION)
  (reduce %intersection %null-char-set char-sets))

(define-deferred %intersection
  (%split-map-2 (%low-binary fix:and)
		(%high-binary %high-drop-n %high-drop-n
			      %high-drop-1 %high-drop-1
			      (lambda (start1 end1 start2 end2 starts ends i)
				(%high-copy-1 (fix:max start1 start2)
					      (fix:min end1 end2)
					      starts ends i)))))

(define (char-set-difference char-set . char-sets)
  (guarantee-char-set char-set 'CHAR-SET-DIFFERENCE)
  (guarantee-char-sets char-sets 'CHAR-SET-DIFFERENCE)
  (fold-left %difference char-set char-sets))

(define-deferred %difference
  (%split-map-2 (%low-binary fix:andc)
		(%high-binary %high-drop-n %high-copy-n
			      %high-drop-1 %high-copy-1
			      (lambda (start1 end1 start2 end2 starts ends i)

				(define (shave-head i start1 start2)
				  (if (fix:< start1 start2)
				      (%high-copy-1 start1 start2
						    starts ends i)
				      i))

				(define (shave-tail i end1 end2)
				  (if (fix:< end2 end1)
				      (%high-copy-1 end2 end1
						    starts ends i)
				      i))
				(shave-tail (shave-head i start1 start2)
					    end1
					    end2)))))

;;;; Support for mapping operations

(define (%split-map-1 %map-low %map-high)
  (lambda (c1)
    (receive (high-starts high-ends)
	(%map-high (char-set-high-starts c1)
		   (char-set-high-ends c1))
      (%make-char-set (%map-low (char-set-low c1))
		      high-starts
		      high-ends))))

(define (%split-map-2 %map-low %map-high)
  (lambda (c1 c2)
    (receive (high-starts high-ends)
	(%map-high (char-set-high-starts c1)
		   (char-set-high-ends c1)
		   (char-set-high-starts c2)
		   (char-set-high-ends c2))
      (%make-char-set (%map-low (char-set-low c1)
				(char-set-low c2))
		      high-starts
		      high-ends))))

(define (%low-unary operation)
  (lambda (low1)
    (let ((low* (%make-low)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i %low-length))
	(vector-8b-set! low* i
			(operation (vector-8b-ref low1 i))))
      low*)))

(define (%low-binary operation)
  (lambda (low1 low2)
    (let ((low (%make-low)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i %low-length))
	(vector-8b-set! low i
			(operation (vector-8b-ref low1 i)
				   (vector-8b-ref low2 i))))
      low)))

(define (%high-binary empty-left empty-right
		      disjoint-left disjoint-right
		      overlap)
  (lambda (starts1 ends1 starts2 ends2)
    (let ((n1 (vector-length starts1))
	  (n2 (vector-length starts2)))
      (let ((starts (make-vector (fix:+ n1 n2)))
	    (ends (make-vector (fix:+ n1 n2))))
	(let ((n
	       (let loop ((i1 0) (i2 0) (i 0))
		 (cond ((fix:>= i1 n1)
			(empty-left starts2 ends2 i2 n2
				    starts ends i))
		       ((fix:>= i2 n2)
			(empty-right starts1 ends1 i1 n1
				     starts ends i))
		       (else
			(let ((start1 (vector-ref starts1 i1))
			      (end1 (vector-ref ends1 i1))
			      (start2 (vector-ref starts2 i2))
			      (end2 (vector-ref ends2 i2)))
			  (cond ((fix:< end1 start2)
				 (loop (fix:+ i1 1)
				       i2
				       (disjoint-left start1 end1
						      starts ends i)))
				((fix:< end2 start1)
				 (loop i1
				       (fix:+ i2 1)
				       (disjoint-right start2 end2
						       starts ends i)))
				(else
				 (loop (fix:+ i1 1)
				       (fix:+ i2 1)
				       (overlap start1 end1
						start2 end2
						starts ends i))))))))))
	  (values (vector-head! starts n)
		  (vector-head! ends n)))))))

(define (%high-copy-n starts1 ends1 i1 n1 starts ends i)
  (subvector-move-left! starts1 i1 n1 starts i)
  (subvector-move-left! ends1 i1 n1 ends i)
  (fix:+ i (fix:- n1 i1)))

(define (%high-drop-n starts1 ends1 i1 n1 starts ends i)
  starts1 ends1 i1 n1 starts ends
  i)

(define (%high-copy-1 start1 end1 starts ends i)
  (vector-set! starts i start1)
  (vector-set! ends i end1)
  (fix:+ i 1))

(define (%high-drop-1 start1 end1 starts ends i)
  start1 end1 starts ends
  i)

;;;; Standard character sets

(define-deferred char-set:upper-case
  (char-set-union (ascii-range->char-set #x41 #x5B)
		  (ascii-range->char-set #xC0 #xD7)
		  (ascii-range->char-set #xD8 #xDE)))
(define-deferred char-set:not-upper-case (char-set-invert char-set:upper-case))
(define-deferred char-upper-case? (%char-set-test char-set:upper-case))

(define-deferred char-set:lower-case
  (char-set-union (ascii-range->char-set #x61 #x7B)
		  (ascii-range->char-set #xE0 #xF7)
		  (ascii-range->char-set #xF8 #xFF)))
(define-deferred char-set:not-lower-case (char-set-invert char-set:lower-case))
(define-deferred char-lower-case? (%char-set-test char-set:lower-case))

(define-deferred char-set:numeric (ascii-range->char-set #x30 #x3A))
(define-deferred char-set:not-numeric (char-set-invert char-set:numeric))
(define-deferred char-numeric? (%char-set-test char-set:numeric))

(define-deferred char-set:graphic
  (char-set-union (ascii-range->char-set #x20 #x7F)
		  (ascii-range->char-set #xA0 #x100)))
(define-deferred char-set:not-graphic (char-set-invert char-set:graphic))
(define-deferred char-graphic? (%char-set-test char-set:graphic))

(define-deferred char-set:whitespace
  (char-set #\newline #\tab #\linefeed #\page #\return #\space
	    (integer->char #xA0)))
(define-deferred char-set:not-whitespace (char-set-invert char-set:whitespace))
(define-deferred char-whitespace? (%char-set-test char-set:whitespace))

(define-deferred char-set:alphabetic
  (char-set-union char-set:upper-case char-set:lower-case))
(define-deferred char-set:not-alphabetic (char-set-invert char-set:alphabetic))
(define-deferred char-alphabetic? (%char-set-test char-set:alphabetic))

(define-deferred char-set:alphanumeric
  (char-set-union char-set:alphabetic char-set:numeric))
(define-deferred char-set:not-alphanumeric
  (char-set-invert char-set:alphanumeric))
(define-deferred char-alphanumeric? (%char-set-test char-set:alphanumeric))

(define-deferred char-set:standard
  (char-set-union char-set:graphic (char-set #\newline)))
(define-deferred char-set:not-standard (char-set-invert char-set:standard))
(define-deferred char-standard? (%char-set-test char-set:standard))

(define-deferred char-set:newline
  (char-set #\newline))

;;; Used in RFCs:

(define-deferred char-set:ascii
  (ascii-range->char-set #x00 #x80))

(define-deferred char-set:ctls
  (char-set-union (ascii-range->char-set #x00 #x20)
		  (ascii-range->char-set #x7F #x80)))
(define-deferred char-ctl? (%char-set-test char-set:ctls))

(define-deferred char-set:wsp (char-set #\space #\tab))
(define-deferred char-wsp? (%char-set-test char-set:wsp))

(define (%char-set-test char-set)
  (lambda (char)
    (char-set-member? char-set char)))

;;;; Backwards compatibility

(define (string->char-set string)
  (guarantee-string string 'STRING->CHAR-SET)
  (let ((n (string-length string))
	(low (%make-low 0)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i n))
      (%low-set! low (vector-8b-ref string i)))
    (%make-char-set low '#() '#())))

(define (char-set->string char-set)
  (guarantee-8-bit-char-set char-set 'CHAR-SET->STRING)
  (let loop ((i 0) (chars '()))
    (if (fix:< i %low-length)
	(loop (fix:+ i 1)
	      (if (%scalar-value-in-char-set? i char-set)
		  (cons (integer->char i) chars)
		  chars))
	(apply string (reverse! chars)))))

(define (predicate->char-set predicate)
  (%scalar-values->char-set
   (filter (lambda (i)
	     (predicate (integer->char i)))
	   (iota #x100))))

(define (char-set-members char-set)
  (guarantee-8-bit-char-set char-set 'CHAR-SET-MEMBERS)
  (let ((low (char-set-low char-set)))
    (let loop ((code #xFF) (chars '()))
      (if (fix:>= code 0)
	  (loop (fix:- code 1)
		(if (%low-ref low code)
		    (cons (integer->char code) chars)
		    chars))
	  chars))))

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
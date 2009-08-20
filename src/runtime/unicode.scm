#| -*-Scheme-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; Unicode support
;;; package: (runtime unicode)

;;; See "http://www.cl.cam.ac.uk/~mgk25/unicode.html".
;;;
;;; UTF-8 encoding
;;; ==============
;;;
;;;  max code  encoding
;;; ---------- -----------------------------------------------------
;;; #x00000080 0xxxxxxx
;;; #x00000800 110xxxxx 10xxxxxx
;;; #x00010000 1110xxxx 10xxxxxx 10xxxxxx
;;; #x00200000 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
;;; #x04000000 111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
;;; #x80000000 1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
;;;
;;; It is possible to represent codes with over-long sequences, but
;;; this is disallowed.  For example, #\A is normally represented as
;;; #x41, but could also be written as #xC1 #x81, or even longer
;;; sequences.
;;;
;;; UTF-16 encoding
;;; ===============
;;;
;;; Codes in the ranges #x0000 through #xD7FF and #xE000 through
;;; #xFFFD are represented as themselves.  Codes in the range #x10000
;;; through #xFFFFF are represented as a pair:
;;;
;;; 110110xxxxxxxxxx 110111xxxxxxxxxx
;;;
;;; where the first 16-bit word contains the MS 10 bits, and the
;;; second contains the LS 10 bits.  As for UTF-8, overlong sequences
;;; are disallowed.
;;;
;;; Some UTF-16 documents start with the code #xFEFF, to identify the
;;; endianness of the document.  If instead #xFFFE is encountered, the
;;; opposite endianness should be used.

(declare (usual-integrations))

(define-syntax with-substring-args
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(expression expression expression expression
				     + expression)
			(cdr form))
	 (let ((string (close-syntax (list-ref form 1) environment))
	       (start (close-syntax (list-ref form 2) environment))
	       (end (close-syntax (list-ref form 3) environment))
	       (caller (close-syntax (list-ref form 4) environment)))
	   `(BEGIN
	      (GUARANTEE-STRING ,string ,caller)
	      (LET* ((,(list-ref form 3)
		      (IF (IF (DEFAULT-OBJECT? ,end) #F ,end)
			  (GUARANTEE-LIMITED-INDEX ,end (STRING-LENGTH ,string)
						   ,caller)
			  (STRING-LENGTH ,string)))
		     (,(list-ref form 2)
		      (IF (IF (DEFAULT-OBJECT? ,start) #F ,start)
			  (GUARANTEE-LIMITED-INDEX ,start ,(list-ref form 3)
						   ,caller)
			  0)))
		,@(map (let ((excludes
			      (list (list-ref form 2) (list-ref form 3))))
			 (lambda (expr)
			   (make-syntactic-closure environment excludes expr)))
		       (list-tail form 5)))))
	 (ill-formed-syntax form)))))

(define (guarantee-limited-index index limit caller)
  (guarantee-index-fixnum index caller)
  (if (not (fix:<= index limit))
      (error:bad-range-argument index caller))
  index)

(define (encoded-string-length string start end type caller validate-char)
  (let loop ((start start) (n 0))
    (if (fix:< start end)
	(let ((start* (validate-char string start end)))
	  (if (not start*)
	      (error:wrong-type-argument string
					 (string-append "a UTF-"
							type
							" string")
					 caller))
	  (loop start* (fix:+ n 1)))
	n)))

(define (encoded-string-valid? string start end validate-char)
  (let loop ((start start))
    (if (fix:< start end)
	(let ((start* (validate-char string start end)))
	  (if start*
	      (loop start*)
	      #f))
	#t)))

(define (coded-input-opener coding)
  (lambda (string #!optional start end)
    (let ((port (open-input-octets string start end)))
      (port/set-coding port coding)
      (port/set-line-ending port 'NEWLINE)
      port)))

(define (coded-output-opener coding)
  (lambda ()
    (let ((port (open-output-octets)))
      (port/set-coding port coding)
      (port/set-line-ending port 'NEWLINE)
      port)))

(define (ended-input-opener be le)
  (lambda (string #!optional start end)
    (if (host-big-endian?)
	(be string start end)
	(le string start end))))

(define (ended-output-opener be le)
  (lambda ()
    (if (host-big-endian?)
	(be)
	(le))))

(define (input-string-caller open-input)
  (lambda (string procedure)
    (let ((port (open-input string)))
      (let ((value (procedure port)))
	(close-input-port port)
	value))))

(define (output-string-caller open-output)
  (lambda (procedure)
    (let ((port (open-output)))
      (procedure port)
      (get-output-string! port))))

;;;; Unicode characters

(define (wide-char? object)
  (and (char? object)
       (legal-code-32? (char->integer object))))

(define-guarantee wide-char "a Unicode character")

(define (unicode-code-point? object)
  (and (%unicode-code-point? object)
       (not (illegal? object))))

(define (%unicode-code-point? object)
  (and (index-fixnum? object)
       (fix:< object char-code-limit)))

(define-guarantee unicode-code-point "a Unicode code point")

(define-integrable (legal-code-32? pt)
  (and (fix:< pt char-code-limit)
       (not (illegal? pt))))

(define-integrable (legal-code-16? pt)
  (not (illegal? pt)))

(define-integrable (illegal? pt)
  (or (and (fix:>= pt #xD800) (fix:< pt #xE000))
      (fix:= pt #xFFFE)
      (fix:= pt #xFFFF)))

#|

Not used at the moment.

(define-integrable (non-character? pt)
  (or (and (fix:>= pt #xD800) (fix:< pt #xE000))
      (and (fix:>= pt #xFDD0) (fix:< pt #xFDF0))
      (fix:= #x00FFFE (fix:and #x00FFFE pt))))

|#

;;;; Alphabets

(define-structure (alphabet (type-descriptor <alphabet>))
  (low #f read-only #t)
  (high1 #f read-only #t)
  (high2 #f read-only #t))

(define-guarantee alphabet "a Unicode alphabet")

(define-integrable (make-alphabet-low)
  (make-string #x100 (integer->char 0)))

(define-integrable (alphabet-low-ref low code-point)
  (not (fix:= (fix:and (vector-8b-ref low (fix:lsh code-point -3))
		       (fix:lsh 1 (fix:and code-point 7)))
	      0)))

(define-integrable (alphabet-low-set! low code-point)
  (vector-8b-set! low
		  (fix:lsh code-point -3)
		  (fix:or (vector-8b-ref low (fix:lsh code-point -3))
			  (fix:lsh 1 (fix:and code-point 7)))))

(define null-alphabet
  (make-alphabet (make-alphabet-low) '#() '#()))

(define (char-in-alphabet? char alphabet)
  (guarantee-wide-char char 'CHAR-IN-ALPHABET?)
  (guarantee-alphabet alphabet 'CHAR-IN-ALPHABET?)
  (%code-point-in-alphabet? (char-code char) alphabet))

(define (%code-point-in-alphabet? pt alphabet)
  (if (fix:< pt #x800)
      (alphabet-low-ref (alphabet-low alphabet) pt)
      (let ((high1 (alphabet-high1 alphabet))
	    (high2 (alphabet-high2 alphabet)))
	(let loop ((lower 0) (upper (vector-length high1)))
	  (and (fix:< lower upper)
	       (let ((index (fix:quotient (fix:+ lower upper) 2)))
		 (cond ((fix:< pt (vector-ref high1 index))
			(loop lower index))
		       ((fix:< (vector-ref high2 index) pt)
			(loop (fix:+ index 1) upper))
		       (else #t))))))))

(define (well-formed-code-point-list? items)
  (if (pair? items)
      (and (well-formed-item? (car items))
	   (let loop ((a (car items)) (items (cdr items)))
	     (if (pair? items)
		 (let ((b (car items))
		       (items (cdr items)))
		   (and (well-formed-item? b)
			(fix:< (if (pair? a) (cdr a) a)
			       (if (pair? b) (car b) b))
			(loop b items)))
		 (null? items))))
      (null? items)))

(define (well-formed-item? item)
  (if (pair? item)
      (and (%unicode-code-point? (car item))
	   (%unicode-code-point? (cdr item))
	   (fix:< (car item) (cdr item)))
      (%unicode-code-point? item)))

(define-guarantee well-formed-code-point-list "a Unicode code-point list")

(define (code-points->alphabet items)
  (guarantee-well-formed-code-point-list items 'CODE-POINTS->ALPHABET)
  (%code-points->alphabet items))

(define (%code-points->alphabet items)
  (receive (low-items high-items)
      (split-list (canonicalize-code-point-list items) #x800)
    (let ((low (make-alphabet-low)))
      (for-each (lambda (item)
		  (if (pair? item)
		      (do ((i (car item) (fix:+ i 1)))
			  ((fix:> i (cdr item)))
			(alphabet-low-set! low i))
		      (alphabet-low-set! low item)))
		low-items)
      (let ((n-high (length high-items)))
	(let ((high1 (make-vector n-high))
	      (high2 (make-vector n-high)))
	  (do ((items high-items (cdr items))
	       (i 0 (fix:+ i 1)))
	      ((not (pair? items)))
	    (if (pair? (car items))
		(begin
		  (vector-set! high1 i (caar items))
		  (vector-set! high2 i (cdar items)))
		(begin
		  (vector-set! high1 i (car items))
		  (vector-set! high2 i (car items)))))
	  (make-alphabet low high1 high2))))))

(define (canonicalize-code-point-list items)
  (if (pair? items)
      (let ((a (car items)))
	(let loop
	    ((al (if (pair? a) (car a) a))
	     (ah (if (pair? a) (cdr a) a))
	     (items (cdr items)))
	  (if (pair? items)
	      (let ((b (car items))
		    (items (cdr items)))
		(let ((bl (if (pair? b) (car b) b))
		      (bh (if (pair? b) (cdr b) b)))
		  (if (fix:= (fix:+ ah 1) bl)
		      (loop al bh items)
		      (cons (if (fix:= al ah) al (cons al ah))
			    (loop bl bh items)))))
	      (list (if (fix:= al ah) al (cons al ah))))))
      '()))

(define (split-list items limit)
  (let loop ((items items) (low '()))
    (if (pair? items)
	(let ((item (car items)))
	  (cond ((not (pair? item))
		 (if (fix:< item limit)
		     (loop (cdr items) (cons item low))
		     (values low items)))
		((fix:< (cdr item) limit)
		 (loop (cdr items) (cons item low)))
		((fix:<= limit (car item))
		 (values low items))
		(else
		 (values (cons (cons (car item) (fix:- limit 1)) low)
			 (cons (cons limit (cdr item)) items)))))
	(values low '()))))

(define (alphabet . chars)
  (%code-points->alphabet (chars->wfcp-list (remove-duplicate-chars chars))))

(define (remove-duplicate-chars chars)
  (let ((table (make-eq-hash-table)))
    (for-each (lambda (char)
		(guarantee-wide-char char 'REMOVE-DUPLICATE-CHARS)
		(hash-table/put! table char #t))
	      chars)
    (hash-table/key-list table)))

(define (chars->wfcp-list chars)
  (let ((chars (sort chars char<?)))
    (if (pair? chars)
	(let loop
	    ((pt (char->integer (car chars)))
	     (chars (cdr chars))
	     (items '()))
	  (if (pair? chars)
	      (let ((pt* (char->integer (car chars)))
		    (chars (cdr chars)))
		(if (fix:= pt* (fix:+ pt 1))
		    (let find-max ((pt* pt*) (chars chars))
		      (if (pair? chars)
			  (let ((pt** (char->integer (car chars)))
				(chars (cdr chars)))
			    (if (fix:= pt** (fix:+ pt* 1))
				(find-max pt** chars)
				(loop pt** chars (cons (cons pt pt*) items))))
			  (reverse! (cons (cons pt pt*) items))))
		    (loop pt* chars (cons pt items))))
	      (reverse! (cons pt items))))
	'())))

(define (alphabet->code-points alphabet)
  (guarantee-alphabet alphabet 'ALPHABET->CODE-POINTS)
  (append! (alphabet-low->code-points (alphabet-low alphabet))
	   (alphabet-high->code-points (alphabet-high1 alphabet)
				       (alphabet-high2 alphabet))))

(define (alphabet-low->code-points low)
  (let find-lower ((i 0) (result '()))
    (if (fix:< i #x800)
	(if (alphabet-low-ref low i)
	    (let ((lower i))
	      (let find-upper ((i (fix:+ i 1)))
		(if (fix:< i #x800)
		    (if (alphabet-low-ref low i)
			(find-upper (fix:+ i 1))
			(find-lower i
				    (cons (if (fix:= lower (fix:- i 1))
					      lower
					      (cons lower (fix:- i 1)))
					  result)))
		    (reverse!
		     (cons (if (fix:= lower (fix:- i 1))
			       lower
			       (cons lower (fix:- i 1)))
			   result)))))
	    (find-lower (fix:+ i 1) result))
	(reverse! result))))

(define (alphabet-high->code-points lower upper)
  (let ((n (vector-length lower)))
    (let loop ((i 0) (result '()))
      (if (fix:< i n)
	  (loop (fix:+ i 1)
		(cons (cons (vector-ref lower i) (vector-ref upper i))
		      result))
	  (reverse! result)))))

(define (8-bit-alphabet? alphabet)
  (and (fix:= (vector-length (alphabet-high1 alphabet)) 0)
       (let ((low (alphabet-low alphabet)))
	 (let loop ((i #x20))
	   (or (fix:= i #x100)
	       (and (fix:= (vector-8b-ref low i) 0)
		    (loop (fix:+ i 1))))))))

(define-guarantee 8-bit-alphabet "an 8-bit alphabet")

(define (char-set->alphabet char-set)
  (guarantee-char-set char-set 'CHAR-SET->ALPHABET)
  (let ((low (make-alphabet-low)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i #x100))
      (if (char-set-member? char-set (integer->char i))
	  (alphabet-low-set! low i)))
    (make-alphabet low '#() '#())))

(define (alphabet->char-set alphabet)
  (guarantee-8-bit-alphabet alphabet 'ALPHABET->CHAR-SET)
  (predicate->char-set (lambda (char) (char-in-alphabet? char alphabet))))

(define (string->alphabet string)
  (guarantee-string string 'STRING->ALPHABET)
  (let ((n (string-length string))
	(low (make-alphabet-low)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i n))
      (alphabet-low-set! low (vector-8b-ref string i)))
    (make-alphabet low '#() '#())))

(define (alphabet->string alphabet)
  (guarantee-8-bit-alphabet alphabet 'ALPHABET->STRING)
  (let loop ((i 0) (chars '()))
    (if (fix:< i #x100)
	(loop (fix:+ i 1)
	      (if (%code-point-in-alphabet? i alphabet)
		  (cons (integer->char i) chars)
		  chars))
	(apply string (reverse! chars)))))

(define (alphabet+ . alphabets)
  (for-each (lambda (alphabet)
	      (guarantee-alphabet alphabet 'ALPHABET+))
	    alphabets)
  (reduce alphabet+2 null-alphabet alphabets))

(define (alphabet+2 a1 a2)
  (receive (high1 high2)
      (alphabet-high+2 (alphabet-high1 a1)
		       (alphabet-high2 a1)
		       (alphabet-high1 a2)
		       (alphabet-high2 a2))
    (make-alphabet (alphabet-low+2 (alphabet-low a1) (alphabet-low a2))
		   high1
		   high2)))

(define (alphabet-low+2 low1 low2)
  (let ((low (make-alphabet-low)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i #x100))
      (vector-8b-set! low i
		      (fix:or (vector-8b-ref low1 i)
			      (vector-8b-ref low2 i))))
    low))

(define (alphabet-high+2 lower1 upper1 lower2 upper2)
  (let ((n1 (vector-length lower1))
	(n2 (vector-length lower2)))
    (let ((lower (make-vector (fix:+ n1 n2)))
	  (upper (make-vector (fix:+ n1 n2))))
      (let ((n
	     (let loop ((i1 0) (i2 0) (i 0))
	       (cond ((fix:= i1 n1)
		      (subvector-move-left! lower2 i2 n2 lower i)
		      (subvector-move-left! upper2 i2 n2 upper i)
		      (fix:+ i (fix:- n2 i2)))
		     ((fix:= i2 n2)
		      (subvector-move-left! lower1 i1 n1 lower i)
		      (subvector-move-left! upper1 i1 n1 upper i)
		      (fix:+ i (fix:- n1 i1)))
		     ((fix:< (vector-ref upper1 i1) (vector-ref lower2 i2))
		      (vector-set! lower i (vector-ref lower1 i1))
		      (vector-set! upper i (vector-ref upper1 i1))
		      (loop (fix:+ i1 1) i2 (fix:+ i 1)))
		     ((fix:< (vector-ref upper2 i2) (vector-ref lower1 i1))
		      (vector-set! lower i (vector-ref lower2 i2))
		      (vector-set! upper i (vector-ref upper2 i2))
		      (loop i1 (fix:+ i2 1) (fix:+ i 1)))
		     (else
		      (vector-set! lower i
				   (min (vector-ref lower1 i1)
					(vector-ref lower2 i2)))
		      (vector-set! upper i
				   (max (vector-ref upper1 i1)
					(vector-ref upper2 i2)))
		      (loop (fix:+ i1 1) (fix:+ i2 1) (fix:+ i 1)))))))
	(if (fix:< n (vector-length lower))
	    (values (vector-head lower n) (vector-head upper n))
	    (values lower upper))))))

(define (alphabet- a1 a2)
  (receive (high1 high2)
      (alphabet-high- (alphabet-high1 a1)
		      (alphabet-high2 a1)
		      (alphabet-high1 a2)
		      (alphabet-high2 a2))
    (make-alphabet (alphabet-low- (alphabet-low a1) (alphabet-low a2))
		   high1
		   high2)))

(define (alphabet-low- low1 low2)
  (let ((low (make-alphabet-low)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i #x100))
      (vector-8b-set! low i
		      (fix:and (vector-8b-ref low1 i)
			       (fix:not (vector-8b-ref low2 i)))))
    low))

(define (alphabet-high- lower1 upper1 lower2 upper2)
  (let ((n1 (vector-length lower1))
	(n2 (vector-length lower2)))
    (let ((lower (make-vector (fix:* n1 2)))
	  (upper (make-vector (fix:* n1 2))))
      (let ((n
	     (let loop ((i1 0) (i2 0) (i 0))
	       (cond ((fix:= i1 n1)
		      i)
		     ((fix:= i2 n2)
		      (subvector-move-left! lower1 i1 n1 lower i)
		      (subvector-move-left! upper1 i1 n1 upper i)
		      (fix:+ i (fix:- n1 i1)))
		     ((fix:< (vector-ref upper1 i1) (vector-ref lower2 i2))
		      (vector-set! lower i (vector-ref lower1 i1))
		      (vector-set! upper i (vector-ref upper1 i1))
		      (loop (fix:+ i1 1) i2 (fix:+ i 1)))
		     ((fix:< (vector-ref upper2 i2) (vector-ref lower1 i1))
		      (loop i1 (fix:+ i2 1) i))
		     ((fix:< (vector-ref lower1 i1) (vector-ref lower2 i2))
		      (vector-set! lower i (vector-ref lower1 i1))
		      (vector-set! upper i (- (vector-ref lower2 i2) 1))
		      (if (fix:<= (vector-ref upper1 i1)
				  (vector-ref upper2 i2))
			  (loop (fix:+ i1 1) (fix:+ i2 1) (fix:+ i 1))
			  (begin
			    (vector-set! lower (fix:+ i 1)
					 (+ (vector-ref upper2 i2) 1))
			    (vector-set! upper (fix:+ i 1)
					 (vector-ref upper1 i1))
			    (loop (fix:+ i1 1) (fix:+ i2 1) (fix:+ i 2)))))
		     ((fix:<= (vector-ref upper1 i1) (vector-ref upper2 i2))
		      (loop (fix:+ i1 1) (fix:+ i2 1) i))
		     (else
		      (vector-set! lower i (+ (vector-ref upper2 i2) 1))
		      (vector-set! upper i (vector-ref upper1 i1))
		      (loop (fix:+ i1 1) (fix:+ i2 1) (fix:+ i 1)))))))
	(if (fix:< n (vector-length lower))
	    (values (vector-head lower n) (vector-head upper n))
	    (values lower upper))))))

;;;; Unicode strings

(define-structure (wide-string (type-descriptor <wide-string>)
			       (constructor %make-wide-string))
  (contents #f read-only #t))

(define-guarantee wide-string "a Unicode string")

(define (make-wide-string length #!optional char)
  (%make-wide-string
   (make-vector length
		(if (if (default-object? char) #f char)
		    (begin
		      (guarantee-wide-char char 'MAKE-WIDE-STRING)
		      char)
		    (integer->char 0)))))

(define (wide-string . chars)
  (for-each (lambda (char) (guarantee-wide-char char 'WIDE-STRING)) chars)
  (%make-wide-string (list->vector chars)))

(define (wide-string-length string)
  (guarantee-wide-string string 'WIDE-STRING-LENGTH)
  (%wide-string-length string))

(define-integrable (%wide-string-length string)
  (vector-length (wide-string-contents string)))

(define (wide-string-ref string index)
  (guarantee-wide-string string 'WIDE-STRING-REF)
  (guarantee-wide-string-index index string 'WIDE-STRING-REF)
  (%wide-string-ref string index))

(define-integrable (%wide-string-ref string index)
  (vector-ref (wide-string-contents string) index))

(define (wide-string-set! string index char)
  (guarantee-wide-string string 'WIDE-STRING-SET!)
  (guarantee-wide-string-index index string 'WIDE-STRING-SET!)
  (guarantee-wide-char char 'WIDE-STRING-SET!)
  (%wide-string-set! string index char))

(define-integrable (%wide-string-set! string index char)
  (vector-set! (wide-string-contents string) index char))

(define (wide-substring string start end)
  (guarantee-wide-substring string start end 'WIDE-SUBSTRING)
  (%wide-substring string start end))

(define (%wide-substring string start end)
  (let ((string* (make-wide-string (fix:- end start))))
    (let ((v1 (wide-string-contents string))
	  (v2 (wide-string-contents string*)))
      (do ((i start (fix:+ i 1))
	   (j 0 (fix:+ j 1)))
	  ((not (fix:< i end)))
	(vector-set! v2 j (vector-ref v1 i))))
    string*))

(define (wide-string-index? index string)
  (and (index-fixnum? index)
       (fix:< index (%wide-string-length string))))

(define-integrable (guarantee-wide-string-index index string caller)
  (if (not (wide-string-index? index string))
      (error:not-wide-string-index index caller)))

(define (error:not-wide-string-index index caller)
  (error:wrong-type-argument index "a Unicode string index" caller))

(define-integrable (guarantee-wide-substring string start end caller)
  (if (not (and (wide-string? string)
		(index-fixnum? start)
		(index-fixnum? end)
		(fix:<= start end)
		(fix:<= end (%wide-string-length string))))
      (guarantee-wide-substring/fail string start end caller)))

(define (guarantee-wide-substring/fail string start end caller)
  (guarantee-wide-string string caller)
  (guarantee-limited-index end (%wide-string-length string) caller)
  (guarantee-limited-index start end caller))

(define (string->wide-string string #!optional start end)
  (%convert-string string start end
		   open-input-string
		   open-wide-output-string))

(define (wide-string->string string #!optional start end)
  (%convert-string string start end
		   open-input-string
		   open-narrow-output-string))

(define (%convert-string string start end open-input open-output)
  (let ((input (open-input string start end))
	(output (open-output)))
    (let loop ()
      (let ((c (read-char input)))
	(if (not (eof-object? c))
	    (begin
	      (write-char c output)
	      (loop)))))
    (get-output-string! output)))

;;;; UTF-32 representation

(define open-utf32-be-input-string
  (coded-input-opener 'UTF-32BE))

(define open-utf32-le-input-string
  (coded-input-opener 'UTF-32LE))

(define open-utf32-input-string
  (ended-input-opener open-utf32-be-input-string
		      open-utf32-le-input-string))

(define call-with-utf32-be-input-string
  (input-string-caller open-utf32-be-input-string))

(define call-with-utf32-le-input-string
  (input-string-caller open-utf32-le-input-string))

(define call-with-utf32-input-string
  (input-string-caller open-utf32-input-string))

(define open-utf32-be-output-string
  (coded-output-opener 'UTF-32BE))

(define open-utf32-le-output-string
  (coded-output-opener 'UTF-32LE))

(define open-utf32-output-string
  (ended-output-opener open-utf32-be-output-string
		       open-utf32-le-output-string))

(define call-with-utf32-be-output-string
  (output-string-caller open-utf32-be-output-string))

(define call-with-utf32-le-output-string
  (output-string-caller open-utf32-le-output-string))

(define call-with-utf32-output-string
  (output-string-caller open-utf32-output-string))

(define (utf32-string->wide-string string #!optional start end)
  (if (host-big-endian?)
      (utf32-be-string->wide-string string start end)
      (utf32-le-string->wide-string string start end)))

(define (utf32-be-string->wide-string string #!optional start end)
  (%convert-string string start end
		   open-utf32-be-input-string
		   open-wide-output-string))

(define (utf32-le-string->wide-string string #!optional start end)
  (%convert-string string start end
		   open-utf32-le-input-string
		   open-wide-output-string))

(define (string->utf32-string string #!optional start end)
  (if (host-big-endian?)
      (string->utf32-be-string string start end)
      (string->utf32-le-string string start end)))

(define (string->utf32-be-string string #!optional start end)
  (%convert-string string start end
		   open-input-string
		   open-utf32-be-output-string))

(define (string->utf32-le-string string #!optional start end)
  (%convert-string string start end
		   open-input-string
		   open-utf32-le-output-string))

(define (utf32-string-length string #!optional start end)
  (if (host-big-endian?)
      (utf32-be-string-length string start end)
      (utf32-le-string-length string start end)))

(define (utf32-be-string-length string #!optional start end)
  (%utf32-string-length string start end "32BE" utf32-be-octets->code-point
			'UTF32-BE-STRING-LENGTH))

(define (utf32-le-string-length string #!optional start end)
  (%utf32-string-length string start end "32LE" utf32-le-octets->code-point
			'UTF32-LE-STRING-LENGTH))

(define (%utf32-string-length string start end type combiner caller)
  (with-substring-args string start end caller
    (encoded-string-length string start end type caller
      (lambda (string start end)
	(validate-utf32-char string start end combiner)))))

(define (utf32-string-valid? string #!optional start end)
  (if (host-big-endian?)
      (utf32-be-string-valid? string start end)
      (utf32-le-string-valid? string start end)))

(define (utf32-be-string-valid? string #!optional start end)
  (%utf32-string-valid? string start end utf32-be-octets->code-point
			'UTF32-BE-STRING-VALID?))

(define (utf32-le-string-valid? string #!optional start end)
  (%utf32-string-valid? string start end utf32-le-octets->code-point
			'UTF32-LE-STRING-VALID?))

(define (%utf32-string-valid? string start end combiner caller)
  (with-substring-args string start end caller
    (encoded-string-valid? string start end
      (lambda (string start end)
	(validate-utf32-char string start end combiner)))))

(define-integrable (utf32-be-octets->code-point b0 b1 b2 b3)
  (+ (* b0 #x01000000)
     (fix:lsh b1 16)
     (fix:lsh b2 8)
     b3))

(define-integrable (utf32-le-octets->code-point b0 b1 b2 b3)
  (+ (* b3 #x01000000)
     (fix:lsh b2 16)
     (fix:lsh b1 8)
     b0))

(define (validate-utf32-char string start end combiner)

  (define-integrable (n i)
    (vector-8b-ref string (fix:+ start i)))

  (if (fix:< start end)
      (and (fix:<= (fix:+ start 4) end)
	   (legal-code-32? (combiner (n 0) (n 1) (n 2) (n 3)))
	   (fix:+ start 4))
      start))

(define (utf32-string? object)
  (and (string? object)
       (utf32-string-valid? object)))

(define (utf32-be-string? object)
  (and (string? object)
       (utf32-be-string-valid? object)))

(define (utf32-le-string? object)
  (and (string? object)
       (utf32-le-string-valid? object)))

(define-guarantee utf32-string "UTF-32 string")
(define-guarantee utf32-be-string "UTF-32BE string")
(define-guarantee utf32-le-string "UTF-32LE string")

;;;; UTF-16 representation

(define open-utf16-be-input-string
  (coded-input-opener 'UTF-16BE))

(define open-utf16-le-input-string
  (coded-input-opener 'UTF-16LE))

(define open-utf16-input-string
  (ended-input-opener open-utf16-be-input-string
		      open-utf16-le-input-string))

(define call-with-utf16-be-input-string
  (input-string-caller open-utf16-be-input-string))

(define call-with-utf16-le-input-string
  (input-string-caller open-utf16-le-input-string))

(define call-with-utf16-input-string
  (input-string-caller open-utf16-input-string))

(define open-utf16-be-output-string
  (coded-output-opener 'UTF-16BE))

(define open-utf16-le-output-string
  (coded-output-opener 'UTF-16LE))

(define open-utf16-output-string
  (ended-output-opener open-utf16-be-output-string
		       open-utf16-le-output-string))

(define call-with-utf16-be-output-string
  (output-string-caller open-utf16-be-output-string))

(define call-with-utf16-le-output-string
  (output-string-caller open-utf16-le-output-string))

(define call-with-utf16-output-string
  (output-string-caller open-utf16-output-string))

(define (utf16-string->wide-string string #!optional start end)
  (if (host-big-endian?)
      (utf16-be-string->wide-string string start end)
      (utf16-le-string->wide-string string start end)))

(define (utf16-be-string->wide-string string #!optional start end)
  (%convert-string string start end
		   open-utf16-be-input-string
		   open-wide-output-string))

(define (utf16-le-string->wide-string string #!optional start end)
  (%convert-string string start end
		   open-utf16-le-input-string
		   open-wide-output-string))

(define (string->utf16-string string #!optional start end)
  (if (host-big-endian?)
      (string->utf16-be-string string start end)
      (string->utf16-le-string string start end)))

(define (string->utf16-be-string string #!optional start end)
  (%convert-string string start end
		   open-input-string
		   open-utf16-be-output-string))

(define (string->utf16-le-string string #!optional start end)
  (%convert-string string start end
		   open-input-string
		   open-utf16-le-output-string))

(define (utf16-string-length string #!optional start end)
  (if (host-big-endian?)
      (utf16-be-string-length string start end)
      (utf16-le-string-length string start end)))

(define (utf16-be-string-length string #!optional start end)
  (%utf16-string-length string start end "16BE" be-octets->digit16
			'UTF16-BE-STRING-LENGTH))

(define (utf16-le-string-length string #!optional start end)
  (%utf16-string-length string start end "16LE" le-octets->digit16
			'UTF16-LE-STRING-LENGTH))

(define (%utf16-string-length string start end type combiner caller)
  (with-substring-args string start end caller
    (encoded-string-length string start end type caller
      (lambda (string start end)
	(validate-utf16-char string start end combiner)))))

(define (utf16-string-valid? string #!optional start end)
  (if (host-big-endian?)
      (utf16-be-string-valid? string start end)
      (utf16-le-string-valid? string start end)))

(define (utf16-be-string-valid? string #!optional start end)
  (%utf16-string-valid? string start end be-octets->digit16
			'UTF16-BE-STRING-VALID?))

(define (utf16-le-string-valid? string #!optional start end)
  (%utf16-string-valid? string start end le-octets->digit16
			'UTF16-LE-STRING-VALID?))

(define (%utf16-string-valid? string start end combiner caller)
  (with-substring-args string start end caller
    (encoded-string-valid? string start end
      (lambda (string start end)
	(validate-utf16-char string start end combiner)))))

(define (validate-utf16-char string start end combiner)

  (define-integrable (n i)
    (vector-8b-ref string (fix:+ start i)))

  (if (fix:< start end)
      (and (fix:<= (fix:+ start 2) end)
	   (let ((d0 (combiner (n 0) (n 1))))
	     (if (utf16-high-surrogate? d0)
		 (and (fix:<= (fix:+ start 4) end)
		      (utf16-low-surrogate? (combiner (n 2) (n 3)))
		      (fix:+ start 4))
		 (and (legal-code-16? d0)
		      (fix:+ start 2)))))
      start))

(define (be-octets->digit16 b0 b1)
  (fix:or (fix:lsh b0 8) b1))

(define (le-octets->digit16 b0 b1)
  (fix:or (fix:lsh b1 8) b0))

(define (combine-utf16-surrogates h l)
  (guarantee-utf16-high-surrogate h 'combine-utf16-surrogates)
  (guarantee-utf16-low-surrogate l 'combine-utf16-surrogates)
  (fix:+ (fix:+ (fix:lsh (fix:and h #x3FF) 10)
		(fix:and l #x3FF))
	 #x10000))

(define (split-into-utf16-surrogates n)
  (guarantee-unicode-code-point n 'split-into-utf16-surrogates)
  (let ((n (fix:- n #x10000)))
    (values (fix:or (fix:and (fix:lsh n -10) #x03FF) #xD800)
	    (fix:or (fix:and n #x03FF) #xDC00))))

(define (utf16-string? object)
  (and (string? object)
       (utf16-string-valid? object)))

(define (utf16-be-string? object)
  (and (string? object)
       (utf16-be-string-valid? object)))

(define (utf16-le-string? object)
  (and (string? object)
       (utf16-le-string-valid? object)))

(define (utf16-high-surrogate? n)
  (and (index-fixnum? n)
       (fix:= #xD800 (fix:and #xFC00 n))))

(define (utf16-low-surrogate? n)
  (and (index-fixnum? n)
       (fix:= #xDC00 (fix:and #xFC00 n))))

(define-guarantee utf16-string "UTF-16 string")
(define-guarantee utf16-be-string "UTF-16BE string")
(define-guarantee utf16-le-string "UTF-16LE string")
(define-guarantee utf16-high-surrogate "UTF-16 high surrogate")
(define-guarantee utf16-low-surrogate "UTF-16 low surrogate")

;;;; UTF-8 representation

(define open-utf8-input-string
  (coded-input-opener 'UTF-8))

(define call-with-utf8-input-string
  (input-string-caller open-utf8-input-string))

(define open-utf8-output-string
  (coded-output-opener 'UTF-8))

(define call-with-utf8-output-string
  (output-string-caller open-utf8-output-string))

(define (string->utf8-string string #!optional start end)
  (%convert-string string start end
		   open-input-string
		   open-utf8-output-string))

(define (utf8-string->string string #!optional start end)
  (%convert-string string start end
		   open-utf8-input-string
		   open-narrow-output-string))

(define (utf8-string->wide-string string #!optional start end)
  (%convert-string string start end
		   open-utf8-input-string
		   open-wide-output-string))

(define (utf8-string-length string #!optional start end)
  (with-substring-args string start end 'UTF8-STRING-LENGTH
    (encoded-string-length string start end "8" 'UTF8-STRING-LENGTH
			   validate-utf8-char)))

(define (utf8-string-valid? string #!optional start end)
  (with-substring-args string start end 'UTF8-STRING-VALID?
    (encoded-string-valid? string start end validate-utf8-char)))

(define (utf8-string? object)
  (and (string? object)
       (utf8-string-valid? object)))

(define-guarantee utf8-string "UTF-8 string")

(define (validate-utf8-char string start end)

  (define-integrable (check-byte i)
    (%valid-trailer? (n i)))

  (define-integrable (n i)
    (vector-8b-ref string (fix:+ start i)))

  (if (fix:< start end)
      (let ((b0 (vector-8b-ref string start)))
	(cond ((fix:< b0 #x80)
	       (fix:+ start 1))
	      ((fix:< b0 #xE0)
	       (and (fix:<= (fix:+ start 2) end)
		    (check-byte 1)
		    (%vs2 b0)
		    (fix:+ start 2)))
	      ((fix:< b0 #xF0)
	       (and (fix:<= (fix:+ start 3) end)
		    (check-byte 1)
		    (check-byte 2)
		    (%vs3 b0 (n 1))
		    (legal-code-16? (%cp3 b0 (n 1) (n 2)))
		    (fix:+ start 3)))
	      ((fix:< b0 #xF8)
	       (and (fix:<= (fix:+ start 4) end)
		    (check-byte 1)
		    (%vs4 b0 (n 1))
		    (check-byte 2)
		    (check-byte 3)
		    (fix:+ start 4)))
	      (else #f)))
      start))

(define-integrable (%vs2 b0)
  (fix:> b0 #xC1))

(define-integrable (%vs3 b0 b1)
  (or (fix:> b0 #xE0) (fix:> b1 #x9F)))

(define-integrable (%vs4 b0 b1)
  (or (fix:> b0 #xF0) (fix:> b1 #x8F)))

(define-integrable (%cp3 b0 b1 b2)
  (fix:or (fix:lsh (fix:and b0 #x0F) 12)
	  (fix:or (fix:lsh (fix:and b1 #x3F) 6)
		  (fix:and b2 #x3F))))

(define-integrable (%valid-trailer? n)
  (fix:= #x80 (fix:and #xC0 n)))

;;;; Per-character combination predicates

(define (for-all-chars-in-string? predicate string #!optional start end coding)
  (let ((port (open-string string start end coding 'FOR-ALL-CHARS-IN-STRING?)))
    (let loop ()
      (let ((char (read-char port)))
	(cond ((eof-object? char) #t)
	      ((predicate char) (loop))
	      (else #f))))))

(define (for-any-char-in-string? predicate string #!optional start end coding)
  (let ((port (open-string string start end coding 'FOR-ANY-CHAR-IN-STRING?)))
    (let loop ()
      (let ((char (read-char port)))
	(cond ((eof-object? char) #f)
	      ((predicate char) #t)
	      (else (loop)))))))

(define (open-string string start end coding caller)
  ((cond ((default-object? coding)
	  open-input-string)
	 ((string? string)
	  (case coding
	    ((UTF-8) open-utf8-input-string)
	    ((UTF-16) open-utf16-input-string)
	    ((UTF-16BE) open-utf16-be-input-string)
	    ((UTF-16LE) open-utf16-le-input-string)
	    ((UTF-32) open-utf32-input-string)
	    ((UTF-32BE) open-utf32-be-input-string)
	    ((UTF-32LE) open-utf32-le-input-string)
	    (else (error:bad-range-argument coding caller))))
	 ((wide-string? string)
	  (error:bad-range-argument coding caller))
	 (else
	  (error:wrong-type-argument string "string" caller)))
   string start end))

(define (alphabet-predicate alphabet)
  (cond ((alphabet? alphabet)
	 (lambda (char) (char-in-alphabet? char alphabet)))
	((char-set? alphabet)
	 (lambda (char) (char-set-member? alphabet char)))
	(else
	 (error:not-alphabet alphabet 'ALPHABET-PREDICATE))))
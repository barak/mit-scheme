#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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
		      (guarantee-unicode-char char 'MAKE-WIDE-STRING)
		      char)
		    (integer->char 0)))))

(define (wide-string . chars)
  (for-each (lambda (char) (guarantee-unicode-char char 'WIDE-STRING)) chars)
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
  (guarantee-unicode-char char 'WIDE-STRING-SET!)
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
      (let ((c (%read-char input)))
	(if (not (eof-object? c))
	    (begin
	      (%write-char c output)
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
  (guarantee-unicode-scalar-value n 'split-into-utf16-surrogates)
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
      (let ((char (%read-char port)))
	(cond ((eof-object? char) #t)
	      ((predicate char) (loop))
	      (else #f))))))

(define (for-any-char-in-string? predicate string #!optional start end coding)
  (let ((port (open-string string start end coding 'FOR-ANY-CHAR-IN-STRING?)))
    (let loop ()
      (let ((char (%read-char port)))
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
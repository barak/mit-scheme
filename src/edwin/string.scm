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

;;;; Character String Operations
;;; package: (edwin string)

;;; This file is designed to be compiled with type and range checking
;;; turned off. The advertised user-visible procedures all explicitly
;;; check their arguments.
;;;
;;; Many of the procedures are split into several user versions that
;;; just validate their arguments and pass them on to an internal
;;; version (prefixed with `%') that assumes all arguments have been
;;; checked.  This avoids repeated argument checks.

(declare (usual-integrations))

;;;; Primitives

(define-primitives
  (primitive-byte-ref 2)
  (primitive-byte-set! 3)
  (primitive-datum-ref 2)
  (primitive-type-ref 2)
  (set-string-length! 2)
  (string-allocate 1)
  substring-move-left!
  substring-move-right!
  vector-8b-fill!
  vector-8b-find-next-char
  vector-8b-find-next-char-ci
  vector-8b-find-previous-char
  vector-8b-find-previous-char-ci)

;;; Primitives that would be open-coded by compiler will only recognize legacy
;;; strings.  We work around this by implementing them by hand using the
;;; low-level operations provided by the runtime's string implementation.

(define byte0-index
  (fix:* 2 (bytes-per-object)))

(declare (integrate-operator string?))
(define (string? object)
  (or (object-type? (ucode-type string) object)
      (bytevector? object)
      (and (object-type? (ucode-type unicode-string) object)
	   (fix:= 1 (fix:and #x03 (primitive-type-ref object 1))))))

(define (string-length string)
  (guarantee string? string 'string-length)
  (%string-length string))

(define-integrable (%string-length string)
  (primitive-datum-ref string 1))

(define-integrable (string-ref string index)
  (integer->char (vector-8b-ref string index)))

(define-integrable (string-set! string index char)
  (vector-8b-set! string index (char->integer char)))

(declare (integrate-operator vector-8b-ref))
(define (vector-8b-ref string index)
  (if (not (string? string))
      (error:not-a string? string 'vector-8b-ref))
  (if (not (index-fixnum? index))
      (error:not-a index-fixnum? index 'vector-8b-ref))
  (if (not (fix:< index (%string-length string)))
      (error:bad-range-argument index 'vector-8b-ref))
  (primitive-byte-ref string (fix:+ byte0-index index)))

(declare (integrate-operator vector-8b-set!))
(define (vector-8b-set! string index u8)
  (if (not (string? string))
      (error:not-a string? string 'vector-8b-set!))
  (if (not (index-fixnum? index))
      (error:not-a index-fixnum? index 'vector-8b-set!))
  (if (not (fix:< index (%string-length string)))
      (error:bad-range-argument index 'vector-8b-set!))
  (if (not (and (index-fixnum? u8) (fix:<= u8 #xff)))
      (error:not-a u8? u8 'vector-8b-set!))
  (primitive-byte-set! string (fix:+ byte0-index index) u8))

(define (string-hash key #!optional modulus)
  (if (default-object? modulus)
      ((ucode-primitive string-hash) key)
      ((ucode-primitive string-hash-mod) key modulus)))

(define (string-ci-hash key #!optional modulus)
  (string-hash (string-downcase key) modulus))

;;;; Basic Operations

(define (make-string length #!optional char)
  (guarantee-string-index length 'MAKE-STRING)
  (if (default-object? char)
      (string-allocate length)
      (begin
	(guarantee char? char 'MAKE-STRING)
	(let ((result (string-allocate length)))
	  (%substring-fill! result 0 length char)
	  result))))

(define (make-vector-8b length #!optional ascii)
  (make-string length (if (default-object? ascii) ascii (integer->char ascii))))

(define (string-fill! string char #!optional start end)
  (substring-fill! string
		   (if (default-object? start) 0 start)
		   (if (default-object? end) (string-length string) end)
		   char))

(define (substring-fill! string start end char)
  (guarantee-substring string start end 'SUBSTRING-FILL)
  (guarantee char? char 'SUBSTRING-FILL)
  (%substring-fill! string start end char))

(define (%substring-fill! string start end char)
  (do ((i start (fix:+ i 1)))
      ((fix:= i end))
    (string-set! string i char)))

(define (string-null? string)
  (guarantee-string string 'STRING-NULL?)
  (%string-null? string))

(define-integrable (%string-null? string)
  (fix:= 0 (string-length string)))

(declare (integrate-operator %substring))
(define (%substring string start end)
  (let ((result (string-allocate (fix:- end start))))
    (%substring-move! string start end result 0)
    result))

(define (substring string start end)
  (guarantee-substring string start end 'SUBSTRING)
  (%substring string start end))

(define (string-head string end)
  (guarantee-string string 'STRING-HEAD)
  (guarantee-substring-end-index end (string-length string) 'STRING-HEAD)
  (%string-head string end))

(define-integrable (%string-head string end)
  (%substring string 0 end))

(define (string-tail string start)
  (guarantee-string string 'STRING-TAIL)
  (guarantee-substring-start-index start (string-length string) 'STRING-TAIL)
  (%substring string start (string-length string)))

(define (string-copy string #!optional start end)
  (substring string
	     (if (default-object? start) 0 start)
	     (if (default-object? end) (string-length string) end)))

(define (ascii-string-copy string)
  (guarantee-string string 'ASCII-STRING-COPY)
  (%ascii-string-copy string))

(define (%ascii-string-copy string)
  (let ((size (string-length string)))
    (let ((result (string-allocate size)))
      (and (%ascii-substring-move! string 0 size result 0)
	   result))))

(define (string-head! string end)
  (declare (no-type-checks) (no-range-checks))
  (guarantee-string string 'STRING-HEAD!)
  (guarantee-substring-end-index end (string-length string) 'STRING-HEAD!)
  (%string-head! string end))

(define %string-head!
  (let ((reuse
	 (named-lambda (%string-head! string end)
	   (declare (no-type-checks) (no-range-checks))
	   (let ((mask (set-interrupt-enables! interrupt-mask/none)))
	     (if (fix:< end (string-length string))
		 (begin
		   (string-set! string end #\nul)
		   (set-string-length! string end)))
	     (let ((new-gc-length (fix:+ 2 (fix:lsh end %octets->words-shift)))
		   (old-gc-length (system-vector-length string)))
	       (let ((delta (fix:- old-gc-length new-gc-length)))
		 (cond ((fix:= delta 1)
			(system-vector-set! string new-gc-length #f))
		       ((fix:> delta 1)
			(system-vector-set!
			 string new-gc-length
			 ((ucode-primitive primitive-object-set-type 2)
			  (ucode-type manifest-nm-vector) (fix:-1+ delta)))))
		 (if (fix:> delta 0)
		     ((ucode-primitive primitive-object-set! 3)
		      string
		      0
		      ((ucode-primitive primitive-object-set-type 2)
		       (ucode-type manifest-nm-vector) new-gc-length)))))
	     (set-interrupt-enables! mask)
	     string))))
    (if (compiled-procedure? reuse)
	reuse
	%string-head)))

(define (string-maximum-length string)
  (guarantee-string string 'STRING-MAXIMUM-LENGTH)
  (fix:- (fix:lsh (fix:- (system-vector-length string) 1)
		  %words->octets-shift)
	 1))

(define %octets->words-shift
  (let ((chars-per-word (bytes-per-object)))
    (case chars-per-word
      ((4) -2)
      ((8) -3)
      (else (error "Can't support this word size:" chars-per-word)))))

(define %words->octets-shift
  (- %octets->words-shift))

(define (%string-copy string)
  (let ((size (string-length string)))
    (let ((result (string-allocate size)))
      (%substring-move! string 0 size result 0)
      result)))

(define (string-copy! to at from #!optional start end)
  (substring-move! from
		   (if (default-object? start) 0 start)
		   (if (default-object? end) (string-length from) end)
		   to
		   at))

(define (string->vector string #!optional start end)
  (let ((start (if (default-object? start) 0 start))
	(end (if (default-object? end) (string-length string) end)))
    (guarantee-substring string start end 'SUBSTRING)
    (let ((result (make-vector (fix:- end start))))
      (do ((i start (fix:+ i 1)))
	  ((not (fix:< i end)))
	(vector-set! result
		     (fix:- i start)
		     (string-ref string i)))
      result)))

(define (string-map procedure string . strings)
  (if (pair? strings)
      (let ((n
	     (apply min
		    (string-length string)
		    (map string-length strings))))
	(let ((result (make-string n)))
	  (do ((i 0 (fix:+ i 1)))
	      ((not (fix:< i n)))
	    (string-set! result i
			 (apply procedure
				(string-ref string i)
				(map (lambda (string)
				       (string-ref string i))
				     strings))))
	  result))
      (let ((n (string-length string)))
	(let ((result (make-string n)))
	  (do ((i 0 (fix:+ i 1)))
	      ((not (fix:< i n)))
	    (string-set! result i (procedure (string-ref string i))))
	  result))))

(define (string-for-each procedure string . strings)
  (if (pair? strings)
      (let ((n
	     (apply min
		    (string-length string)
		    (map string-length strings))))
	(do ((i 0 (fix:+ i 1)))
	    ((not (fix:< i n)) unspecific)
	  (apply procedure
		 (string-ref string i)
		 (map (lambda (string)
			(string-ref string i))
		      strings))))
      (let ((n (string-length string)))
	(do ((i 0 (fix:+ i 1)))
	    ((not (fix:< i n)) unspecific)
	  (procedure (string-ref string i))))))

(define (string . objects)
  (%string-append (map ->string objects)))

(define (->string object)
  (cond ((string? object) object)
	((symbol? object) (symbol->string object))
	((8-bit-char? object) (make-string 1 object))
	(else (%->string object 'STRING))))

(define (%->string object caller)
  (cond ((not object) "")
	((number? object) (number->string object))
	((uri? object) (uri->string object))
	((pathname? object) (->namestring object))
	(else (error:wrong-type-argument object "string component" caller))))

(define (char->string char)
  (guarantee 8-bit-char? char 'CHAR->STRING)
  (make-string 1 char))

(define (list->string chars)
  ;; LENGTH will signal an error if CHARS is not a proper list.
  (let ((result (string-allocate (length chars))))
    (let loop ((chars chars) (index 0))
      (if (pair? chars)
	  (begin
	    (guarantee 8-bit-char? (car chars))
	    (string-set! result index (car chars))
	    (loop (cdr chars) (fix:+ index 1)))
	  result))))

(define (string->list string #!optional start end)
  (substring->list string
		   (if (default-object? start) 0 start)
		   (if (default-object? end) (string-length string) end)))

(define (substring->list string start end)
  (guarantee-substring string start end 'SUBSTRING->LIST)
  (%substring->list string start end))

(define (%substring->list string start end)
  (if (fix:= start end)
      '()
      (let loop ((index (fix:- end 1)) (chars '()))
	(if (fix:= start index)
	    (cons (string-ref string index) chars)
	    (loop (fix:- index 1) (cons (string-ref string index) chars))))))

(define (string-move! string1 string2 start2)
  (guarantee-string string1 'STRING-MOVE!)
  (guarantee-string string2 'STRING-MOVE!)
  (guarantee-string-index start2 'STRING-MOVE!)
  (let ((end1 (string-length string1)))
    (if (not (fix:<= (fix:+ start2 end1) (string-length string2)))
	(error:bad-range-argument start2 'STRING-MOVE!))
    (%substring-move! string1 0 end1 string2 start2)))

(define (substring-move! string1 start1 end1 string2 start2)
  (guarantee-substring string1 start1 end1 'SUBSTRING-MOVE!)
  (guarantee-string string2 'SUBSTRING-MOVE!)
  (guarantee-string-index start2 'SUBSTRING-MOVE!)
  (if (not (fix:<= (fix:+ start2 (fix:- end1 start1)) (string-length string2)))
      (error:bad-range-argument start2 'SUBSTRING-MOVE!))
  (%substring-move! string1 start1 end1 string2 start2))

(define (%substring-move! string1 start1 end1 string2 start2)
  ;; Calling the primitive is expensive, so avoid it for small copies.
  (let-syntax
      ((unrolled-move-left
	(sc-macro-transformer
	 (lambda (form environment)
	   environment
	   (let ((n (cadr form)))
	     `(BEGIN
		(STRING-SET! STRING2 START2 (STRING-REF STRING1 START1))
		,@(let loop ((i 1))
		    (if (< i n)
			`((STRING-SET! STRING2 (FIX:+ START2 ,i)
				       (STRING-REF STRING1 (FIX:+ START1 ,i)))
			  ,@(loop (+ i 1)))
			'())))))))
       (unrolled-move-right
	(sc-macro-transformer
	 (lambda (form environment)
	   environment
	   (let ((n (cadr form)))
	     `(BEGIN
		,@(let loop ((i 1))
		    (if (< i n)
			`(,@(loop (+ i 1))
			  (STRING-SET! STRING2 (FIX:+ START2 ,i)
				       (STRING-REF STRING1 (FIX:+ START1 ,i))))
			'()))
		(STRING-SET! STRING2 START2 (STRING-REF STRING1 START1))))))))
    (let ((n (fix:- end1 start1)))
      (if (or (not (eq? string2 string1)) (fix:< start2 start1))
	  (cond ((fix:> n 4)
		 (if (fix:> n 32)
		     (substring-move-left! string1 start1 end1 string2 start2)
		     (let loop ((i1 start1) (i2 start2))
		       (if (fix:< i1 end1)
			   (begin
			     (string-set! string2 i2 (string-ref string1 i1))
			     (loop (fix:+ i1 1) (fix:+ i2 1)))))))
		((fix:= n 4) (unrolled-move-left 4))
		((fix:= n 3) (unrolled-move-left 3))
		((fix:= n 2) (unrolled-move-left 2))
		((fix:= n 1) (unrolled-move-left 1)))
	  (cond ((fix:> n 4)
		 (if (fix:> n 32)
		     (substring-move-right! string1 start1 end1 string2 start2)
		     (let loop ((i1 end1) (i2 (fix:+ start2 n)))
		       (if (fix:> i1 start1)
			   (let ((i1 (fix:- i1 1))
				 (i2 (fix:- i2 1)))
			     (string-set! string2 i2 (string-ref string1 i1))
			     (loop i1 i2))))))
		((fix:= n 4) (unrolled-move-right 4))
		((fix:= n 3) (unrolled-move-right 3))
		((fix:= n 2) (unrolled-move-right 2))
		((fix:= n 1) (unrolled-move-right 1))))
      (fix:+ start2 n))))

;;; Almost all symbols are ascii, so it is worthwhile to handle them
;;; specially.  In this procedure, we `optimistically' move the
;;; characters, but if we find any non-ascii characters, we
;;; immediately return #F.  Success is signalled by returning the
;;; second string.  NOTE that the second string will likely be mutated
;;; in either case.
(define (%ascii-substring-move! string1 start1 end1 string2 start2)
  (let-syntax
      ((unrolled-move-left
	(sc-macro-transformer
	 (lambda (form environment)
	   environment
	   (let ((n (cadr form)))
	     `(LET ((CODE (VECTOR-8B-REF STRING1 START1)))
		(AND (FIX:< CODE #x80)
                     (BEGIN
                       (VECTOR-8B-SET! STRING2 START2 CODE)
                       ,(let loop ((i 1))
                          (if (< i n)
                              `(LET ((CODE
				      (VECTOR-8B-REF STRING1
						     (FIX:+ START1 ,i))))
                                 (AND (FIX:< CODE #x80)
                                      (BEGIN
					(VECTOR-8B-SET! STRING2
							(FIX:+ START2 ,i)
							CODE)
                                        ,(loop (+ i 1)))))
                              'STRING2)))))))))
       (unrolled-move-right
	(sc-macro-transformer
	 (lambda (form environment)
	   environment
	   (let ((n (cadr form)))
	     `(LET ((CODE (VECTOR-8B-REF STRING1 (FIX:+ START1 ,(- n 1)))))
		(AND (FIX:< CODE #x80)
                     (BEGIN
                       (VECTOR-8B-SET! STRING2 (FIX:+ START2 ,(- n 1)) CODE)
                       ,(let loop ((i (- n 1)))
                          (if (> i 0)
                              `(LET ((CODE
				      (VECTOR-8B-REF STRING1
						     (FIX:+ START1 ,(- i 1)))))
                                 (AND (FIX:< CODE #x80)
                                      (BEGIN
				       (VECTOR-8B-SET! STRING2
						       (FIX:+ START2 ,(- i 1))
						       CODE)
                                        ,(loop (- i 1)))))
                              'STRING2))))))))))
    (let ((n (fix:- end1 start1)))
      (if (or (not (eq? string2 string1)) (fix:< start2 start1))
	  (cond ((fix:> n 4)
		 (let loop ((i1 start1) (i2 start2))
		   (if (fix:< i1 end1)
		       (let ((code (vector-8b-ref string1 i1)))
			 (and (fix:< code #x80)
			      (begin
				(vector-8b-set! string2 i2 code)
				(loop (fix:+ i1 1) (fix:+ i2 1)))))
		       string2)))
		((fix:= n 4) (unrolled-move-left 4))
		((fix:= n 3) (unrolled-move-left 3))
		((fix:= n 2) (unrolled-move-left 2))
		((fix:= n 1) (unrolled-move-left 1)))
	  (cond ((fix:> n 4)
		 (let loop ((i1 end1) (i2 (fix:+ start2 n)))
		   (if (fix:> i1 start1)
		       (let ((i1 (fix:- i1 1))
			     (i2 (fix:- i2 1)))
			 (let ((code (vector-8b-ref string1 i1)))
			   (and (fix:< code #x80)
				(begin
				  (vector-8b-set! string2 i2 code)
				  (loop i1 i2)))))
		       string2)))
		((fix:= n 4) (unrolled-move-right 4))
		((fix:= n 3) (unrolled-move-right 3))
		((fix:= n 2) (unrolled-move-right 2))
		((fix:= n 1) (unrolled-move-right 1)))))))

(define (string-append . strings)
  (%string-append strings))

(define (%string-append strings)
  (let ((result
	 (string-allocate
	  (let loop ((strings strings) (length 0))
	    (if (pair? strings)
		(begin
		  (guarantee-string (car strings) 'STRING-APPEND)
		  (loop (cdr strings)
			(fix:+ (string-length (car strings)) length)))
		length)))))
    (let loop ((strings strings) (index 0))
      (if (pair? strings)
	  (let ((size (string-length (car strings))))
	    (%substring-move! (car strings) 0 size result index)
	    (loop (cdr strings) (fix:+ index size)))
	  result))))

(define (reverse-string string)
  (guarantee-string string 'REVERSE-STRING)
  (%reverse-substring string 0 (string-length string)))

(define (reverse-substring string start end)
  (guarantee-substring string start end 'REVERSE-SUBSTRING)
  (%reverse-substring string start end))

(define (%reverse-substring string start end)
  (let ((n (fix:- end start)))
    (let ((result (make-string n)))
      (do ((i start (fix:+ i 1))
	   (j (fix:- n 1) (fix:- j 1)))
	  ((fix:= i end))
	(string-set! result j (string-ref string i)))
      result)))

(define (reverse-string! string)
  (guarantee-string string 'REVERSE-STRING!)
  (%reverse-substring! string 0 (string-length string)))

(define (reverse-substring! string start end)
  (guarantee-substring string start end 'REVERSE-SUBSTRING!)
  (%reverse-substring! string start end))

(define (%reverse-substring! string start end)
  (let ((k (fix:+ start (fix:quotient (fix:- end start) 2))))
    (do ((i start (fix:+ i 1))
	 (j (fix:- end 1) (fix:- j 1)))
	((fix:= i k))
      (let ((char (string-ref string j)))
	(string-set! string j (string-ref string i))
	(string-set! string i char)))))

;;; Binary <-> textual converters

(define (string->bytevector string #!optional start end)
  (let* ((end (fix:end-index end (string-length string) 'string->bytevector))
	 (start (fix:start-index start end 'string->bytevector))
	 (bv (make-bytevector (fix:- end start))))
    (do ((i start (fix:+ i 1))
	 (j 0 (fix:+ j 1)))
	((not (fix:< i end)))
      (bytevector-u8-set! bv j (char->integer (string-ref string i))))
    bv))

(define (bytevector->string bv #!optional start end)
  (let* ((end (fix:end-index end (bytevector-length bv) 'bytevector->string))
	 (start (fix:start-index start end 'bytevector->string))
	 (string (make-string (fix:- end start))))
    (do ((i start (fix:+ i 1))
	 (j 0 (fix:+ j 1)))
	((not (fix:< i end)))
      (string-set! string j (integer->char (bytevector-u8-ref bv i))))
    string))

(define (decorated-string-append prefix infix suffix strings)
  (let ((infix (string-append suffix infix prefix)))
    (%string-append
     (if (pair? strings)
	 (cons* prefix
		(car strings)
		(let loop ((strings (cdr strings)))
		  (if (pair? strings)
		      (cons* infix
			     (car strings)
			     (loop (cdr strings)))
		      (list suffix))))
	 '()))))

(define (burst-string string delimiter allow-runs?)
  (let ((end (string-length string))
	(predicate (delimiter->predicate delimiter)))

    (define (find-start start)
      (if allow-runs?
	  (let loop ((index start))
	    (if (fix:< index end)
		(if (predicate (string-ref string index))
		    (loop (fix:+ index 1))
		    (find-end index (fix:+ index 1)))
		'()))
	  (find-end start start)))

    (define (find-end start index)
      (let loop ((index index))
	(if (fix:< index end)
	    (if (predicate (string-ref string index))
		(cons (string-copy string start index)
		      (find-start (fix:+ index 1)))
		(loop (fix:+ index 1)))
	    (list (string-copy string start end)))))

    (find-start 0)))

(define (delimiter->predicate delimiter)
  (cond ((char? delimiter) (char=-predicate delimiter))
	((char-set? delimiter) (char-set-predicate delimiter))
	((unary-procedure? delimiter) delimiter)
	(else (error:wrong-type-argument delimiter "delimiter" 'burst-string))))

(define (random-byte-vector n #!optional state)
  (let ((bv (random-bytevector n state))
	(s (make-vector-8b n)))
    (do ((i 0 (fix:+ i 1)))
	((not (fix:< i n)))
      (vector-8b-set! s i (bytevector-u8-ref bv i)))
    s))

;;;; Case

(define (string-upper-case? string)
  (guarantee-string string 'STRING-UPPER-CASE?)
  (%substring-upper-case? string 0 (string-length string)))

(define (substring-upper-case? string start end)
  (guarantee-substring string start end 'SUBSTRING-UPPER-CASE?)
  (%substring-upper-case? string start end))

(define (%substring-upper-case? string start end)
  (let find-upper ((start start))
    (and (fix:< start end)
	 (let ((char (string-ref string start)))
	   (if (char-upper-case? char)
	       (let search-rest ((start (fix:+ start 1)))
		 (or (fix:= start end)
		     (and (not (char-lower-case? (string-ref string start)))
			  (search-rest (fix:+ start 1)))))
	       (and (not (char-lower-case? char))
		    (find-upper (fix:+ start 1))))))))

(define (string-upcase string)
  (guarantee-string string 'STRING-UPCASE)
  (%string-upcase string))

(define (%string-upcase string)
  (let ((end (string-length string)))
    (let ((string* (make-string end)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i end))
	(string-set! string* i (char-upcase (string-ref string i))))
      string*)))

(define (string-upcase! string)
  (guarantee-string string 'STRING-UPCASE!)
  (%substring-upcase! string 0 (string-length string)))

(define (substring-upcase! string start end)
  (guarantee-substring string start end 'SUBSTRING-UPCASE!)
  (%substring-upcase! string start end))

(define (%substring-upcase! string start end)
  (do ((i start (fix:+ i 1)))
      ((fix:= i end))
    (string-set! string i (char-upcase (string-ref string i)))))

(define (string-lower-case? string)
  (guarantee-string string 'STRING-LOWER-CASE?)
  (%substring-lower-case? string 0 (string-length string)))

(define (substring-lower-case? string start end)
  (guarantee-substring string start end 'SUBSTRING-LOWER-CASE?)
  (%substring-lower-case? string start end))

(define (%substring-lower-case? string start end)
  (let find-lower ((start start))
    (and (fix:< start end)
	 (let ((char (string-ref string start)))
	   (if (char-lower-case? char)
	       (let search-rest ((start (fix:+ start 1)))
		 (or (fix:= start end)
		     (and (not (char-upper-case? (string-ref string start)))
			  (search-rest (fix:+ start 1)))))
	       (and (not (char-upper-case? char))
		    (find-lower (fix:+ start 1))))))))

(define (string-downcase string)
  (guarantee-string string 'STRING-DOWNCASE)
  (%string-downcase string))

(define (%string-downcase string)
  (let ((end (string-length string)))
    (let ((string* (make-string end)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i end))
	(string-set! string* i (char-downcase (string-ref string i))))
      string*)))

(define (string-downcase! string)
  (guarantee-string string 'STRING-DOWNCASE!)
  (substring-downcase! string 0 (string-length string)))

(define (substring-downcase! string start end)
  (guarantee-substring string start end 'SUBSTRING-DOWNCASE!)
  (%substring-downcase! string start end))

(define (%substring-downcase! string start end)
  (do ((i start (fix:+ i 1)))
      ((fix:= i end))
    (string-set! string i (char-downcase (string-ref string i)))))

(define (string-capitalized? string)
  (guarantee-string string 'STRING-CAPITALIZED?)
  (substring-capitalized? string 0 (string-length string)))

(define (substring-capitalized? string start end)
  (guarantee-substring string start end 'SUBSTRING-CAPITALIZED?)
  (%substring-capitalized? string start end))

(define (%substring-capitalized? string start end)
  ;; Testing for capitalization is somewhat more involved than testing
  ;; for upper or lower case.  This algorithm requires that the first
  ;; word be capitalized, and that the subsequent words be either
  ;; lower case or capitalized.  This is a very general definition of
  ;; capitalization; if you need something more specific you should
  ;; call this procedure on the individual words.
  (letrec
      ((find-first-word
	(lambda (start)
	  (and (fix:< start end)
	       (let ((char (string-ref string start)))
		 (if (char-upper-case? char)
		     (scan-word-tail (fix:+ start 1))
		     (and (not (char-lower-case? char))
			  (find-first-word (fix:+ start 1))))))))
       (scan-word-tail
	(lambda (start)
	  (or (fix:= start end)
	      (let ((char (string-ref string start)))
		(if (char-lower-case? char)
		    (scan-word-tail (fix:+ start 1))
		    (and (not (char-upper-case? char))
			 (find-subsequent-word (fix:+ start 1))))))))
       (find-subsequent-word
	(lambda (start)
	  (or (fix:= start end)
	      (let ((char (string-ref string start)))
		(if (char-alphabetic? char)
		    (scan-word-tail (fix:+ start 1))
		    (find-subsequent-word (fix:+ start 1))))))))
    (find-first-word start)))

(define (string-capitalize string)
  (guarantee-string string 'STRING-CAPITALIZE)
  (let ((string (%string-copy string)))
    (%substring-capitalize! string 0 (string-length string))
    string))

(define (string-capitalize! string)
  (guarantee-string string 'STRING-CAPITALIZE!)
  (%substring-capitalize! string 0 (string-length string)))

(define (substring-capitalize! string start end)
  (guarantee-substring string start end 'SUBSTRING-CAPITALIZE!)
  (%substring-capitalize! string start end))

(define (%substring-capitalize! string start end)
  ;; This algorithm capitalizes the first word in the substring and
  ;; downcases the subsequent words.  This is arbitrary, but seems
  ;; useful if the substring happens to be a sentence.  Again, if you
  ;; need finer control, parse the words yourself.
  (let ((index
	 (%substring-find-next-char-in-set string start end
					   char-set:alphabetic)))
    (if index
	(begin
	  (%substring-upcase! string index (fix:+ index 1))
	  (%substring-downcase! string (fix:+ index 1) end)))))

;;;; Replace

(define (string-replace string char1 char2)
  (guarantee-string string 'STRING-REPLACE)
  (guarantee char? char1 'STRING-REPLACE)
  (guarantee char? char2 'STRING-REPLACE)
  (let ((string (%string-copy string)))
    (%substring-replace! string 0 (string-length string) char1 char2)
    string))

(define (substring-replace string start end char1 char2)
  (guarantee-substring string start end 'SUBSTRING-REPLACE)
  (guarantee char? char1 'SUBSTRING-REPLACE)
  (guarantee char? char2 'SUBSTRING-REPLACE)
  (let ((string (%string-copy string)))
    (%substring-replace! string start end char1 char2)
    string))

(define (string-replace! string char1 char2)
  (guarantee-string string 'STRING-REPLACE!)
  (guarantee char? char1 'STRING-REPLACE!)
  (guarantee char? char2 'STRING-REPLACE!)
  (%substring-replace! string 0 (string-length string) char1 char2))

(define (substring-replace! string start end char1 char2)
  (guarantee-substring string start end 'SUBSTRING-REPLACE!)
  (guarantee char? char1 'SUBSTRING-REPLACE!)
  (guarantee char? char2 'SUBSTRING-REPLACE!)
  (%substring-replace! string start end char1 char2))

(define (%substring-replace! string start end char1 char2)
  (let loop ((start start))
    (let ((index (%substring-find-next-char string start end char1)))
      (if index
	  (begin
	    (string-set! string index char2)
	    (loop (fix:+ index 1)))))))

;;;; Compare

(define (string-compare string1 string2 if= if< if>)
  (guarantee-2-strings string1 string2 'STRING-COMPARE)
  (%string-compare string1 string2 if= if< if>))

(define (%string-compare string1 string2 if= if< if>)
  (let ((length1 (string-length string1))
	(length2 (string-length string2)))
    (let ((end (fix:min length1 length2)))
      (let loop ((index 0))
	(cond ((fix:= index end)
	       (if (fix:= index length1)
		   (if (fix:= index length2)
		       (if=)
		       (if<))
		   (if>)))
	      ((char=? (string-ref string1 index)
		       (string-ref string2 index))
	       (loop (fix:+ index 1)))
	      ((char<? (string-ref string1 index)
		       (string-ref string2 index))
	       (if<))
	      (else
	       (if>)))))))

(define (string-compare-ci string1 string2 if= if< if>)
  (guarantee-2-strings string1 string2 'STRING-COMPARE-CI)
  (%string-compare-ci string1 string2 if= if< if>))

(define (%string-compare-ci string1 string2 if= if< if>)
  (let ((length1 (string-length string1))
	(length2 (string-length string2)))
    (let ((end (fix:min length1 length2)))
      (let loop ((index 0))
	(cond ((fix:= index end)
	       (if (fix:= index length1)
		   (if (fix:= index length2)
		       (if=)
		       (if<))
		   (if>)))
	      ((char-ci=? (string-ref string1 index)
			  (string-ref string2 index))
	       (loop (fix:+ index 1)))
	      ((char-ci<? (string-ref string1 index)
			  (string-ref string2 index))
	       (if<))
	      (else
	       (if>)))))))

(define (string-prefix? string1 string2)
  (guarantee-2-strings string1 string2 'STRING-PREFIX?)
  (%substring-prefix? string1 0 (string-length string1)
		      string2 0 (string-length string2)))

(define (substring-prefix? string1 start1 end1 string2 start2 end2)
  (guarantee-2-substrings string1 start1 end1
			  string2 start2 end2
			  'SUBSTRING-PREFIX?)
  (%substring-prefix? string1 start1 end1
		      string2 start2 end2))

(define (%substring-prefix? string1 start1 end1 string2 start2 end2)
  (let ((length (fix:- end1 start1)))
    (and (fix:<= length (fix:- end2 start2))
	 (fix:= (%substring-match-forward string1 start1 end1
					  string2 start2 end2)
		length))))

(define (string-prefix-ci? string1 string2)
  (guarantee-2-strings string1 string2 'STRING-PREFIX-CI?)
  (%substring-prefix-ci? string1 0 (string-length string1)
			 string2 0 (string-length string2)))

(define (substring-prefix-ci? string1 start1 end1 string2 start2 end2)
  (guarantee-2-substrings string1 start1 end1
			  string2 start2 end2
			  'SUBSTRING-PREFIX-CI?)
  (%substring-prefix-ci? string1 start1 end1
			 string2 start2 end2))

(define (%substring-prefix-ci? string1 start1 end1 string2 start2 end2)
  (let ((length (fix:- end1 start1)))
    (and (fix:<= length (fix:- end2 start2))
	 (fix:= (%substring-match-forward-ci string1 start1 end1
					     string2 start2 end2)
		length))))

(define (string-suffix? string1 string2)
  (guarantee-2-strings string1 string2 'STRING-SUFFIX?)
  (%substring-suffix? string1 0 (string-length string1)
		      string2 0 (string-length string2)))

(define (substring-suffix? string1 start1 end1 string2 start2 end2)
  (guarantee-2-substrings string1 start1 end1
			  string2 start2 end2
			  'SUBSTRING-SUFFIX?)
  (%substring-suffix? string1 start1 end1
		      string2 start2 end2))

(define (%substring-suffix? string1 start1 end1 string2 start2 end2)
  (let ((length (fix:- end1 start1)))
    (and (fix:<= length (fix:- end2 start2))
	 (fix:= (%substring-match-backward string1 start1 end1
					   string2 start2 end2)
		length))))

(define (string-suffix-ci? string1 string2)
  (guarantee-2-strings string1 string2 'STRING-SUFFIX-CI?)
  (%substring-suffix-ci? string1 0 (string-length string1)
			 string2 0 (string-length string2)))

(define (substring-suffix-ci? string1 start1 end1 string2 start2 end2)
  (guarantee-2-substrings string1 start1 end1
			  string2 start2 end2
			  'SUBSTRING-SUFFIX-CI?)
  (%substring-suffix-ci? string1 start1 end1
			 string2 start2 end2))

(define (%substring-suffix-ci? string1 start1 end1 string2 start2 end2)
  (let ((length (fix:- end1 start1)))
    (and (fix:<= length (fix:- end2 start2))
	 (fix:= (%substring-match-backward-ci string1 start1 end1
					      string2 start2 end2)
		length))))

(define (string=? string1 string2)
  (guarantee-2-strings string1 string2 'STRING=?)
  (%string=? string1 string2))

(define (%string=? string1 string2)
  (let ((end (string-length string1)))
    (and (fix:= end (string-length string2))
	 (let loop ((i 0))
	   (or (fix:= i end)
	       (and (char=? (string-ref string1 i) (string-ref string2 i))
		    (loop (fix:+ i 1))))))))

(define (string-ci=? string1 string2)
  (guarantee-2-strings string1 string2 'STRING-CI=?)
  (%string-ci=? string1 string2))

(define (%string-ci=? string1 string2)
  (let ((end (string-length string1)))
    (and (fix:= end (string-length string2))
	 (let loop ((i 0))
	   (or (fix:= i end)
	       (and (char-ci=? (string-ref string1 i) (string-ref string2 i))
		    (loop (fix:+ i 1))))))))

(define (substring=? string1 start1 end1 string2 start2 end2)
  (guarantee-2-substrings string1 start1 end1
			  string2 start2 end2
			  'SUBSTRING=?)
  (%substring=? string1 start1 end1 string2 start2 end2))

(define (%substring=? string1 start1 end1 string2 start2 end2)
  (and (fix:= (fix:- end1 start1) (fix:- end2 start2))
       (let loop ((i1 start1) (i2 start2))
	 (or (fix:= i1 end1)
	     (and (char=? (string-ref string1 i1) (string-ref string2 i2))
		  (loop (fix:+ i1 1) (fix:+ i2 1)))))))

(define (substring-ci=? string1 start1 end1 string2 start2 end2)
  (guarantee-2-substrings string1 start1 end1
			  string2 start2 end2
			  'SUBSTRING-CI=?)
  (%substring-ci=? string1 start1 end1 string2 start2 end2))

(define (%substring-ci=? string1 start1 end1 string2 start2 end2)
  (and (fix:= (fix:- end1 start1) (fix:- end2 start2))
       (let loop ((i1 start1) (i2 start2))
	 (or (fix:= i1 end1)
	     (and (char-ci=? (string-ref string1 i1) (string-ref string2 i2))
		  (loop (fix:+ i1 1) (fix:+ i2 1)))))))

(define (string<? string1 string2)
  (guarantee-2-strings string1 string2 'STRING<?)
  (%string<? string1 string2))

(define (%string<? string1 string2)
  (let ((end1 (string-length string1))
	(end2 (string-length string2)))
    (let ((end (fix:min end1 end2)))
      (let loop ((i 0))
	(if (fix:= i end)
	    (fix:< end1 end2)
	    (or (char<? (string-ref string1 i) (string-ref string2 i))
		(and (char=? (string-ref string1 i) (string-ref string2 i))
		     (loop (fix:+ i 1)))))))))

(define (string-ci<? string1 string2)
  (guarantee-2-strings string1 string2 'STRING-CI<?)
  (%string-ci<? string1 string2))

(define (%string-ci<? string1 string2)
  (let ((end1 (string-length string1))
	(end2 (string-length string2)))
    (let ((end (fix:min end1 end2)))
      (let loop ((i 0))
	(if (fix:= i end)
	    (fix:< end1 end2)
	    (or (char-ci<? (string-ref string1 i) (string-ref string2 i))
		(and (char-ci=? (string-ref string1 i) (string-ref string2 i))
		     (loop (fix:+ i 1)))))))))

(define (substring<? string1 start1 end1 string2 start2 end2)
  (guarantee-2-substrings string1 start1 end1
			  string2 start2 end2
			  'SUBSTRING<?)
  (%substring<? string1 start1 end1 string2 start2 end2))

(define (%substring<? string1 start1 end1 string2 start2 end2)
  (let ((len1 (fix:- end1 start1))
	(len2 (fix:- end2 start2)))
    (let ((end (fix:+ start1 (fix:min len1 len2))))
      (let loop ((i1 start1) (i2 start2))
	(if (fix:= i1 end)
	    (fix:< len1 len2)
	    (or (char<? (string-ref string1 i1) (string-ref string2 i2))
		(and (char=? (string-ref string1 i1) (string-ref string2 i2))
		     (loop (fix:+ i1 1) (fix:+ i2 1)))))))))

(define (substring-ci<? string1 start1 end1 string2 start2 end2)
  (guarantee-2-substrings string1 start1 end1
			  string2 start2 end2
			  'SUBSTRING-CI<?)
  (%substring-ci<? string1 start1 end1 string2 start2 end2))

(define (%substring-ci<? string1 start1 end1 string2 start2 end2)
  (let ((len1 (fix:- end1 start1))
	(len2 (fix:- end2 start2)))
    (let ((end (fix:+ start1 (fix:min len1 len2))))
      (let loop ((i1 start1) (i2 start2))
	(if (fix:= i1 end)
	    (fix:< len1 len2)
	    (or (char-ci<? (string-ref string1 i1) (string-ref string2 i2))
		(and (char-ci=? (string-ref string1 i1)
				(string-ref string2 i2))
		     (loop (fix:+ i1 1) (fix:+ i2 1)))))))))

(define-integrable (string>? string1 string2)
  (string<? string2 string1))

(define-integrable (string-ci>? string1 string2)
  (string-ci<? string2 string1))

(define-integrable (string>=? string1 string2)
  (not (string<? string1 string2)))

(define-integrable (string-ci>=? string1 string2)
  (not (string-ci<? string1 string2)))

(define-integrable (string<=? string1 string2)
  (not (string<? string2 string1)))

(define-integrable (string-ci<=? string1 string2)
  (not (string-ci<? string2 string1)))

(define (string-match-forward string1 string2)
  (guarantee-2-strings string1 string2 'STRING-MATCH-FORWARD)
  (%substring-match-forward string1 0 (string-length string1)
			    string2 0 (string-length string2)))

(define (substring-match-forward string1 start1 end1 string2 start2 end2)
  (guarantee-2-substrings string1 start1 end1
			  string2 start2 end2
			  'SUBSTRING-MATCH-FORWARD)
  (%substring-match-forward string1 start1 end1 string2 start2 end2))

(define (%substring-match-forward string1 start1 end1 string2 start2 end2)
  (let ((end (fix:+ start1 (fix:min (fix:- end1 start1) (fix:- end2 start2)))))
    (let loop ((i1 start1) (i2 start2))
      (if (or (fix:= i1 end)
	      (not (char=? (string-ref string1 i1)
			   (string-ref string2 i2))))
	  (fix:- i1 start1)
	  (loop (fix:+ i1 1) (fix:+ i2 1))))))

(define (string-match-forward-ci string1 string2)
  (guarantee-2-strings string1 string2 'STRING-MATCH-FORWARD-CI)
  (%substring-match-forward-ci string1 0 (string-length string1)
			       string2 0 (string-length string2)))

(define (substring-match-forward-ci string1 start1 end1 string2 start2 end2)
  (guarantee-2-substrings string1 start1 end1
			  string2 start2 end2
			  'SUBSTRING-MATCH-FORWARD-CI)
  (%substring-match-forward-ci string1 start1 end1 string2 start2 end2))

(define (%substring-match-forward-ci string1 start1 end1 string2 start2 end2)
  (let ((end (fix:+ start1 (fix:min (fix:- end1 start1) (fix:- end2 start2)))))
    (let loop ((i1 start1) (i2 start2))
      (if (or (fix:= i1 end)
	      (not (char-ci=? (string-ref string1 i1)
			      (string-ref string2 i2))))
	  (fix:- i1 start1)
	  (loop (fix:+ i1 1) (fix:+ i2 1))))))

(define (string-match-backward string1 string2)
  (guarantee-2-strings string1 string2 'STRING-MATCH-BACKWARD)
  (%substring-match-backward string1 0 (string-length string1)
			     string2 0 (string-length string2)))

(define (substring-match-backward string1 start1 end1 string2 start2 end2)
  (guarantee-2-substrings string1 start1 end1
			  string2 start2 end2
			  'SUBSTRING-MATCH-BACKWARD)
  (%substring-match-backward string1 start1 end1 string2 start2 end2))

(define (%substring-match-backward string1 start1 end1 string2 start2 end2)
  (let ((start (fix:- end1 (fix:min (fix:- end1 start1) (fix:- end2 start2)))))
    (if (fix:= end1 start)
	0
	(let loop ((i1 (fix:- end1 1)) (i2 (fix:- end2 1)))
	  (if (char=? (string-ref string1 i1) (string-ref string2 i2))
	      (if (fix:= i1 start)
		  (fix:- end1 i1)
		  (loop (fix:- i1 1) (fix:- i2 1)))
	      (fix:- end1 (fix:+ i1 1)))))))

(define (string-match-backward-ci string1 string2)
  (guarantee-2-strings string1 string2 'STRING-MATCH-BACKWARD-CI)
  (%substring-match-backward-ci string1 0 (string-length string1)
				string2 0 (string-length string2)))

(define (substring-match-backward-ci string1 start1 end1 string2 start2 end2)
  (guarantee-2-substrings string1 start1 end1
			  string2 start2 end2
			  'SUBSTRING-MATCH-BACKWARD-CI)
  (%substring-match-backward-ci string1 start1 end1 string2 start2 end2))

(define (%substring-match-backward-ci string1 start1 end1 string2 start2 end2)
  (let ((start (fix:- end1 (fix:min (fix:- end1 start1) (fix:- end2 start2)))))
    (if (fix:= end1 start)
	0
	(let loop ((i1 (fix:- end1 1)) (i2 (fix:- end2 1)))
	  (if (char-ci=? (string-ref string1 i1) (string-ref string2 i2))
	      (if (fix:= i1 start)
		  (fix:- end1 i1)
		  (loop (fix:- i1 1) (fix:- i2 1)))
	      (fix:- end1 (fix:+ i1 1)))))))

;;;; Trim

(define (string-trim-left string #!optional char-set)
  (let ((index
	 (string-find-next-char-in-set string
				       (if (default-object? char-set)
					   char-set:not-whitespace
					   char-set))))
    (if index
	(%substring string index (string-length string))
	"")))

(define (string-trim-right string #!optional char-set)
  (let ((index
	 (string-find-previous-char-in-set string
					   (if (default-object? char-set)
					       char-set:not-whitespace
					       char-set))))
    (if index
	(%substring string 0 (fix:+ index 1))
	"")))

(define (string-trim string #!optional char-set)
  (let* ((char-set
	 (if (default-object? char-set)
	     char-set:not-whitespace
	     char-set))
	 (index (string-find-next-char-in-set string char-set)))
    (if index
	(%substring string
		    index
		    (fix:+ (string-find-previous-char-in-set string char-set)
			   1))
	"")))

;;;; Pad

(define (string-pad-right string n #!optional char)
  (guarantee-string string 'STRING-PAD-RIGHT)
  (guarantee-string-index n 'STRING-PAD-RIGHT)
  (let ((length (string-length string)))
    (if (fix:= length n)
	string
	(let ((result (string-allocate n)))
	  (if (fix:> length n)
	      (%substring-move! string 0 n result 0)
	      (begin
		(%substring-move! string 0 length result 0)
		(%substring-fill! result length n
				  (if (default-object? char)
				      #\space
				      (begin
					(guarantee char? char 'STRING-PAD-RIGHT)
					char)))))
	  result))))

(define (string-pad-left string n #!optional char)
  (guarantee-string string 'STRING-PAD-LEFT)
  (guarantee-string-index n 'STRING-PAD-LEFT)
  (let ((length (string-length string)))
    (if (fix:= length n)
	string
	(let ((result (string-allocate n))
	      (i (fix:- n length)))
	  (if (fix:< i 0)
	      (%substring-move! string (fix:- 0 i) length result 0)
	      (begin
		(%substring-fill! result 0 i
				  (if (default-object? char)
				      #\space
				      (begin
					(guarantee char? char 'STRING-PAD-RIGHT)
					char)))
		(%substring-move! string 0 length result i)))
	  result))))

;;;; Character search

(define (string-find-next-char string char)
  (guarantee-string string 'STRING-FIND-NEXT-CHAR)
  (guarantee char? char 'STRING-FIND-NEXT-CHAR)
  (%substring-find-next-char string 0 (string-length string) char))

(define (substring-find-next-char string start end char)
  (guarantee-substring string start end 'SUBSTRING-FIND-NEXT-CHAR)
  (guarantee char? char 'SUBSTRING-FIND-NEXT-CHAR)
  (%substring-find-next-char string start end char))

(define (%substring-find-next-char string start end char)
  (let loop ((i start))
    (cond ((fix:= i end) #f)
	  ((char=? (string-ref string i) char) i)
	  (else (loop (fix:+ i 1))))))

(define (string-find-next-char-ci string char)
  (guarantee-string string 'STRING-FIND-NEXT-CHAR-CI)
  (guarantee char? char 'STRING-FIND-NEXT-CHAR-CI)
  (%substring-find-next-char-ci string 0 (string-length string) char))

(define (substring-find-next-char-ci string start end char)
  (guarantee-substring string start end 'SUBSTRING-FIND-NEXT-CHAR-CI)
  (guarantee char? char 'SUBSTRING-FIND-NEXT-CHAR-CI)
  (%substring-find-next-char-ci string start end char))

(define (%substring-find-next-char-ci string start end char)
  (let loop ((i start))
    (cond ((fix:= i end) #f)
	  ((char-ci=? (string-ref string i) char) i)
	  (else (loop (fix:+ i 1))))))

(define (string-find-previous-char string char)
  (guarantee-string string 'STRING-FIND-PREVIOUS-CHAR)
  (guarantee char? char 'STRING-FIND-PREVIOUS-CHAR)
  (%substring-find-previous-char string 0 (string-length string) char))

(define (substring-find-previous-char string start end char)
  (guarantee-substring string start end 'SUBSTRING-FIND-PREVIOUS-CHAR)
  (guarantee char? char 'SUBSTRING-FIND-PREVIOUS-CHAR)
  (%substring-find-previous-char string start end char))

(define (%substring-find-previous-char string start end char)
  (if (fix:= start end)
      #f
      (let loop ((i (fix:- end 1)))
	(cond ((char=? (string-ref string i) char) i)
	      ((fix:= start i) #f)
	      (else (loop (fix:- i 1)))))))

(define (string-find-previous-char-ci string char)
  (guarantee-string string 'STRING-FIND-PREVIOUS-CHAR-CI)
  (guarantee char? char 'STRING-FIND-PREVIOUS-CHAR-CI)
  (%substring-find-previous-char-ci string 0 (string-length string) char))

(define (substring-find-previous-char-ci string start end char)
  (guarantee-substring string start end 'SUBSTRING-FIND-PREVIOUS-CHAR-CI)
  (guarantee char? char 'SUBSTRING-FIND-PREVIOUS-CHAR-CI)
  (%substring-find-previous-char-ci string start end char))

(define (%substring-find-previous-char-ci string start end char)
  (if (fix:= start end)
      #f
      (let loop ((i (fix:- end 1)))
	(cond ((char-ci=? (string-ref string i) char) i)
	      ((fix:= start i) #f)
	      (else (loop (fix:- i 1)))))))

(define (string-find-next-char-in-set string char-set)
  (guarantee-string string 'STRING-FIND-NEXT-CHAR-IN-SET)
  (guarantee char-set? char-set 'STRING-FIND-NEXT-CHAR-IN-SET)
  (%substring-find-next-char-in-set string 0 (string-length string) char-set))

(define (substring-find-next-char-in-set string start end char-set)
  (guarantee-substring string start end 'SUBSTRING-FIND-NEXT-CHAR-IN-SET)
  (guarantee char-set? char-set 'SUBSTRING-FIND-NEXT-CHAR-IN-SET)
  (%substring-find-next-char-in-set string start end char-set))

(define-integrable (%substring-find-next-char-in-set string start end char-set)
  ((ucode-primitive substring-find-next-char-in-set)
   string start end (char-set-table char-set)))

(define (string-find-previous-char-in-set string char-set)
  (guarantee-string string 'STRING-FIND-PREVIOUS-CHAR-IN-SET)
  (guarantee char-set? char-set 'STRING-FIND-PREVIOUS-CHAR-IN-SET)
  (%substring-find-previous-char-in-set string 0 (string-length string)
					char-set))

(define (substring-find-previous-char-in-set string start end char-set)
  (guarantee-substring string start end 'SUBSTRING-FIND-PREVIOUS-CHAR-IN-SET)
  (guarantee char-set? char-set 'SUBSTRING-FIND-PREVIOUS-CHAR-IN-SET)
  (%substring-find-previous-char-in-set string start end char-set))

(define (%substring-find-previous-char-in-set string start end char-set)
  ((ucode-primitive substring-find-previous-char-in-set)
   string start end (char-set-table char-set)))

;;;; String search

(define (substring? pattern text)
  (and (string-search-forward pattern text) #t))

(define (string-search-forward pattern text)
  (guarantee-string pattern 'STRING-SEARCH-FORWARD)
  (guarantee-string text 'STRING-SEARCH-FORWARD)
  (%substring-search-forward text 0 (string-length text)
			     pattern 0 (string-length pattern)))

(define (substring-search-forward pattern text tstart tend)
  (guarantee-string pattern 'SUBSTRING-SEARCH-FORWARD)
  (guarantee-substring text tstart tend 'SUBSTRING-SEARCH-FORWARD)
  (%substring-search-forward text tstart tend
			     pattern 0 (string-length pattern)))

(define (string-search-backward pattern text)
  (guarantee-string pattern 'STRING-SEARCH-BACKWARD)
  (guarantee-string text 'STRING-SEARCH-BACKWARD)
  (%substring-search-backward text 0 (string-length text)
			      pattern 0 (string-length pattern)))

(define (substring-search-backward pattern text tstart tend)
  (guarantee-string pattern 'SUBSTRING-SEARCH-BACKWARD)
  (guarantee-substring text tstart tend 'SUBSTRING-SEARCH-BACKWARD)
  (%substring-search-backward text tstart tend
			      pattern 0 (string-length pattern)))

(define (string-search-all pattern text)
  (guarantee-string pattern 'STRING-SEARCH-ALL)
  (guarantee-string text 'STRING-SEARCH-ALL)
  (%substring-search-all text 0 (string-length text)
			 pattern 0 (string-length pattern)))

(define (substring-search-all pattern text tstart tend)
  (guarantee-string pattern 'SUBSTRING-SEARCH-ALL)
  (guarantee-substring text tstart tend 'SUBSTRING-SEARCH-ALL)
  (%substring-search-all text tstart tend
			 pattern 0 (string-length pattern)))

(define (%substring-search-forward text tstart tend pattern pstart pend)
  ;; Returns index of first matched char, or #F.
  (if (fix:< (fix:- pend pstart) 4)
      (%dumb-substring-search-forward text tstart tend pattern pstart pend)
      (%bm-substring-search-forward text tstart tend pattern pstart pend)))

(define (%dumb-substring-search-forward text tstart tend pattern pstart pend)
  (if (fix:= pstart pend)
      0
      (let* ((leader (string-ref pattern pstart))
	     (plen (fix:- pend pstart))
	     (tend (fix:- tend plen)))
	(let loop ((tstart tstart))
	  (let ((tstart
		 (let find-leader ((tstart tstart))
		   (and (fix:<= tstart tend)
			(if (char=? leader (string-ref text tstart))
			    tstart
			    (find-leader (fix:+ tstart 1)))))))
	    (and tstart
		 (if (substring=? text (fix:+ tstart 1) (fix:+ tstart plen)
				  pattern (fix:+ pstart 1) pend)
		     tstart
		     (loop (fix:+ tstart 1)))))))))

(define (%substring-search-backward text tstart tend pattern pstart pend)
  ;; Returns index following last matched char, or #F.
  (if (fix:< (fix:- pend pstart) 4)
      (%dumb-substring-search-backward text tstart tend pattern pstart pend)
      (%bm-substring-search-backward text tstart tend pattern pstart pend)))

(define (%dumb-substring-search-backward text tstart tend pattern pstart pend)
  (if (fix:= pstart pend)
      0
      (let* ((pend-1 (fix:- pend 1))
	     (trailer (string-ref pattern pend-1))
	     (plen (fix:- pend pstart))
	     (tstart+plen (fix:+ tstart plen)))
	(let loop ((tend tend))
	  (let ((tend
		 (let find-trailer ((tend tend))
		   (and (fix:<= tstart+plen tend)
			(if (char=? trailer (string-ref text (fix:- tend 1)))
			    tend
			    (find-trailer (fix:- tend 1)))))))
	    (and tend
		 (if (substring=? text (fix:- tend plen) (fix:- tend 1)
				  pattern pstart pend-1)
		     tend
		     (loop (fix:- tend 1)))))))))

(define (%substring-search-all text tstart tend pattern pstart pend)
  (let ((plen (fix:- pend pstart)))
    (cond ((fix:= plen 1)
	   (let ((c (string-ref pattern pstart)))
	     (let loop ((ti tend) (occurrences '()))
	       (let ((index (%substring-find-previous-char text tstart ti c)))
		 (if index
		     (loop index (cons index occurrences))
		     occurrences)))))
	  #;    ;This may not be worthwhile -- I have no measurements.
	  ((fix:< plen 4)
	   (let loop ((ti tend) (occurrences '()))
	     (let ((index
		    (%dumb-substring-search-backward text tstart ti
						     pattern pstart pend)))
	       (if index
		   (loop (fix:+ index (fix:- plen 1)) (cons index occurrences))
		   occurrences))))
	  (else
	   (%bm-substring-search-all text tstart tend pattern pstart pend)))))

;;;; Boyer-Moore String Search

;;; Cormen, Leiserson, and Rivest, "Introduction to Algorithms",
;;; Chapter 34, "String Matching".

(define (%bm-substring-search-forward text tstart tend pattern pstart pend)
  (let ((m (fix:- pend pstart))
	(pstart-1 (fix:- pstart 1))
	(pend-1 (fix:- pend 1))
	(lambda* (compute-last-occurrence-function pattern pstart pend))
	(gamma
	 (compute-good-suffix-function pattern pstart pend
				       (compute-gamma0 pattern pstart pend))))
    (let ((tend-m (fix:- tend m))
	  (m-1 (fix:- m 1)))
      (let outer ((s tstart))
	(and (fix:<= s tend-m)
	     (let inner ((pj pend-1) (tj (fix:+ s m-1)))
	       (if (fix:= (vector-8b-ref pattern pj) (vector-8b-ref text tj))
		   (if (fix:= pstart pj)
		       s
		       (inner (fix:- pj 1) (fix:- tj 1)))
		   (outer
		    (fix:+ s
			   (fix:max (fix:- (fix:- pj pstart-1)
					   (lambda* (vector-8b-ref text tj)))
				    (gamma (fix:- pj pstart))))))))))))

(define (%bm-substring-search-backward text tstart tend pattern pstart pend)
  (let ((m (fix:- pend pstart))
	(pend-1 (fix:- pend 1))
	(rpattern (reverse-substring pattern pstart pend)))
    (let ((tstart+m (fix:+ tstart m))
	  (lambda* (compute-last-occurrence-function rpattern 0 m))
	  (gamma
	   (compute-good-suffix-function rpattern 0 m
					 (compute-gamma0 rpattern 0 m))))
      (let outer ((s tend))
	(and (fix:>= s tstart+m)
	     (let inner ((pj pstart) (tj (fix:- s m)))
	       (if (fix:= (vector-8b-ref pattern pj) (vector-8b-ref text tj))
		   (if (fix:= pend-1 pj)
		       s
		       (inner (fix:+ pj 1) (fix:+ tj 1)))
		   (outer
		    (fix:- s
			   (fix:max (fix:- (fix:- pend pj)
					   (lambda* (vector-8b-ref text tj)))
				    (gamma (fix:- pend-1 pj))))))))))))

(define (%bm-substring-search-all text tstart tend pattern pstart pend)
  (let ((m (fix:- pend pstart))
	(pstart-1 (fix:- pstart 1))
	(pend-1 (fix:- pend 1))
	(lambda* (compute-last-occurrence-function pattern pstart pend))
	(gamma0 (compute-gamma0 pattern pstart pend)))
    (let ((gamma (compute-good-suffix-function pattern pstart pend gamma0))
	  (tend-m (fix:- tend m))
	  (m-1 (fix:- m 1)))
      (let outer ((s tstart) (occurrences '()))
	(if (fix:<= s tend-m)
	    (let inner ((pj pend-1) (tj (fix:+ s m-1)))
	      (if (fix:= (vector-8b-ref pattern pj) (vector-8b-ref text tj))
		  (if (fix:= pstart pj)
		      (outer (fix:+ s gamma0) (cons s occurrences))
		      (inner (fix:- pj 1) (fix:- tj 1)))
		  (outer (fix:+ s
				(fix:max (fix:- (fix:- pj pstart-1)
						(lambda*
						 (vector-8b-ref text tj)))
					 (gamma (fix:- pj pstart))))
			 occurrences)))
	    (reverse! occurrences))))))

(define (compute-last-occurrence-function pattern pstart pend)
  (let ((lam (make-vector 256 0)))
    (do ((j pstart (fix:+ j 1)))
	((fix:= j pend))
      (vector-set! lam
		   (vector-8b-ref pattern j)
		   (fix:+ (fix:- j pstart) 1)))
    (lambda (symbol)
      (vector-ref lam symbol))))

(define (compute-good-suffix-function pattern pstart pend gamma0)
  (let ((m (fix:- pend pstart)))
    (let ((pi
	   (compute-prefix-function (reverse-substring pattern pstart pend)
				    0 m))
	  (gamma (make-vector m gamma0))
	  (m-1 (fix:- m 1)))
      (do ((l 0 (fix:+ l 1)))
	  ((fix:= l m))
	(let ((j (fix:- m-1 (vector-ref pi l)))
	      (k (fix:- (fix:+ 1 l) (vector-ref pi l))))
	  (if (fix:< k (vector-ref gamma j))
	      (vector-set! gamma j k))))
      (lambda (index)
	(vector-ref gamma index)))))

(define (compute-gamma0 pattern pstart pend)
  (let ((m (fix:- pend pstart)))
    (fix:- m
	   (vector-ref (compute-prefix-function pattern pstart pend)
		       (fix:- m 1)))))

(define (compute-prefix-function pattern pstart pend)
  (let* ((m (fix:- pend pstart))
	 (pi (make-vector m)))
    (vector-set! pi 0 0)
    (let outer ((k 0) (q 1))
      (if (fix:< q m)
	  (let ((k
		 (let ((pq (vector-8b-ref pattern (fix:+ pstart q))))
		   (let inner ((k k))
		     (cond ((fix:= pq (vector-8b-ref pattern (fix:+ pstart k)))
			    (fix:+ k 1))
			   ((fix:= k 0)
			    k)
			   (else
			    (inner (vector-ref pi (fix:- k 1)))))))))
	    (vector-set! pi q k)
	    (outer k (fix:+ q 1)))))
    pi))

;;;; Guarantors
;;
;; The guarantors are integrated.  Most are structured as combination of
;; simple tests which the compiler can open-code, followed by a call to a
;; GUARANTEE-.../FAIL version which does the tests again to signal a
;; meaningful message.  Structuring the code this way significantly
;; reduces code bloat from large integrated procedures.

(declare (integrate-operator guarantee-string))
(define-guarantee string "string")

(define-integrable (guarantee-2-strings object1 object2 procedure)
  (if (not (and (string? object1) (string? object2)))
      (guarantee-2-strings/fail object1 object2 procedure)))

(define (guarantee-2-strings/fail object1 object2 procedure)
  (cond ((not (string? object1))
	 (error:wrong-type-argument object1 "string" procedure))
	((not (string? object2))
	 (error:wrong-type-argument object2 "string" procedure))))

(define-integrable (guarantee-string-index object caller)
  (if (not (index-fixnum? object))
      (error:wrong-type-argument object "string index" caller)))

(define-integrable (guarantee-substring string start end caller)
  (if (not (and (string? string)
		(index-fixnum? start)
		(index-fixnum? end)
		(fix:<= start end)
		(fix:<= end (string-length string))))
      (guarantee-substring/fail string start end caller)))

(define (guarantee-substring/fail string start end caller)
  (guarantee-string string caller)
  (guarantee-substring-end-index end (string-length string) caller)
  (guarantee-substring-start-index start end caller))

(define-integrable (guarantee-substring-end-index end length caller)
  (guarantee-string-index end caller)
  (if (not (fix:<= end length))
      (error:bad-range-argument end caller))
  end)

(define-integrable (guarantee-substring-start-index start end caller)
  (guarantee-string-index start caller)
  (if (not (fix:<= start end))
      (error:bad-range-argument start caller))
  start)

(define-integrable (guarantee-2-substrings string1 start1 end1
					   string2 start2 end2
					   procedure)
  (guarantee-substring string1 start1 end1 procedure)
  (guarantee-substring string2 start2 end2 procedure))
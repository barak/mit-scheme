#| -*-Scheme-*-

$Id: unicode.scm,v 1.9 2003/04/14 19:40:04 cph Exp $

Copyright 2001,2003 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
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
;;; Additionally, the codes #xD800 through #xDFFF, #xFFFE, and #xFFFF
;;; are disallowed, as they are not valid Unicode characters.
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
		      (IF (OR (DEFAULT-OBJECT? ,end) (NOT ,end))
			  (STRING-LENGTH ,string)
			  (GUARANTEE-SUBSTRING-END-INDEX
			   ,end (STRING-LENGTH ,string) ,caller)))
		     (,(list-ref form 2)
		      (IF (OR (DEFAULT-OBJECT? ,start) (NOT ,start))
			  0
			  (GUARANTEE-SUBSTRING-START-INDEX
			   ,start ,(list-ref form 3) ,caller))))
		,@(map (lambda (expr)
			 (make-syntactic-closure environment
			     (list (list-ref form 2) (list-ref form 3))
			   expr))
		       (list-tail form 5)))))
	 (ill-formed-syntax form)))))

(define (encoded-string-length string start end type caller validate-char)
  (let loop ((start start) (n 0))
    (if (fix:< start end)
	(let ((start* (validate-char string start end)))
	  (if (not start*)
	      (error:wrong-type-argument string
					 (string-append "UTF-" type " string")
					 caller))
	  (loop start* (fix:+ n 1)))
	n)))

(define (read-byte port)
  (let ((char (read-char port)))
    (if (eof-object? char)
	char
	(let ((b (char->integer char)))
	  (if (not (fix:< b #x100))
	      (error "Illegal input byte:" b))
	  b))))

(define-integrable (write-byte byte port)
  (write-char (integer->char byte) port))

(define (initialize-package!)
  (set! ws-output-port-type (make-port-type ws-output-operations #f))
  (set! ws-input-port-type (make-port-type ws-input-operations #f))
  unspecific)

;;;; Unicode characters

(define (wide-char? object)
  (and (char? object)
       (fix:= (char-bits object) 0)
       (unicode-code-point? (char-code object))))

(define (guarantee-wide-char object caller)
  (if (not (wide-char? object))
      (error:not-wide-char object caller)))

(define (error:not-wide-char object caller)
  (error:wrong-type-argument object "Unicode character" caller))

(define (unicode-code-point? object)
  (and (index-fixnum? object)
       (if (fix:< object #x10000)
	   (not (illegal-code? object))
	   (fix:< object char-code-limit))))

(define-integrable (guarantee-unicode-code-point object caller)
  (if (not (unicode-code-point? object))
      (error:not-unicode-code-point object caller)))

(define (error:not-unicode-code-point object caller)
  (error:wrong-type-argument object "Unicode code point" caller))

(define-integrable (illegal-code? pt)
  (or (fix:= #xD800 (fix:and #xF800 pt))
      (fix:= #xFFFE (fix:and #xFFFE pt))))

;;;; Alphabets

(define-structure (alphabet (type-descriptor <alphabet>))
  (low #f read-only #t)
  (high1 #f read-only #t)
  (high2 #f read-only #t))

(define-integrable (guarantee-alphabet object caller)
  (if (not (alphabet? object))
      (error:not-alphabet object caller)))

(define (error:not-alphabet object caller)
  (error:wrong-type-argument object "Unicode alphabet" caller))

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
	     (or (not (pair? items))
		 (let ((b (car items))
		       (items (cdr items)))
		   (and (well-formed-item? b)
			(fix:< (if (pair? a) (cdr a) a)
			       (if (pair? b) (car b) b))
			(loop b items))))))
      (null? items)))

(define (well-formed-item? item)
  (if (pair? item)
      (and (unicode-code-point? (car item))
	   (unicode-code-point? (cdr item))
	   (fix:< (car item) (cdr item)))
      (unicode-code-point? item)))

(define-integrable (guarantee-well-formed-code-point-list object caller)
  (if (not (well-formed-code-point-list? object))
      (error:not-well-formed-code-point-list object caller)))

(define (error:not-well-formed-code-point-list object caller)
  (error:wrong-type-argument object "Unicode code-point list" caller))

(define (code-points->alphabet items)
  (guarantee-well-formed-code-point-list items 'CODE-POINTS->ALPHABET)
  (%code-points->alphabet items))

(define (%code-points->alphabet items)
  (call-with-values (lambda () (split-list items #x800))
    (lambda (low-items high-items)
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
	    (make-alphabet low high1 high2)))))))

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
		 (values (cons (cons (car item) (- limit 1)) low)
			 (cons (cons limit (cdr item)) items)))))
	(values low '()))))

(define (alphabet . chars)
  (%code-points->alphabet
   (sorted-chars->wfcp-list (remove-duplicate-chars chars))))

(define (remove-duplicate-chars chars)
  (let ((table (make-eq-hash-table)))
    (for-each (lambda (char)
		(guarantee-wide-char char 'REMOVE-DUPLICATE-CHARS)
		(hash-table/put! table char #t))
	      chars)
    (hash-table/key-list table)))

(define (sorted-chars->wfcp-list chars)
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

(define-integrable (guarantee-8-bit-alphabet object caller)
  (if (not (8-bit-alphabet? object))
      (error:not-8-bit-alphabet object caller)))

(define (error:not-8-bit-alphabet object caller)
  (error:wrong-type-argument object "8-bit alphabet" caller))

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
  (call-with-values
      (lambda ()
	(alphabet-high+2 (alphabet-high1 a1)
			 (alphabet-high2 a1)
			 (alphabet-high1 a2)
			 (alphabet-high2 a2)))
    (lambda (high1 high2)
      (make-alphabet (alphabet-low+2 (alphabet-low a1) (alphabet-low a2))
		     high1
		     high2))))

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
  (call-with-values
      (lambda ()
	(alphabet-high- (alphabet-high1 a1)
			(alphabet-high2 a1)
			(alphabet-high1 a2)
			(alphabet-high2 a2)))
    (lambda (high1 high2)
      (make-alphabet (alphabet-low- (alphabet-low a1) (alphabet-low a2))
		     high1
		     high2))))

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

(define-integrable (guarantee-wide-string object caller)
  (if (not (wide-string? object))
      (error:not-wide-string object caller)))

(define (error:not-wide-string object caller)
  (error:wrong-type-argument object "Unicode string" caller))

(define (make-wide-string length #!optional char)
  (%make-wide-string
   (make-vector length
		(if (default-object? char)
		    (integer->char 0)
		    (begin
		      (guarantee-wide-char char 'MAKE-WIDE-STRING)
		      char)))))

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

(define (wide-string-index? index string)
  (and (index-fixnum? index)
       (fix:< index (%wide-string-length string))))

(define-integrable (guarantee-wide-string-index index string caller)
  (if (not (wide-string-index? index string))
      (error:not-wide-string-index index caller)))

(define (error:not-wide-string-index index caller)
  (error:wrong-type-argument index "Unicode string index" caller))

(define (open-wide-output-string)
  (make-port ws-output-port-type (make-ws-output-state)))

(define (call-with-wide-output-string generator)
  (let ((port (open-wide-output-string)))
    (generator port)
    (get-output-string port)))

(define ws-output-port-type)

(define (make-ws-output-state)
  (let ((v (make-vector 17)))
    (vector-set! v 0 0)
    v))

(define ws-output-operations
  `((WRITE-CHAR
     ,(lambda (port char)
	(guarantee-wide-char char 'WRITE-CHAR)
	(without-interrupts
	 (lambda ()
	   (let* ((v (port/state port))
		  (n (vector-ref v 0))
		  (n* (fix:+ n 1))
		  (v
		   (if (fix:= (vector-length v) n*)
		       (vector-grow v (fix:+ n* n))
		       v)))
	     (vector-set! v n* char)
	     (vector-set! v 0 n*))))))
    (EXTRACT-OUTPUT!
     ,(lambda (port)
	(%make-wide-string
	 (without-interrupts
	  (lambda ()
	    (let ((v (port/state port)))
	      (subvector v 1 (fix:+ (vector-ref v 0) 1))))))))
    (WRITE-SELF
     ,(lambda (port port*)
	port
	(write-string " to wide string" port*)))))

(define (string->wide-string string #!optional start end)
  (let ((input
	 (open-input-string string
			    (if (default-object? start) #f start)
			    (if (default-object? end) #f end))))
    (call-with-wide-output-string
     (lambda (output)
       (let loop ()
	 (let ((char (read-char input)))
	   (if (not (eof-object? char))
	       (begin
		 (write-char char output)
		 (loop)))))))))

(define (open-wide-input-string string #!optional start end)
  (with-substring-args string start end 'OPEN-WIDE-INPUT-STRING
    (make-port ws-input-port-type (make-ws-input-state string start end))))

(define ws-input-port-type)

(define-structure (ws-input-state (type vector)
				  (conc-name ws-input-state/))
  (string #f read-only #t)
  start
  (end #f read-only #t))

(define-integrable (ws-input-port/string port)
  (ws-input-state/string (port/state port)))

(define-integrable (ws-input-port/start port)
  (ws-input-state/start (port/state port)))

(define-integrable (set-ws-input-port/start! port index)
  (set-ws-input-state/start! (port/state port) index))

(define-integrable (ws-input-port/end port)
  (ws-input-state/end (port/state port)))

(define ws-input-operations
  `((CHAR-READY?
     ,(lambda (port interval)
	interval
	(fix:< (ws-input-port/start port) (ws-input-port/end port))))
    (DISCARD-CHAR
     ,(lambda (port)
	(set-ws-input-port/start! port (fix:+ (ws-input-port/start port) 1))))
    (PEEK-CHAR
     ,(lambda (port)
	(let ((start (ws-input-port/start port)))
	  (if (fix:< start (ws-input-port/end port))
	      (%wide-string-ref (ws-input-port/string port)
				start)
	      (make-eof-object port)))))
    (READ-CHAR
     ,(lambda (port)
	(let ((start (ws-input-port/start port)))
	  (if (fix:< start (ws-input-port/end port))
	      (begin
		(set-ws-input-port/start! port (fix:+ start 1))
		(%wide-string-ref (ws-input-port/string port) start))
	      (make-eof-object port)))))
    (WRITE-SELF
     ,(lambda (port output-port)
	port
	(write-string " from wide string" output-port)))))

(define (wide-string->string string #!optional start end)
  (let ((input
	 (open-wide-input-string string
				 (if (default-object? start) #f start)
				 (if (default-object? end) #f end))))
    (call-with-output-string
     (lambda (output)
       (let loop ()
	 (let ((char (read-char input)))
	   (if (not (eof-object? char))
	       (begin
		 (write-char char output)
		 (loop)))))))))

;;;; UTF-32 representation

(define (read-utf32-char port)
  (if (host-big-endian?)
      (read-utf32-be-char port)
      (read-utf32-le-char port)))

(define (read-utf32-be-char port)
  (%read-utf32-char port utf32-be-bytes->code-point 'READ-UTF32-BE-CHAR))

(define (read-utf32-le-char port)
  (%read-utf32-char port utf32-le-bytes->code-point 'READ-UTF32-LE-CHAR))

(define-integrable (%read-utf32-char port combiner caller)
  (let ((b0 (read-byte port)))
    (if (eof-object? b0)
	b0
	(let* ((b1 (read-byte port))
	       (b2 (read-byte port))
	       (b3 (read-byte port)))
	  (if (or (eof-object? b1)
		  (eof-object? b2)
		  (eof-object? b3))
	      (error "Truncated UTF-32 input."))
	  (let ((pt (combiner b0 b1 b2 b3)))
	    (guarantee-unicode-code-point pt caller)
	    (integer->char pt))))))

(define-integrable (utf32-be-bytes->code-point b0 b1 b2 b3)
  (+ (* b0 #x01000000)
     (fix:lsh b1 16)
     (fix:lsh b2 8)
     b3))

(define-integrable (utf32-le-bytes->code-point b0 b1 b2 b3)
  (+ (* b3 #x01000000)
     (fix:lsh b2 16)
     (fix:lsh b1 8)
     b0))

(define (write-utf32-char char port)
  (if (host-big-endian?)
      (write-utf32-be-char char port)
      (write-utf32-le-char char port)))

(define (write-utf32-be-char char port)
  (guarantee-wide-char char 'WRITE-UTF32-BE-CHAR)
  (%write-utf32-be-char char port))

(define (write-utf32-le-char char port)
  (guarantee-wide-char char 'WRITE-UTF32-LE-CHAR)
  (%write-utf32-le-char char port))

(define-integrable (%write-utf32-be-char char port)
  (let ((pt (char->integer char)))
    (write-byte 0 port)
    (write-byte (fix:lsh pt -16) port)
    (write-byte (fix:lsh pt -8) port)
    (write-byte (fix:and pt #xFF) port)))

(define-integrable (%write-utf32-le-char char port)
  (let ((pt (char->integer char)))
    (write-byte (fix:and pt #xFF) port)
    (write-byte (fix:lsh pt -8) port)
    (write-byte (fix:lsh pt -16) port)
    (write-byte 0 port)))

(define (utf32-string->wide-string string #!optional start end)
  (%utf32-string->wide-string string
			      (if (default-object? start) #f start)
			      (if (default-object? end) #f end)
			      (if (host-big-endian?)
				  read-utf32-be-char
				  read-utf32-le-char)))

(define (utf32-be-string->wide-string string #!optional start end)
  (%utf32-string->wide-string string
			      (if (default-object? start) #f start)
			      (if (default-object? end) #f end)
			      read-utf32-be-char))

(define (utf32-le-string->wide-string string #!optional start end)
  (%utf32-string->wide-string string
			      (if (default-object? start) #f start)
			      (if (default-object? end) #f end)
			      read-utf32-le-char))

(define (%utf32-string->wide-string string start end read-utf32-char)
  (let ((input (open-input-string string start end)))
    (call-with-wide-output-string
     (lambda (output)
       (let loop ()
	 (let ((char (read-utf32-char input)))
	   (if (not (eof-object? char))
	       (begin
		 (write-char char output)
		 (loop)))))))))

(define (wide-string->utf32-string string #!optional start end)
  (%wide-string->utf32-string string
			      (if (default-object? start) #f start)
			      (if (default-object? end) #f end)
			      (if (host-big-endian?)
				  %write-utf32-be-char
				  %write-utf32-le-char)))

(define (wide-string->utf32-be-string string #!optional start end)
  (%wide-string->utf32-string string
			      (if (default-object? start) #f start)
			      (if (default-object? end) #f end)
			      %write-utf32-be-char))

(define (wide-string->utf32-le-string string #!optional start end)
  (%wide-string->utf32-string string
			      (if (default-object? start) #f start)
			      (if (default-object? end) #f end)
			      %write-utf32-le-char))

(define (%wide-string->utf32-string string start end write-utf32-char)
  (let ((input (open-wide-input-string string start end)))
    (call-with-output-string
     (lambda (output)
       (let loop ()
	 (let ((char (read-char input)))
	   (if (not (eof-object? char))
	       (begin
		 (write-utf32-char char output)
		 (loop)))))))))

(define (utf32-string-length string #!optional start end)
  (if (host-big-endian?)
      (%utf32-string-length string
			    (if (default-object? start) #f start)
			    (if (default-object? end) #f end)
			    "32BE" utf32-be-bytes->code-point
			    'UTF32-STRING-LENGTH)
      (%utf32-string-length string
			    (if (default-object? start) #f start)
			    (if (default-object? end) #f end)
			    "32LE" utf32-le-bytes->code-point
			    'UTF32-STRING-LENGTH)))

(define (utf32-be-string-length string #!optional start end)
  (%utf32-string-length string
			(if (default-object? start) #f start)
			(if (default-object? end) #f end)
			"32BE" utf32-be-bytes->code-point
			'UTF32-BE-STRING-LENGTH))

(define (utf32-le-string-length string #!optional start end)
  (%utf32-string-length string
			(if (default-object? start) #f start)
			(if (default-object? end) #f end)
			"32LE" utf32-le-bytes->code-point
			'UTF32-LE-STRING-LENGTH))

(define (%utf32-string-length string start end type combiner caller)
  (with-substring-args string start end caller
    (encoded-string-length string start end type caller
      (lambda (string start end)

	(define-integrable (n i)
	  (vector-8b-ref string (fix:+ start i)))

	(if (fix:< start end)
	    (let ((start* (fix:+ start 4)))
	      (and (fix:<= start* end)
		   (let ((pt (combiner (n 0) (n 1) (n 2) (n 3))))
		     (and (unicode-code-point? pt)
			  start*))))
	    start)))))

;;;; UTF-16 representation

(define (read-utf16-char port)
  (if (host-big-endian?)
      (read-utf16-be-char port)
      (read-utf16-le-char port)))

(define (read-utf16-be-char port)
  (%read-utf16-char port be-bytes->digit16 'READ-UTF16-BE-CHAR))

(define (read-utf16-le-char port)
  (%read-utf16-char port le-bytes->digit16 'READ-UTF16-LE-CHAR))

(define-integrable (%read-utf16-char port combinator caller)
  (let ((d0 (read-utf16-digit port combinator)))
    (if (eof-object? d0)
	d0
	(let ((pt
	       (if (high-surrogate? d0)
		   (let ((d1 (read-utf16-digit port combinator)))
		     (if (eof-object? d1)
			 (error "Truncated UTF-16 input."))
		     (if (not (low-surrogate? d1))
			 (error "Illegal UTF-16 subsequent digit:" d1))
		     (combine-surrogates d0 d1))
		   d0)))
	  (guarantee-unicode-code-point pt caller)
	  (integer->char pt)))))

(define-integrable (read-utf16-digit port combinator)
  (let ((b0 (read-byte port)))
    (if (eof-object? b0)
	b0
	(let ((b1 (read-byte port)))
	  (if (eof-object? b1)
	      (error "Truncated UTF-16 input."))
	  (combinator b0 b1)))))

(define (write-utf16-char char port)
  (if (host-big-endian?)
      (write-utf16-be-char char port)
      (write-utf16-le-char char port)))

(define (write-utf16-be-char char port)
  (guarantee-wide-char char 'WRITE-UTF16-BE-CHAR)
  (%write-utf16-be-char char port))

(define (write-utf16-le-char char port)
  (guarantee-wide-char char 'WRITE-UTF16-LE-CHAR)
  (%write-utf16-le-char char port))

(define-integrable (%write-utf16-be-char char port)
  (%write-utf16-char char port
		     (lambda (digit output)
		       (output (fix:lsh digit -8))
		       (output (fix:and digit #x00FF)))))

(define-integrable (%write-utf16-le-char char port)
  (%write-utf16-char char port
		     (lambda (digit output)
		       (output (fix:and digit #x00FF))
		       (output (fix:lsh digit -8)))))

(define-integrable (%write-utf16-char char port dissecter)
  (let ((pt (char->integer char))
	(write-byte (lambda (byte) (write-byte byte port))))
    (if (fix:< pt #x10000)
	(dissecter pt write-byte)
	(let ((s (fix:- pt #x10000)))
	  (dissecter (fix:or #xD800 (fix:lsh s -10)) write-byte)
	  (dissecter (fix:or #xDC00 (fix:and s #x3FF)) write-byte)))))

(define (utf16-string->wide-string string #!optional start end)
  (%utf16-string->wide-string string
			      (if (default-object? start) #f start)
			      (if (default-object? end) #f end)
			      (if (host-big-endian?)
				  read-utf16-be-char
				  read-utf16-le-char)))

(define (utf16-be-string->wide-string string #!optional start end)
  (%utf16-string->wide-string string
			      (if (default-object? start) #f start)
			      (if (default-object? end) #f end)
			      read-utf16-be-char))

(define (utf16-le-string->wide-string string #!optional start end)
  (%utf16-string->wide-string string
			      (if (default-object? start) #f start)
			      (if (default-object? end) #f end)
			      read-utf16-le-char))

(define (%utf16-string->wide-string string start end read-utf16-char)
  (let ((input (open-input-string string start end)))
    (call-with-wide-output-string
     (lambda (output)
       (let loop ()
	 (let ((char (read-utf16-char input)))
	   (if (not (eof-object? char))
	       (begin
		 (write-char char output)
		 (loop)))))))))

(define (wide-string->utf16-string string #!optional start end)
  (%wide-string->utf16-string string
			      (if (default-object? start) #f start)
			      (if (default-object? end) #f end)
			      (if (host-big-endian?)
				  %write-utf16-be-char
				  %write-utf16-le-char)))

(define (wide-string->utf16-be-string string #!optional start end)
  (%wide-string->utf16-string string
			      (if (default-object? start) #f start)
			      (if (default-object? end) #f end)
			      %write-utf16-be-char))

(define (wide-string->utf16-le-string string #!optional start end)
  (%wide-string->utf16-string string
			      (if (default-object? start) #f start)
			      (if (default-object? end) #f end)
			      %write-utf16-le-char))

(define (%wide-string->utf16-string string start end write-utf16-char)
  (let ((input (open-wide-input-string string start end)))
    (call-with-output-string
     (lambda (output)
       (let loop ()
	 (let ((char (read-char input)))
	   (if (not (eof-object? char))
	       (begin
		 (write-utf16-char char output)
		 (loop)))))))))

(define (utf16-string-length string #!optional start end)
  (if (host-big-endian?)
      (%utf16-string-length string
			    (if (default-object? start) #f start)
			    (if (default-object? end) #f end)
			    "16BE" be-bytes->digit16
			    'UTF16-STRING-LENGTH)
      (%utf16-string-length string
			    (if (default-object? start) #f start)
			    (if (default-object? end) #f end)
			    "16LE" le-bytes->digit16
			    'UTF16-STRING-LENGTH)))

(define (utf16-be-string-length string #!optional start end)
  (%utf16-string-length string
			(if (default-object? start) #f start)
			(if (default-object? end) #f end)
			"16BE" be-bytes->digit16
			'UTF16-BE-STRING-LENGTH))

(define (utf16-le-string-length string #!optional start end)
  (%utf16-string-length string
			(if (default-object? start) #f start)
			(if (default-object? end) #f end)
			"16LE" le-bytes->digit16
			'UTF16-LE-STRING-LENGTH))

(define (%utf16-string-length string start end type combiner caller)
  (with-substring-args string start end caller
    (encoded-string-length string start end type caller
      (lambda (string start end)

	(define-integrable (n i)
	  (vector-8b-ref string (fix:+ start i)))

	(if (fix:< start end)
	    (and (fix:<= (fix:+ start 2) end)
		 (let ((d0 (combiner (n 0) (n 1))))
		   (if (high-surrogate? d0)
		       (and (fix:<= (fix:+ start 4) end)
			    (let ((d1 (combiner (n 2) (n 3))))
			      (and (low-surrogate? d1)
				   (let ((pt (combine-surrogates d0 d1)))
				     (and (unicode-code-point? pt)
					  (fix:+ start 4))))))
		       (and (unicode-code-point? d0)
			    (fix:+ start 2)))))
	    start)))))

(define-integrable (be-bytes->digit16 b0 b1)
  (fix:or (fix:lsh b0 8) b1))

(define-integrable (le-bytes->digit16 b0 b1)
  (fix:or (fix:lsh b1 8) b0))

(define-integrable (high-surrogate? n)
  (fix:= #xD800 (fix:and #xFC00 n)))

(define-integrable (low-surrogate? n)
  (fix:= #xDC00 (fix:and #xFC00 n)))

(define-integrable (combine-surrogates n0 n1)
  (fix:+ (fix:+ (fix:lsh (fix:and n0 #x3FF) 10)
		(fix:and n1 #x3FF))
	 #x10000))

;;;; UTF-8 representation

(define (read-utf8-char port)
  (read-utf8-char-from-source
   (lambda ()
     (let ((b (read-byte port)))
       (if (eof-object? b)
	   #f
	   b)))))

(define (read-utf8-char-from-source source)
  (let ((b0 (source))
	(get-next
	 (lambda ()
	   (let ((b (source)))
	     (if (not b)
		 (error "Truncated UTF-8 input."))
	     (if (not (%valid-trailer? b))
		 (error "Illegal subsequent UTF-8 byte:" b))
	     b))))
    (if b0
	(integer->char
	 (cond ((fix:< b0 #x80)
		b0)
	       ((fix:< b0 #xE0)
		(%vc2 b0)
		(%cp2 b0 (get-next)))
	       ((fix:< b0 #xF0)
		(let ((b1 (get-next)))
		  (%vc3 b0 b1)
		  (%cp3 b0 b1 (get-next))))
	       ((fix:< b0 #xF8)
		(let ((b1 (get-next)))
		  (%vc4 b0 b1)
		  (let ((b2 (get-next)))
		    (%cp4 b0 b1 b2 (get-next)))))
	       (else
		(error "Illegal UTF-8 byte:" b0))))
	(make-eof-object #f))))

(define (utf8-string->wide-string string #!optional start end)
  (let ((input
	 (open-input-string string
			    (if (default-object? start) #f start)
			    (if (default-object? end) #f end))))
    (call-with-wide-output-string
     (lambda (output)
       (let loop ()
	 (let ((char (read-utf8-char input)))
	   (if (not (eof-object? char))
	       (begin
		 (write-char char output)
		 (loop)))))))))

(define (write-utf8-char char port)
  (guarantee-wide-char char 'WRITE-UTF8-CHAR)
  (%write-utf8-char char port))

(define (%write-utf8-char char port)
  (let ((pt (char->integer char)))

    (define-integrable (initial-char n-bits offset)
      (fix:or (fix:and (fix:lsh #xFF (fix:+ n-bits 1)) #xFF)
	      (fix:lsh pt (fix:- 0 offset))))

    (define-integrable (subsequent-char offset)
      (fix:or #x80 (fix:and (fix:lsh pt (fix:- 0 offset)) #x3F)))

    (cond ((fix:< pt #x00000080)
	   (write-byte pt port))
	  ((fix:< pt #x00000800)
	   (write-byte (initial-char 5 6) port)
	   (write-byte (subsequent-char 0) port))
	  ((fix:< pt #x00010000)
	   (write-byte (initial-char 4 12) port)
	   (write-byte (subsequent-char 6) port)
	   (write-byte (subsequent-char 0) port))
	  (else
	   (write-byte (initial-char 3 18) port)
	   (write-byte (subsequent-char 12) port)
	   (write-byte (subsequent-char 6) port)
	   (write-byte (subsequent-char 0) port)))))

(define (wide-string->utf8-string string #!optional start end)
  (let ((input
	 (open-wide-input-string string
				 (if (default-object? start) #f start)
				 (if (default-object? end) #f end))))
    (call-with-output-string
     (lambda (output)
       (let loop ()
	 (let ((char (read-char input)))
	   (if (not (eof-object? char))
	       (begin
		 (%write-utf8-char char output)
		 (loop)))))))))

(define (utf8-string-length string #!optional start end)
  (with-substring-args string start end 'UTF8-STRING-LENGTH
    (encoded-string-length string start end "8" 'UTF8-STRING-LENGTH
      (lambda (string start end)

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
			  (fix:+ start 3)))
		    ((fix:< b0 #xF8)
		     (and (fix:<= (fix:+ start 4) end)
			  (check-byte 1)
			  (%vs4 b0 (n 1))
			  (check-byte 2)
			  (check-byte 3)
			  (fix:+ start 4)))
		    (else #f)))
	    start)))))

(define-integrable (%vc2 b0)
  (if (not (%vs2 b0))
      (error "Illegal UTF-8 sequence:" b0)))

(define-integrable (%vc3 b0 b1)
  (if (not (%vs3 b0 b1))
      (error "Illegal UTF-8 sequence:" b0 b1)))

(define-integrable (%vc4 b0 b1)
  (if (not (%vs4 b0 b1))
      (error "Illegal UTF-8 sequence:" b0 b1)))

(define-integrable (%vs2 b0)
  (fix:> b0 #xC1))

(define-integrable (%vs3 b0 b1)
  (or (fix:> b0 #xE0) (fix:> b1 #x9F)))

(define-integrable (%vs4 b0 b1)
  (or (fix:> b0 #xF0) (fix:> b1 #x8F)))

(define-integrable (%cp2 b0 b1)
  (fix:or (fix:lsh (fix:and b0 #x1F) 6)
	  (fix:and b1 #x3F)))

(define-integrable (%cp3 b0 b1 b2)
  (fix:or (fix:lsh (fix:and b0 #x0F) 12)
	  (fix:or (fix:lsh (fix:and b1 #x3F) 6)
		  (fix:and b2 #x3F))))

(define-integrable (%cp4 b0 b1 b2 b3)
  (fix:or (fix:lsh (fix:and b0 #x07) 18)
	  (fix:or (fix:lsh (fix:and b1 #x3F) 12)
		  (fix:or (fix:lsh (fix:and b2 #x3F) 6)
			  (fix:and b3 #x3F)))))

(define-integrable (%valid-trailer? n)
  (fix:= #x80 (fix:and #xC0 n)))
#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/char.scm,v 14.2 1988/10/15 17:19:05 cph Rel $

Copyright (c) 1988 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Character Abstraction
;;; package: (runtime character)

(declare (usual-integrations))

(define-primitives
  make-char char-code char-bits char->integer integer->char char->ascii
  char-ascii? ascii->char char-upcase char-downcase)

(define-integrable (char? object)
  (object-type? (ucode-type character) object))

(define-integrable char-code-limit #x80)
(define-integrable char-bits-limit #x20)
(define-integrable char-integer-limit #x1000)

(define-integrable (chars->ascii chars)
  (map char->ascii chars))

(define-integrable (code->char code)
  (make-char code 0))

(define-integrable (char=? x y)
  (= (char->integer x) (char->integer y)))

(define-integrable (char<? x y)
  (< (char->integer x) (char->integer y)))

(define-integrable (char<=? x y)
  (<= (char->integer x) (char->integer y)))

(define-integrable (char>? x y)
  (> (char->integer x) (char->integer y)))

(define-integrable (char>=? x y)
  (>= (char->integer x) (char->integer y)))

(define-integrable (char-ci->integer char)
  (char->integer (char-upcase char)))

(define-integrable (char-ci=? x y)
  (= (char-ci->integer x) (char-ci->integer y)))

(define-integrable (char-ci<? x y)
  (< (char-ci->integer x) (char-ci->integer y)))

(define-integrable (char-ci<=? x y)
  (<= (char-ci->integer x) (char-ci->integer y)))

(define-integrable (char-ci>? x y)
  (> (char-ci->integer x) (char-ci->integer y)))

(define-integrable (char-ci>=? x y)
  (>= (char-ci->integer x) (char-ci->integer y)))

(define 0-code)
(define upper-a-code)
(define lower-a-code)
(define space-char)
(define hyphen-char)
(define backslash-char)

(define (initialize-package!)
  (set! 0-code (char-code (ascii->char #x30)))
  (set! upper-a-code (char-code (ascii->char #x41)))
  (set! lower-a-code (char-code (ascii->char #x61)))
  (set! space-char (ascii->char #x20))
  (set! hyphen-char (ascii->char #x2D))
  (set! backslash-char (ascii->char #x5C)))

(define named-codes
  '(("Backspace" . #x08)
    ("Tab" . #x09)
    ("Linefeed" . #x0A)
    ("Page" . #x0C)
    ("Return" . #x0D)
    ("Call" . #x1A)
    ("Altmode" . #x1B)
    ("Backnext" . #x1F)
    ("Space" . #x20)
    ("Rubout" . #x7F)

    ;; ASCII codes.  Some of these are aliases for previous
    ;; definitions, and will not appear as output.
    ("NUL" . #x00)
    ("SOH" . #x01)
    ("STX" . #x02)
    ("ETX" . #x03)
    ("EOT" . #x04)
    ("ENQ" . #x05)
    ("ACK" . #x06)
    ("BEL" . #x07)
    ("BS" . #x08)
    ("HT" . #x09)
    ("LF" . #x0A)
    ("VT" . #x0B)
    ("FF" . #x0C)
    ("CR" . #x0D)
    ("SO" . #x0E)
    ("SI" . #x0F)
    ("DLE" . #x10)
    ("DC1" . #x11)
    ("DC2" . #x12)
    ("DC3" . #x13)
    ("DC4" . #x14)
    ("NAK" . #x15)
    ("SYN" . #x16)
    ("ETB" . #x17)
    ("CAN" . #x18)
    ("EM" . #x19)
    ("SUB" . #x1A)
    ("ESC" . #x1B)
    ("FS" . #x1C)
    ("GS" . #x1D)
    ("RS" . #x1E)
    ("US" . #x1F)
    ("DEL" . #x7F)
    ))

(define named-bits
  '(("M" . #x01)
    ("Meta" . #x01)
    ("C" . #x02)
    ("Control" . #x02)
    ("S" . #x04)
    ("Super" . #x04)
    ("H" . #x08)
    ("Hyper" . #x08)
    ("T" . #x10)
    ("Top" . #x10)
    ))

(define (-map-> alist string start end)
  (define (loop entries)
    (and (not (null? entries))
	 (let ((key (caar entries)))
	   (if (substring-ci=? string start end
			       key 0 (string-length key))
	       (cdar entries)
	       (loop (cdr entries))))))
  (loop alist))

(define (<-map- alist n)
  (define (loop entries)
    (and (not (null? entries))
	 (if (= n (cdar entries))
	     (caar entries)
	     (loop (cdr entries)))))
  (loop alist))

(define (digit->char digit #!optional radix)
  (cond ((default-object? radix) (set! radix 10))
	((not (and (<= 2 radix) (<= radix 36)))
	 (error "DIGIT->CHAR: Bad radix" radix)))
  (and (<= 0 digit) (< digit radix)
       (code->char (if (< digit 10)
		       (+ digit 0-code)
		       (+ (- digit 10) upper-a-code)))))

(define (char->digit char #!optional radix)
  (cond ((default-object? radix) (set! radix 10))
	((not (and (<= 2 radix) (<= radix 36)))
	 (error "CHAR->DIGIT: Bad radix" radix)))
  (and (zero? (char-bits char))
       (let ((code (char-code char)))
	 (define (try base-digit base-code)
	   (let ((n (+ base-digit (- code base-code))))
	     (and (<= base-digit n)
		  (< n radix)
		  n)))
	 (or (try 0 0-code)
	     (try 10 upper-a-code)
	     (try 10 lower-a-code)))))

(define (name->char string)
  (let ((end (string-length string))
	(bits '()))
    (define (loop start)
      (let ((left (- end start)))
	(cond ((zero? left)
	       (error "Missing character name"))
	      ((= left 1)
	       (let ((char (string-ref string start)))
		 (if (char-graphic? char)
		     (char-code char)
		     (error "Non-graphic character" char))))
	      (else
	       (let ((hyphen (substring-find-next-char string start end
						       hyphen-char)))
		 (if (not hyphen)
		     (name->code string start end)
		     (let ((bit (-map-> named-bits string start hyphen)))
		       (if (not bit)
			   (name->code string start end)
			   (begin (if (not (memv bit bits))
				      (set! bits (cons bit bits)))
				  (loop (1+ hyphen)))))))))))
    (let ((code (loop 0)))
      (make-char code (apply + bits)))))

(define (name->code string start end)
  (if (substring-ci=? string start end "Newline" 0 7)
      (char-code char:newline)
      (or (-map-> named-codes string start end)
	  (error "Unknown character name" (substring string start end)))))

(define (char->name char #!optional slashify?)
  (if (default-object? slashify?) (set! slashify? false))
  (define (loop weight bits)
    (if (zero? bits)
	(let ((code (char-code char)))
	  (let ((base-char (code->char code)))
	    (cond ((<-map- named-codes code))
		  ((and slashify?
			(not (zero? (char-bits char)))
			(or (char=? base-char backslash-char)
			    (char-set-member? char-set/atom-delimiters
					      base-char)))
		   (string-append "\\" (string base-char)))
		  ((char-graphic? base-char)
		   (string base-char))
		  (else
		   (string-append "<code "
				  (write-to-string code)
				  ">")))))
	(let ((qr (integer-divide bits 2)))
	  (let ((rest (loop (* weight 2) (integer-divide-quotient qr))))
	    (if (zero? (integer-divide-remainder qr))
		rest
		(string-append (or (<-map- named-bits weight)
				   (string-append "<bit "
						  (write-to-string weight)
						  ">"))
			       "-"
			       rest))))))
  (loop 1 (char-bits char)))
#| -*-Scheme-*-

$Id: char.scm,v 14.8 1998/01/20 18:40:24 adams Exp $

Copyright (c) 1988-1998 Massachusetts Institute of Technology

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
  (char? 1)
  make-char char-code char-bits char->integer integer->char char->ascii
  char-ascii? ascii->char char-upcase char-downcase)

(define-integrable char-code-limit #x80)
(define-integrable char-bits-limit #x20)
(define-integrable char-integer-limit #x1000)

(define-integrable (chars->ascii chars)
  (map char->ascii chars))

(define-integrable (code->char code)
  (make-char code 0))

(define-integrable (char=? x y)
  (fix:= (char->integer x) (char->integer y)))

(define-integrable (char<? x y)
  (fix:< (char->integer x) (char->integer y)))

(define-integrable (char<=? x y)
  (fix:<= (char->integer x) (char->integer y)))

(define-integrable (char>? x y)
  (fix:> (char->integer x) (char->integer y)))

(define-integrable (char>=? x y)
  (fix:>= (char->integer x) (char->integer y)))

(define-integrable (char-ci->integer char)
  (char->integer (char-upcase char)))

(define-integrable (char-ci=? x y)
  (fix:= (char-ci->integer x) (char-ci->integer y)))

(define-integrable (char-ci<? x y)
  (fix:< (char-ci->integer x) (char-ci->integer y)))

(define-integrable (char-ci<=? x y)
  (fix:<= (char-ci->integer x) (char-ci->integer y)))

(define-integrable (char-ci>? x y)
  (fix:> (char-ci->integer x) (char-ci->integer y)))

(define-integrable (char-ci>=? x y)
  (fix:>= (char-ci->integer x) (char-ci->integer y)))

(define 0-code)
(define upper-a-code)
(define lower-a-code)
(define hyphen-char)
(define backslash-char)

(define (initialize-package!)
  (set! 0-code (char-code (ascii->char #x30)))
  ;; Next two codes are offset by 10 to speed up CHAR->DIGIT.
  (set! upper-a-code (fix:- (char-code (ascii->char #x41)) 10))
  (set! lower-a-code (fix:- (char-code (ascii->char #x61)) 10))
  (set! hyphen-char (ascii->char #x2D))
  (set! backslash-char (ascii->char #x5C))
  unspecific)

(define (digit->char digit #!optional radix)
  (if (not (fix:fixnum? digit))
      (error:wrong-type-argument digit "digit" 'DIGIT->CHAR))
  (and (fix:<= 0 digit)
       (fix:< digit
	      (cond ((default-object? radix)
		     10)
		    ((and (fix:fixnum? radix)
			  (fix:<= 2 radix) (fix:<= radix 36))
		     radix)
		    (else
		     (error:wrong-type-argument radix "radix" 'DIGIT->CHAR))))
       (string-ref "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" digit)))

(define (char->digit char #!optional radix)
  (if (not (char? char))
      (error:wrong-type-argument char "character" 'CHAR->DIGIT))
  (and (fix:= 0 (char-bits char))
       (let ((code (char-code char))
	     (radix
	      (cond ((default-object? radix)
		     10)
		    ((and (fix:fixnum? radix)
			  (fix:<= 2 radix) (fix:<= radix 36))
		     radix)
		    (else
		     (error:wrong-type-argument radix "radix" 'CHAR->DIGIT)))))
	 (let ((n (fix:- code 0-code)))
	   (if (and (fix:<= 0 n) (fix:< n radix))
	       n
	       (let ((n (fix:- code upper-a-code)))
		 (if (and (fix:<= 10 n) (fix:< n radix))
		     n
		     (let ((n (fix:- code lower-a-code)))
		       (if (and (fix:<= 10 n) (fix:< n radix))
			   n
			   #f)))))))))

;;;; Character Names

(define (name->char string)
  (let ((end (string-length string))
	(bits '()))
    (define (loop start)
      (let ((left (fix:- end start)))
	(cond ((fix:= 0 left)
	       (error "Missing character name"))
	      ((fix:= 1 left)
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
				  (loop (fix:+ hyphen 1)))))))))))
    (let ((code (loop 0)))
      (make-char code (apply + bits)))))

(define (name->code string start end)
  (if (substring-ci=? string start end "Newline" 0 7)
      (char-code char:newline)
      (or (-map-> named-codes string start end)
	  (numeric-name->code string start end)
	  (error "Unknown character name" (substring string start end)))))

(define (numeric-name->code string start end)
  (and (> (- end start) 6)
       (substring-ci=? string start (+ start 5) "<code" 0 5)
       (substring-ci=? string (- end 1)  end    ">" 0 1)
       (string->number (substring string (+ start 5) (- end 1)) 10)))

(define (char->name char #!optional slashify?)
  (if (default-object? slashify?) (set! slashify? false))
  (define (loop weight bits)
    (if (fix:= 0 bits)
	(let ((code (char-code char)))
	  (let ((base-char (code->char code)))
	    (cond ((<-map- named-codes code))
		  ((and slashify?
			(not (fix:= 0 (char-bits char)))
			(or (char=? base-char backslash-char)
			    (char-set-member? char-set/atom-delimiters
					      base-char)))
		   (string-append "\\" (string base-char)))
		  ((char-graphic? base-char)
		   (string base-char))
		  (else
		   (string-append "<code"
				  (number->string code 10)
				  ">")))))
	(let ((qr (integer-divide bits 2)))
	  (let ((rest (loop (fix:* weight 2) (integer-divide-quotient qr))))
	    (if (fix:= 0 (integer-divide-remainder qr))
		rest
		(string-append (or (<-map- named-bits weight)
				   (string-append "<bits-"
						  (number->string weight 10)
						  ">"))
			       "-"
			       rest))))))
  (loop 1 (char-bits char)))

(define (-map-> alist string start end)
  (and (not (null? alist))
       (let ((key (caar alist)))
	 (if (substring-ci=? string start end
			     key 0 (string-length key))
	     (cdar alist)
	     (-map-> (cdr alist) string start end)))))

(define (<-map- alist n)
  (and (not (null? alist))
       (if (fix:= n (cdar alist))
	   (caar alist)
	   (<-map- (cdr alist) n))))

(define named-codes
  '(
    ;; Some are aliases for previous definitions, and will not appear
    ;; as output.

    ("Backspace" . #x08)
    ("Tab" . #x09)
    ("Linefeed" . #x0A)
    ("Newline" . #x0A)
    ("Page" . #x0C)
    ("Return" . #x0D)
    ("Call" . #x1A)
    ("Altmode" . #x1B)
    ("Escape" . #x1B)
    ("Backnext" . #x1F)
    ("Space" . #x20)
    ("Rubout" . #x7F)

    ;; ASCII codes

    ("NUL" . #x0)			; ^@
    ("SOH" . #x1)			; ^A
    ("STX" . #x2)			; ^B
    ("ETX" . #x3)			; ^C
    ("EOT" . #x4)			; ^D
    ("ENQ" . #x5)			; ^E
    ("ACK" . #x6)			; ^F
    ("BEL" . #x7)			; ^G
    ("BS" . #x8)			; ^H <Backspace>
    ("HT" . #x9)			; ^I <Tab>
    ("LF" . #xA)			; ^J <Linefeed> <Newline>
    ("NL" . #xA)			; ^J <Linefeed> <Newline>
    ("VT" . #xB)			; ^K
    ("FF" . #xC)			; ^L <Page>
    ("NP" . #xC)			; ^L <Page>
    ("CR" . #xD)			; ^M <Return>
    ("SO" . #xE)			; ^N
    ("SI" . #xF)			; ^O
    ("DLE" . #x10)			; ^P
    ("DC1" . #x11)			; ^Q
    ("DC2" . #x12)			; ^R
    ("DC3" . #x13)			; ^S
    ("DC4" . #x14)			; ^T
    ("NAK" . #x15)			; ^U
    ("SYN" . #x16)			; ^V
    ("ETB" . #x17)			; ^W
    ("CAN" . #x18)			; ^X
    ("EM" . #x19)			; ^Y
    ("SUB" . #x1A)			; ^Z <Call>
    ("ESC" . #x1B)			; ^[ <Altmode> <Escape>
    ("FS" . #x1C)			; ^\
    ("GS" . #x1D)			; ^]
    ("RS" . #x1E)			; ^^
    ("US" . #x1F)			; ^_ <Backnext>
    ("SP" . #x20)			; <Space>
    ("DEL" . #x7F)			; ^? <Rubout>
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
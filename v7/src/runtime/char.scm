;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/char.scm,v 13.42 1987/07/27 21:56:05 cph Rel $
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Character Abstraction

(declare (usual-integrations))

(let-syntax ((define-primitives
	       (macro names
		 `(BEGIN ,@(map (lambda (name)
				  `(LOCAL-ASSIGNMENT
				    SYSTEM-GLOBAL-ENVIRONMENT
				    ',name
				    ,(make-primitive-procedure name)))
				names)))))
  (define-primitives
   make-char char-code char-bits char->integer integer->char char->ascii
   char-ascii? ascii->char char-upcase char-downcase))

(define char-code-limit #x80)
(define char-bits-limit #x20)
(define char-integer-limit (* char-code-limit char-bits-limit))

(define (chars->ascii chars)
  (map char->ascii chars))

(define (code->char code)
  (make-char code 0))

(define (char=? x y)
  (= (char->integer x) (char->integer y)))

(define (char<? x y)
  (< (char->integer x) (char->integer y)))

(define (char<=? x y)
  (<= (char->integer x) (char->integer y)))

(define (char>? x y)
  (> (char->integer x) (char->integer y)))

(define (char>=? x y)
  (>= (char->integer x) (char->integer y)))

(define (char-ci->integer char)
  (char->integer (char-upcase char)))

(define (char-ci=? x y)
  (= (char-ci->integer x) (char-ci->integer y)))

(define (char-ci<? x y)
  (< (char-ci->integer x) (char-ci->integer y)))

(define (char-ci<=? x y)
  (<= (char-ci->integer x) (char-ci->integer y)))

(define (char-ci>? x y)
  (> (char-ci->integer x) (char-ci->integer y)))

(define (char-ci>=? x y)
  (>= (char-ci->integer x) (char-ci->integer y)))

(define char?)
(define digit->char)
(define char->digit)
(define name->char)
(define char->name)
(let ()

(define char-type
  (microcode-type 'CHARACTER))

(define 0-code (char-code (ascii->char #x30)))
(define upper-a-code (char-code (ascii->char #x41)))
(define lower-a-code (char-code (ascii->char #x61)))
(define space-char (ascii->char #x20))
(define hyphen-char (ascii->char #x2D))
(define backslash-char (ascii->char #x5C))

(define named-codes
  `(("Backspace" . #x08)
    ("Tab" . #x09)
    ("Linefeed" . #x0A)
    ("VT" . #x0B)
    ("Page" . #x0C)
    ("Return" . #x0D)
    ("Call" . #x1A)
    ("Altmode" . #x1B)
    ("Backnext" . #x1F)
    ("Space" . #x20)
    ("Rubout" . #x7F)
    ))

(define named-bits
  `(("C" . #o01)
    ("Control" . #o01)
    ("M" . #o02)
    ("Meta" . #o02)
    ("S" . #o04)
    ("Super" . #o04)
    ("H" . #o10)
    ("Hyper" . #o10)
    ("T" . #o20)
    ("Top" . #o20)
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

(set! char?
(named-lambda (char? object)
  (primitive-type? char-type object)))

(set! digit->char
(named-lambda (digit->char digit #!optional radix)
  (cond ((unassigned? radix) (set! radix 10))
	((not (and (<= 2 radix) (<= radix 36)))
	 (error "DIGIT->CHAR: Bad radix" radix)))
  (and (<= 0 digit) (< digit radix)
       (code->char (if (< digit 10)
		       (+ digit 0-code)
		       (+ (- digit 10) upper-a-code))))))

(set! char->digit
(named-lambda (char->digit char #!optional radix)
  (cond ((unassigned? radix) (set! radix 10))
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
	     (try 10 lower-a-code))))))

(set! name->char
(named-lambda (name->char string)
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
      (make-char code (apply + bits))))))

(define (name->code string start end)
  (if (substring-ci=? string start end "Newline" 0 7)
      (char-code char:newline)
      (or (-map-> named-codes string start end)
	  (error "Unknown character name" (substring string start end)))))

(set! char->name
(named-lambda (char->name char #!optional slashify?)
  (if (unassigned? slashify?) (set! slashify? false))
  (define (loop weight bits)
    (if (zero? bits)
	(let ((code (char-code char)))
	  (let ((base-char (code->char code)))
	    (cond ((<-map- named-codes code))
		  ((and slashify?
			(not (zero? (char-bits char)))
			(or (char=? base-char backslash-char)
			    (char-set-member? (access atom-delimiters
						      parser-package)
					      base-char)))
		   (string-append "\\" (char->string base-char)))
		  ((char-graphic? base-char)
		   (char->string base-char))
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
  (loop 1 (char-bits char))))

)

;;;; Character Sets

(define (char-set? object)
  (and (string? object) (= (string-length object) 256)))

(define (char-set . chars)
  (let ((char-set (string-allocate 256)))
    (vector-8b-fill! char-set 0 256 0)
    (for-each (lambda (char) (vector-8b-set! char-set (char->ascii char) 1))
	      chars)
    char-set))

(define (predicate->char-set predicate)
  (let ((char-set (string-allocate 256)))
    (define (loop code)
      (if (< code 256)
	  (begin (vector-8b-set! char-set code
				 (if (predicate (ascii->char code)) 1 0))
		 (loop (1+ code)))))
    (loop 0)
    char-set))

(define (char-set-members char-set)
  (define (loop code)
    (cond ((>= code 256) '())
	  ((zero? (vector-8b-ref char-set code)) (loop (1+ code)))
	  (else (cons (ascii->char code) (loop (1+ code))))))
  (loop 0))

(define (char-set-member? char-set char)
  (let ((ascii (char-ascii? char)))
    (and ascii (not (zero? (vector-8b-ref char-set ascii))))))

(define (char-set-invert char-set)
  (predicate->char-set
   (lambda (char) (not (char-set-member? char-set char)))))

(define (char-set-union char-set-1 char-set-2)
  (predicate->char-set
   (lambda (char)
     (or (char-set-member? char-set-1 char)
	 (char-set-member? char-set-2 char)))))

(define (char-set-intersection char-set-1 char-set-2)
  (predicate->char-set
   (lambda (char)
     (and (char-set-member? char-set-1 char)
	  (char-set-member? char-set-2 char)))))

(define (char-set-difference char-set-1 char-set-2)
  (predicate->char-set
   (lambda (char)
     (and (char-set-member? char-set-1 char)
	  (not (char-set-member? char-set-2 char))))))

;;;; System Character Sets

(define char-set:upper-case
  (predicate->char-set
   (let ((lower (ascii->char #x41))
	 (upper (ascii->char #x5A)))
     (lambda (char)
       (and (char<=? lower char)
	    (char<=? char upper))))))

(define char-set:lower-case
  (predicate->char-set
   (let ((lower (ascii->char #x61))
	 (upper (ascii->char #x7A)))
     (lambda (char)
       (and (char<=? lower char)
	    (char<=? char upper))))))

(define char-set:numeric
  (predicate->char-set
   (let ((lower (ascii->char #x30))
	 (upper (ascii->char #x39)))
     (lambda (char)
       (and (char<=? lower char)
	    (char<=? char upper))))))

(define char-set:alphabetic
  (char-set-union char-set:upper-case char-set:lower-case))

(define char-set:alphanumeric
  (char-set-union char-set:alphabetic char-set:numeric))

(define char-set:graphic
  (predicate->char-set
   (let ((lower (ascii->char #x20))
	 (upper (ascii->char #x7E)))
     (lambda (char)
       (and (char<=? lower char)
	    (char<=? char upper))))))

(define char-set:standard
  (char-set-union char-set:graphic (char-set (ascii->char #x0D))))

(define char-set:whitespace
  (char-set (ascii->char #x09)	;Tab
	    (ascii->char #x0A)	;Linefeed
	    (ascii->char #x0C)	;Page
	    (ascii->char #x0D)	;Return
	    (ascii->char #x20)	;Space
	    ))

(define char-set:not-whitespace
  (char-set-invert char-set:whitespace))

(define ((char-set-predicate char-set) char)
  (char-set-member? char-set char))

(define char-upper-case? (char-set-predicate char-set:upper-case))
(define char-lower-case? (char-set-predicate char-set:lower-case))
(define char-numeric? (char-set-predicate char-set:numeric))
(define char-alphabetic? (char-set-predicate char-set:alphabetic))
(define char-alphanumeric? (char-set-predicate char-set:alphanumeric))
(define char-graphic? (char-set-predicate char-set:graphic))
(define char-standard? (char-set-predicate char-set:standard))
(define char-whitespace? (char-set-predicate char-set:whitespace))

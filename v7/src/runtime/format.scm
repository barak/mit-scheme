;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/format.scm,v 13.41 1987/01/23 00:12:19 jinx Exp $
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
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Output Formatter

(declare (usual-integrations))

;;; Please don't believe this implementation!  I don't like either the
;;; calling interface or the control string syntax, but I need the
;;; functionality pretty badly and I don't have the time to think
;;; about all of that right now -- CPH.

(define format)
(let ()

;;;; Top Level

(set! format
(named-lambda (format port-or-string . arguments)
  (cond ((null? port-or-string)
	 (if (and (not (null? arguments))
		  (string? (car arguments)))
	     (with-output-to-string
	      (lambda ()
		(format-start (car arguments) (cdr arguments))))
	     (error "Missing format string" 'FORMAT)))
	((string? port-or-string)
	 (format-start port-or-string arguments)
	 *the-non-printing-object*)
	((output-port? port-or-string)
	 (if (and (not (null? arguments))
		  (string? (car arguments)))
	     (begin (with-output-to-port port-or-string
		      (lambda ()
			(format-start (car arguments) (cdr arguments))))
		    *the-non-printing-object*)
	     (error "Missing format string" 'FORMAT)))
	(else
	 (error "Unrecognizable first argument" 'FORMAT
		port-or-string)))))

(define (format-start string arguments)
  (format-loop string arguments)
  ((access :flush-output *current-output-port*)))

(declare (integrate *unparse-char *unparse-string *unparse-object))

(define (*unparse-char char)
  (declare (integrate char))
  ((access :write-char *current-output-port*) char))

(define (*unparse-string string)
  (declare (integrate string))
  ((access :write-string *current-output-port*) string))

(define (*unparse-object object)
  (declare (integrate object))
  ((access unparse-object unparser-package) object *current-output-port*))

(define (format-loop string arguments)
  (let ((index (string-find-next-char string #\~)))
    (cond (index
	   (if (not (zero? index))
	       (*unparse-string (substring string 0 index)))
	   (parse-dispatch (string-tail string (1+ index))
			   arguments
			   '()
			   '()
			   (lambda (remaining-string remaining-arguments)
			     (format-loop remaining-string
					  remaining-arguments))))
	  ((null? arguments)
	   (*unparse-string string))
	  (else
	   (error "Too many arguments" 'FORMAT arguments)))))

(define (parse-dispatch string supplied-arguments parsed-arguments modifiers
			receiver)
  ((vector-ref format-dispatch-table (vector-8b-ref string 0))
   string
   supplied-arguments
   parsed-arguments
   modifiers
   receiver))

;;;; Argument Parsing

(define ((format-wrapper operator) 
	 string supplied-arguments parsed-arguments modifiers receiver)
  ((apply operator modifiers (reverse! parsed-arguments))
   (string-tail string 1)
   supplied-arguments
   receiver))

(define ((parse-modifier keyword)
	 string supplied-arguments parsed-arguments modifiers receiver)
  (parse-dispatch (string-tail string 1)
		  supplied-arguments
		  parsed-arguments
		  (cons keyword modifiers)
		  receiver))

(define (parse-digit string supplied-arguments parsed-arguments modifiers
		     receiver)
  (let accumulate ((acc (char->digit (string-ref string 0) 10))
		   (i 1))
    (if (char-numeric? (string-ref string i))
	(accumulate (+ (* acc 10)
		       (char->digit (string-ref string i) 10))
		    (1+ i))
	(parse-dispatch (string-tail string i)
			supplied-arguments
			(cons acc parsed-arguments)
			modifiers
			receiver))))

(define (parse-ignore string supplied-arguments parsed-arguments modifiers
		      receiver)
  (parse-dispatch (string-tail string 1)
		  supplied-arguments
		  parsed-arguments
		  modifiers
		  receiver))

(define (parse-arity string supplied-arguments parsed-arguments modifiers
		     receiver)
  (parse-dispatch (string-tail string 1)
		  supplied-arguments
		  (cons (length supplied-arguments) parsed-arguments)
		  modifiers
		  receiver))

(define (parse-argument string supplied-arguments parsed-arguments modifiers
			receiver)
  (parse-dispatch (string-tail string 1)
		  (cdr supplied-arguments)
		  (cons (car supplied-arguments) parsed-arguments)
		  modifiers
		  receiver))

(define (string-tail string index)
  (substring string index (string-length string)))

;;;; Formatters

(define (((format-insert-character character) modifiers #!optional n)
	 string arguments receiver)
  (if (unassigned? n)
      (*unparse-char character)
      (let loop ((i 0))
	(if (not (= i n))
	    (begin (*unparse-char character)
		   (loop (1+ i))))))
  (receiver string arguments))

(define format-insert-return (format-insert-character char:newline))
(define format-insert-tilde (format-insert-character #\~))
(define format-insert-space (format-insert-character #\Space))

(define ((format-ignore-comment modifiers) string arguments receiver)
  (receiver (substring string
		       (1+ (string-find-next-char string char:newline))
		       (string-length string))
	    arguments))

(define format-ignore-whitespace)
(let ()

(define newline-string
  (char->string char:newline))

(define (eliminate-whitespace string)
  (let ((limit (string-length string)))
    (let loop ((n 0))
      (cond ((= n limit) "")
	    ((let ((char (string-ref string n)))
	       (and (char-whitespace? char)
		    (not (char=? char char:newline))))
	     (loop (1+ n)))
	    (else
	     (substring string n limit))))))

(set! format-ignore-whitespace
(named-lambda ((format-ignore-whitespace modifiers) string arguments receiver)
  (receiver (cond ((null? modifiers) (eliminate-whitespace string))
		  ((memq 'AT modifiers)
		   (string-append newline-string
				  (eliminate-whitespace string)))
		  (else string))
	    arguments)))
)

(define ((format-string modifiers #!optional n-columns)
	 string arguments receiver)
  (if (null? arguments)
      (error "Too few arguments" 'FORMAT string))
  (if (unassigned? n-columns)
      (*unparse-string (car arguments))
      (unparse-string-into-fixed-size (car arguments) #!FALSE
				      n-columns modifiers))
  (receiver string (cdr arguments)))

(define ((format-object modifiers #!optional n-columns)
	 string arguments receiver)
  (if (null? arguments)
      (error "Too few arguments" 'FORMAT string))
  (if (unassigned? n-columns)
      (*unparse-object (car arguments))
      (unparse-object-into-fixed-size (car arguments) n-columns modifiers))
  (receiver string (cdr arguments)))

(define ((format-code modifiers #!optional n-columns)
	 string arguments receiver)
  (if (null? arguments)
      (error "Too few arguments" 'FORMAT string))
  (if (unassigned? n-columns)
      (*unparse-object (unsyntax (car arguments)))
      (unparse-object-into-fixed-size (unsyntax (car arguments))
				      n-columns
				      modifiers))
  (receiver string (cdr arguments)))

(define (unparse-object-into-fixed-size object n-columns modifiers)
  (let ((output (write-to-string object n-columns)))
    (unparse-string-into-fixed-size (cdr output)
				    (car output)
				    n-columns
				    modifiers)))

(define (unparse-string-into-fixed-size string already-truncated?
					n-columns modifiers)
  (let ((padding (- n-columns (string-length string))))
    (cond ((and (zero? padding) (not already-truncated?))
	   (*unparse-string string))
	  ((positive? padding)
	   (let ((pad-string (make-string padding #\Space)))
	     (if (memq 'AT modifiers)
		 (begin (*unparse-string string)
			(*unparse-string pad-string))
		 (begin (*unparse-string pad-string)
			(*unparse-string string)))))
	  ;; This is pretty random -- figure out something better.
	  ((memq 'COLON modifiers)
	   (*unparse-string (substring string 0 (- n-columns 4)))
	   (*unparse-string " ..."))
	  (else
	   (*unparse-string (substring string 0 n-columns))))))

;;;; Dispatcher Setup

(define format-dispatch-table
  (make-initialized-vector
   128
   (lambda (character)
     (lambda (string supplied-arguments parsed-arguments modifiers receiver)
       (error "Unknown formatting character" 'FORMAT character)))))

(define (add-dispatcher! char dispatcher)
  (if (char-alphabetic? char)
      (begin (vector-set! format-dispatch-table
			  (char->ascii (char-downcase char))
			  dispatcher)
	     (vector-set! format-dispatch-table
			  (char->ascii (char-upcase char))
			  dispatcher))
      (vector-set! format-dispatch-table
		   (char->ascii char)
		   dispatcher)))

(add-dispatcher! #\0 parse-digit)
(add-dispatcher! #\1 parse-digit)
(add-dispatcher! #\2 parse-digit)
(add-dispatcher! #\3 parse-digit)
(add-dispatcher! #\4 parse-digit)
(add-dispatcher! #\5 parse-digit)
(add-dispatcher! #\6 parse-digit)
(add-dispatcher! #\7 parse-digit)
(add-dispatcher! #\8 parse-digit)
(add-dispatcher! #\9 parse-digit)
(add-dispatcher! #\, parse-ignore)
(add-dispatcher! #\# parse-arity)
(add-dispatcher! #\V parse-argument)
(add-dispatcher! #\@ (parse-modifier 'AT))
(add-dispatcher! #\: (parse-modifier 'COLON))

;;;
;;; (format format-string arg arg ...)
;;; (format port format-string arg arg ...)
;;;
;;; Format strings are normally interpreted literally, except that
;;; certain escape sequences allow insertion of computed values.  The
;;; following escape sequences are recognized:
;;;
;;; ~n% inserts n newlines
;;; ~n~ inserts n tildes
;;; ~nX inserts n spaces
;;;
;;; ~<c> inserts the next argument.
;;; ~n<c> right justifies the argument in a field of size n.
;;; ~n@<c> left justifies the argument in a field of size n.
;;;
;;; where <c> may be:
;;; S meaning the argument is a string and should be used literally.
;;; O meaning the argument is an object and should be printed first.
;;; C meaning the object is SCode and should be unsyntaxed and printed.
;;; 
;;; If the resulting string is too long, it is truncated.
;;; ~n:<c> or ~n:@<c> means print trailing dots when truncating.
;;; 

(add-dispatcher! #\% (format-wrapper format-insert-return))
(add-dispatcher! #\~ (format-wrapper format-insert-tilde))
(add-dispatcher! #\X (format-wrapper format-insert-space))
(add-dispatcher! #\; (format-wrapper format-ignore-comment))
(add-dispatcher! char:newline (format-wrapper format-ignore-whitespace))
(add-dispatcher! #\S (format-wrapper format-string))
(add-dispatcher! #\O (format-wrapper format-object))
(add-dispatcher! #\C (format-wrapper format-code))

;;; end LET.
)
)
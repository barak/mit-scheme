#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sicp/studen.scm,v 1.2 1990/11/14 14:58:18 cph Exp $

Copyright (c) 1987, 1988, 1989, 1990 Massachusetts Institute of Technology

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

;;;; Environment, syntax and read table hacking for 6.001 students.

(declare (usual-integrations))

;;; Define the #/ syntax.

(in-package (->environment '(runtime parser))
  (define (parse-object/char-forward-quote)
    (discard-char)
    (if (char=? #\/ (peek-char))
	(read-char)
	(name->char
	 (let loop ()
	   (cond ((char=? #\/ (peek-char))
		  (discard-char)
		  (string (read-char)))
		 ((char-set-member? char-set/char-delimiters (peek-char))
		  (string (read-char)))
		 (else
		  (let ((string (read-string char-set/char-delimiters)))
		    (if (let ((char (peek-char/eof-ok)))
			  (and char
			       (char=? #\- char)))
			(begin (discard-char)
			       (string-append string "-" (loop)))
			string)))))))))

(parser-table/set-entry! system-global-parser-table
			 "#\/"
			 (access parse-object/char-forward-quote
				 (->environment '(runtime parser))))

(define environment-warning-hook)

(define user-global-environment)

(define student-package
  (make-environment

;;;; Syntax Restrictions

(define sicp-parser-table
  (parser-table/copy system-global-parser-table))

(define *student-parser-table*)

(define sicp-syntax-table
  (make-syntax-table))

(define *student-syntax-table*)

(define (enable-system-syntax)
  (set-current-parser-table! system-global-parser-table)
  (set-repl/syntax-table! (nearest-repl) system-global-syntax-table))

(define (disable-system-syntax)
  (set-current-parser-table! *student-parser-table*)
  (set-repl/syntax-table! (nearest-repl) *student-syntax-table*))

(define (initialize-syntax!)
  ;; First hack the parser (reader) table
  ;; Remove backquote and comma
  (let ((undefined-entry
	 (access parse-object/undefined-atom-delimiter
		 (->environment '(runtime parser)))))
    (parser-table/set-entry! sicp-parser-table "`" undefined-entry)
    (parser-table/set-entry! sicp-parser-table "," undefined-entry))
  ;; Add brackets as extended alphabetic since they are used in book (ugh!)
  (parser-table/entry
   system-global-parser-table
   "/"
   (lambda (parse-object collect-list)
     (parser-table/set-entry! sicp-parser-table "[" parse-object collect-list)
     (parser-table/set-entry! sicp-parser-table "]" parse-object
			      collect-list)))
  ;; Now, hack the syntax (special form) table.
  (let ((move
	 (lambda (from to)
	   (syntax-table-define sicp-syntax-table to
	     (or (syntax-table-ref system-global-syntax-table from)
		 (error "Missing syntactic keyword" from))))))
    (for-each (lambda (name) (move name name))
	      '(ACCESS BEGIN BKPT COLLECT COND CONS-STREAM DEFINE
		       DELAY ERROR IF LAMBDA LET MAKE-ENVIRONMENT
		       QUOTE SET! THE-ENVIRONMENT))
    (move 'AND 'CONJUNCTION)
    (move 'OR 'DISJUNCTION)
    (move 'BEGIN 'SEQUENCE))
  (set! *student-parser-table* (parser-table/copy sicp-parser-table))
  (set! *student-syntax-table* (syntax-table/copy sicp-syntax-table))
  #T)

;;;; Global Environment

(define (global-environment-enabled?)
  (or (eq? user-global-environment system-global-environment)
      (environment-has-parent? user-global-environment)))

(define (in-user-environment-chain? environment)
  (or (eq? environment user-global-environment)
      (and (environment-has-parent? environment)
	   (in-user-environment-chain? (environment-parent environment)))))

(define ic-environment/remove-parent!)
(define ic-environment/set-parent!)

(let ((e (->environment '(runtime environment))))
  (set! ic-environment/remove-parent! (access ic-environment/remove-parent! e))
  (set! ic-environment/set-parent! (access ic-environment/set-parent! e)))

(define (disable-global-environment)
  (ic-environment/remove-parent! user-global-environment)
  'DISABLED)

(define (enable-global-environment)
  (ic-environment/set-parent! user-global-environment
			      system-global-environment)
  'ENABLED)

(define (student-environment-warning-hook environment)
  (if (not (in-user-environment-chain? environment))
      (begin
	(newline)
	(write-string
	 "This environment is part of the Scheme system outside the student system.")
	(newline)
	(write-string
	 "Performing side-effects in it may damage to the system."))))

;;;; Feature hackery

(define (enable-language-features . prompt)
  prompt
  (without-interrupts
   (lambda ()
     (enable-global-environment)
     (enable-system-syntax)))
  unspecific)

(define (disable-language-features . prompt)
  prompt
  (without-interrupts
   (lambda ()
     (disable-global-environment)
     (disable-system-syntax)))
  unspecific)

(define (language-features-enabled?)
  (global-environment-enabled?))

;;;; Clean environment hackery

(define user-global-names
  '(
    (%EXIT)
    (%GE)
    (%IN) 
    (%OUT)
    (%VE)
    (*)
    (*ARGS*)
    (*PROC*)
    (*RESULT*)
    (+)
    (-)
    (-1+)
    (/)
    (1+)
    (<)
    (<=)
    (=)
    (>)
    (>=)
    (ABS)
    (ACCUMULATE)
    (ACCUMULATE-DELAYED)
    (ADD-STREAMS)
    (ADVICE)
    (ADVISE-ENTRY)
    (ADVISE-EXIT)
    (ALPHALESS?)
    (AND . AND*)
    (APPEND)
    (APPEND-STREAMS)
    (APPLICABLE? . PROCEDURE?)
    (APPLY)
    (ASCII)
    (ASSOC)
    (ASSQ)
    (ASSV)
    (ATAN)
    (ATOM?)
    (BREAK . BREAK-ENTRY)
    (BREAK-BOTH . BREAK)
    (BREAK-ENTRY)
    (BREAK-EXIT)
    (BREAKPOINT-PROCEDURE)

    (CAR)
    (CAAAAR)
    (CAAADR)
    (CAAAR)
    (CAADAR)
    (CAADDR)
    (CAADR)
    (CAAR)
    (CADAAR)
    (CADADR)
    (CADAR)
    (CADDAR)
    (CADDDR)
    (CADDR)
    (CADR)
    (CD)
    (CDR)
    (CDAAAR)
    (CDAADR)
    (CDAAR)
    (CDADAR)
    (CDADDR)
    (CDADR)
    (CDAR)
    (CDDAAR)
    (CDDADR)
    (CDDAR)
    (CDDDAR)
    (CDDDDR)
    (CDDDR)
    (CDDR)
    (CEILING)
    (CHAR)
    (CLEAR-GRAPHICS)
    (CLEAR-POINT)
    (CLOSE-CHANNEL)
    (CONS)
    (CONS*)
    (COPY-FILE)
    (COS)
    (DEBUG)
    (DELETE-FILE)
    (DRAW-LINE-TO)
    (DRAW-POINT)

    (EIGHTH)
    (EMPTY-STREAM?)
    (ENABLE-LANGUAGE-FEATURES)
    (ENUMERATE-FRINGE)
    (ENUMERATE-INTERVAL)
    (ENVIRONMENT?)
    (EQ?)
    (EQUAL?)
    (EQV?)
    (ERROR-PROCEDURE)
    (EVAL)
    (EVEN?)
    (EXP)
    (EXPLODE)
    (EXPT)
    (FALSE)
    (FIFTH)
    (FILE-EXISTS?)
    (FILTER)
    (FIRST)
    (FLATMAP)
    (FLATTEN)
    (FLOOR)
    (FORCE)
    (FOURTH)
    (GCD)
    (GENERATE-UNINTERNED-SYMBOL)
    (GRAPHICS-AVAILABLE?)
    (GRAPHICS-TEXT)
    (HEAD)
    (IMPLODE)
    (INIT-GRAPHICS)
    (INTEGER-DIVIDE)
    (INTEGER?)
    (INTEGERS-FROM)
    (INTEGERS)
    (INTERLEAVE-DELAYED)
    (LAST . LAST-PAIR)
    (LENGTH)
    (LIST)
    (LIST* . CONS*)
    (LIST-REF)
    (LIST-TAIL)
    (LIST?)
    (LOAD)
    (LOAD-NOISILY)
    (LOG)

    (MAP-STREAM)
    (MAP-STREAM-2)
    (MAPC . FOR-EACH)
    (MAPCAR . MAP)
    (MAX)
    (MEMBER)
    (MEMQ)
    (MEMV)
    (MERGE)
    (MIN)
    (NEGATIVE?)
    (NEWLINE)
    (NIL)
    (NOT)
    (NTH)
    (NTH-STREAM)
    (NTHCDR)
    (NULL?)
    (NUMBER?)
    (OBJECT-TYPE)
    (ODD?)
    (OPEN-READER-CHANNEL . OPEN-INPUT-FILE)
    (OPEN-PRINTER-CHANNEL . OPEN-OUTPUT-FILE)
    (OR . OR*)
    (PAIR?)
    (POSITION-PEN)
    (POSITIVE?)
    (PP)
    (PRIN1 . WRITE)
    (PRINC . DISPLAY)
    (PRINT . WRITE-LINE)
    (PRINT-STREAM)
    (PROCEED)
    (QUIT)
    (QUOTIENT)
    (RANDOM)
    (READ)
    (READ-FROM-KEYBOARD)
    (REMAINDER)
    (REVERSE)
    (ROUND)
    (RUNTIME)
    (SCALE-STREAM)

    (SECOND)
    (SET-CAR!)
    (SET-CDR!)
    (SEVENTH)
    (SIN)
    (SIXTH)
    (SPREAD-TUPLE)
    (SQRT)
    (STRING-LESS?. STRING<?)
    (SYMBOL?)
    (T)
    (TAIL)
    (TAN)
    (THE-EMPTY-STREAM)
    (THIRD)
    (TRACE . TRACE-ENTRY)
    (TRACE-BOTH . TRACE)
    (TRACE-ENTRY)
    (TRACE-EXIT)
    (TRUE)
    (TRUNCATE)
    (UNADVISE)
    (UNADVISE-ENTRY)
    (UNADVISE-EXIT)
    (UNBREAK)
    (UNBREAK-ENTRY)
    (UNBREAK-EXIT)
    (UNTRACE)
    (UNTRACE-ENTRY)
    (UNTRACE-EXIT)
    (USER-GLOBAL-ENVIRONMENT . #T)
    (USER-INITIAL-ENVIRONMENT . #T)
    (VECTOR)
    (VECTOR-CONS)
    (VECTOR-REF)
    (VECTOR-SET!)
    (VECTOR-SIZE . VECTOR-LENGTH)
    (VECTOR?)
    (WHERE)
    (ZERO?)))

;;; Environment setup code

(define (warn-about-missing-objects missing)
  (for-each
   (lambda (name)
     (newline)
     (write-string "Warning -- missing name: ")
     (write name))
   missing))

(define (setup-user-global-environment!)
  (define (copy-if-proc object)
    (if (compound-procedure? object)
	(scode-eval (lambda-components (procedure-lambda object)
		      make-lambda)
		    (procedure-environment object))
	object))

  (build-environment
   user-global-names
   system-global-environment	; Where to look
   system-global-environment	; Parent frame
   copy-if-proc			; What to do to each value
   (lambda (frame missing)
     (scode-eval (scode-quote
		  (begin
		    (set! user-global-environment (the-environment))
		    (set! user-initial-environment (make-environment))))
		 frame)
     (set! user-global-environment frame)
     (set! user-initial-environment
	   (lexical-reference frame 'user-initial-environment))
     (warn-about-missing-objects missing))))

;;;; Saving and restoring the student system

(define student-band-pathname)

(define (initialize-system)
  (set! init-file-pathname
	(let ((old-init-file-pathname (init-file-pathname)))
	  (lambda ()
	    (merge-pathnames (make-pathname #f #f #f "sicp" #f #f)
			     old-init-file-pathname))))
  (set! student-band-pathname
	(merge-pathnames
	 (make-pathname #f #f #f "sicp" "bin" #f)
	 (->pathname
	  (or ((make-primitive-procedure 'reload-band-name))
	      ((make-primitive-procedure 'microcode-tables-filename))))))
  (add-event-receiver!
   event:after-restart
   (lambda ()
     (if (language-features-enabled?)
	 (disable-language-features))
     (if (not (graphics-available?))
	 (begin
	   (newline)
	   (display "*** Note: no graphics available in this system. ***")))))
  #T)

(define (reload #!optional filename)
  (disk-restore
   (if (default-object? filename)
       student-band-pathname
       (merge-pathnames (->pathname filename)
			student-band-pathname))))   

(define (student-band #!optional filename)
  (if (not (default-object? filename))
      (set! student-band-pathname
	    (merge-pathnames (->pathname filename)
			     student-band-pathname)))
  (disk-save student-band-pathname))

(define (student-dump filename)
  (dump-world filename))

;;; End STUDENT-PACKAGE.
))

;;;; Exports

(define enable-language-features
  (access enable-language-features student-package))

(define disable-language-features
  (access disable-language-features student-package))

(define reload
  (access reload student-package))

(define student-band
  (access student-band student-package))

(define student-dump
  (access student-dump student-package))

;;; Install the student package

((access initialize-syntax! student-package))
((access setup-user-global-environment! student-package))
((access initialize-system student-package))
(set! environment-warning-hook
      (access student-environment-warning-hook student-package))
(set-repl/environment! (nearest-repl) user-initial-environment)
(disable-language-features)
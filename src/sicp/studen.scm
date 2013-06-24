#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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
			string))))))))
  
  (define char-set/mit-scheme-atom-delimiters
    char-set/atom-delimiters)

  (define char-set/sicp-atom-delimiters
    (char-set-difference
     char-set/mit-scheme-atom-delimiters
     (char-set #\[ #\])))

  (define (set-atom-delimiters! kind)
    (set! char-set/atom-delimiters
	  (case kind
	    ((mit-scheme)
	     char-set/mit-scheme-atom-delimiters)
	    ((sicp)
	     char-set/sicp-atom-delimiters)
	    (else
	     (error "set-atom-delimiters!: Unknown kind")))))

) ;; end in-package

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

(define set-atom-delimiters!
  (access set-atom-delimiters! (->environment '(runtime parser))))

(define (enable-system-syntax)
  (set! *parser-table* system-global-parser-table)
  (set-atom-delimiters! 'mit-scheme)
  (set-repl/syntax-table! (nearest-repl) system-global-syntax-table))

(define (disable-system-syntax)
  (set! *parser-table* *student-parser-table*)
  (set-atom-delimiters! 'sicp)
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
   "@"
   (lambda (parse-object collect-list)
     (parser-table/set-entry! sicp-parser-table "[" parse-object collect-list)
     (parser-table/set-entry! sicp-parser-table "]" parse-object
			      collect-list)))
  ;; Now, hack the syntax (special form) table.
  (let ((move
	 (lambda (from to)
	   (syntax-table/define sicp-syntax-table to
	     (or (syntax-table/ref system-global-syntax-table from)
		 (error "Missing syntactic keyword" from))))))
    (for-each (lambda (name) (move name name))
	      '(
		;; These special forms are shared.
		COLLECT COND CONS-STREAM DEFINE
		DELAY IF LAMBDA LET MAKE-ENVIRONMENT
		QUOTE SEQUENCE SET! THE-ENVIRONMENT
		;; The following are needed because some of the above are
		;; macros and they are not syntactically closed.  Yuck!
		ACCESS BEGIN NAMED-LAMBDA))
    (move 'AND 'CONJUNCTION)
    (move 'OR 'DISJUNCTION))
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
    (BKPT)
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
    (CEILING . CEILING->EXACT)
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
    (ERROR)
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
    (FLOOR . FLOOR->EXACT)
    (FORCE)
    (FOURTH)
    (GCD)
    (GE)
    (GENERATE-UNINTERNED-SYMBOL)
    (GRAPHICS-AVAILABLE?)
    (GRAPHICS-TEXT)
    (HEAD)
    (IMPLODE)
    (IN)
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
    (OUT)
    (PAIR?)
    (POSITION-PEN)
    (POSITIVE?)
    (PP . STUDENT-PP)
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
    (RESTART)
    (REVERSE)
    (ROUND . ROUND->EXACT)
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
    (TRUNCATE . TRUNCATE->EXACT)
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
    (VE)
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
	 (system-library-directory-pathname false)))
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
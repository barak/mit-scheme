#| -*- Mode: Scheme -*-

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

;;;; File attributes parser
;;; package: (runtime parser file-attributes)

(declare (usual-integrations))

;;; This code will parse "file attributes line" found in the first
;;; or second line of file and delimited by the special -*- sequence.

;;; Here are sample attribute lines taken from various files
;;; found in the wild.  They won't be parsed because they are not
;;; in the first two lines.

;;; ---------------
#| -*-Scheme-*-
This file is part of MIT/GNU Scheme.
|#

#||-*- mode:lisp;
       package:(FOOBAR :USE (GLOBAL BAZ)
                       :SHADOW (CAR CDR CONS));
       base:10
   -*- ||#

;;; -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*-

;;; -*- Mode: C; tab-width: 4; -*-

;;; For Emacs: -*- mode:cperl; mode:folding -*-

;;; -*- Mode:Lisp; Package:XLIB; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;;; -*-mode:C;tab-width:3-*-

;;; -*-mode:c; c-style:k&r; c-basic-offset:4; -*-

;;;-*-Mode:LISP;Syntax: Common-Lisp;Package:ib;Base:10-*-

;;;-*-mode:lisp;parser:read-*-

;;; -*-Mode:Perl; perl-indent-level:8-*-

;;; -*-mode:JavaScript;coding:latin-1;-*- Time-stamp: "2006-08-09 16:18:45 ADT"

;;; -*- Mode: C; indent-tabs-mode:nil; c-basic-offset: 8-*- */

;;; -*- coding:utf-8;mode:python;mode:font-lock -*-

;;; -*- test-case-name: twisted.test.test_htb -*-

;;; -*- mode: C; c-file-style: "gnu" -*-

;;;-*- syntax:COMMON-LISP; Package: (ITERATE :use "COMMON-LISP" :colon-mode :external) -*-

;;; -*- package IDE-ini -*-

;;; -*- Mode: Emacs-Lisp; outline-regexp: " \n;;;;+" -*-

;;; It should surprise no one that the following comes from a python file.
;;; -*-*- encoding: utf-8 -*-*-

;;; ---------------

;;; The most general case is a series of key/value pairs where the key
;;; is followed by a colon and the pairs are separated or delimited by
;;; semicolons.  Whitespace is optional and cannot be relied upon to
;;; delimit the end of a key or a value.

;;; If the parser used the standard atom delimiters and the system
;;; global parser table, a file attributes line like
;;; "-*-mode:lisp;parser:read-*-" would be interpreted as the symbol
;;; '-*-mode:lisp followed by a comment.  We therefore need to run the
;;; parser with modified settings.

;;; We need two modes.  The first is the mode where we are expecting
;;; the key of a key/value pair.  Since the key is delimited by a
;;; colon, or by whitespace followed by a colon, we need the colon
;;; character to be atom-delimiter.

;;; The second mode is when we are reading the value of a key/value
;;; pair.  The value is read as an ordinary lisp object.  This is
;;; slightly different from the standard settings of the Scheme
;;; reader.

;;; The actual way we parse the mode line is to stay in the first mode
;;; until we read a colon character.  At that point, we switch to the
;;; second mode in order to read a single value and return to the
;;; first mode immediately afterwards.

;; These are the char-sets and parser table for use in the mode where
;; we are parsing anything but a value.  (mode 1)
(define char-set/file-attributes-atom-delimiters)
(define char-set/file-attributes-constituents)
(define file-attributes-parser-table)

(define (parse-file-attributes-item parse port)
  ;; Prepare the parser for first mode.
  (fluid-let ((*parser-associate-positions?* #f)
	      (*parser-atom-delimiters*
	       char-set/file-attributes-atom-delimiters)
	      (*parser-canonicalize-symbols?* #f)
	      (*parser-constituents* char-set/file-attributes-constituents)
	      (*parser-enable-file-attributes-parsing?* #f) ; no recursion!
	      (*parser-keyword-style* #f)
	      (*parser-radix* 10)
	      (*parser-table* file-attributes-parser-table))
    (parse port system-global-environment)))

(define (parse-file-attributes-value parse port)
  ;; Prepare the parser for second mode.
  (fluid-let ((*parser-associate-positions?* #f)
	      (*parser-atom-delimiters* char-set/atom-delimiters)
	      (*parser-canonicalize-symbols?* #f)
	      (*parser-constituents* char-set/constituents)
	      (*parser-enable-file-attributes-parsing?* #f) ; no recursion!
	      ;; enable prefix keywords
	      (*parser-keyword-style* 'prefix)
	      (*parser-radix* 10)
	      (*parser-table* system-global-parser-table))
    (parse port system-global-environment)))

(define (parse-file-attributes-line port db multiline)
  (declare (ignore db))
  (tokens->alist
   (tokenize-file-attributes-line port multiline)))

;; If we don't see a COLON or a SEMICOLON often enough, we'll assume
;; that we're confused by an ill-formed attributes line and abandon
;; the parsing.
(define file-attributes-confusion-limit 3)

(define (tokenize-file-attributes-line port multiline)
  (let ((parser (top-level-parser port)))

    (define (tokenize confusion-count tokens)
      (if (> confusion-count file-attributes-confusion-limit)
	  (begin
	    (warn "Ill-formed file attributes list.")
	    #f)

	  (let ((token (if (and (pair? tokens)
				(eq? (car tokens) colon-token))
			   (parse-file-attributes-value parser port)
			   (parse-file-attributes-item parser port))))

	    (cond ((eof-object? token) (if multiline
					   (error:premature-eof port)
					   token))

		  ((or (eq? token colon-token)
		       (eq? token semicolon-token))
		   ;; saw a colon or semicolon, we're still on track.
		   (tokenize 0 (cons token tokens)))

		  ((eq? token newline-token)
		   (if multiline
		       ;; discard if multiline
		       (tokenize (+ confusion-count 1) tokens)
		       ;; If we hit the end of line while parsing a single
		       ;; line, then the file attributes line is ill-formed.
		       (begin
			 (warn "Ill-formed file attributes line.")
			 #f)))

		  ((symbol? token)
		   (let ((token* (if (null? tokens)
				     (trim-initial-token token)
				     token)))
		     (cond ((not token*) (tokenize confusion-count tokens))
			   ((string-suffix? "-*-" (symbol-name token*))
			    (let ((token** (trim-final-token token*)))
			      (if token**
				  (reverse (cons token** tokens))
				  (reverse tokens))))
			   (else (tokenize (+ confusion-count 1)
					   (cons token* tokens))))))

		  (else (tokenize (+ confusion-count 1)
				  (cons token tokens)))))))

    (tokenize 0 '())))

;;; In the case where the file attributes line has spurious *-
;;; characters, and perhaps is not whitespace delimited, these
;;; characters will end up being the first token or prepended to the
;;; first token.  Examples:
;;;  -*-*- encoding: utf-8 -*-*-
;;;  -*-*-*-logrus-*-*-*-
;;;  -*-*- coding: latin-1 -*-*-

(define (trim-initial-token sym)
  (if (string-prefix? "*-" (symbol-name sym))
      (do ((token-string (symbol-name sym) (string-tail token-string 2)))
	  ((not (string-prefix? "*-" token-string))
	   (if (zero? (string-length token-string))
	       #f
	       (string->symbol token-string)))))
  sym)

;;; If the final token is a symbol that is not whitespace delimited,
;;; then the end marker will be attached to the token.  Furthermore,
;;; if there are spurious -* characters, these will have been attached
;;; as well.  Examples:
;;; -*-Scheme-*-
;;; -*-outline-*-*-
(define (trim-final-token sym)
  (do ((token-string
	(let ((s (symbol-name sym)))
	  (string-head s (- (string-length s) 3)))
	(string-head token-string (- (string-length token-string) 2))))
      ((not (string-suffix? "-*" token-string))
       (if (zero? (string-length token-string))
	   #f
	   (string->symbol token-string)))))

;;; Given a list of tokens, create an alist of keys and values.
(define (tokens->alist tokens)

  ;; A single token is a mode indicator
  (define (parse-mode mode-token)
    (list (cons 'MODE mode-token)))

  ;; An attribute consists of a key, colon, value
  ;; triplet.  The key must be a symbol.
  (define (parse-attribute tokens)
    (let ((key (car tokens))
	  (t1 (cdr tokens)))
      (if (or (not (symbol? key))
	      (not (pair? t1)))
	  (ill-formed)
	  (let ((colon (car t1))
		(t2 (cdr t1)))
	    (if (or (not (eq? colon colon-token))
		    (not (pair? t2)))
		(ill-formed)
		(let ((value (car t2))
		      (t3 (cdr t2)))
		  (if (not (null? t3))
		      (ill-formed)
		      (cons key value))))))))

  (define (parse-attributes-alist tokens)
    (define (group alist accum tail)
      (if (pair? tail)
	  (let ((token (car tail)))
	    (if (eq? token semicolon-token)
		(let ((entry (parse-attribute (reverse accum))))
		  (group (if entry
			     (cons entry alist)
			     alist)
			 '()
			 (cdr tail)))
		(group alist (cons token accum) (cdr tail))))
	  (if (null? accum)
	      (reverse alist)
	      (reverse (let ((entry (parse-attribute (reverse accum))))
			 (if entry
			     (cons entry alist)
			     alist))))))
    (group '() '() tokens))

  (define (ill-formed)
    (warn "Ill-formed file attributes list.")
    #f)

  (if (pair? tokens)
      (cond ((memq semicolon-token tokens)
	     (parse-attributes-alist tokens))

	    ((memq colon-token tokens)
	     (list (parse-attribute tokens)))

	    ((null? (cdr tokens))
	     (parse-mode (car tokens)))

	    (else (list tokens)))
      #f))

(define (initialize-package!)
  (let* ((constituents char-set/constituents)
	 (atom-delimiters
	  (char-set-union char-set:whitespace
			  ;; Note that colon is a delimiter!
			  (string->char-set "()[]{}\":;'`,")
			  (char-set #\U+00AB #\U+00BB)))
	 (symbol-leaders
	  (char-set-difference constituents
			       (char-set-union atom-delimiters
					       char-set/number-leaders)))
	 (special-number-leaders
	  (string->char-set "bBoOdDxXiIeEsSlL"))
	 (store-char (lambda (v c h) (vector-set! v (char->integer c) h)))
	 (store-char-set
	  (lambda (v c h)
	    (for-each (lambda (c) (store-char v c h))
		      (char-set-members c)))))
    (let ((initial (make-vector #x100 #f))
	  (special (make-vector #x100 #f)))
      (store-char-set initial char-set:whitespace handler:whitespace)
      (store-char initial #\newline handler:newline)
      (store-char-set initial char-set/number-leaders handler:atom)
      (store-char-set initial symbol-leaders handler:symbol)
      (store-char-set special special-number-leaders handler:number)
      (store-char initial #\( handler:list)
      (store-char special #\( handler:vector)
      (store-char initial #\) handler:close-parenthesis)
      (store-char initial #\: handler:colon)
      (store-char initial #\; handler:semicolon)
      (store-char initial #\' handler:quote)
      (store-char initial #\` handler:quasiquote)
      (store-char initial #\, handler:unquote)
      (store-char initial #\" handler:string)
      (store-char initial #\# handler:special)
      (store-char special #\f handler:false)
      (store-char special #\F handler:false)
      (store-char special #\t handler:true)
      (store-char special #\T handler:true)
      (store-char special #\\ handler:char)
;    (store-char special #\! handler:named-constant)
      (set! file-attributes-parser-table (make-parser-table initial special))
      )
    (set! char-set/file-attributes-atom-delimiters atom-delimiters)
    (set! char-set/file-attributes-constituents constituents))
  unspecific)

(define (handler:newline port db ctx char)
  (declare (ignore port db ctx char))
  newline-token)

(define (handler:colon port db ctx char)
  (declare (ignore port db ctx char))
  colon-token)

(define (handler:semicolon port db ctx char)
  (declare (ignore port db ctx char))
  semicolon-token)

(define colon-token (list 'COLON))
(define newline-token (list 'NEWLINE))
(define semicolon-token (list 'SEMICOLON))


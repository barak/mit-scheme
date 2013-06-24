#| -*-Scheme-*-

$Id: rfc822.scm,v 3.6 2003/02/14 18:28:13 cph Exp $

Copyright 1999,2000,2003 Massachusetts Institute of Technology

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

;;;; IMAIL mail reader: RFC-822 support

(declare (usual-integrations))

(define rfc822:char-set:header-constituents
  (char-set-difference (ascii-range->char-set 33 127)
		       (char-set #\:)))

(define rfc822:char-set:not-header-constituents
  (char-set-invert rfc822:char-set:header-constituents))

(define (rfc822:header-field-name? string start end)
  (and (fix:< start end)
       (not (substring-find-next-char-in-set
	     string start end rfc822:char-set:not-header-constituents))))

(define char-set:rfc822-quoted
  (char-set-invert
   (char-set-union char-set:alphanumeric
		   (apply char-set (string->list " !#$%&'*+-/=?^_`{|}~")))))

(define (rfc822:quote-string string)
  (if (string-find-next-char-in-set string char-set:rfc822-quoted)
      (let loop ((chars (string->list string)) (result (list #\")))
	(if (null? chars)
	    (list->string (reverse! (cons #\" result)))
	    (loop (cdr chars)
		  (cons (car chars)
			(if (or (char=? #\\ (car chars))
				(char=? #\" (car chars)))
			    (cons #\\ result)
			    result)))))
      string))

(define (rfc822:first-address string)
  (let ((addresses (rfc822:string->addresses string)))
    (and (pair? addresses)
	 (car addresses))))

(define (rfc822:addresses->string addresses)
  (decorated-string-append "" ", " "" addresses))

(define (rfc822:string->addresses string)
  (let ((address-list
	 (rfc822:strip-quoted-names
	  (rfc822:strip-whitespace! (rfc822:string->tokens string)))))
    (if (and address-list (null? (cdr address-list)))
	(car address-list)
	(map (lambda (string)
	       (let ((string (string-trim string)))
		 (let ((end (string-length string)))
		   (let loop ((start 0))
		     (let ((index
			    (substring-find-next-char-in-set
			     string start end char-set:whitespace)))
		       (if index
			   (begin
			     (string-set! string index #\space)
			     (loop (fix:+ index 1)))))))
		 string))
	     (burst-string string #\, #f)))))

(define (rfc822:canonicalize-address-string string)
  (rfc822:addresses->string (rfc822:string->addresses string)))

;;;; Parsers

(define (rfc822:received-header-components string)
  (let ((from #f)
	(by #f)
	(via #f)
	(with '())
	(id #f)
	(for #f)
	(lose (lambda () (error "Malformed Received header:" string))))
    (let loop ((tokens
		(rfc822:strip-whitespace! (rfc822:string->tokens string))))
      (cond ((not (pair? tokens))
	     (lose))
	    ((eqv? #\; (car tokens))
	     (values from by via (reverse! with) id for
		     (let ((pv (rfc822:parse-date-time (cdr tokens))))
		       (if (not (and (pair? pv) (null? (cdr pv))))
			   (lose))
		       (car pv))))
	    ((not (string? (car tokens)))
	     (lose))
	    ((string-ci=? "from" (car tokens))
	     (let ((pv (rfc822:parse-domain (cdr tokens))))
	       (if (not pv)
		   (lose))
	       (set! from (car pv))
	       (loop (cdr pv))))
	    ((string-ci=? "by" (car tokens))
	     (let ((pv (rfc822:parse-domain (cdr tokens))))
	       (if (not pv)
		   (lose))
	       (set! by (car pv))
	       (loop (cdr pv))))
	    ((string-ci=? "via" (car tokens))
	     (if (not (pair? (cdr tokens)))
		 (lose))
	     (set! via (cadr tokens))
	     (loop (cddr tokens)))
	    ((string-ci=? "with" (car tokens))
	     (if (not (pair? (cdr tokens)))
		 (lose))
	     (set! with (cons (cadr tokens) with))
	     (loop (cddr tokens)))
	    ((string-ci=? "id" (car tokens))
	     (let ((pv
		    (or (rfc822:parse-msg-id (cdr tokens))
			;; Kludge: it's a common error for mailers to
			;; put malformed message IDs here.
			(and (pair? (cdr tokens))
			     (string? (car tokens))
			     (cdr tokens)))))
	       (if (not pv)
		   (lose))
	       (set! id (car pv))
	       (loop (cdr pv))))
	    ((string-ci=? "for" (car tokens))
	     (let ((pv
		    (or (rfc822:parse-addr-spec (cdr tokens))
			;; Kludge: some losing mailers do this, even
			;; though it's illegal.
			(rfc822:parse-msg-id (cdr tokens)))))
	       (if (not pv)
		   (lose))
	       (set! for (car pv))
	       (loop (cdr pv))))
	    (else (lose))))))

(define (rfc822:parse-date-time tokens)
  (let ((pv1 (rfc822:parse-date tokens)))
    (and pv1
	 (let ((pv2 (rfc822:parse-time (cdr pv1))))
	   (and pv2
		(let ((pv3 (rfc822:parse-time-zone (cdr pv2))))
		  (and pv3
		       (cons (string->universal-time
			      (string-append (car pv1)
					     " "
					     (car pv2)
					     " "
					     (car pv3)))
			     (cdr pv3)))))))))

(define (rfc822:parse-date tokens)
  (let* ((pv1 (rfc822:parse-day-of-week tokens))
	 (pv2 (rfc822:parse-number (cdr pv1))))
    (and pv2
	 (let ((pv3 (rfc822:parse-month (cdr pv2))))
	   (and pv3
		(let ((pv4 (rfc822:parse-number (cdr pv3))))
		  (and pv4
		       (cons (string-append (if (car pv1)
						(string-append (car pv1) ", ")
						"")
					    (car pv2)
					    " "
					    (car pv3)
					    " "
					    (car pv4))
			     (cdr pv4)))))))))

(define (rfc822:parse-day-of-week tokens)
  (if (and (pair? tokens)
	   (string? (car tokens))
	   (parse-date/time-component string->day-of-week (car tokens))
	   (pair? (cdr tokens))
	   (eqv? #\, (cadr tokens)))
      (cons (car tokens) (cddr tokens))
      (cons #f tokens)))

(define (rfc822:parse-month tokens)
  (and (pair? tokens)
       (string? (car tokens))
       (parse-date/time-component string->month (car tokens))
       tokens))

(define (rfc822:parse-time tokens)
  (let ((pv1 (rfc822:parse-number tokens)))
    (and pv1
	 (pair? (cdr pv1))
	 (eqv? #\: (cadr pv1))
	 (let ((pv2 (rfc822:parse-number (cddr pv1))))
	   (and pv2
		(pair? (cdr pv2))
		(eqv? #\: (cadr pv2))
		(let ((pv3 (rfc822:parse-number (cddr pv2))))
		  (and pv3
		       (cons (string-append (car pv1)
					    ":"
					    (car pv2)
					    ":"
					    (car pv3))
			     (cdr pv3)))))))))

(define (rfc822:parse-time-zone tokens)
  (and (pair? tokens)
       (string? (car tokens))
       (parse-date/time-component string->time-zone (car tokens))
       tokens))

(define (parse-date/time-component string->component string)
  (let ((v (ignore-errors (lambda () (string->component string)))))
    (and (not (condition? v))
	 v)))

(define (rfc822:parse-msg-id tokens)
  (and (pair? tokens)
       (eqv? #\< (car tokens))
       (let ((addr-spec (rfc822:parse-addr-spec (cdr tokens))))
	 (and (pair? addr-spec)
	      (pair? (cdr addr-spec))
	      (eqv? #\> (cadr addr-spec))
	      (cons (car addr-spec) (cddr addr-spec))))))

(define (rfc822:parse-addr-spec tokens)
  (let ((local-part (rfc822:parse-list tokens #\. rfc822:parse-word)))
    (and (pair? local-part)
	 (pair? (cdr local-part))
	 (eqv? #\@ (cadr local-part))
	 (let ((domain (rfc822:parse-domain (cddr local-part))))
	   (and (pair? domain)
		(cons (string-append
		       (decorated-string-append "" "." "" (car local-part))
		       "@"
		       (decorated-string-append "" "." "" (car domain)))
		      (cdr domain)))))))

(define (rfc822:parse-domain tokens)
  (rfc822:parse-list tokens #\.
    (lambda (tokens)
      (and (pair? tokens)
	   (string? (car tokens))
	   (not (char=? #\" (string-ref (car tokens) 0)))
	   tokens))))

(define (rfc822:parse-word tokens)
  (and (pair? tokens)
       (string? (car tokens))
       (not (char=? #\[ (string-ref (car tokens) 0)))
       tokens))

(define (rfc822:parse-number tokens)
  (and (pair? tokens)
       (string? (car tokens))
       (exact-nonnegative-integer? (string->number (car tokens)))
       tokens))

(define (rfc822:parse-list tokens separator parse-element)
  (let ((first (parse-element tokens)))
    (and first
	 (let loop ((tokens (cdr first)) (words (list (car first))))
	   (let ((next
		  (and (pair? tokens)
		       (eqv? separator (car tokens))
		       (parse-element (cdr tokens)))))
	     (if next
		 (loop (cdr next) (cons (car next) words))
		 (cons (reverse! words) tokens)))))))

;;;; Token-stream filters

(define (rfc822:tokens->string tokens)
  (call-with-output-string
   (lambda (port)
     (do ((tokens tokens (cdr tokens)))
	 ((not (pair? tokens)))
       (cond ((char? (car tokens))
	      (write-char (car tokens) port))
	     ((string? (car tokens))
	      (write-string (car tokens) port))
	     ((and (pair? (car tokens))
		   (eq? 'ILLEGAL (caar tokens)))
	      (write-char (cdar tokens) port))
	     (else
	      (error "Malformed RFC-822 token stream:" tokens)))))))

(define rfc822:strip-whitespace!
  (list-deletor!
   (lambda (token)
     (cond ((char? token) (eqv? #\space token))
	   ((string? token) (char=? #\( (string-ref token 0)))
	   (else #f)))))

(define (rfc822:strip-quoted-names tokens)
  (rfc822:parse-list tokens #\,
    (lambda (tokens)
      (or (rfc822:parse-addr-spec tokens)
	  (let ((tokens
		 (let loop
		     ((tokens
		       (let ((word (rfc822:parse-word tokens)))
			 (if word
			     (cdr word)
			     tokens))))
		   (let ((word (rfc822:parse-word tokens)))
		     (if word
			 (loop (cdr word))
			 tokens)))))
	    (and (pair? tokens)
		 (eqv? #\< (car tokens))
		 (let ((addr-spec
			(rfc822:parse-addr-spec
			 (let ((domains
				(rfc822:parse-list (cdr tokens) #\,
				  (lambda (tokens)
				    (and (pair? tokens)
					 (eqv? #\@ (car tokens))
					 (rfc822:parse-domain
					  (cdr tokens)))))))
			   (if (and domains
				    (pair? (cdr domains))
				    (eqv? #\: (cadr domains)))
			       (cddr domains)
			       (cdr tokens))))))
		   (and addr-spec
			(pair? (cdr addr-spec))
			(eqv? #\> (cadr addr-spec))
			(cons (car addr-spec)
			      (cddr addr-spec))))))))))

(define (rfc822:strip-comments tokens)
  (list-transform-negative tokens
    (lambda (token)
      (and (string? token)
	   (char=? #\( (string-ref token 0))))))

;;;; Tokenizer

(define rfc822:string->tokens
  (let* ((special-chars
	  (char-set #\( #\) #\[ #\] #\< #\> #\@ #\, #\; #\: #\\ #\" #\.))
	 (atom-chars
	  (char-set-difference (ascii-range->char-set #x21 #x7F)
			       special-chars))
	 (special-char?
	  (lambda (char) (char-set-member? special-chars char)))
	 (atom-char? (lambda (char) (char-set-member? atom-chars char)))
	 (loser
	  (lambda (chars)
	    (list (cons 'UNTERMINATED (apply string (reverse! chars))))))
	 (next-lwsp?
	  (lambda (port)
	    (let ((char (input-port/peek-char port)))
	      (and (not (eof-object? char))
		   (char-lwsp? char))))))
    (lambda (input-string)
      (let ((port (open-input-string input-string)))
	(define (dispatch)
	  (let ((char (input-port/read-char port)))
	    (cond ((eof-object? char)
		   '())
		  ((char-lwsp? char)
		   (cons #\space (skip-whitespace)))
		  ((char=? #\newline char)
		   (if (next-lwsp? port)
		       (cons #\space (skip-whitespace))
		       (loser '())))
		  ((atom-char? char)
		   ;; atom
		   (let loop ((chars (list char)))
		     (let ((char (input-port/peek-char port)))
		       (if (and (not (eof-object? char))
				(atom-char? char))
			   (begin
			     (input-port/discard-char port)
			     (loop (cons char chars)))
			   (cons (apply string (reverse! chars))
				 (dispatch))))))
		  ((char=? #\" char)
		   ;; quoted string
		   (let loop ((chars (list char)))
		     (let ((char (input-port/read-char port)))
		       (cond ((eof-object? char)
			      (loser chars))
			     ((char=? #\" char)
			      (cons (apply string (reverse! (cons char chars)))
				    (dispatch)))
			     ((char=? #\\ char)
			      (let ((char (input-port/read-char port))
				    (chars (cons char chars)))
				(if (eof-object? char)
				    (loser chars)
				    (loop (cons char chars)))))
			     ((char=? #\newline char)
			      (if (next-lwsp? port)
				  (loop chars)
				  (loser chars)))
			     (else
			      (loop (cons char chars)))))))
		  ((char=? #\( char)
		   ;; comment
		   (let loop ((level 1) (chars (list char)))
		     (let ((char (input-port/read-char port)))
		       (cond ((eof-object? char)
			      (loser chars))
			     ((char=? #\( char)
			      (loop (+ level 1) (cons char chars)))
			     ((char=? #\) char)
			      (let ((chars (cons char chars)))
				(if (= level 1)
				    (cons (apply string (reverse! chars))
					  (dispatch))
				    (loop (- level 1) chars))))
			     ((char=? #\\ char)
			      (let ((char (input-port/read-char port))
				    (chars (cons char chars)))
				(if (eof-object? char)
				    (loser chars)
				    (loop level (cons char chars)))))
			     ((char=? #\newline char)
			      (if (next-lwsp? port)
				  (loop level chars)
				  (loser chars)))
			     (else
			      (loop level (cons char chars)))))))
		  ((char=? #\[ char)
		   ;; domain literal
		   (let loop ((chars (list char)))
		     (let ((char (input-port/peek-char port)))
		       (cond ((or (eof-object? char)
				  (char=? #\[ char))
			      (loser chars))
			     ((char=? #\] char)
			      (input-port/discard-char port)
			      (cons (apply string (reverse! (cons char chars)))
				    (dispatch)))
			     ((char=? #\\ char)
			      (input-port/discard-char port)
			      (let ((char (input-port/read-char port))
				    (chars (cons char chars)))
				(if (eof-object? char)
				    (loser chars)
				    (loop (cons char chars)))))
			     ((char=? #\newline char)
			      (input-port/discard-char port)
			      (if (next-lwsp? char)
				  (loop chars)
				  (loser chars)))
			     (else
			      (input-port/discard-char port)
			      (loop (cons char chars)))))))
		  (else
		   (cons (if (special-char? char) char (cons 'ILLEGAL char))
			 (dispatch))))))

	(define (skip-whitespace)
	  (let ((char (input-port/peek-char port)))
	    (cond ((eof-object? char)
		   '())
		  ((char-lwsp? char)
		   (input-port/discard-char port)
		   (skip-whitespace))
		  ((char=? #\newline char)
		   (input-port/discard-char port)
		   (if (next-lwsp? port)
		       (skip-whitespace)
		       (loser '())))
		  (else
		   (dispatch)))))

	(dispatch)))))

(define (char-lwsp? char)
  (or (char=? #\space char)
      (char=? #\tab char)))
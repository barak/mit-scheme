;;; -*-Scheme-*-
;;;
;;; $Id: rfc822.scm,v 1.7 2000/05/17 20:53:32 cph Exp $
;;;
;;; Copyright (c) 1999-2000 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

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

(define (rfc822:first-address string)
  (let ((addresses (rfc822:string->addresses string)))
    (and (pair? addresses)
	 (car addresses))))

(define (rfc822:addresses->string addresses)
  (if (null? addresses)
      ""
      (decorated-string-append "" ", " "" addresses)))

(define (rfc822:string->addresses string)
  (let ((address-list
	 (rfc822:strip-quoted-names
	  (let loop ((tokens (rfc822:string->tokens string)))
	    (if (pair? tokens)
		(let ((rest (loop (cdr tokens))))
		  (if (cond ((char? (car tokens))
			     (eqv? #\space (car tokens)))
			    ((string? (car tokens))
			     (char=? #\( (string-ref (car tokens) 0)))
			    (else #t))
		      rest
		      (cons (car tokens) rest)))
		'())))))
    (if (and address-list (null? (cdr address-list)))
	(car address-list)
	(map string-trim (burst-string string #\, #f)))))

(define (rfc822:strip-quoted-names tokens)
  (rfc822:parse-list tokens #\,
    (lambda (tokens)
      (or (rfc822:parse-addr-spec tokens)
	  (let ((word (rfc822:parse-word tokens)))
	    (and word
		 (let ((tokens
			(let loop ((tokens (cdr word)))
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
				     (cddr addr-spec))))))))))))

(define (rfc822:strip-comments tokens)
  (list-transform-negative tokens
    (lambda (token)
      (and (string? token)
	   (char=? #\( (string-ref token 0))))))

(define (rfc822:received-header-components string)
  (let ((from #f)
	(by #f)
	(via #f)
	(with '())
	(id #f)
	(for #f)
	(lose (lambda () (error "Malformed Received header:" string))))
    (let loop ((tokens (rfc822:string->tokens string)))
      (cond ((not (pair? tokens))
	     (lose))
	    ((eqv? #\: (car tokens))
	     (values from by via (reverse! with) id for
		     (string->universal-time (rfc822:tokens->string tokens))))
	    ((not (string? (car tokens)))
	     (lose))
	    ((string-ci=? "from" (car tokens))
	     (let ((tokens (rfc822:parse-domain (cdr tokens))))
	       (if (not tokens)
		   (lose))
	       (set! from (car tokens))
	       (loop (cdr tokens))))
	    ((string-ci=? "by" (car tokens))
	     (let ((tokens (rfc822:parse-domain (cdr tokens))))
	       (if (not tokens)
		   (lose))
	       (set! from (car tokens))
	       (loop (cdr tokens))))
	    ((string-ci=? "via" (car tokens))
	     (if (not (pair? (cdr tokens)))
		 (lose))
	     (set! from (cadr tokens))
	     (loop (cddr tokens)))
	    ((string-ci=? "with" (car tokens))
	     (if (not (pair? (cdr tokens)))
		 (lose))
	     (set! with (cons (cadr tokens) with))
	     (loop (cddr tokens)))
	    ((string-ci=? "id" (car tokens))
	     (let ((tokens (rfc822:parse-msg-id (cdr tokens))))
	       (if (not tokens)
		   (lose))
	       (set! id (car tokens))
	       (loop (cdr tokens))))
	    ((string-ci=? "for" (car tokens))
	     (let ((tokens (rfc822:parse-addr-spec (cdr tokens))))
	       (if (not tokens)
		   (lose))
	       (set! for (car tokens))
	       (loop (cdr tokens))))
	    (else (lose))))))

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

(define (rfc822:tokens->string tokens)
  (let ((port (make-accumulator-output-port)))
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
	     (error "Malformed RFC-822 token stream:" tokens))))
    (get-output-from-accumulator port)))

;;;; Parser

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
      (let ((port (string->input-port input-string)))
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
;;; -*-Scheme-*-
;;;
;;; $Id: rfc822.scm,v 1.1 2000/01/04 22:51:45 cph Exp $
;;;
;;; Copyright (c) 1999 Massachusetts Institute of Technology
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

(define (rfc822-first-address field)
  (let ((addresses (rfc822-strip-quoted-names field)))
    (and (pair? addresses)
	 (car addresses))))

(define (rfc822-addresses->string addresses)
  (if (null? addresses)
      ""
      (separated-append addresses ", ")))

;;;; Address extractor

(define (rfc822-strip-quoted-names string)
  (let ((address-list
	 (rfc822-strip-quoted-names-1 (string->rfc822-tokens string))))
    (if (and address-list (null? (cdr address-list)))
	(car address-list)
	(map string-trim (burst-string string #\, #f)))))

(define (rfc822-strip-quoted-names-1 tokens)
  (define (parse-addr-spec tokens)
    (let ((local-part (parse-list tokens parse-word #\.)))
      (and local-part
	   (not (null? (cdr local-part)))
	   (eqv? #\@ (cadr local-part))
	   (let ((domain (parse-domain (cddr local-part))))
	     (and domain
		  (cons (string-append (separated-append (car local-part) ".")
				       "@"
				       (separated-append (car domain) "."))
			(cdr domain)))))))
  (define (parse-domain tokens)
    (parse-list tokens
		(lambda (tokens)
		  (and (not (null? tokens))
		       (string? (car tokens))
		       (not (eqv? #\" (string-ref (car tokens) 0)))
		       tokens))
		#\.))
  (define (parse-list tokens parse-element separator)
    (let ((first (parse-element tokens)))
      (and first
	   (let loop ((tokens (cdr first)) (words (list (car first))))
	     (let ((next
		    (and (not (null? tokens))
			 (eqv? separator (car tokens))
			 (parse-element (cdr tokens)))))
	       (if next
		   (loop (cdr next) (cons (car next) words))
		   (cons (reverse! words) tokens)))))))
  (define (parse-word tokens)
    (and (not (null? tokens))
	 (string? (car tokens))
	 (not (eqv? #\[ (string-ref (car tokens) 0)))
	 tokens))
  (parse-list
   tokens
   (lambda (tokens)
     (or (parse-addr-spec tokens)
	 (let ((word (parse-word tokens)))
	   (and word
		(let ((tokens
		       (let loop ((tokens (cdr word)))
			 (let ((word (parse-word tokens)))
			   (if word
			       (loop (cdr word))
			       tokens)))))
		  (and (not (null? tokens))
		       (eqv? #\< (car tokens))
		       (let ((addr-spec
			      (parse-addr-spec
			       (let ((domains
				      (parse-list
				       (cdr tokens)
				       (lambda (tokens)
					 (and (not (null? tokens))
					      (eqv? #\@ (car tokens))
					      (parse-domain (cdr tokens))))
				       #\,)))
				 (if (and domains
					  (not (null? (cdr domains)))
					  (eqv? #\: (cadr domains)))
				     (cddr domains)
				     (cdr tokens))))))
			 (and addr-spec
			      (not (null? (cdr addr-spec)))
			      (eqv? #\> (cadr addr-spec))
			      (cons (car addr-spec) (cddr addr-spec))))))))))
   #\,))

;;;; Parser

(define (string->rfc822-tokens string)
  (rfc822-clean-tokens (rfc822-read-tokens (string->input-port string))))

(define (rfc822-clean-tokens tokens)
  (let loop ((tokens tokens))
    (if (null? tokens)
	'()
	(let ((rest (loop (cdr tokens))))
	  (if (cond ((char? (car tokens))
		     (eqv? #\space (car tokens)))
		    ((string? (car tokens))
		     (char=? #\( (string-ref (car tokens) 0)))
		    (else true))
	      rest
	      (cons (car tokens) rest))))))

(define rfc822-read-tokens
  (let* ((special-chars
	  (char-set #\( #\) #\[ #\] #\< #\> #\@ #\, #\; #\: #\\ #\" #\.))
	 (atom-chars
	  (char-set-difference (ascii-range->char-set #x21 #x7F)
			       special-chars)))
    (lambda (port)
      (let ((special-char?
	     (lambda (char) (char-set-member? special-chars char)))
	    (atom-char? (lambda (char) (char-set-member? atom-chars char)))
	    (lwsp?
	     (lambda (char) (or (char=? #\space char) (char=? #\tab char))))
	    (loser
	     (lambda (chars)
	       (list (cons 'UNTERMINATED (apply string (reverse! chars)))))))
	(let dispatch ()
	  (let ((char (input-port/read-char port)))
	    (cond ((eof-object? char)
		   '())
		  ((lwsp? char)
		   (do ()
		       ((not (lwsp? (input-port/peek-char port))))
		     (input-port/discard-char port))
		   (cons #\space (dispatch)))
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
			      (let ((char (input-port/peek-char port)))
				(if (lwsp? char)
				    (begin
				      (input-port/discard-char port)
				      (loop (cons char chars)))
				    (loser chars))))
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
			      (let ((char (input-port/peek-char port)))
				(if (lwsp? char)
				    (begin
				      (input-port/discard-char port)
				      (loop level (cons char chars)))
				    (loser chars))))
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
			      (let ((char (input-port/peek-char port)))
				(if (lwsp? char)
				    (begin
				      (input-port/discard-char port)
				      (loop (cons char chars)))
				    (loser chars))))
			     (else
			      (input-port/discard-char port)
			      (loop (cons char chars)))))))
		  ((char=? #\newline char)
		   (let ((char (input-port/peek-char port)))
		     (if (and (not (eof-object? char))
			      (lwsp? char))
			 (dispatch)
			 '())))
		  (else
		   (cons (if (special-char? char)
			     char
			     (cons 'ILLEGAL char))
			 (dispatch))))))))))
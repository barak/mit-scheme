;;; -*-Scheme-*-
;;;
;;; $Id: imap-response.scm,v 1.2 2000/04/22 05:06:56 cph Exp $
;;;
;;; Copyright (c) 2000 Massachusetts Institute of Technology
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

;;;; IMAP Server Response Reader

(declare (usual-integrations))

(define (imap:read-server-response port)
  (let ((tag (read-string char-set:space port)))
    (if (eof-object? tag)
	tag
	(begin
	  (discard-known-char #\space port)
	  (let ((response
		 (cond ((string=? "+" tag)
			(cons 'CONTINUE (read-response-text port)))
		       ((string=? "*" tag)
			(read-untagged-response port))
		       ((let ((end (string-length tag)))
			  (let ((index (imap:match:tag tag 0 end)))
			    (and index
				 (fix:= index end))))
			(read-tagged-response tag port))
		       (else
			(error "Malformed server response:" tag)))))
	    (discard-known-char #\newline port)
	    response)))))

(define (read-untagged-response port)
  (let ((x (read-atom port)))
    (if (atom-is-number? x)
	(let ((n (string->number x)))
	  (discard-known-char #\space port)
	  (let ((x (read-interned-atom port)))
	    (cons x
		  (case x
		    ((EXISTS RECENT EXPUNGE) (list n))
		    ((FETCH) (read-fetch-response port))
		    (else (error "Malformed response code:" x))))))
	(let ((x (intern x)))
	  (cons x
		(case x
		  ((OK NO BAD) (cons #f (read-response-text port)))
		  ((PREAUTH BYE) (read-response-text port))
		  ((FLAGS) (read-flags-response port))
		  ((MAILBOX) (read-mailbox-response port))
		  ((LIST LSUB) (read-list-response port))
		  ((SEARCH) (read-search-response port))
		  ((STATUS) (read-status-response port))
		  ((CAPABILITY) (read-capability-response port))
		  (else (error "Malformed response code:" x))))))))

(define (read-tagged-response tag port)
  (let ((x (read-interned-atom port)))
    (if (memq x '(OK NO BAD))
	(cons* x tag (read-response-text port))
	(error "Malformed response code:" x))))

(define (read-flags-response port)
  (discard-known-char #\space port)
  (read-list port read-flag))

(define (read-mailbox-response port)
  (discard-known-char #\space port)
  (list (read-text port)))

(define (read-list-response port)
  (let ((flags (read-flags-response port)))
    (discard-known-char #\space port)
    (let ((delim (read-nstring port)))
      (discard-known-char #\space port)
      (list flags delim (read-astring port)))))

(define (read-search-response port)
  (read-open-list read-nz-number port))

(define (read-status-response port)
  (discard-known-char #\space port)
  (let ((mailbox (read-astring port)))
    (discard-known-char #\space port)
    (list mailbox
	  (read-list port
		     (lambda (port)
		       (let ((name (read-atom port)))
			 (discard-known-char #\space port)
			 (cons name (read-number port))))))))

(define (read-capability-response port)
  (read-open-list read-interned-atom port))

(define (read-response-text port)
  (discard-known-char #\space port)
  (let ((code
	 (and (char=? #\[ (peek-char-no-eof port))
	      (read-response-text-code port))))
    (cons code
	  (if (char=? #\= (peek-char port))
	      (read-mime2-text port)
	      (list (read-text port))))))

(define (read-response-text-code port)
  (discard-known-char #\[ port)
  (let ((code
	 (let ((x (intern (read-resp-text-atom port))))
	   (case x
	     ((ALERT PARSE READ-ONLY READ-WRITE TRYCREATE)
	      x)
	     ((UIDVALIDITY UNSEEN)
	      (discard-known-char #\space port)
	      (list x (read-nz-number port)))
	     ((PERMANENTFLAGS)
	      (discard-known-char #\space port)
	      (cons x (read-list port read-pflag)))
	     (else
	      (if (char=? #\space (peek-char-no-eof port))
		  (begin
		    (read-char port)
		    (list x (read-resp-text-tail port)))
		  x))))))
    (discard-known-char #\] port)
    (discard-known-char #\space port)
    code))

(define (read-fetch-response port)
  (discard-known-char #\space port)
  (read-list port
    (lambda (port)
      (let ((x (intern (read-fetch-keyword port))))
	(cons x
	      (case x
		((ENVELOPE)
		 (discard-known-char #\space port)
		 (read-generic port))
		((FLAGS)
		 (read-flags-response port))
		((INTERNALDATE)
		 (discard-known-char #\space port)
		 (list (read-quoted port)))
		((RFC822 RFC822.HEADER RFC822.TEXT)
		 (discard-known-char #\space port)
		 (list (read-nstring port)))
		((RFC822.SIZE)
		 (discard-known-char #\space port)
		 (list (read-number port)))
		((BODY)
		 (if (char=? #\[ (peek-char-no-eof port))
		     (let ((section
			    (parse-section (read-bracketed-string port))))
		       (discard-known-char #\space port)
		       (let ((n
			      (and (char-numeric? (peek-char-no-eof port))
				   (let ((n (read-number port)))
				     (discard-known-char #\space port)
				     n))))
			 (list section n (read-nstring port))))
		     (begin
		       (discard-known-char #\space port)
		       (list (read-generic port)))))
		((BODYSTRUCTURE)
		 (discard-known-char #\space port)
		 (list (read-generic port)))
		((UID)
		 (discard-known-char #\space port)
		 (list (read-nz-number port)))
		(else
		 (error "Illegal fetch keyword:" x))))))))

(define (parse-section string)
  (let ((pv (parse-string imap:parse:section string)))
    (and pv
	 (parser-token pv 'SECTION))))

(define (read-generic port)
  (let ((char (peek-char-no-eof port)))
    (cond ((char=? #\" char) (read-quoted port))
	  ((char=? #\{ char) (read-literal port))
	  ((char=? #\( char) (read-list port))
	  ((imap:atom-char? char)
	   (let ((atom (read-atom port)))
	     (if (atom-is-number? atom)
		 (string->number atom)
		 (intern atom))))
	  (else (error "Illegal IMAP syntax:" char)))))

(define (read-astring port)
  (let ((char (peek-char-no-eof port)))
    (cond ((char=? #\" char) (read-quoted port))
	  ((char=? #\{ char) (read-literal port))
	  ((imap:atom-char? char) (read-atom port))
	  (else (error "Illegal astring syntax:" char)))))

(define (read-nstring port)
  (let ((v (read-astring port)))
    (if (and (symbol? v) (not (eq? v 'NIL)))
	(error "Illegal nstring:" v)
	v)))

(define (read-quoted port)
  (discard-known-char #\" port)
  (let ((port* (make-accumulator-output-port))
	(lose (lambda () (error "Malformed quoted string."))))
    (let loop ()
      (let ((char (read-char-no-eof port)))
	(cond ((imap:quoted-char? char)
	       (write-char char port*)
	       (loop))
	      ((char=? #\" char)
	       (get-output-from-accumulator port*))
	      ((char=? #\\ char)
	       (let ((char (read-char-no-eof char)))
		 (if (imap:quoted-special? char)
		     (begin
		       (write-char char port*)
		       (loop))
		     (lose))))
	      (else (lose)))))))

(define (read-literal port)
  (discard-known-char #\{ port)
  (let ((n (read-number port)))
    (discard-known-char #\} port)
    (discard-known-char #\newline port)
    (let ((s (make-string n)))
      (let loop ((i 0) (j 0))
	(cond ((fix:< i n)
	       (let ((char (read-char-no-eof port)))
		 (string-set! s j char)
		 (loop (fix:+ i (if (char=? char #\newline) 2 1))
		       (fix:+ j 1))))
	      ((fix:< j n)
	       (set-string-length! s j))))
      s)))

(define (read-list port #!optional read-item)
  (read-closed-list #\( #\)
		    (if (default-object? read-item) read-generic read-item)
		    port))

(define (read-closed-list open close read-item port)
  (discard-known-char open port)
  (if (char=? close (peek-char-no-eof port))
      (begin
	(read-char port)
	'())
      (let loop ((items (list (read-item port))))
	(let ((char (peek-char-no-eof port)))
	  (cond ((char=? char #\space)
		 (read-char port)
		 (loop (cons (read-item port) items)))
		((char=? char #\()
		 (loop (cons (read-item port) items)))
		((char=? char close)
		 (read-char port)
		 (reverse! items))
		(else
		 (error "Illegal list delimiter:" char)))))))

(define (read-open-list read-item port)
  (let loop ((items '()))
    (let ((char (peek-char-no-eof port)))
      (cond ((char=? char #\space)
	     (read-char port)
	     (loop (cons (read-item port) items)))
	    ((char=? char #\newline)
	     (reverse! items))
	    (else
	     (error "Illegal list delimiter:" char))))))

(define (read-bracketed-string port)
  (discard-known-char #\[ port)
  (let ((s (read-string char-set:close-bracket port)))
    (discard-known-char #\] port)
    s))

(define (read-pflag port)
  (discard-known-char #\\ port)
  (intern
   (if (char=? #\* (peek-char-no-eof port))
       (begin
	 (read-char port)
	 "\\*")
       (string-append "\\" (read-atom port)))))

(define (read-flag port)
  (intern
   (if (char=? #\\ (peek-char-no-eof port))
       (begin
	 (read-char port)
	 (string-append "\\" (read-atom port)))
       (read-atom port))))

(define (string-reader constituents)
  (let ((delimiters (char-set-invert constituents)))
    (lambda (port)
      (read-string delimiters port))))

(define (non-null-string-reader constituents)
  (let ((reader (string-reader constituents)))
    (lambda (port)
      (let ((s (reader port)))
	(if (string-null? s)
	    (error "Empty string.")
	    s)))))

(define read-number
  (let ((reader (non-null-string-reader char-set:numeric)))
    (lambda (port)
      (string->number (reader port)))))

(define (read-nz-number port)
  (let ((n (read-number port)))
    (if (> n 0)
	n
	(error "Zero not allowed here."))))

(define read-tag
  (non-null-string-reader imap:char-set:tag-char))

(define read-atom
  (non-null-string-reader imap:char-set:atom-char))

(define read-resp-text-atom
  (non-null-string-reader
   (char-set-difference imap:char-set:atom-char (char-set #\]))))

(define read-text
  ;; This is supposed to be non-null, but Cyrus sometimes sends null.
  (string-reader imap:char-set:text-char))

(define read-resp-text-tail
  ;; This is also supposed to be non-null.
  (string-reader
   (char-set-difference imap:char-set:text-char (char-set #\]))))

(define read-fetch-keyword
  (non-null-string-reader
   (char-set-union char-set:alphanumeric (char-set #\.))))

(define (read-interned-atom port)
  (intern (read-atom port)))

(define (read-mime2-text port)
  (discard-known-char #\= port)
  (discard-known-char #\? port)
  (let ((charset (read-mime2-token port)))
    (discard-known-char #\? port)
    (let ((encoding (read-mime2-token port)))
      (discard-known-char #\? port)
      (let ((encoded-text (read-mime2-encoded-text port)))
	(discard-known-char #\? port)
	(discard-known-char #\= port)
	(list charset encoding encoded-text)))))

(define read-mime2-token
  (non-null-string-reader
   (char-set-difference char-set:graphic
			(string->char-set " ()<>@,;:\"/[]?.="))))

(define read-mime2-encoded-text
  (non-null-string-reader
   (char-set-difference char-set:graphic
			(string->char-set " ?"))))

(define atom-is-number?
  (let ((char-set:not-numeric (char-set-invert char-set:numeric)))
    (lambda (atom)
      (not (string-find-next-char-in-set atom char-set:not-numeric)))))

(define char-set:space
  (char-set #\space))

(define char-set:close-bracket
  (char-set #\]))

(define (read-char-no-eof port)
  (let ((char (read-char port)))
    (if (eof-object? char)
	(error "Unexpected end of file:" port))
    char))

(define (peek-char-no-eof port)
  (let ((char (peek-char port)))
    (if (eof-object? char)
	(error "Unexpected end of file:" port))
    char))

(define (discard-known-char char port)
  (let ((char* (read-char-no-eof port)))
    (if (not (char=? char char*))
	(error "Missing newline in literal:" char*))))
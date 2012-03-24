#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

;;;; RFC 2822 headers
;;; package: (runtime rfc2822-headers)

(declare (usual-integrations))

(define (make-rfc2822-header name value)
  (guarantee-header-name name 'make-rfc2822-header)
  (guarantee-header-value value 'make-rfc2822-header)
  (make-header name value))

(define-record-type <rfc2822-header>
    (make-header name value)
    rfc2822-header?
  (name rfc2822-header-name)
  (value rfc2822-header-value))

(define-guarantee rfc2822-header "RFC 2822 header field")

(set-record-type-unparser-method! <rfc2822-header>
  (simple-unparser-method 'rfc2822-header
    (lambda (header)
      (list (rfc2822-header-name header)))))

(define (header-name? object)
  (and (interned-symbol? object)
       (not (eq? object '||))
       (string-in-char-set? (symbol-name object) char-set:rfc2822-name)))

(define-guarantee header-name "RFC 2822 header-field name")

(define (header-value? object)
  (and (string? object)
       (string-in-char-set? object char-set:rfc2822-text)))

(define-guarantee header-value "RFC 2822 header-field value")

(define (string-in-char-set? string char-set)
  (let ((end (string-length string)))
    (let loop ((i 0))
      (if (fix:< i end)
	  (and (char-set-member? char-set (string-ref string i))
	       (loop (fix:+ i 1)))
	  #t))))

(define (guarantee-rfc2822-headers object #!optional caller)
  (guarantee-list-of-type object
			  rfc2822-header?
			  "list of RFC 2822 header fields"
			  caller))

(define (first-rfc2822-header name headers)
  (guarantee-rfc2822-headers headers 'FIRST-RFC2822-HEADER)
  (find (lambda (header)
	  (eq? (rfc2822-header-name header) name))
	headers))

(define (all-rfc2822-headers name headers)
  (guarantee-rfc2822-headers headers 'ALL-RFC2822-HEADERS)
  (filter (lambda (header)
	    (eq? (rfc2822-header-name header) name))
	  headers))

;;;;; Output

(define (rfc2822-headers->string headers)
  (call-with-output-string
    (lambda (port)
      (write-rfc2822-headers headers port))))

(define (write-rfc2822-headers headers port)
  (guarantee-rfc2822-headers headers 'WRITE-RFC2822-HEADERS)
  (for-each (lambda (header)
              (write-header header port))
            headers)
  (newline port))

(define (write-header header port)
  (write-name (rfc2822-header-name header) port)
  (write-string ": " port)
  ;; Needs to handle line folding someday, but that requires
  ;; understanding details of the header structure.
  (write-string (rfc2822-header-value header) port)
  (newline port))

(define (write-name name port)
  (let* ((name (symbol-name name))
         (end (string-length name)))
    (if (char-alphabetic? (string-ref name 0))
	(letrec
	    ((start-word
	      (lambda (i)
		(if (fix:< i end)
		    (begin
		      (write-char (char-upcase (string-ref name i)) port)
		      (finish-word (fix:+ i 1))))))
	     (finish-word
	      (lambda (i)
		(if (fix:< i end)
		    (let ((char (string-ref name i))
			  (i (fix:+ i 1)))
		      (write-char char port)
		      (if (char=? char #\-)
			  (start-word i)
			  (finish-word i)))))))
	  (start-word 0))
        (write-string name port))))

;;;;; Input

(define (string->rfc2822-headers string)
  (call-with-input-string string read-rfc2822-headers))

(define (read-rfc2822-headers port)
  (let loop ((headers '()))
    (let ((header (read-rfc2822-header port)))
      (if header
	  (loop (cons header headers))
	  (reverse! headers)))))

(define (read-rfc2822-header port)
  (let ((line (read-rfc2822-folded-line port)))
    (and line
	 (let ((colon (string-find-next-char line #\:)))
	   (if (not colon)
	       (parse-error port "Missing colon in header field:" line))
	   (make-rfc2822-header (intern (string-head line colon))
				(let ((end (string-length line)))
				  (substring line
					     (skip-wsp-left line
							    (fix:+ colon 1)
							    end)
					     end)))))))

(define (read-rfc2822-folded-line port)
  (let ((line (read-line port)))
    (cond ((string-null? line)
	   #f)
	  ((char-wsp? (string-ref line 0))
	   (parse-error port
			"Unmatched continuation line:"
			'READ-RFC2822-FOLDED-LINE))
	  (else
	   (call-with-output-string
	     (lambda (out)
	       (let loop ((line line))
		 (let ((end (skip-wsp-right line 0 (string-length line))))
		   (write-substring line
				    (skip-wsp-left line 0 end)
				    end
				    out))
		 (if (let ((char (peek-char port)))
		       (if (eof-object? char)
			   (parse-error port
					"Premature EOF:"
					'READ-RFC2822-FOLDED-LINE))
		       (char-wsp? char))
		     (begin
		       (write-char #\space out)
		       (loop (read-line port)))))))))))

(define (skip-wsp-left string start end)
  (let loop ((i start))
    (if (and (fix:< i end)
	     (char-wsp? (string-ref string i)))
	(loop (fix:+ i 1))
	i)))

(define (skip-wsp-right string start end)
  (let loop ((i end))
    (if (and (fix:> i start)
	     (char-wsp? (string-ref string (fix:- i 1))))
	(loop (fix:- i 1))
	i)))

;;;; Quotation

(define (quote-rfc2822-text string #!optional start end)
  (let ((input (open-input-string string start end))
	(output (open-output-string)))
    (let loop ((quote? #f))
      (let ((char (read-char input)))
	(cond ((eof-object? char)
	       (let ((s (get-output-string output)))
		 (if quote?
		     (string-append "\"" s "\"")
		     s)))
	      ((char-set-member? char-set:rfc2822-qtext char)
	       (write-char char output)
	       (loop quote?))
	      ((char-set-member? char-set:rfc2822-text char)
	       (write-char #\\ output)
	       (write-char char output)
	       (loop #t))
	      (else
	       (error:bad-range-argument string 'quote-rfc2822-string)))))))

(define parser:rfc2822-quoted-string
  (*parser
   (seq "\""
	(map (lambda (string)
	       (call-with-output-string
		 (lambda (output)
		   (let ((input (open-input-string string)))
		     (let loop ()
		       (let ((char (read-char input)))
			 (if (not (eof-object? char))
			     (begin
			       (write-char (if (char=? char #\\)
					       (read-char input)
					       char)
					   output)
			       (loop)))))))))
	     (match (* (alt (char-set char-set:rfc2822-qtext)
			    (seq "\\" (char-set char-set:rfc2822-text))))))
	"\"")))

;;;; Initialization

(define char-set:rfc2822-name)
(define char-set:rfc2822-text)
(define char-set:rfc2822-qtext)

(define condition-type:rfc2822-parse-error)
(define parse-error)

(define (initialize-package!)
  (set! char-set:rfc2822-name
	(char-set-difference char-set:ascii
			     (char-set-union char-set:ctls
					     (char-set #\space #\:)
					     char-set:upper-case)))
  (set! char-set:rfc2822-text
	(char-set-difference char-set:ascii
			     (char-set #\null #\linefeed #\return)))
  (set! char-set:rfc2822-qtext
	(char-set-difference char-set:rfc2822-text
			     (char-set #\tab #\space #\delete #\\ #\")))
  (set! condition-type:rfc2822-parse-error
	(make-condition-type 'RFC2822-PARSE-ERROR
	    condition-type:port-error
	    '(MESSAGE IRRITANTS)
	  (lambda (condition port)
	    (write-string "Error while parsing RFC 2822 headers: " port)
	    (format-error-message (access-condition condition 'MESSAGE)
				  (access-condition condition 'IRRITANTS)
				  port))))
  (set! parse-error
	(let ((signal
	       (condition-signaller condition-type:rfc2822-parse-error
				    '(PORT MESSAGE IRRITANTS)
				    standard-error-handler)))
	  (lambda (port message . irritants)
	    (signal port message irritants))))
  unspecific)
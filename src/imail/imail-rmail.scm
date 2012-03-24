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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; IMAIL mail reader: RMAIL back end

(declare (usual-integrations))

(define-class <rmail-folder-type> (<file-folder-type>))

(define-file-folder-type <rmail-folder-type> "Rmail"
  (lambda (pathname)
    (check-file-prefix pathname "BABYL OPTIONS:")))

;;;; Server

(define-method create-file-folder-file (url (type <rmail-folder-type>))
  type
  (call-with-binary-output-file (pathname-url-pathname url)
    (lambda (port)
      (write-rmail-file-header (make-rmail-folder-header-fields '()) port))))

;;;; Folder

(define-class (<rmail-folder> (constructor (locator))) (<file-folder>)
  (header-fields define standard))

(define-method rmail-folder-header-fields ((folder <folder>))
  (compute-rmail-folder-header-fields folder))

(define (compute-rmail-folder-header-fields folder)
  (make-rmail-folder-header-fields (folder-flags folder)))

(define (make-rmail-folder-header-fields flags)
  (list (make-header-field "Version" "5")
	(make-header-field "Labels"
			   (decorated-string-append
			    "" "," ""
			    (flags->rmail-labels flags)))
	(make-header-field "Note" "This is the header of an rmail file.")
	(make-header-field "Note" "If you are seeing it in rmail,")
	(make-header-field "Note" "it means the file has no messages in it.")))

(define-method %open-file-resource (url (type <rmail-folder-type>))
  type
  (maybe-make-resource url make-rmail-folder))

;;;; Message

(define-class (<rmail-message>
	       (constructor (header-fields body flags
					   displayed-header-fields
					   internal-time)))
    (<file-message>)
  displayed-header-fields
  internal-time)

(define-generic rmail-message-displayed-header-fields (message))

(define-file-external-message-method rmail-message-displayed-header-fields
  <rmail-message>
  'DISPLAYED-HEADER-FIELDS
  string->header-fields)

(define-method rmail-message-displayed-header-fields ((message <message>))
  message
  'UNDEFINED)

(let ((accessor (slot-accessor <rmail-message> 'INTERNAL-TIME)))
  (define-method message-internal-time ((message <rmail-message>))
    (or (accessor message)
	(call-next-method message))))

(define-method make-message-copy ((message <message>) (folder <rmail-folder>))
  folder
  (make-rmail-message (message-header-fields message)
		      (file-message-body message)
		      (list-copy (message-permanent-flags message))
		      (rmail-message-displayed-header-fields message)
		      (message-internal-time message)))

;;;; Read RMAIL file

(define-method revert-file-folder ((folder <rmail-folder>))
  (read-file-folder-contents folder
    (lambda (port)
      (set-rmail-folder-header-fields! folder (read-rmail-prolog port))
      (let loop ((line #f) (index 0) (messages '()))
	(if (= 0 (remainder index 100))
	    (imail-ui:progress-meter index #f))
	(call-with-values (lambda () (read-rmail-message folder port line))
	  (lambda (message line)
	    (if message
		(begin
		  (attach-message! message folder index)
		  (loop line (+ index 1) (cons message messages)))
		(list->vector (reverse! messages)))))))))

(define (read-rmail-prolog port)
  (if (not (rmail-prolog-start-line? (read-required-line port)))
      (error "Not an RMAIL file:" port))
  (lines->header-fields (read-lines-to-eom port)))

(define (read-rmail-message folder port read-ahead-line)
  (let ((line (or read-ahead-line (read-line port))))
    (cond ((eof-object? line)
	   (values #f #f))
	  ((rmail-prolog-start-line? line)
	   (discard-to-eom port)
	   (read-rmail-message folder port #f))
	  ((rmail-message-start-line? line)
	   (values (read-rmail-message-1 folder port) #f))
	  ((umail-delimiter? line)
	   (read-umail-message folder line port
	     (lambda (line)
	       (or (rmail-prolog-start-line? line)
		   (rmail-message-start-line? line)
		   (umail-delimiter? line)))))
	  (else
	   (error "Malformed RMAIL file:" port)))))

(define (read-rmail-message-1 folder port)
  (receive (formatted? flags) (read-rmail-attributes-line port)
    (let* ((headers (read-rmail-alternate-headers port))
	   (displayed-headers (read-rmail-displayed-headers port))
	   (body (read-rmail-body port))
	   (finish
	    (lambda (headers displayed-headers)
	      (make-rmail-message headers
				  body
				  flags
				  displayed-headers
				  (rmail-internal-time folder headers)))))
      (if formatted?
	  (finish headers displayed-headers)
	  (finish displayed-headers 'UNDEFINED)))))

(define (read-rmail-attributes-line port)
  (let ((line (read-required-line port)))
    (let ((n (string-length line))
	  (lose
	   (lambda ()
	     (error "Malformed RMAIL message-attributes line:" line))))
      (if (not (and (fix:>= n 3)
		    (char=? (string-ref line 1) #\,)))
	  (lose))
      (values (cond ((char=? (string-ref line 0) #\0) #f)
		    ((char=? (string-ref line 0) #\1) #t)
		    (else (lose)))
	      (let loop ((i 2) (flags '()) (unseen? #f))
		(if (fix:< i n)
		    (if (or (char=? (string-ref line i) #\space)
			    (char=? (string-ref line i) #\,))
			(loop (fix:+ i 1) flags unseen?)
			(let scan-token ((i* (fix:+ i 1)))
			  (if (or (fix:= i* n)
				  (char=? (string-ref line i*) #\space)
				  (char=? (string-ref line i*) #\,))
			      (let ((flag (substring line i i*)))
				(if (string-ci=? flag "unseen")
				    (loop i* flags #t)
				    (loop i* (cons flag flags) unseen?)))
			      (scan-token (fix:+ i* 1)))))
		    (if unseen?
			(reverse! flags)
			(cons "seen" (reverse! flags)))))))))

(define (read-rmail-alternate-headers port)
  (let ((start (xstring-port/position port)))
    (make-file-external-ref
     start
     (let* ((separator rmail-message:headers-separator)
	    (s0 (string-ref separator 0))
	    (sl (string-length separator)))
       (let loop ()
	 (let ((char (read-required-char port)))
	   (cond ((char=? char #\newline)
		  (let ((end (- (xstring-port/position port) 1)))
		    (if (not (string=? separator (read-required-line port)))
			(error "Missing RMAIL headers-separator string:" port))
		    end))
		 ((char=? char s0)
		  (let ((line (read-required-line port)))
		    (if (substring=? line 0 (string-length line)
				     separator 1 sl)
			(- (xstring-port/position port)
			   (+ (string-length line) 1))
			(loop))))
		 (else
		  (skip-to-line-start port)
		  (loop)))))))))

(define (read-rmail-displayed-headers port)
  (let ((start (xstring-port/position port)))
    (skip-past-blank-line port)
    (make-file-external-ref start (- (xstring-port/position port) 1))))

(define (read-rmail-body port)
  (let ((start (xstring-port/position port)))
    (input-port/discard-chars port rmail-message:end-char-set)
    (input-port/discard-char port)
    (make-file-external-ref start (- (xstring-port/position port) 1))))

(define (rmail-internal-time folder ref)
  (let ((v
	 (find (internal-header-field-predicate "INTERNAL-TIME")
	       (file-folder-internal-headers folder ref))))
    (and v
	 (parse-header-field-date v))))

;;;; Write RMAIL file

(define-method write-file-folder ((folder <rmail-folder>) pathname)
  (call-with-binary-output-file pathname
    (lambda (port)
      (write-rmail-file-header (rmail-folder-header-fields folder) port)
      (for-each-vector-element (file-folder-messages folder)
	(lambda (message)
	  (write-rmail-message message port)))
      (output-port/synchronize-output port))))

(define-method append-message-to-file (message url (type <rmail-folder-type>))
  type
  (call-with-binary-append-file (pathname-url-pathname url)
    (lambda (port)
      (write-rmail-message message port))))

(define (write-rmail-file-header header-fields port)
  (write-string "BABYL OPTIONS: -*- rmail -*-" port)
  (newline port)
  (write-header-fields header-fields port)
  (write-char rmail-message:end-char port))

(define (write-rmail-message message port)
  (write-char rmail-message:start-char port)
  (newline port)
  (let ((headers
	 (let ((headers (message-header-fields message))
	       (time (message-internal-time message)))
	   (if time
	       (append headers
		       (list
			(make-internal-header-field
			 "INTERNAL-TIME"
			 (universal-time->string time))))
	       headers)))
	(displayed-headers (rmail-message-displayed-header-fields message)))
    (let ((formatted? (not (eq? 'UNDEFINED displayed-headers))))
      (write-rmail-attributes-line message formatted? port)
      (if formatted? (write-header-fields headers port))
      (write-string rmail-message:headers-separator port)
      (newline port)
      (write-header-fields (if formatted? displayed-headers headers) port)
      (write-message-body message port)
      (fresh-line port)
      (write-char rmail-message:end-char port))))

(define (write-rmail-attributes-line message formatted? port)
  (write-char (if formatted? #\1 #\0) port)
  (write-char #\, port)
  (receive (attributes labels)
      (flags->rmail-markers (message-permanent-flags message))
    (let ((write-markers
	   (lambda (markers)
	     (for-each (lambda (marker)
			 (write-char #\space port)
			 (write-string marker port)
			 (write-char #\, port))
		       markers))))
      (write-markers attributes)
      (write-char #\, port)
      (write-markers labels)))
  (newline port))

;;;; Attributes and labels

(define (rmail-markers->flags attributes labels)
  (let loop ((strings (append attributes labels)) (flags '()))
    (if (pair? strings)
	(loop (cdr strings) (cons (car strings) flags))
	(reverse!
	 (if (flags-member? "unseen" flags)
	     (flags-delete! "unseen" flags)
	     (cons "seen" flags))))))

(define (flags->rmail-markers flags)
  (let loop
      ((flags
	(if (flags-member? "seen" flags)
	    (flags-delete "seen" flags)
	    (cons "unseen" flags)))
       (attributes '())
       (labels '()))
    (if (pair? flags)
	(if (member (car flags) rmail-attributes)
	    (loop (cdr flags) (cons (car flags) attributes) labels)
	    (loop (cdr flags) attributes (cons (car flags) labels)))
	(values (reverse! attributes) (reverse! labels)))))

(define (flags->rmail-labels flags)
  (call-with-values (lambda () (flags->rmail-markers flags))
    (lambda (attributes labels)
      attributes
      labels)))

;;;; Syntactic Markers

(define (rmail-prolog-start-line? line)
  (string-prefix? "BABYL OPTIONS:" line))

(define (rmail-prolog-end-line? line)
  (string-prefix? "\037" line))

(define (rmail-message-start-line? line)
  (string=? "\f" line))

(define rmail-message:headers-separator
  "*** EOOH ***")

(define rmail-message:start-char
  #\page)

(define rmail-message:end-char
  (integer->char #x1f))

(define rmail-message:end-char-set
  (char-set rmail-message:end-char))

(define rmail-attributes
  '("deleted" "answered" "unseen" "filed" "forwarded" "edited" "resent"))

(define (read-lines-to-eom port)
  (source->list
   (lambda ()
     (if (eqv? rmail-message:end-char (peek-char port))
	 (begin
	   (read-char port)		;discard
	   (eof-object))
	 (read-required-line port)))))

(define (read-to-eom port)
  (let ((string (read-string rmail-message:end-char-set port)))
    (if (or (eof-object? string)
	    (eof-object? (read-char port)))
	(error "EOF while reading RMAIL message body:" port))
    string))

(define (discard-to-eom port)
  (input-port/discard-chars port rmail-message:end-char-set)
  (input-port/discard-char port))
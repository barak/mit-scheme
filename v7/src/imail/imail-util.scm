;;; -*-Scheme-*-
;;;
;;; $Id: imail-util.scm,v 1.19 2000/05/20 03:24:31 cph Exp $
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

;;;; IMAIL mail reader: utilities

(declare (usual-integrations))

(define (guarantee-index index procedure)
  (if (not (index-fixnum? index))
      (error:wrong-type-argument index "index" procedure)))

(define (source->list source)
  (let ((item (source)))
    (if (eof-object? item)
	'()
	(let ((head (list item)))
	  (let loop ((prev head))
	    (let ((item (source)))
	      (if (eof-object? item)
		  head
		  (let ((this (list item)))
		    (set-cdr! prev this)
		    (loop this)))))))))

(define (list->source items)
  (lambda ()
    (if (pair? items)
	(let ((item (car items)))
	  (set! items (cdr items))
	  item)
	(make-eof-object #f))))

(define (cut-list! items predicate)
  (if (or (not (pair? items)) (predicate (car items)))
      (values '() items)
      (let loop ((prev items) (this (cdr items)))
	(if (or (not (pair? this)) (predicate (car this)))
	    (begin
	      (set-cdr! prev '())
	      (values items this))
	    (loop this (cdr this))))))

(define (burst-list items predicate)
  (let loop ((items items) (groups '()))
    (if (pair? items)
	(let find-next ((items (cdr items)) (group (list (car items))))
	  (if (and (pair? items) (not (predicate (car items))))
	      (find-next (cdr items) (cons (car items) group))
	      (loop items (cons (reverse! group) groups))))
	(reverse! groups))))

(define (count-matching-items items predicate)
  (let loop ((items items) (count 0))
    (if (pair? items)
	(loop (cdr items)
	      (if (predicate (car items))
		  (fix:+ count 1)
		  count))
	count)))

(define (remove-duplicates items predicate)
  (let loop ((items items) (items* '()))
    (if (pair? items)
	(loop (cdr items)
	      (if (let loop ((items* (cdr items)))
		    (and (pair? items*)
			 (or (predicate (car items) (car items*))
			     (loop (cdr items*)))))
		  items*
		  (cons (car items) items*)))
	(reverse! items*))))

(define (remove-duplicates! items predicate)
  (define (trim-initial-segment items)
    (cond ((pair? items)
	   (if (test-item items)
	       (trim-initial-segment (cdr items))
	       (begin
		 (locate-initial-segment items (cdr items))
		 items)))
	  ((null? items) items)
	  (else (lose))))

  (define (locate-initial-segment prev this)
    (cond ((pair? this)
	   (if (test-item this)
	       (set-cdr! prev (trim-initial-segment (cdr this)))
	       (locate-initial-segment this (cdr this))))
	  ((not (null? this)) (lose))))

  (define (test-item items)
    (let loop ((items* (cdr items)))
      (and (pair? items*)
	   (or (predicate (car items) (car items*))
	       (loop (cdr items*))))))

  (define (lose)
    (error:wrong-type-argument items "list" 'REMOVE-DUPLICATES!))

  (trim-initial-segment items))

;; The cryptic LWSP means Linear White SPace.  We use it because it
;; is the terminology from RFC 822.

(define (char-lwsp? char)
  (or (char=? #\space char)
      (char=? #\tab char)))

(define char-set:lwsp
  (char-set #\space #\tab))

(define (quote-lines lines)
  (map (lambda (line)
	 (string-append "\t" line))
       lines))

(define (unquote-lines lines)
  (map (lambda (line)
	 (if (and (fix:> (string-length line) 0)
		  (char=? #\tab (string-ref line 0)))
	     (string-tail line 1)
	     (error "Unquoted line:" line)))
       lines))

(define (string->lines string #!optional line-ending)
  (substring->lines string 0 (string-length string)
		    (if (default-object? line-ending) "\n" line-ending)))

(define (substring->lines string start end #!optional line-ending)
  (let ((line-ending (if (default-object? line-ending) "\n" line-ending))
	(n (string-length line-ending)))
    (let ((indexes (substring-search-all line-ending string start end)))
      (if (pair? indexes)
	  (begin
	    (let loop ((start start) (indexes indexes))
	      (let ((start* (fix:+ (car indexes) n)))
		(set-car! indexes (substring string start (car indexes)))
		(cond ((pair? (cdr indexes))
		       (loop start* (cdr indexes)))
		      ((fix:< start* end)
		       (set-cdr! indexes
				 (list (substring string start* end)))))))
	    indexes)
	  (if (fix:< start end)
	      (list (if (and (fix:= start 0)
			     (fix:= end (string-length string)))
			string
			(substring string start end)))
	      '())))))

(define (lines->string lines #!optional line-ending)
  (decorated-string-append "" ""
			   (if (default-object? line-ending) "\n" line-ending)
			   lines))

(define (short-name->pathname name)
  (merge-pathnames name (current-home-directory)))

(define (pathname->short-name pathname)
  (enough-namestring pathname (current-home-directory)))

(define (write-header-fields headers port)
  (for-each (lambda (header)
	      (write-header-field header port))
	    headers))

(define (write-header-field header port)
  (%write-header-field (header-field-name header)
		       (header-field-value header)
		       port))

(define (%write-header-field name value port)
  (write-string name port)
  (write-char #\: port)
  (write-string value port)
  (newline port))

(define (read-lines port)
  (source->list (lambda () (read-line port))))

(define (read-header-lines port)
  (source->list
   (lambda ()
     (let ((line (read-required-line port)))
       (if (string-null? line)
	   (make-eof-object port)
	   line)))))

(define (read-required-line port)
  (let ((line (read-line port)))
    (if (eof-object? line)
	(error "Premature end of file:" port))
    line))

(define (abbreviate-exact-nonnegative-integer n k)
  (if (< n (expt 10 (- k 1)))
      (string-append (string-pad-left (number->string n) (- k 1)) " ")
      (let ((s
	     (fluid-let ((flonum-unparser-cutoff `(RELATIVE ,k ENGINEERING)))
	       (number->string (exact->inexact n)))))
	(let ((regs (re-string-match "\\([0-9.]+\\)e\\([0-9]+\\)" s)))
	  (let ((mantissa (re-match-extract s regs 1))
		(exponent (string->number (re-match-extract s regs 2))))
	    (if (> exponent 12)
		(make-string k #\+)
		(string-append
		 (let ((l (string-length mantissa))
		       (k (- k 1)))
		   (cond ((< l k)
			  (string-pad-left mantissa k))
			 ((= l k)
			  mantissa)
			 ((char=? #\. (string-ref mantissa (- k 1)))
			  (string-append " " (string-head mantissa (- k 1))))
			 (else
			  (string-head mantissa k))))
		 (case exponent
		   ((0) " ")
		   ((3) "k")
		   ((6) "M")
		   ((9) "G")
		   ((12) "T")))))))))

(define (burst-comma-list-string string)
  (list-transform-negative (map string-trim (burst-string string #\, #f))
    string-null?))

;;;; Ordered-string-vector completion

(define (hash-table/ordered-key-vector table <)
  (let ((v (list->vector (hash-table/key-list table))))
    (sort! v <)
    v))

(define (ordered-string-vector-completer strings)
  (lambda (string if-unique if-not-unique if-not-found)
    (ordered-vector-minimum-match strings string identity-procedure
				  string-order (string-prefix-matcher string)
				  if-unique if-not-unique if-not-found)))

(define (ordered-string-vector-completer-ci strings)
  (lambda (string if-unique if-not-unique if-not-found)
    (ordered-vector-minimum-match strings string identity-procedure
				  string-order-ci
				  (string-prefix-matcher-ci string)
				  if-unique if-not-unique if-not-found)))

(define (ordered-string-vector-matches strings)
  (lambda (string)
    (ordered-vector-matches strings string identity-procedure
			    string-order (string-prefix-matcher string))))

(define (ordered-string-vector-matches-ci strings)
  (lambda (string)
    (ordered-vector-matches strings string identity-procedure
			    string-order-ci
			    (string-prefix-matcher-ci string))))

(define (string-order x y)
  (let ((lx (string-length x))
	(ly (string-length y)))
    (let ((i (substring-match-forward x 0 lx y 0 ly)))
      (if (fix:< i lx)
	  (if (fix:< i ly)
	      (if (char<? (string-ref x i) (string-ref y i)) 'LESS 'GREATER)
	      'GREATER)
	  (if (fix:< i ly)
	      'LESS
	      'EQUAL)))))

(define (string-order-ci x y)
  (let ((lx (string-length x))
	(ly (string-length y)))
    (let ((i (substring-match-forward-ci x 0 lx y 0 ly)))
      (if (fix:< i lx)
	  (if (fix:< i ly)
	      (if (char-ci<? (string-ref x i) (string-ref y i)) 'LESS 'GREATER)
	      'GREATER)
	  (if (fix:< i ly)
	      'LESS
	      'EQUAL)))))

(define (string-prefix-matcher prefix)
  (let ((l (string-length prefix)))
    (lambda (x y)
      (let ((i (string-match-forward x y)))
	(and (fix:>= i l)
	     i)))))

(define (string-prefix-matcher-ci prefix)
  (let ((l (string-length prefix)))
    (lambda (x y)
      (let ((i (string-match-forward-ci x y)))
	(and (fix:>= i l)
	     i)))))

;;;; Filename Completion

(define (pathname-complete-string pathname filter
				  if-unique if-not-unique if-not-found)
  (let loop
      ((pathnames (filtered-completions (merge-pathnames pathname) filter)))
    (if (pair? pathnames)
	(if (pair? (cdr pathnames))
	    (if-not-unique
	     (string-greatest-common-prefix
	      (map ->namestring pathnames))
	     (lambda ()
	       (map canonicalize-pathname pathnames)))
	    (let ((pathname (car pathnames)))
	      (let ((pathnames
		     (filtered-list (pathname-as-directory pathname) filter)))
		(if (pair? pathnames)
		    (loop pathnames)
		    (if-unique pathname)))))
	(if-not-found))))

(define (pathname-completions-list pathname filter)
  (map canonicalize-pathname
       (filtered-completions (merge-pathnames pathname) filter)))

(define (filtered-completions pathname filter)
  (let ((directory (directory-namestring pathname)))
    (if (safe-file-directory? directory)
	(let ((prefix (file-namestring pathname))
	      (channel (directory-channel-open directory)))
	  (let loop ((result '()))
	    (let ((name (directory-channel-read-matching channel prefix)))
	      (if name
		  (loop
		   (if (filter name)
		       (cons (parse-namestring (string-append directory name)
					       #f #f)
			     result)
		       result))
		  (begin
		    (directory-channel-close channel)
		    result)))))
	'())))

(define (filtered-list pathname filter)
  (let ((directory (directory-namestring pathname)))
    (if (safe-file-directory? directory)
	(let ((channel (directory-channel-open directory)))
	  (let loop ((result '()))
	    (let ((name (directory-channel-read channel)))
	      (if name
		  (loop
		   (if (filter name)
		       (cons (parse-namestring (string-append directory name)
					       #f #f)
			     result)
		       result))
		  (begin
		    (directory-channel-close channel)
		    result)))))
	'())))

(define (safe-file-directory? pathname)
  (call-with-current-continuation
   (lambda (k)
     (bind-condition-handler (list condition-type:file-error
				   condition-type:port-error)
	 (lambda (condition)
	   condition
	   (k #f))
       (lambda ()
	 (file-directory? pathname))))))

(define (canonicalize-pathname pathname)
  (if (safe-file-directory? pathname)
      (pathname-as-directory pathname)
      pathname))
;;; -*-Scheme-*-
;;;
;;; $Id: imail-util.scm,v 1.5 2000/02/03 04:48:54 cph Exp $
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

(define (string->lines string)
  (let ((lines (burst-string string #\newline #f)))
    (if (pair? (cdr lines))
	(let loop ((prev lines) (this (cdr lines)))
	  (cond ((pair? (cdr this)) (loop this (cdr this)))
		((string-null? (car this)) (set-cdr! prev (cdr this))))))
    lines))

(define (lines->string lines)
  (suffixed-append lines "\n"))

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

(define (separated-append tokens separator)
  (cond ((not (pair? tokens)) "")
	((not (pair? (cdr tokens))) (car tokens))
	(else
	 (let ((string
		(make-string
		 (let ((ns (string-length separator)))
		   (do ((tokens (cdr tokens) (cdr tokens))
			(count (string-length (car tokens))
			       (fix:+ count
				      (fix:+ (string-length (car tokens))
					     ns))))
		       ((not (pair? tokens)) count))))))
	   (let loop
	       ((tokens (cdr tokens))
		(index (copy (car tokens) 0)))
	     (if (pair? tokens)
		 (loop (cdr tokens)
		       (string-move! (car tokens)
				     string
				     (string-move! separator string index)))))
	   string))))

(define (suffixed-append tokens suffix)
  (if (pair? tokens)
      (let ((string
	     (make-string
	      (let ((ns (string-length suffix)))
		(do ((tokens tokens (cdr tokens))
		     (count 0
			    (fix:+ count
				   (fix:+ (string-length (car tokens)) ns))))
		    ((not (pair? tokens)) count))))))
	(let loop ((tokens (cdr tokens)) (index 0))
	  (if (pair? tokens)
	      (loop (cdr tokens)
		    (string-move! suffix
				  string
				  (string-move! (car tokens) string index)))))
	string)
      ""))

(define (prefixed-append tokens prefix)
  (if (pair? tokens)
      (let ((string
	     (make-string
	      (let ((ns (string-length prefix)))
		(do ((tokens tokens (cdr tokens))
		     (count 0
			    (fix:+ count
				   (fix:+ (string-length (car tokens)) ns))))
		    ((not (pair? tokens)) count))))))
	(let loop ((tokens (cdr tokens)) (index 0))
	  (if (pair? tokens)
	      (loop (cdr tokens)
		    (string-move! (car tokens)
				  string
				  (string-move! prefix string index)))))
	string)
      ""))

(define (string-move! from to index)
  (let ((end (string-length from)))
    (if (fix:< end 32)
	;; When transferring less than 32 bytes, it's faster to do
	;; inline than to call the primitive.
	(let loop ((fi 0) (ti index))
	  (if (fix:= fi end)
	      ti
	      (begin
		(string-set! to ti (string-ref from fi))
		(loop (fix:+ fi 1) (fix:+ ti 1)))))
	(substring-move-left! from 0 end to index))))

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
#| -*-Scheme-*-

$Id: logmer.scm,v 1.18 2000/02/01 01:58:14 cph Exp $

Copyright (c) 1988-2000 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; RCS Log Merge

(declare (usual-integrations))

(define (rcs-directory-log directory #!optional output-file)
  (let ((output-file
	 (merge-pathnames (if (or (default-object? output-file)
				  (not output-file))
			      "RCS.log"
			      output-file)
			  (pathname-as-directory directory)))
	(port (notification-output-port)))
    (write-string "regenerating log for directory: " port)
    (write (->namestring directory))
    (write-string "..." port)
    (newline port)
    (let ((pathnames (rcs-directory-read directory)))
      (if (let ((time (file-modification-time-indirect output-file)))
	    (or (not time)
		(there-exists? pathnames
		  (lambda (w.r)
		    (> (file-modification-time-indirect (cdr w.r)) time)))))
	  (begin
	    (write-string "total files: " port)
	    (write (length pathnames) port)
	    (newline port)
	    (let ((entries (read-entries pathnames port)))
	      (write-string "total entries: " port)
	      (write (length entries) port)
	      (newline port)
	      (let ((entries (sort-entries entries)))
		(write-string "sorting finished" port)
		(newline port)
		(call-with-output-file output-file
		  (lambda (port)
		    (format/entries entries port))))))
	  (begin
	    (write-string " log is up to date" port)
	    (newline port))))))

(define (format/entries entries port)
  (let ((groups (compress-entries entries)))
    (if (not (null? groups))
	(begin
	  (format/group (car groups) port)
	  (for-each (lambda (group)
		      (write-string "----------------------------" port)
		      (newline port)
		      (format/group group port))
		    (cdr groups))))))

(define (format/group group port)
  (for-each (lambda (entry)
	      (format/entry (cdr entry) (car entry) port))
	    group)
  (newline port)
  (write-string (delta/log (car (car group))) port)
  (newline port))

(define (format/entry filename delta port)
  (write-string "file: " port)
  (write-string filename port)
  (write-string ";  revision: " port)
  (write-string (delta/number delta) port)
  (write-string "\ndate: " port)
  (write-string (date->string (delta/date delta)) port)
  (write-string ";  author: " port)
  (write-string (delta/author delta) port)
  (write-string ";  state: " port)
  (write-string (delta/state delta) port)
  (newline port))

(define (compress-entries entries)
  (if (null? entries)
      '()
      (let ((entry (car entries)))
	(let loop
	    ((entries (cdr entries))
	     (receiver
	      (lambda (similar entries)
		(cons (cons entry similar)
		      (compress-entries entries)))))
	  (if (or (null? entries)
		  (not (string=? (delta/log (car entry))
				 (delta/log (car (car entries))))))
	      (receiver '() entries)
	      (loop (cdr entries)
		    (lambda (similar entries*)
		      (receiver (cons (car entries) similar)
				entries*))))))))

(define (read-entries pairs notification-port)
  (let ((prefix (greatest-common-prefix (map car pairs))))
    (append-map!
     (lambda (w.r)
       (map (let ((filename (->namestring (enough-pathname (car w.r) prefix))))
	      (lambda (delta)
		(cons delta filename)))
	    (read-file (cdr w.r) notification-port)))
     pairs)))

(define (sort-entries entries)
  (sort entries
	(lambda (x y)
	  (date<? (delta/date (car y)) (delta/date (car x))))))

(define (read-file pathname notification-port)
  (if notification-port
      (begin
	(write-string "read-file " notification-port)
	(write-string (->namestring pathname) notification-port)
	(newline notification-port)))
  (let ((deltas (rcstext->deltas (rcs/read-file pathname 'LOG-ONLY))))
    (for-each (lambda (delta)
		(set-delta/log! delta
				(let ((log (string-trim (delta/log delta))))
				  (if (string-null? log)
				      empty-log-message
				      log))))
	      deltas)
    (list-transform-negative deltas delta/trivial-log?)))

(define (delta/trivial-log? delta)
  (string=? (delta/log delta) "Initial revision"))

(define empty-log-message "*** empty log message ***")

(define (rcstext->deltas rcstext)
  (let ((head (rcstext/head rcstext)))
    (if (not head)
	'()
	(let loop ((input (list head)) (output '()))
	  (if (null? input)
	      output
	      (let ((input* (append (delta/branches (car input)) (cdr input))))
		(loop (if (delta/next (car input))
			  (cons (delta/next (car input)) input*)
			  input*)
		      (cons (car input) output))))))))

(define (rcs-directory-read pathname)
  (let ((files '()))
    (define (scan-directory directory original-directory)
      (let ((directory (pathname-as-directory directory))
	    (original-directory (pathname-as-directory original-directory)))
	(for-each (lambda (pathname)
		    (scan-file pathname
			       (merge-pathnames (file-pathname pathname)
						original-directory)))
		  (directory-read directory #f))))

    (define (scan-file pathname original-pathname)
      (let ((attributes (file-attributes-direct pathname)))
	(if (not attributes)
	    (warn "Cannot get attributes.  Path might contain stale symlink."
		  (error-irritant/noise "\n;   ")
		  original-pathname
		  (error-irritant/noise "\n; points to\n;   ")
		  pathname)
	    (let ((type (file-attributes/type attributes)))
	      (cond ((not type)
		     (if (not (or (ignored-file-name? pathname)
				  (ignored-file-name? original-pathname)))
			 (let ((control (rcs-control-file pathname)))
			   (if control
			       (begin
				 (set! files
				       (cons (cons original-pathname control)
					     files))
				 unspecific)))))
		    ((eq? type #t)
		     (if (not (member (file-namestring pathname)
				      '("." ".." "RCS")))
			 (scan-directory pathname original-pathname)))
		    ((string? type)
		     (scan-file (merge-pathnames type
						 (directory-pathname pathname))
				original-pathname)))))))

    (define (rcs-control-file pathname)
      (let ((directory (directory-pathname pathname))
	    (name (string-append (file-namestring pathname) ",v")))
	(let ((p (merge-pathnames name (merge-pathnames "RCS/" directory))))
	  (if (regular-file? p)
	      p
	      (let ((p (merge-pathnames name directory)))
		(if (regular-file? p)
		    p
		    #f))))))

    (define (regular-file? pathname)
      (let ((attributes (file-attributes pathname)))
	(and attributes
	     (not (file-attributes/type attributes)))))

    (define (ignored-file-name? pathname)
      (let ((name (file-namestring pathname)))
	(or (string-suffix? ",v" name)
	    (string-suffix? "~" name)
	    (string-prefix? "#" name))))

    (scan-directory pathname pathname)
    files))

(define (greatest-common-prefix pathnames)
  (if (null? pathnames)
      (->pathname "")
      (let ((prefix 'NONE))
	(for-each (lambda (pathname)
		    (let ((directory (pathname-directory pathname)))
		      (set! prefix
			    (if (eq? prefix 'NONE)
				directory
				(let common-prefix ((x prefix) (y directory))
				  (if (or (null? x)
					  (null? y)
					  (not (equal? (car x) (car y))))
				      '()
				      (cons (car x)
					    (common-prefix (cdr x)
							   (cdr y)))))))))
		  pathnames)
	(pathname-new-directory "" prefix))))
#| -*-Scheme-*-

$Id: logmer.scm,v 1.13 1995/11/12 05:58:57 cph Exp $

Copyright (c) 1988-95 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

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
    (newline port)
    (write-string "regenerating log for directory: " port)
    (write (->namestring directory))
    (write-string "..." port)
    (let ((pathnames (rcs-directory-read directory)))
      (if (let ((time (file-modification-time-indirect output-file)))
	    (or (not time)
		(there-exists? pathnames
		  (lambda (w.r)
		    (> (file-modification-time-indirect (cdr w.r)) time)))))
	  (begin
	    (newline port)
	    (write-string "total files: " port)
	    (write (length pathnames) port)
	    (let ((entries (read-entries pathnames port)))
	      (newline port)
	      (write-string "total entries: " port)
	      (write (length entries) port)
	      (let ((entries (sort-entries entries)))
		(newline port)
		(write-string "sorting finished" port)
		(call-with-output-file output-file
		  (lambda (port)
		    (format/entries entries port))))))
	  (write-string " log is up to date" port)))))

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
	(newline notification-port)
	(write-string "read-file " notification-port)
	(write-string (->namestring pathname) notification-port)))
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
    (define (scan-directory pathname)
      (for-each scan-file
		(directory-read (pathname-as-directory pathname) #f)))

    (define (scan-file pathname)
      (let ((attributes (file-attributes-direct pathname)))
	(let ((type (file-attributes/type attributes)))
	  (cond ((not type)
		 (maybe-add-file pathname pathname))
		((eq? type #t)
		 (if (not (member (file-namestring pathname) '("." "..")))
		     (scan-directory pathname)))
		((string? type)
		 (let ((working-file
			(merge-pathnames type (directory-pathname pathname))))
		   (if (regular-file? working-file)
		       (maybe-add-file pathname working-file))))))))

    (define (maybe-add-file pathname working-file)
      (if (not (ignored-file-name? pathname))
	  (let ((control (rcs-control-file working-file)))
	    (if control
		(begin
		  (set! files (cons (cons pathname control) files))
		  unspecific)))))

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

    (scan-directory pathname)
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
#| -*-Scheme-*-

$Id: logmer.scm,v 1.8 1995/11/11 07:42:36 cph Exp $

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

(define trace-port false)

(define (rcs-directory-log output-file directory)
  (format-to-file
   output-file
   (fluid-let ((trace-port (nearest-cmdl/port)))
     (let ((entries
	    (sort-entries
	     (let ((entries
		    (read-entries
		     (let ((pathnames (rcs-directory-read directory)))
		       (newline trace-port)
		       (write-string "total files: " trace-port)
		       (write (length pathnames) trace-port)
		       pathnames))))
	       (newline trace-port)
	       (write-string "total entries: " trace-port)
	       (write (length entries) trace-port)
	       entries))))
       (newline trace-port)
       (write-string "sorting finished" trace-port)
       entries))))

(define (format-to-file output-file entries)
  (with-output-to-file output-file
    (lambda ()
      (format/entries entries))))

(define (format/entries entries)
  (let ((groups (compress-entries entries)))
    (if (not (null? groups))
	(begin
	  (format/group (car groups))
	  (for-each (lambda (group)
		      (write-string "----------------------------")
		      (newline)
		      (format/group group))
		    (cdr groups))))))

(define (format/group group)
  (for-each (lambda (entry)
	      (format/entry (cdr entry) (car entry)))
	    group)
  (newline)
  (write-string (delta/log (car (car group))))
  (newline))

(define (format/entry filename delta)
  (write-string "file: ")
  (write-string filename)
  (write-string ";  revision: ")
  (write-string (delta/number delta))
  (write-string "\ndate: ")
  (write-string (date->string (delta/date delta)))
  (write-string ";  author: ")
  (write-string (delta/author delta))
  (write-string ";  state: ")
  (write-string (delta/state delta))
  (newline))

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

(define (read-entries pathnames)
  (let ((prefix (greatest-common-prefix pathnames)))
    (append-map! (lambda (pathname)
		   (map (let ((filename (working-file-string pathname prefix)))
			  (lambda (delta)
			    (cons delta filename)))
			(read-file pathname)))
		 pathnames)))

(define (working-file-string pathname prefix)
  (let ((relative-pathname (enough-pathname pathname prefix)))
    (let ((filename
	   (->namestring
	    (pathname-new-directory
	     relative-pathname
	     (let ((directory (pathname-directory relative-pathname)))
	       (and directory (delete "RCS" directory)))))))
    (if (string-suffix? ",v" filename)
	(substring filename 0 (- (string-length filename) 2))
	filename))))

(define (sort-entries entries)
  (sort entries
	(lambda (x y)
	  (date<? (delta/date (car y)) (delta/date (car x))))))

(define (read-file pathname)
  (if trace-port
      (begin
	(newline trace-port)
	(write-string "read-file " trace-port)
	(write-string (->namestring pathname) trace-port)))
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
		 (maybe-add-file pathname))
		((eq? type #t)
		 (if (not (member (file-namestring pathname) '("." "..")))
		     (scan-directory pathname)))
		((string? type)
		 (let ((pathname
			(merge-pathnames type (directory-pathname pathname))))
		   (if (regular-file? pathname)
		       (maybe-add-file (pathname-simplify pathname)))))))))

    (define (maybe-add-file pathname)
      (if (not (ignored-file-name? pathname))
	  (let ((control (rcs-control-file pathname)))
	    (if control
		(begin
		  (set! files (cons control files))
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
#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/rcs/logmer.scm,v 1.2 1991/01/19 04:21:08 cph Exp $

Copyright (c) 1988, 1991 Massachusetts Institute of Technology

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

(define (rcs-directory-log output-file . directories)
  (format-to-file
   output-file
   (fluid-let ((trace-port (cmdl/output-port (nearest-cmdl))))
     (let ((entries
	    (sort-entries
	     (let ((entries
		    (read-entries
		     (let ((pathnames (mapcan rcs-directory-read directories)))
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
  (mapcan (let ((prefix (length (greatest-common-prefix pathnames))))
	    (lambda (pathname)
	      (map (let ((filename (working-file-string pathname prefix)))
		     (lambda (delta)
		       (cons delta filename)))
		   (read-file pathname))))
	  pathnames))

(define (working-file-string pathname prefix)
  (let ((filename
	 (pathname->string
	  (pathname-new-directory
	   pathname
	   (let ((directory (list-tail (pathname-directory pathname) prefix)))
	     (if (and (not (null? directory))
		      (equal? (car (last-pair directory)) "RCS"))
		 (except-last-pair directory)
		 directory))))))
    (if (string-suffix? ",v" filename)
	(substring filename 0 (- (string-length filename) 2))
	filename)))

(define (sort-entries entries)
  (sort entries
	(lambda (x y)
	  (date<? (delta/date (car y)) (delta/date (car x))))))

(define (read-file pathname)
  (if trace-port
      (begin
	(newline trace-port)
	(write-string "read-file " trace-port)
	(write-string (pathname->string pathname) trace-port)))
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

(define (rcs-directory-read filename)
  (let ((pathname
	 (pathname->absolute-pathname
	  (pathname-as-directory (->pathname filename)))))
    (map (let ((directory-path (pathname-directory-path pathname)))
	   (lambda (filename)
	     (merge-pathnames directory-path (string->pathname filename))))
	 (list-transform-positive
	     (generate-filenames (pathname-directory-string pathname))
	   (lambda (filename)
	     (string-suffix? ",v" filename))))))

(define (string-suffix? string1 string2)
  (substring-suffix? string1 0 (string-length string1)
		     string2 0 (string-length string2)))

(define (substring-suffix? string1 start1 end1 string2 start2 end2)
  (let ((length (- end1 start1)))
    (and (<= length (- end2 start2))
	 (= (substring-match-backward string1 start1 end1
				      string2 start2 end2)
	    length))))

(define (generate-filenames directory-string)
  (let loop ((name (open-directory directory-string)))
    (if name
	(cons name (loop (directory-read)))
	'())))

(define open-directory
  (make-primitive-procedure 'OPEN-DIRECTORY))

(define directory-read
  (make-primitive-procedure 'DIRECTORY-READ))

(define (greatest-common-prefix pathnames)
  (if (null? pathnames)
      '()
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
	(if (equal? prefix '(ROOT))
	    '()
	    prefix))))
#| -*-Scheme-*-

Copyright (c) 1993 Massachusetts Institute of Technology

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

;;;; Routines to preserve file dates and times.

(declare (usual-integrations))

(define decode-time
  (access decode-time (->environment '(runtime date/time))))

(define encode-time
  (access encode-time (->environment '(runtime date/time))))

(define decoded-time-structure-tag
  (access decoded-time-structure-tag (->environment '(runtime date/time))))

(define *dumped-dates* "_dates_.dat")

(define (dump-dates . paths)
  (with-output-to-file *dumped-dates*
    (lambda ()
      (for-each
       (lambda (path)
	 (for-each
	  (lambda (path)
	    (write-line (cons (enough-namestring path)
			      (subvector->list (decode-time
						(file-modification-time path))
					       1 8))))
	  (directory-read path)))
       paths))))

(define (restore-dates)
  (for-each
   (lambda (info)
     (let ((path (car info))
	   (time (encode-time
		  (list->vector
		   (cons decoded-time-structure-tag (cdr info))))))
       (set-file-times! path (file-modification-time path) time)))
   (read-file *dumped-dates*)))
	      
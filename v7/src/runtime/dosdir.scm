#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/dosdir.scm,v 1.2 1992/08/06 13:40:16 jinx Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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

;;;; Directory Operations -- DOS (copy of unxdir version 14.9)
;;; package: (runtime directory)

(declare (usual-integrations))

(define (directory-read pattern #!optional sort?)
  (if (if (default-object? sort?) true sort?)
      (sort (directory-read-nosort pattern) pathname<?)
      (directory-read-nosort pattern)))

(define (directory-read-nosort pattern)
  (let ((pattern
	 (let ((pattern (merge-pathnames pattern)))
	   (let ((name (pathname-name pattern))
		 (type (pathname-type pattern)))
	     (if (or name type)
		 pattern
		 (make-pathname (pathname-host pattern)
				(pathname-device pattern)
				(pathname-directory pattern)
				'WILD
				'WILD
				(pathname-version pattern)))))))
    (let ((directory-path (directory-pathname pattern)))
      (map (lambda (pathname)
	     (merge-pathnames pathname directory-path))
	   (let ((pathnames
		  (map ->pathname
		       (generate-directory-pathnames directory-path))))
	     (if (and (eq? (pathname-name pattern) 'WILD)
		      (eq? (pathname-type pattern) 'WILD))
		 pathnames
		 (list-transform-positive pathnames
		   (let ((match-name
			  (component-matcher (pathname-name pattern)))
			 (match-type
			  (component-matcher (pathname-type pattern))))
		     (lambda (instance)
		       (and (match-name (pathname-name instance))
			    (match-type (pathname-type instance))))))))))))

(define (generate-directory-pathnames pathname)
  (let ((channel (directory-channel-open (->namestring pathname))))
    (let loop ((result '()))
      (let ((name (directory-channel-read channel)))
	(if name
	    (loop (cons name result))
	    (begin
	      (directory-channel-close channel)
	      result))))))

(define (component-matcher pattern)
  ;; For the time being, this only understands one asterisk,
  ;; and does not understand question marks.
  (cond ((eq? pattern 'WILD)
	 (lambda (instance)
	   instance			; ignored
	   true))
	((and (string? pattern) (string-find-next-char pattern #\*))
	 =>
	 (lambda (posn)
	   (let* ((len (string-length pattern))
		  (min-len (-1+ len)))
	     (cond ((zero? posn)
		    (let ((suffix (substring pattern 1 len)))
		      (lambda (instance)
			(and (string? instance)
			     (let ((len* (string-length instance)))
			       (and (>= len* min-len)
				    (string=? suffix
					      (substring instance
							 (- len* min-len)
							 len*))))))))
		   ((= posn (-1+ len))
		    (let ((prefix (substring pattern 0 min-len)))
		      (lambda (instance)
			(and (string? instance)
			     (let ((len* (string-length instance)))
			       (and (>= len* min-len)
				    (string=? prefix
					      (substring instance 0
							 min-len))))))))
		   (else
		    (let ((prefix (substring pattern 0 posn))
			  (suffix (substring pattern (1+ posn) len))
			  (suffix-len (- len (1+ posn))))
		      (lambda (instance)
			(and (string? instance)
			     (let ((len* (string-length instance)))
			       (and (>= len* min-len)
				    (string=? prefix
					      (substring instance 0 posn))
				    (string=? suffix
					      (substring instance
							 (- len* suffix-len)
							 len*))))))))))))
	(else
	 (lambda (instance)
	   (equal? pattern instance)))))

(define (pathname<? x y)
  (or (component<? (pathname-name x) (pathname-name y))
      (and (equal? (pathname-name x) (pathname-name y))
	   (component<? (pathname-type x) (pathname-type y)))))

(define (component<? x y)
  (and y
       (or (not x)
	   (and (string? y)
		(or (not (string? x))
		    (string<? x y))))))
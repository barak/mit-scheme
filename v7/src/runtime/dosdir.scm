#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/dosdir.scm,v 1.5 1992/08/28 16:06:37 jinx Exp $

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

(define directory-read/adjust-patterns? true)

(define (directory-read pattern #!optional sort?)
  (if (if (default-object? sort?) true sort?)
      (sort (directory-read-nosort pattern) pathname<?)
      (directory-read-nosort pattern)))

(define (directory-read-nosort pattern)
  (let ((pattern
	 (let ((pattern (adjust-directory-pattern (merge-pathnames pattern))))
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

(define (adjust-directory-pattern pathname)
  (if (and directory-read/adjust-patterns?
	   (not (pathname-type pathname))
	   (let ((name (pathname-name pathname)))
	     (and (string? name)
		  (let ((len (string-length name)))
		    (and (> len 0)
			 (char=? (string-ref name (-1+ len)) #\*))))))
      (pathname-new-type pathname 'WILD)
      pathname))

(define (generate-directory-pathnames pathname)
  (let ((channel (directory-channel-open (->namestring pathname))))
    (let loop ((result '()))
      (let ((name (directory-channel-read channel)))
	(if name
	    (loop (cons name result))
	    (begin
	      (directory-channel-close channel)
	      result))))))

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

;;; This matcher does not currently understand question marks
;;; but understands multiple asterisks.
;;; Question marks are hard because in the presence of asterisks,
;;; simple-minded left-to-right processing no longer works.  e.g.
;;; "*foo?bar*" matching "foogbazfoogbar".

(define (component-matcher pattern)
  (cond ((eq? pattern 'WILD)
	 (lambda (instance)
	   instance			; ignored
	   true))
	((and (string? pattern) (string-find-next-char pattern #\*))
	 =>
	 (lambda (posn)
	   (let* ((len (string-length pattern))
		  (posn*
		   (substring-find-next-char pattern (1+ posn) len #\*)))
	     (if (not posn*)
		 (simple-wildcard-matcher pattern posn)
		 (let ((prefix (substring pattern 0 posn)))
		   (let loop ((segments (list (substring pattern
							 (1+ posn)
							 posn*)))
			      (posn posn*))
		     (let* ((start (1+ posn))
			    (posn*
			     (substring-find-next-char pattern start len #\*)))
		       (if (not posn*)
			   (full-wildcard-matcher
			    prefix
			    (list-transform-negative (reverse! segments)
			      string-null?)
			    (substring pattern start len))
			   (loop (cons (substring pattern start posn*)
				       segments)
				 posn*)))))))))
	(else
	 (lambda (instance)
	   (equal? pattern instance)))))

(define (simple-wildcard-matcher pattern posn)
  (let* ((len (string-length pattern))
	 (min-len (-1+ len)))
    (cond ((zero? min-len)
	   ;; e.g. "*"
	   (lambda (instance)
	     instance			; ignored
	     true))
	  ((zero? posn)
	   ;; e.g. "*foo"
	   (lambda (instance)
	     (and (string? instance)
		  (let ((len* (string-length instance)))
		    (and (>= len* min-len)
			 (substring=? pattern 1 len
				      instance (- len* min-len) len*))))))
	  ((= posn (-1+ len))
	   ;; e.g. "bar*"
	   (lambda (instance)
	     (and (string? instance)
		  (let ((len* (string-length instance)))
		    (and (>= len* min-len)
			 (substring=? pattern 0 min-len
				      instance 0 min-len))))))
	  (else
	   ;; e.g. "foo*bar"
	   (let* ((suffix-start (1+ posn))
		  (suffix-len (- len suffix-start)))
	     (lambda (instance)
	       (and (string? instance)
		    (let ((len* (string-length instance)))
		      (and (>= len* min-len)
			   (substring=? pattern 0 posn
					instance 0 posn)
			   (substring=? pattern suffix-start len
					instance (- len* suffix-len)
					len*))))))))))

(define (full-wildcard-matcher prefix segments suffix)
  (cond ((null? segments)
	 ;; Degenerate case, e.g. "prefix**suffix"
	 (simple-wildcard-matcher (string-append prefix "*" suffix)
				  (string-length prefix)))
	#|
	((null? (cdr segments))
	 ;; Special case the single middle segment.
	 ;; Disabled because it is hardly worth it.
	 (let ((prelen (string-length prefix))
	       (suflen (string-length suffix)))
	   (let* ((middle (car segments))
		  (midlen (string-length middle))
		  (totlen (+ prelen midlen suflen)))
	     (cond ((string-null? prefix)
		    (if (string-null? suffix)
			;; e.g. "*middle*"
			(lambda (instance)
			  (and (string? instance)
			       (let ((len (string-length instance)))
				 (and (>= len totlen)
				      (substring? middle instance)))))
			;; e.g. "*middle*suffix"
			(lambda (instance)
			  (and (string? instance)
			       (let ((len (string-length instance)))
				 (and (>= len totlen)
				      (let ((end (- len suflen)))
					(and (substring=? suffix 0 suflen
							  instance end len)
					     (substring?
					      middle
					      (substring instance 0
							 end))))))))))
		   ((string-null? suffix)
		    ;; e.g. "prefix*middle*"
		    (lambda (instance)
		      (and (string? instance)
			   (let ((len (string-length instance)))
			     (and (>= len totlen)
				  (substring=? prefix 0 prelen
					       instance 0 prelen)
				  (substring? middle
					      (substring instance prelen
							 len)))))))
		   (else
		    ;; e.g. "prefix*middle*suffix"
		    (lambda (instance)
		      (and (string? instance)
			   (let ((len (string-length instance)))
			     (and (>= len totlen)
				  (let ((end (- len suflen)))
				    (substring=? prefix 0 prelen
						 instance 0 prelen)
				    (substring=? suffix 0 suflen
						 instance end len)
				    (substring? middle
						(substring instance prelen
							   end))))))))))))
	|#

	((and (null? (cdr segments))
	      (string-null? prefix)
	      (string-null? suffix))
	 ;; Special case "*foo*"
	 (let* ((middle (car segments))
		(totlen (string-length middle)))
	   (lambda (instance)
	     (and (string? instance)
		  (>= (string-length instance) totlen)
		  (substring? middle instance)))))	   

	(else
	 (let* ((prelen (string-length prefix))
		(suflen (string-length suffix))
		(totlen (+ prelen
			   (reduce + 0 (map string-length segments))
			   suflen)))

	   (define (segment-matcher segments)
	     ;; This handles the "*foo*bar*baz*" part
	     (let ((segment (car segments))
		   (rest (cdr segments)))
	       (if (null? rest)
		   (lambda (instance)
		     (substring? segment instance))
		   (let ((next (segment-matcher rest))
			 (len (string-length segment)))
		     (lambda (instance)
		       (let ((posn (substring? segment instance)))
			 (and posn
			      (next
			       (substring instance (+ posn len)
					  (string-length instance))))))))))

	   (let ((tester (segment-matcher segments)))
	     (cond ((string-null? prefix)
		    (if (string-null? suffix)
			;; e.g. "*foo*bar*"
			(lambda (instance)
			  (and (string? instance)
			       (>= (string-length instance) totlen)
			       (tester instance)))
			;; e.g. "*foo*bar*suffix"
			(lambda (instance)
			  (and (string? instance)
			       (let ((len (string-length instance)))
				 (and (>= len totlen)
				      (let ((end (- len suflen)))
					(and (substring=? suffix 0 suflen
							  instance end len)
					     (tester (substring instance 0
								end))))))))))

		 ((string-null? suffix)
		  ;; e.g. "prefix*foo*bar*"
		  (lambda (instance)
		    (and (string? instance)
			 (let ((len (string-length instance)))
			   (and (>= len totlen)
				(substring=? prefix 0 prelen
					     instance 0 prelen)
				(tester (substring instance prelen len)))))))

		 (else
		  ;; e.g. "prefix*foo*bar*suffix"
		  (lambda (instance)
		    (and (string? instance)
			 (let ((len (string-length instance)))
			   (and (>= len totlen)
				(let ((end (- len suflen)))
				  (and (substring=? prefix 0 prelen
						    instance 0 prelen)
				       (substring=? suffix 0 suflen
						    instance end len)
				       (tester (substring instance prelen
							  end)))))))))))))))
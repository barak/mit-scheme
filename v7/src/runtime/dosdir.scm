#| -*-Scheme-*-

$Id: dosdir.scm,v 1.13 2007/01/05 15:33:09 cph Exp $

Copyright (c) 1992, 1999-2001 Massachusetts Institute of Technology

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

;;;; DOS Directory Reader
;;; package: (runtime directory)

(declare (usual-integrations))

(define directory-read/adjust-patterns? true)
(define *expand-directory-prefixes?* true)

(define (directory-read pattern #!optional sort?)
  (if (if (default-object? sort?) true sort?)
      (sort (directory-read-nosort pattern) pathname<?)
      (directory-read-nosort pattern)))

(define (directory-read-nosort pattern)
  (let ((pattern
	 (let ((pattern (adjust-directory-pattern (merge-pathnames pattern))))
	   (if (directory-pathname? pattern)
	       (make-pathname (pathname-host pattern)
			      (pathname-device pattern)
			      (pathname-directory pattern)
			      'WILD
			      'WILD
			      (pathname-version pattern))
	       pattern))))
    (let ((directory-path (directory-pathname pattern)))
      (map (lambda (pathname)
	     (merge-pathnames pathname directory-path))
	   (let ((pathnames
		  (let ((fnames (generate-directory-pathnames directory-path)))
		    (fluid-let ((*expand-directory-prefixes?* false))
		      (map ->pathname fnames)))))
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
		       (let ((posn (string-search-forward segment instance)))
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
#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/unxdir.scm,v 14.5 1991/07/17 08:54:53 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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

;;;; Directory Operations -- unix
;;; package: (runtime directory)

(declare (usual-integrations))

(define (directory-read pattern #!optional sort?)
  (if (if (default-object? sort?) true sort?)
      (sort (directory-read-nosort pattern) pathname<?)
      (directory-read-nosort pattern)))

(define (directory-read-nosort pattern)
  (let ((pattern
	 (let ((pattern (pathname->absolute-pathname (->pathname pattern))))
	   (if (or (pathname-name pattern)
		   (pathname-type pattern)
		   (pathname-version pattern))
	       pattern
	       (make-pathname (pathname-host pathname)
			      (pathname-device pathname)
			      (pathname-directory pathname)
			      'WILD 'WILD 'WILD)))))
    (let ((directory-path (pathname-directory-path pattern)))
      (let ((pathnames (generate-directory-pathnames directory-path)))
	(cond ((and (eq? 'WILD (pathname-name pattern))
		    (eq? 'WILD (pathname-type pattern))
		    (eq? 'WILD (pathname-version pattern)))
	       pathnames)
	      ((not (eq? (pathname-version pattern) 'NEWEST))
	       (list-transform-positive pathnames
		 (lambda (instance)
		   (and (match-component (pathname-name pattern)
					 (pathname-name instance))
			(match-component (pathname-type pattern)
					 (pathname-type instance))
			(match-component (pathname-version pattern)
					 (pathname-version instance))))))
	      (else
	       (extract-greatest-versions 
		(list-transform-positive pathnames
		  (lambda (instance)
		    (and (match-component (pathname-name pattern)
					  (pathname-name instance))
			 (match-component (pathname-type pattern)
					  (pathname-type instance))))))))))))

(define (generate-directory-pathnames pathname)
  (dynamic-wind
   (lambda () unspecific)
   (lambda ()
     (let loop
	 ((name
	   ((ucode-primitive open-directory 1) (pathname->string pathname)))
	  (result '()))
       (if name
	   (loop ((ucode-primitive directory-read 0))
		 (cons (merge-pathnames (string->pathname name) pathname)
		       result))
	   result)))
   (ucode-primitive directory-close 0)))

(define (extract-greatest-versions pathnames)
  (let ((name-alist '()))
    (for-each (lambda (pathname)
		(let ((name (pathname-name pathname))
		      (type (pathname-type pathname)))
		  (let ((name-entry (associate-on-name name name-alist)))
		    (if (not name-entry)
			(set! name-alist
			      (cons (list name (cons type pathname))
				    name-alist))
			(let ((type-entry
			       (associate-on-type type (cdr name-entry))))
			  (cond ((not type-entry)
				 (set-cdr! name-entry
					   (cons (cons type pathname)
						 (cdr name-entry))))
				((version<? (pathname-version (cdr type-entry))
					    (pathname-version pathname))
				 (set-cdr! type-entry pathname))))))))
	      pathnames)
    (mapcan (lambda (name-entry)
	      (map cdr (cdr name-entry)))
	    name-alist)))

(define (match-component pattern instance)
  (or (eq? pattern 'WILD)
      (equal? pattern instance)))

(define (pathname<? x y)
  (or (string<? (pathname-name x) (pathname-name y))
      (and (string=? (pathname-name x) (pathname-name y))
	   (or (type<? (pathname-type x) (pathname-type y))
	       (and (equal? (pathname-type x) (pathname-type y))
		    (version<? (pathname-version x) (pathname-version y)))))))

(define (initialize-package!)
  (set! associate-on-name (association-procedure string=? car))
  (set! type<? (component<? string<?))
  (set! version<? (component<? <)))

(define associate-on-name)

(define-integrable (associate-on-type type types)
  (assoc type types))

(define ((component<? <) x y)
  (cond ((not x) y)
	((eq? 'UNSPECIFIC x) (and y (not (eq? 'UNSPECIFIC y))))
	(else (and y (not (eq? 'UNSPECIFIC y)) (< x y)))))

(define type<?)
(define version<?)
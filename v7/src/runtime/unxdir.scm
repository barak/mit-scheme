#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/unxdir.scm,v 14.1 1988/06/13 10:49:56 cph Exp $

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
;;; package: (directory)

(declare (usual-integrations))

(define (initialize-package!)
  (set! associate-on-name (association-procedure string=? car))
  (set! type<? (component<? string<?))
  (set! version<? (component<? <)))

(define (directory-read pattern)
  "Returns a list of all the files in `pattern' -- correctly handles
wildcarding of whole pathname components, for example

	*.scm
	/u/cph/*/foo.*.*
	foo.*.3
	bar.*.0

but doesn't do more general wildcarding like

	foo*bar.scm
"
  (sort-pathnames
   (let ((pattern (pathname->absolute-pathname (->pathname pattern))))
     (map (let ((directory-path (pathname-directory-path pattern)))
	    (lambda (pathname)
	      (merge-pathnames directory-path pathname)))
	  (let ((pathnames
		 (generate-directory-pathnames
		  (pathname-directory-string pattern))))
	    (if (eq? (pathname-version pattern) 'NEWEST)
		(extract-greatest-versions 
		 (list-transform-positive pathnames
		   (lambda (instance)
		     (match-name&type pattern instance))))
		(list-transform-positive pathnames
		  (lambda (instance)
		    (and (match-name&type pattern instance)
			 (match-component
			  (pathname-version pattern)
			  (pathname-version instance)))))))))))

(define (match-name&type pattern instance)
  (and (match-component (pathname-name pattern) (pathname-name instance))
       (match-component (pathname-type pattern) (pathname-type instance))))

(define (match-component pattern instance)
  (or (eq? pattern 'WILD)
      (equal? pattern instance)))

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

(define (sort-pathnames pathnames)
  (sort pathnames pathname<?))

(define (pathname<? x y)
  (or (string<? (pathname-name x) (pathname-name y))
      (and (string=? (pathname-name x) (pathname-name y))
	   (or (type<? (pathname-type x) (pathname-type y))
	       (and (equal? (pathname-type x) (pathname-type y))
		    (version<? (pathname-version x) (pathname-version y)))))))

(define associate-on-name)

(define-integrable (associate-on-type type types)
  (assoc type types))

(define ((component<? <) x y)
  (cond ((not x) y)
	((eq? 'UNSPECIFIC x) (and y (not (eq? 'UNSPECIFIC y))))
	(else (and y (not (eq? 'UNSPECIFIC y)) (< x y)))))

(define type<?)
(define version<?)

(define (generate-directory-pathnames directory-string)
  (map string->pathname
       (let loop ((name ((ucode-primitive open-directory) directory-string)))
	 (if name
	     (cons name (loop ((ucode-primitive directory-read))))
	     '()))))
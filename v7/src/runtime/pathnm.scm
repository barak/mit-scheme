;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/pathnm.scm,v 13.44 1987/08/20 04:03:53 cph Rel $
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Pathnames

(declare (usual-integrations))
#|
A pathname component is normally one of:

* A string, which is the literal component.

* 'WILD, meaning that the component is wildcarded.  Such components
may have special meaning to certain directory operations.

* #F, meaning that the component was not supplied.  This has special
meaning to `merge-pathnames', in which such components are
substituted.

* 'UNSPECIFIC, which means the same thing as #F except that it is
never defaulted by `merge-pathnames'.  Normally there is no way to
specify such a component value with `string->pathname'.

A pathname consists of 5 components, not all necessarily meaningful,
as follows:

* The DEVICE is usually a physical device, as in the Twenex `ps:'.

* The DIRECTORY is a list of components.  If the first component is
'ROOT, then the directory path is absolute.  Otherwise it is relative.
Two special components allowed only in directories are the symbols
'SELF and 'UP which are equivalent to Unix' "." and ".." respectively.

* The NAME is the proper name part of the filename.

* The TYPE usually indicates something about the contents of the file.
Certain system procedures will default the type to standard type
strings.

* The VERSION is special.  Unlike an ordinary component, it is never a
string, but may be either a positive integer, 'NEWEST, 'UNSPECIFIC,
'WILD, or #F.  Many system procedures will default the version to
'NEWEST, which means to search the directory for the highest version
numbered file.

This file requires the following procedures and variables which define
the conventions for the particular file system in use:

(symbol->pathname symbol)
(pathname-parse string (lambda (device directory name type version)))
(pathname-unparse device directory name type version)
(pathname-unparse-name name type version)
(pathname-as-directory pathname)
(pathname-newest pathname)
working-directory-package
(access reset! working-directory-package)
init-file-pathname
(home-directory-pathname)
(working-directory-pathname)
(set-working-directory-pathname! name)

See the files unkpth.scm, vmspth.scm, or unxpth.scm for examples.|#

;;;; Basic Pathnames

;;; The following definition won't work because the type system isn't
;;; defined when this file is loaded:

;;; (define-structure pathname
;;;   (device false read-only true)
;;;   (directory false read-only true)
;;;   (name false read-only true)
;;;   (type false read-only true)
;;;   (version false read-only true))

(define make-pathname)
(define pathname?)
(let ((pathname-tag "pathname"))
  (set! make-pathname
    (named-lambda (make-pathname device directory name type version)
      (vector pathname-tag device directory name type version)))
  (set! pathname?
    (named-lambda (pathname? object)
      (and (vector? object)
	   (not (zero? (vector-length object)))
	   (eq? pathname-tag (vector-ref object 0))))))

(declare (integrate-operator pathname-device
			     pathname-directory
			     pathname-name
			     pathname-type
			     pathname-version))

(define (pathname-device pathname)
  (declare (integrate pathname))
  (vector-ref pathname 1))

(define (pathname-directory pathname)
  (declare (integrate pathname))
  (vector-ref pathname 2))

(define (pathname-name pathname)
  (declare (integrate pathname))
  (vector-ref pathname 3))

(define (pathname-type pathname)
  (declare (integrate pathname))
  (vector-ref pathname 4))

(define (pathname-version pathname)
  (declare (integrate pathname))
  (vector-ref pathname 5))

(declare (integrate copy-pathname))

(define copy-pathname
  vector-copy)

(define (pathname-absolute? pathname)
  (let ((directory (pathname-directory pathname)))
    (and (pair? directory)
	 (eq? (car directory) 'ROOT))))
(define (pathname-directory-path pathname)
  (make-pathname (pathname-device pathname)
		 (pathname-directory pathname)
		 false
		 false
		 false))

(define (pathname-name-path pathname)
  (make-pathname false
		 false
		 (pathname-name pathname)
		 (pathname-type pathname)
		 (pathname-version pathname)))

(define (pathname-new-device pathname device)
  (make-pathname device
		 (pathname-directory pathname)
		 (pathname-name pathname)
		 (pathname-type pathname)
		 (pathname-version pathname)))

(define (pathname-new-directory pathname directory)
  (make-pathname (pathname-device pathname)
		 directory
		 (pathname-name pathname)
		 (pathname-type pathname)
		 (pathname-version pathname)))

(define (pathname-new-name pathname name)
  (make-pathname (pathname-device pathname)
		 (pathname-directory pathname)
		 name
		 (pathname-type pathname)
		 (pathname-version pathname)))

(define (pathname-new-type pathname type)
  (make-pathname (pathname-device pathname)
		 (pathname-directory pathname)
		 (pathname-name pathname)
		 type
		 (pathname-version pathname)))

(define (pathname-new-version pathname version)
  (make-pathname (pathname-device pathname)
		 (pathname-directory pathname)
		 (pathname-name pathname)
		 (pathname-type pathname)
		 version))

;;;; Pathname Syntax

(define (->pathname object)
  (cond ((pathname? object) object)
	((string? object) (string->pathname object))
	((symbol? object) (symbol->pathname object))
	(else (error "Unable to coerce into pathname" object))))

(define (string->pathname string)
  (parse-pathname string make-pathname))

(define (pathname->string pathname)
  (pathname-unparse (pathname-device pathname)
		    (pathname-directory pathname)
		    (pathname-name pathname)
		    (pathname-type pathname)
		    (pathname-version pathname)))

(define (pathname-directory-string pathname)
  (pathname-unparse (pathname-device pathname)
		    (pathname-directory pathname)
		    false
		    false
		    false))

(define (pathname-name-string pathname)
  (pathname-unparse false
		    false
		    (pathname-name pathname)
		    (pathname-type pathname)
		    (pathname-version pathname)))

(define (pathname-components pathname receiver)
  (receiver (pathname-device pathname)
	    (pathname-directory pathname)
	    (pathname-name pathname)
	    (pathname-type pathname)
	    (pathname-version pathname)))

(define (pathname-extract pathname . fields)
  (make-pathname (and (memq 'DEVICE fields)
		      (pathname-device pathname))
		 (and (memq 'DIRECTORY fields)
		      (pathname-directory pathname))
		 (and (memq 'NAME fields)
		      (pathname-name pathname))
		 (and (memq 'TYPE fields)
		      (pathname-type pathname))
		 (and (memq 'VERSION fields)
		      (pathname-version pathname))))

(define (pathname-extract-string pathname . fields)
  (pathname-unparse (and (memq 'DEVICE fields)
			 (pathname-device pathname))
		    (and (memq 'DIRECTORY fields)
			 (pathname-directory pathname))
		    (and (memq 'NAME fields)
			 (pathname-name pathname))
		    (and (memq 'TYPE fields)
			 (pathname-type pathname))
		    (and (memq 'VERSION fields)
			 (pathname-version pathname))))

;;;; Pathname Merging

(define (pathname->absolute-pathname pathname)
  (merge-pathnames pathname (working-directory-pathname)))

(define (merge-pathnames pathname default)
  (make-pathname
   (or (pathname-device pathname) (pathname-device default))
   (simplify-directory
    (let ((directory (pathname-directory pathname))
	  (default (pathname-directory default)))
      (cond ((null? directory) default)
	    ((or (eq? directory 'UNSPECIFIC)
		 (null? default)
		 (eq? default 'UNSPECIFIC))
	     directory)
	    ((pair? directory)
	     (cond ((eq? (car directory) 'ROOT) directory)
		   ((pair? default) (append default directory))
		   (else (error "Illegal pathname directory" default))))
	    (else (error "Illegal pathname directory" directory)))))
   (or (pathname-name pathname) (pathname-name default))
   (or (pathname-type pathname) (pathname-type default))
   (or (pathname-version pathname) (pathname-version default))))

(define simplify-directory)
(let ()

(set! simplify-directory
  (named-lambda (simplify-directory directory)
    (cond ((not (pair? directory)) directory)
	  ((eq? (car directory) 'ROOT)
	   (cons 'ROOT (simplify-tail (simplify-root-tail (cdr directory)))))
	  (else (simplify-tail directory)))))

(define (simplify-root-tail directory)
  (if (and (pair? directory)
	   (memq (car directory) '(SELF UP)))
      (simplify-root-tail (cdr directory))
      directory))

(define (simplify-tail directory)
  (cond ((not (pair? directory)) directory)
	((eq? (car directory) 'SELF) (simplify-tail (cdr directory)))
	((not (pair? (cdr directory))) directory)
	((eq? (cadr directory) 'UP) (simplify-tail (cddr directory)))
	(else (cons (car directory) (simplify-tail (cdr directory))))))

)

;;;; Truenames

(define pathname->input-truename
  (let ((truename-exists?
	 (let ((file-exists? (make-primitive-procedure 'FILE-EXISTS?)))
	   (lambda (pathname)
	     (and (file-exists? (pathname->string pathname))
		  pathname)))))
    (named-lambda (pathname->input-truename pathname)
      (let ((pathname (pathname->absolute-pathname pathname)))
	(cond ((not (eq? 'NEWEST (pathname-version pathname)))
	       (truename-exists? pathname))
	      ((not pathname-newest)
	       (truename-exists? (pathname-new-version pathname false)))
	      (else
	       (pathname-newest pathname)))))))

(define (pathname->output-truename pathname)
  (let ((pathname (pathname->absolute-pathname pathname)))
    (if (eq? 'NEWEST (pathname-version pathname))
	(pathname-new-version
	 pathname
	 (and pathname-newest
	      (let ((greatest (pathname-newest pathname)))
		(if greatest
		    (let ((version (pathname-version greatest)))
		      (and version
			   (1+ version)))
		    1))))
	pathname)))

(define (canonicalize-input-filename filename)
  (let ((pathname (->pathname filename)))
    (let ((truename (pathname->input-truename pathname)))
      (if (not truename) (error "No such file" pathname))
      (pathname->string truename))))

(define (canonicalize-output-filename filename)
  (pathname->string (pathname->output-truename (->pathname filename))))

(define (file-exists? filename)
  (pathname->input-truename (->pathname filename)))
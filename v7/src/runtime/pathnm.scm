#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/pathnm.scm,v 14.13 1991/08/23 23:26:48 arthur Exp $

Copyright (c) 1988-91 Massachusetts Institute of Technology

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

;;;; Pathnames
;;; package: (runtime pathname)

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

(define-structure (pathname
		   (named (string->symbol "#[(runtime pathname)pathname]"))
		   (copier pathname-copy)
		   (print-procedure
		    (unparser/standard-method 'PATHNAME
		      (lambda (state pathname)
			(unparse-object state (pathname->string pathname))))))
  (host false read-only true)
  (device false read-only true)
  (directory false read-only true)
  (name false read-only true)
  (type false read-only true)
  (version false read-only true))

(define (pathname-components pathname receiver)
  (receiver (pathname-host pathname)
	    (pathname-device pathname)
	    (pathname-directory pathname)
	    (pathname-name pathname)
	    (pathname-type pathname)
	    (pathname-version pathname)))

(define (pathname-absolute? pathname)
  (let ((directory (pathname-directory pathname)))
    (and (pair? directory)
	 (eq? (car directory) 'ROOT))))

(define (pathname-relative? pathname pathname*)
  (and (equal? (pathname-host pathname)
	       (pathname-host pathname*))
       (equal? (pathname-device pathname)
	       (pathname-device pathname*))
       (let loop
	   ((directory (pathname-directory pathname))
	    (directory* (pathname-directory pathname*)))
	 (if (null? directory*)
	     (make-pathname false
			    false
			    directory
			    (pathname-name pathname)
			    (pathname-type pathname)
			    (pathname-version pathname))
	     (and (not (null? directory))
		  (equal? (car directory) (car directory*))
		  (loop (cdr directory) (cdr directory*)))))))

(define (pathname-directory-path pathname)
  (make-pathname (pathname-host pathname)
		 (pathname-device pathname)
		 (pathname-directory pathname)
		 false
		 false
		 false))

(define (pathname-name-path pathname)
  (make-pathname false
		 false
		 false
		 (pathname-name pathname)
		 (pathname-type pathname)
		 (pathname-version pathname)))

(define (pathname-new-host pathname host)
  (make-pathname host
		 (pathname-device pathname)
		 (pathname-directory pathname)
		 (pathname-name pathname)
		 (pathname-type pathname)
		 (pathname-version pathname)))

(define (pathname-new-device pathname device)
  (make-pathname (pathname-host pathname)
		 device
		 (pathname-directory pathname)
		 (pathname-name pathname)
		 (pathname-type pathname)
		 (pathname-version pathname)))

(define (pathname-new-directory pathname directory)
  (make-pathname (pathname-host pathname)
		 (pathname-device pathname)
		 directory
		 (pathname-name pathname)
		 (pathname-type pathname)
		 (pathname-version pathname)))

(define (pathname-new-name pathname name)
  (make-pathname (pathname-host pathname)
		 (pathname-device pathname)
		 (pathname-directory pathname)
		 name
		 (pathname-type pathname)
		 (pathname-version pathname)))

(define (pathname-new-type pathname type)
  (make-pathname (pathname-host pathname)
		 (pathname-device pathname)
		 (pathname-directory pathname)
		 (pathname-name pathname)
		 type
		 (pathname-version pathname)))

(define (pathname-new-version pathname version)
  (make-pathname (pathname-host pathname)
		 (pathname-device pathname)
		 (pathname-directory pathname)
		 (pathname-name pathname)
		 (pathname-type pathname)
		 version))

(define (pathname-default-host pathname host)
  (if (pathname-host pathname)
      pathname
      (pathname-new-host pathname host)))

(define (pathname-default-device pathname device)
  (if (pathname-device pathname)
      pathname
      (pathname-new-device pathname device)))

(define (pathname-default-directory pathname directory)
  (if (pathname-directory pathname)
      pathname
      (pathname-new-directory pathname directory)))

(define (pathname-default-name pathname name)
  (if (pathname-name pathname)
      pathname
      (pathname-new-name pathname name)))

(define (pathname-default-type pathname type)
  (if (pathname-type pathname)
      pathname
      (pathname-new-type pathname type)))

(define (pathname-default-version pathname version)
  (if (pathname-version pathname)
      pathname
      (pathname-new-version pathname version)))

(define (pathname-default pathname host device directory name type version)
  (make-pathname (or (pathname-host pathname) host)
		 (or (pathname-device pathname) device)
		 (or (pathname-directory pathname) directory)
		 (or (pathname-name pathname) name)
		 (or (pathname-type pathname) type)
		 (or (pathname-version pathname) version)))

;;;; Pathname Syntax

(define (->pathname object)
  (cond ((pathname? object) object)
	((string? object) (string->pathname object))
	((symbol? object) (symbol->pathname object))
	(else (error "Unable to coerce into pathname" object))))

(define (string->pathname string)
  (parse-pathname string make-pathname))

(define (pathname->string pathname)
  (pathname-unparse (pathname-host pathname)
		    (pathname-device pathname)
		    (pathname-directory pathname)
		    (pathname-name pathname)
		    (pathname-type pathname)
		    (pathname-version pathname)))

(define (pathname-directory-string pathname)
  (pathname-unparse (pathname-host pathname)
		    (pathname-device pathname)
		    (pathname-directory pathname)
		    false
		    false
		    false))

(define (pathname-name-string pathname)
  (pathname-unparse false
		    false
		    false
		    (pathname-name pathname)
		    (pathname-type pathname)
		    (pathname-version pathname)))

;;;; Pathname Merging

(define (pathname->absolute-pathname pathname)
  (merge-pathnames pathname (working-directory-pathname)))

(define (merge-pathnames pathname default)
  (make-pathname
   (or (pathname-host pathname) (pathname-host default))
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

(define (simplify-directory directory)
  (if (or (null? directory)
	  (not (list? directory)))
      directory
      (let ((head (car directory))
	    (tail (delq 'SELF (cdr directory))))
	(if (eq? head 'ROOT)
	    (cons 'ROOT (simplify-tail (simplify-root-tail tail)))
	    (simplify-tail (cons head tail))))))

(define (simplify-root-tail directory)
  (if (and (not (null? directory))
	   (eq? (car directory) 'UP))
      (simplify-root-tail (cdr directory))
      directory))

(define (simplify-tail directory)
  (reverse!
   (let loop ((elements (reverse directory)))
     (if (null? elements)
	 '()
	 (let ((head (car elements))
	       (tail (loop (cdr elements))))
	   (if (and (eq? head 'UP)
		    (not (null? tail))
		    (not (eq? (car tail) 'UP)))
	       (cdr tail)
	       (cons head tail)))))))

;;;; Truenames

(define (canonicalize-input-filename filename)
  (pathname->string (canonicalize-input-pathname filename)))

(define (canonicalize-input-pathname filename)
  (let ((pathname (->pathname filename)))
    (let ((truename (pathname->input-truename pathname)))
      (or truename
	  (canonicalize-input-pathname
	   (error:open-file pathname "The file does not exist."))))))

(define (pathname->input-truename pathname)
  (let ((pathname (pathname->absolute-pathname pathname))
	(truename-exists?
	 (lambda (pathname)
	   (and ((ucode-primitive file-exists?) (pathname->string pathname))
		pathname))))
    (cond ((not (eq? 'NEWEST (pathname-version pathname)))
	   (truename-exists? pathname))
	  ((not pathname-newest)
	   (truename-exists? (pathname-new-version pathname false)))
	  (else
	   (pathname-newest pathname)))))

(define (canonicalize-output-filename filename)
  (pathname->string (canonicalize-output-pathname filename)))

(define-integrable (canonicalize-output-pathname filename)
  (pathname->output-truename (->pathname filename)))

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

(define (canonicalize-overwrite-filename filename)
  (pathname->string (canonicalize-overwrite-pathname filename)))

(define-integrable (canonicalize-overwrite-pathname filename)
  (pathname->overwrite-truename (->pathname filename)))

(define (pathname->overwrite-truename pathname)
  (let ((pathname (pathname->absolute-pathname pathname)))
    (cond ((not (eq? 'NEWEST (pathname-version pathname)))
	   pathname)
	  ((not pathname-newest)
	   (pathname-new-version pathname false))
	  ((pathname-newest pathname))
	  (else
	   (pathname-new-version pathname 1)))))

(define (file-exists? filename)
  (pathname->input-truename (->pathname filename)))

(define (init-file-truename)
  (let ((pathname (init-file-pathname)))
    (and pathname
	 (or (pathname->input-truename
	      (merge-pathnames pathname (working-directory-pathname)))
	     (pathname->input-truename
	      (merge-pathnames pathname (home-directory-pathname)))))))

(define (initialize-package!)
  (reset-library-directory-path!)
  (add-event-receiver! event:after-restore reset-library-directory-path!))

(define (reset-library-directory-path!)
  (set! library-directory-path
	(if (implemented-primitive-procedure? microcode-library-path)
	    (map (lambda (filename)
		   (pathname-as-directory (string->pathname filename)))
		 (vector->list (microcode-library-path)))
	    (list 
	     (pathname-directory-path
	      (string->pathname (microcode-tables-filename))))))
  unspecific)

(define-primitives
  (microcode-library-path 0)
  (microcode-tables-filename 0))

(define library-directory-path)

(define (system-library-pathname pathname)
  (if (and (pathname-absolute? pathname)
	   (pathname->input-truename pathname))
      pathname
      (let loop ((directories library-directory-path))
	(if (null? directories)
	    (system-library-pathname
	     (->pathname
	      (error:open-file pathname
			       "Cannot find file in system library path.")))
	    (or (pathname->input-truename
		 (merge-pathnames pathname (car directories)))
		(loop (cdr directories)))))))

(define (system-library-directory-pathname pathname)
  (if (not pathname)
      (let ((pathname
	     (list-search-positive library-directory-path file-directory?)))
	(if (not pathname)
	    (error "can't find system library directory"))
	(pathname-as-directory pathname))
      (let loop ((directories library-directory-path))
	(and (not (null? directories))
	     (let ((pathname (merge-pathnames pathname (car directories))))
	       (if (file-directory? pathname)
		   (pathname-as-directory pathname)
		   (loop (cdr directories))))))))
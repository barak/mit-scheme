;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/pathnm.scm,v 13.42 1987/03/12 02:16:14 jinx Exp $
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

;;; A pathname component is normally one of:

;;; * A string, which is the literal component.

;;; * 'WILD, meaning that the component is wildcarded.  Such
;;; components may have special meaning to certain directory
;;; operations.

;;; * 'UNSPECIFIC, meaning that the component was supplied, but null.
;;; This means about the same thing as "". (maybe it should be
;;; eliminated in favor of that?)

;;; * #F, meaning that the component was not supplied.  This has
;;; special meaning to `merge-pathnames', in which such components are
;;; substituted.

;;; A pathname consists of 5 components, not all necessarily
;;; meaningful, as follows:

;;; * The DEVICE is usually a physical device, as in the Twenex `ps:'.

;;; * The DIRECTORY is a list of components.  If the first component
;;; is the null string, then the directory path is absolute.
;;; Otherwise it is relative.

;;; * The NAME is the proper name part of the filename.

;;; * The TYPE usually indicates something about the contents of the
;;; file.  Certain system procedures will default the type to standard
;;; type strings.

;;; * The VERSION is special.  Unlike an ordinary component, it is
;;; never a string, but may be either a positive integer, 'NEWEST,
;;; 'WILD, 'UNSPECIFIC, or #F.  Many system procedures will default
;;; the version to 'NEWEST, which means to search the directory for
;;; the highest version numbered file.

;;; This file requires the following procedures and variables which
;;; define the conventions for the particular file system in use:
;;;
;;; (symbol->pathname symbol)
;;; (string->pathname string)
;;; (pathname-unparse device directory name type version)
;;; (pathname-unparse-name name type version)
;;; (simplify-directory directory)
;;; working-directory-package
;;; (access reset! working-directory-package)
;;; init-file-pathname
;;; (home-directory-pathname)
;;; (working-directory-pathname)
;;; (set-working-directory-pathname! name)
;;;
;;; See the files unkpth.scm, vmspth.scm, or unxpth.scm for examples.

;;;; Basic Pathnames

(define (pathname? object)
  (and (environment? object)
       (eq? (environment-procedure object) make-pathname)))

(define (make-pathname device directory name type version)
  (define string #F)

  (define (:print-self)
    (unparse-with-brackets
     (lambda ()
       (write-string "PATHNAME ")
       (write (pathname->string (the-environment))))))

  (the-environment))

(define (pathname-components pathname receiver)
  (receiver (access device pathname)
	    (access directory pathname)
	    (access name pathname)
	    (access type pathname)
	    (access version pathname)))

(define (pathname-device pathname)
  (access device pathname))

(define (pathname-directory pathname)
  (access directory pathname))

(define (pathname-name pathname)
  (access name pathname))

(define (pathname-type pathname)
  (access type pathname))

(define (pathname-version pathname)
  (access version pathname))

(define (pathname-extract pathname . fields)
  (pathname-components pathname
    (lambda (device directory name type version)
      (make-pathname (and (memq 'DEVICE fields) device)
		     (and (memq 'DIRECTORY fields) directory)
		     (and (memq 'NAME fields) name)
		     (and (memq 'TYPE fields) type)
		     (and (memq 'VERSION fields) version)))))

(define (pathname-absolute? pathname)
  (let ((directory (pathname-directory pathname)))
    (and (not (null? directory))
	 (string-null? (car directory)))))

(define (pathname-new-device pathname device)
  (pathname-components pathname
    (lambda (old-device directory name type version)
      (make-pathname device directory name type version))))

(define (pathname-new-directory pathname directory)
  (pathname-components pathname
    (lambda (device old-directory name type version)
      (make-pathname device directory name type version))))

(define (pathname-new-name pathname name)
  (pathname-components pathname
    (lambda (device directory old-name type version)
      (make-pathname device directory name type version))))

(define (pathname-new-type pathname type)
  (pathname-components pathname
    (lambda (device directory name old-type version)
      (make-pathname device directory name type version))))

(define (pathname-new-version pathname version)
  (pathname-components pathname
    (lambda (device directory name type old-version)
      (make-pathname device directory name type version))))

(define (pathname-directory-path pathname)
  (pathname-components pathname
    (lambda (device directory name type version)
      (make-pathname device directory #F #F #F))))

(define (pathname-directory-string pathname)
  (pathname-components pathname
    (lambda (device directory name type version)
      (pathname-unparse device directory #F #F #F))))

(define (pathname-name-path pathname)
  (pathname-components pathname
    (lambda (device directory name type version)
      (make-pathname #F #F name type version))))

(define (pathname-name-string pathname)
  (pathname-components pathname
    (lambda (device directory name type version)
      (pathname-unparse #F #F name type version))))

;;;; Parse and unparse.

;;; Defined in terms of operating system dependent procedures.

(define (->pathname object)
  (cond ((pathname? object) object)
	((string? object) (string->pathname object))
	((symbol? object) (symbol->pathname object))
	(else (error "Unable to coerce into pathname" object))))

(define (pathname->string pathname)
  (or (access string pathname)
      (let ((string (pathname-components pathname pathname-unparse)))
	(set! (access string pathname) string)
	string)))

(define (pathname-extract-string pathname . fields)
  (pathname-components pathname
    (lambda (device directory name type version)
      (pathname-unparse (and (memq 'DEVICE fields) device)
			(and (memq 'DIRECTORY fields) directory)
			(and (memq 'NAME fields) name)
			(and (memq 'TYPE fields) type)
			(and (memq 'VERSION fields) version)))))

;;;; Merging pathnames

(define (merge-pathnames pathname default)
  (make-pathname (or (pathname-device pathname) (pathname-device default))
		 (simplify-directory
		  (let ((directory (pathname-directory pathname)))
		    (cond ((null? directory) (pathname-directory default))
			  ((string-null? (car directory)) directory)
			  (else
			   (append (pathname-directory default) directory)))))
		 (or (pathname-name pathname) (pathname-name default))
		 (or (pathname-type pathname) (pathname-type default))
		 (or (pathname-version pathname) (pathname-version default))))

(define (pathname-as-directory pathname)
  (let ((file (pathname-unparse-name (pathname-name pathname)
				     (pathname-type pathname)
				     (pathname-version pathname))))
    (if (string-null? file)
	pathname
	(make-pathname (pathname-device pathname)
		       (append (pathname-directory pathname)
			       (list file))
		       #F #F #F))))

(define (pathname->absolute-pathname pathname)
  (merge-pathnames pathname (working-directory-pathname)))

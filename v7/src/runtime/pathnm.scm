;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/pathnm.scm,v 13.41 1987/01/23 00:17:26 jinx Exp $
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

;;; A pathname consists of 5 components, as follows:

;;; * The DEVICE is usually a physical device, as in the Twenex `ps:'.
;;; Unix does not use this field.

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

;;; A note about parsing of filename strings: the standard syntax for
;;; a filename string is "<name>.<version>.<type>".  Since the Unix
;;; file system treats "." just like any other character, it is
;;; possible to give files strange names like "foo.bar.baz.mum".  In
;;; this case, the resulting name would be "foo.bar.baz", and the
;;; resulting type would be "mum".  In general, degenerate filenames
;;; (including names with non-numeric versions) are parsed such that
;;; the characters following the final "." become the type, while the
;;; characters preceding the final "." become the name.

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

;;;; Parse

(define (->pathname object)
  (cond ((pathname? object) object)
	((string? object) (string->pathname object))
	((symbol? object)
	 (string->pathname (string-downcase (symbol->string object))))
	(else (error "Unable to coerce into pathname" object))))

(define string->pathname)
(let ()

(set! string->pathname
(named-lambda (string->pathname string)
  (parse-pathname (canonicalize-filename-string string)
		  make-pathname)))

(define (parse-pathname string receiver)
  (let ((components (divide-into-components (string-trim string))))
    (if (null? components)
	(receiver #F #F #F #F #F)
	(let ((components
	       (append (expand-directory-prefixes (car components))
		       (cdr components))))
	  (parse-name (car (last-pair components))
	    (lambda (name type version)
	      (receiver #F
			(map (lambda (component)
			       (if (string=? "*" component)
				   'WILD
				   component))
			     (except-last-pair components))
			name type version)))))))

(define (divide-into-components string)
  (let ((end (string-length string)))
    (define (loop start)
      (let ((index (substring-find-next-char string start end #\/)))
	(if index
	    (cons (substring string start index)
		  (loop (1+ index)))
	    (list (substring string start end)))))
    (loop 0)))

(define (expand-directory-prefixes string)
  (if (string-null? string)
      (list string)
      (case (string-ref string 0)
	((#\$)
	 (divide-into-components
	  (get-environment-variable
	   (substring string 1 (string-length string)))))
	((#\~)
	 (let ((user-name (substring string 1 (string-length string))))
	   (divide-into-components
	    (if (string-null? user-name)
		(get-environment-variable "HOME")
		(get-user-home-directory user-name)))))
	(else (list string)))))

(define get-environment-variable
  (let ((primitive (make-primitive-procedure 'GET-ENVIRONMENT-VARIABLE)))
    (lambda (name)
      (or (primitive name)
	  (error "GET-ENVIRONMENT-VARIABLE: Unbound name" name)))))

(define get-user-home-directory
  (let ((primitive (make-primitive-procedure 'GET-USER-HOME-DIRECTORY)))
    (lambda (user-name)
      (or (primitive user-name)
	  (error "User has no home directory" user-name)))))

(define (parse-name string receiver)
  (let ((start 0)
	(end (string-length string)))
    (define (find-next-dot start)
      (substring-find-next-char string start end #\.))

    (define (find-previous-dot start)
      (substring-find-previous-char string start end #\.))

    (define (parse-version start)
      (cond ((= start end) 'UNSPECIFIC)
	    ((substring=? string start end "*" 0 1) 'WILD)
	    ((substring-find-next-char string start end #\*)
	     (substring string start end))
	    (else
	     (let ((n (digits->number (reverse! (substring->list string start
								 end))
				      1 0)))
	       (if (and n (>= n 0))
		   (if (= n 0) 'NEWEST n)
		   (substring string start end))))))

    (if (= start end)
	(receiver #F #F #F)
	(let ((index (find-next-dot start)))
	  (if index
	      (let ((start* (1+ index))
		    (name (wildify string start index)))
		(if (= start* end)
		    (receiver name 'UNSPECIFIC 'UNSPECIFIC)
		    (or (let ((index (find-next-dot start*)))
			  (and index
			       (let ((version (parse-version (1+ index))))
				 (and (not (string? version))
				      (receiver name
						(wildify string start* index)
						version)))))
			(let ((index (find-previous-dot start)))
			  (receiver (wildify string start index)
				    (wildify string (1+ index) end)
				    #F)))))
	      (receiver (wildify string start end) #F #F))))))

(define (digits->number digits weight accumulator)
  (if (null? digits)
      accumulator
      (let ((value (char->digit (car digits) 10)))
	(and value
	     (digits->number (cdr digits)
			     (* weight 10)
			     (+ (* weight value) accumulator))))))

(define (wildify string start end)
  (if (substring=? string start end "*" 0 1)
      'WILD
      (substring string start end)))

;;; end LET.
)

;;;; Unparse

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

(define pathname-unparse)
(define pathname-unparse-name)
(let ()

(set! pathname-unparse
(named-lambda (pathname-unparse device directory name type version)
  (unparse-device
   device
   (unparse-directory directory
		      (pathname-unparse-name name type version)))))

(define (unparse-device device rest)
  (let ((device-string (unparse-component device)))
    (if device-string
	(string-append device-string ":" rest)
	rest)))

(define (unparse-directory directory rest)
  (cond ((null? directory) rest)
	((pair? directory)
	 (let loop ((directory directory))
	   (let ((directory-string (unparse-component (car directory)))
		 (rest (if (null? (cdr directory))
			   rest
			   (loop (cdr directory)))))
	     (if directory-string
		 (string-append directory-string "/" rest)
		 rest))))
	(else
	 (error "Unrecognizable directory" directory))))

(set! pathname-unparse-name
(named-lambda (pathname-unparse-name name type version)
  (let ((name-string (unparse-component name))
	(type-string (unparse-component type))
	(version-string (unparse-version version)))
    (cond ((not name-string) "")
	  ((not type-string) name-string)
	  ((eq? type-string 'UNSPECIFIC) (string-append name-string "."))
	  ((not version-string) (string-append name-string "." type-string))
	  ((eq? version-string 'UNSPECIFIC)
	   (string-append name-string "." type-string "."))
	  (else
	   (string-append name-string "." type-string "." version-string))))))

(define (unparse-version version)
  (if (eq? version 'NEWEST)
      "0"
      (unparse-component version)))

(define (unparse-component component)
  (cond ((not component) #F)
	((eq? component 'UNSPECIFIC) component)
	((eq? component 'WILD) "*")
	((string? component) component)
	((and (integer? component) (> component 0))
	 (list->string (number->digits component '())))
	(else (error "Unknown component" component))))

(define (number->digits number accumulator)
  (if (zero? number)
      accumulator
      (let ((qr (integer-divide number 10)))
	(number->digits (integer-divide-quotient qr)
			(cons (digit->char (integer-divide-remainder qr))
			      accumulator)))))

;;; end LET.
)

(define merge-pathnames)
(let ()

(set! merge-pathnames
(named-lambda (merge-pathnames pathname default)
  (make-pathname (or (pathname-device pathname) (pathname-device default))
		 (simplify-directory
		  (let ((directory (pathname-directory pathname)))
		    (cond ((null? directory) (pathname-directory default))
			  ((string-null? (car directory)) directory)
			  (else
			   (append (pathname-directory default) directory)))))
		 (or (pathname-name pathname) (pathname-name default))
		 (or (pathname-type pathname) (pathname-type default))
		 (or (pathname-version pathname) (pathname-version default)))))

(define (simplify-directory directory)
  (cond ((null? directory) directory)
	((string=? (car directory) ".")
	 (simplify-directory (cdr directory)))
	((null? (cdr directory)) directory)
	((string=? (cadr directory) "..")
	 (simplify-directory (cddr directory)))
	(else
	 (cons (car directory)
	       (simplify-directory (cdr directory))))))

)

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

;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/unxpth.scm,v 1.2 1987/03/17 18:54:38 cph Exp $
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

;;;; Unix pathname parsing and unparsing.

(declare (usual-integrations))

;;; A note about parsing of filename strings: the standard syntax for
;;; a filename string is "<name>.<version>.<type>".  Since the Unix
;;; file system treats "." just like any other character, it is
;;; possible to give files strange names like "foo.bar.baz.mum".  In
;;; this case, the resulting name would be "foo.bar.baz", and the
;;; resulting type would be "mum".  In general, degenerate filenames
;;; (including names with non-numeric versions) are parsed such that
;;; the characters following the final "." become the type, while the
;;; characters preceding the final "." become the name.

;;;; Parse

(define (symbol->pathname symbol)
  (string->pathname (string-downcase (symbol->string symbol))))

(define string->pathname)
(define home-directory-pathname)
(let ()

(set! string->pathname
  (named-lambda (string->pathname string)
    (parse-pathname string make-pathname)))

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

(set! home-directory-pathname
  (lambda ()
    (make-pathname #F
		   (divide-into-components (get-environment-variable "HOME"))
		   #F
		   #F
		   #F)))	

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

(define (digits->number digits weight accumulator)
  (if (null? digits)
      accumulator
      (let ((value (char->digit (car digits) 10)))
	(and value
	     (digits->number (cdr digits)
			     (* weight 10)
			     (+ (* weight value) accumulator))))))

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

(define (wildify string start end)
  (if (substring=? string start end "*" 0 1)
      'WILD
      (substring string start end)))

;;; end LET.
)

;;;; Unparse

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
	     (string-append name-string "." type-string "."
			    version-string))))))

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

;;;; Utility for merge pathnames

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

;;;; Working Directory

(define working-directory-pathname)
(define set-working-directory-pathname!)

(define working-directory-package
  (make-environment

(define primitive
  (make-primitive-procedure 'WORKING-DIRECTORY-PATHNAME))

(define pathname)

(define (reset!)
  (set! pathname
	(string->pathname
	 (let ((string (primitive)))
	   (let ((length (string-length string)))
	     (if (or (zero? length)
		     (not (char=? #\/ (string-ref string (-1+ length)))))
		 (string-append string "/")
		 string))))))

(set! working-directory-pathname
  (named-lambda (working-directory-pathname)
    pathname))

(set! set-working-directory-pathname!
  (named-lambda (set-working-directory-pathname! name)
    (set! pathname
	  (pathname-as-directory
	   (pathname->absolute-pathname (->pathname name))))
    pathname))

;;; end WORKING-DIRECTORY-PACKAGE
))

(define init-file-pathname
  (make-pathname #F #F ".scheme" "init" #F))
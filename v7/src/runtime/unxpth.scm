;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/unxpth.scm,v 1.4 1987/07/19 21:43:50 cph Rel $
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

(define parse-pathname)
(define pathname-as-directory)
(define home-directory-pathname)
(let ()

(set! parse-pathname
  (named-lambda (parse-pathname string receiver)
    (let ((end (string-length string)))
      (parse-device string 0 end
	(lambda (device start)
	  (let ((components
		 (let ((components
			(substring-components string start end #\/)))
		   (append (expand-directory-prefixes (car components))
			   (cdr components)))))
	    (parse-name (car (last-pair components))
	      (lambda (name type version)
		(receiver device
			  (parse-directory-components
			   (except-last-pair components))
			  name type version)))))))))

(define (parse-directory-components components)
  (if (null? components)
      '()
      (cons (if (string-null? (car components))
		'ROOT
		(parse-directory-component (car components)))
	    (map parse-directory-component (cdr components)))))

(set! pathname-as-directory
  (named-lambda (pathname-as-directory pathname)
    (make-pathname
     (pathname-device pathname)
     (let ((directory (pathname-directory pathname)))
       (let ((file (pathname-unparse-name (pathname-name pathname)
					  (pathname-type pathname)
					  (pathname-version pathname))))
	 (if (string-null? file)
	     directory
	     (let ((file-components (list (parse-directory-component file))))
	       (cond ((or (null? directory) (eq? directory 'UNSPECIFIC))
		      file-components)
		     ((pair? directory)
		      (append directory file-components))
		     (else (error "Illegal pathname directory" directory)))))))
     false false false)))

(define (parse-device string start end receiver)
  (let ((index (substring-find-next-char string start end #\:)))
    (if index
	(receiver (substring string start index) (1+ index))
	(receiver false start))))

(define (parse-directory-component component)
  (cond ((string=? "*" component) 'WILD)
	((string=? "." component) 'SELF)
	((string=? ".." component) 'UP)
	(else component)))

(define (expand-directory-prefixes string)
  (if (string-null? string)
      (list string)
      (case (string-ref string 0)
	((#\$)
	 (string-components
	  (get-environment-variable
	   (substring string 1 (string-length string)))
	  #\/))
	((#\~)
	 (let ((user-name (substring string 1 (string-length string))))
	   (string-components
	    (if (string-null? user-name)
		(get-environment-variable "HOME")
		(get-user-home-directory user-name))
	    #\/)))
	(else (list string)))))

(set! home-directory-pathname
  (lambda ()
    (pathname-as-directory
     (string->pathname (get-environment-variable "HOME")))))

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
      (cond ((= start end) "")
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
	(receiver false false false)
	(let ((index (find-next-dot start)))
	  (if index
	      (let ((start* (1+ index))
		    (name (wildify string start index)))
		(if (= start* end)
		    (receiver name "" "")
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
				    false)))))
	      (receiver (wildify string start end) false false))))))

(define (wildify string start end)
  (if (substring=? string start end "*" 0 1)
      'WILD
      (substring string start end)))

(define (string-components string delimiter)
  (substring-components string 0 (string-length string) delimiter))

(define (substring-components string start end delimiter)
  (define (loop start)
    (let ((index (substring-find-next-char string start end delimiter)))
      (if index
	  (cons (substring string start index)
		(loop (1+ index)))
	  (list (substring string start end)))))
  (loop start))

(define (digits->number digits weight accumulator)
  (if (null? digits)
      accumulator
      (let ((value (char->digit (car digits) 10)))
	(and value
	     (digits->number (cdr digits)
			     (* weight 10)
			     (+ (* weight value) accumulator))))))

;;; end LET.
)

;;;; Unparse

(define pathname-unparse)
(define pathname-unparse-name)
(let ()

(set! pathname-unparse
  (named-lambda (pathname-unparse device directory name type version)
    (string-append (let ((device-string (unparse-component device)))
		     (if device-string
			 (string-append device-string ":")
			 ""))
		   (unparse-directory directory)
		   (pathname-unparse-name name type version))))

(define (unparse-directory directory)
  (define (loop directory)
    (if (null? directory)
	""
	(string-append (unparse-directory-component (car directory))
		       "/"
		       (loop (cdr directory)))))
  (cond ((null? directory) "")
	((pair? directory)
	 (string-append (if (eq? (car directory) 'ROOT)
			    ""
			    (unparse-directory-component (car directory)))
			"/"
			(loop (cdr directory))))
	(else (error "Illegal pathname directory" directory))))

(define (unparse-directory-component component)
  (cond ((eq? component 'WILD) "*")
	((eq? component 'SELF) ".")
	((eq? component 'UP) "..")
	((string? component) component)
	(else (error "Illegal pathname directory component" component))))

(set! pathname-unparse-name
  (named-lambda (pathname-unparse-name name type version)
    (let ((name (unparse-component name))
	  (type (unparse-component type))
	  (version (unparse-version version)))
      (cond ((not name) "")
	    ((not type) name)
	    ((not version) (string-append name "." type))
	    (else (string-append name "." type "." version))))))

(define (unparse-component component)
  (cond ((or (not component) (string? component)) component)
	((eq? component 'UNSPECIFIC) false)
	((eq? component 'WILD) "*")
	(else (error "Illegal pathname component" component))))

(define (unparse-version version)
  (cond ((or (not version) (string? version)) version)
	((eq? version 'UNSPECIFIC) false)
	((eq? version 'WILD) "*")
	((eq? version 'NEWEST) "0")
	((and (integer? version) (> version 0))
	 (list->string (number->digits version '())))
	(else (error "Illegal pathname version" version))))

(define (number->digits number accumulator)
  (if (zero? number)
      accumulator
      (let ((qr (integer-divide number 10)))
	(number->digits (integer-divide-quotient qr)
			(cons (digit->char (integer-divide-remainder qr))
			      accumulator)))))

;;; end LET.
)

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
  (string->pathname ".scheme.init"))
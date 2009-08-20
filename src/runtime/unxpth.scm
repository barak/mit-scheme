#| -*-Scheme-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; Unix Pathnames
;;; package: (runtime pathname unix)

(declare (usual-integrations))

(define (make-unix-host-type index)
  (make-host-type index
		  'UNIX
		  unix/parse-namestring
		  unix/pathname->namestring
		  unix/make-pathname
		  unix/pathname-wild?
		  unix/directory-pathname?
		  unix/directory-pathname
		  unix/file-pathname
		  unix/pathname-as-directory
		  unix/directory-pathname-as-file
		  unix/pathname->truename
		  unix/user-homedir-pathname
		  unix/init-file-pathname
		  unix/pathname-simplify))

(define (initialize-package!)
  (add-pathname-host-type! 'UNIX make-unix-host-type))

;;;; Pathname Parser

(define (unix/parse-namestring string host)
  (let ((end (string-length string)))
    (let ((components
	   (expand-directory-prefixes
	    (substring-components string 0 end #\/))))
      (parse-name (car (last-pair components))
	(lambda (name type)
	  (%make-pathname host
			  'UNSPECIFIC
			  (let ((components (except-last-pair components)))
			    (and (not (null? components))
				 (simplify-directory
				  (if (string=? "" (car components))
				      (cons 'ABSOLUTE
					    (parse-directory-components
					     (cdr components)))
				      (cons 'RELATIVE
					    (parse-directory-components
					     components))))))
			  name
			  type
			  'UNSPECIFIC))))))

(define (expand-directory-prefixes components)
  (let ((string (car components))
	(replace-head
	 (lambda (string)
	   ;; If STRING has a trailing slash, and it's followed by a
	   ;; slash, drop the trailing slash to avoid doubling.
	   (let ((head (string-components string #\/)))
	     (append (if (and (pair? (cdr components))
			      (pair? (cdr head))
			      (string-null? (car (last-pair head))))
			 (except-last-pair head)
			 head)
		     (cdr components))))))
    (let ((end (string-length string)))
      (if (or (= 0 end)
	      (not *expand-directory-prefixes?*))
	  components
	  (case (string-ref string 0)
	    ((#\$)
	     (if (= 1 end)
		 components
		 (let ((value
			(get-environment-variable (substring string 1 end))))
		   (if (not value)
		       components
		       (replace-head value)))))
	    ((#\~)
	     (let ((expansion
		    (ignore-errors
		     (lambda ()
		       (if (= 1 end)
			   (current-home-directory)
			   (user-home-directory (substring string 1 end)))))))
	       (if (condition? expansion)
		   components
		   (replace-head (->namestring expansion)))))
	    (else components))))))

(define (simplify-directory directory)
  (if (and (eq? (car directory) 'RELATIVE) (null? (cdr directory)))
      #f
      directory))

(define (parse-directory-components components)
  (if (there-exists? components string-null?)
      (error "Directory contains null component:" components))
  (map parse-directory-component components))

(define (parse-directory-component component)
  (if (string=? ".." component)
      'UP
      component))

(define (string-components string delimiter)
  (substring-components string 0 (string-length string) delimiter))

(define (substring-components string start end delimiter)
  (let loop ((start start))
    (let ((index (substring-find-next-char string start end delimiter)))
      (if index
	  (cons (substring string start index) (loop (+ index 1)))
	  (list (substring string start end))))))

(define (parse-name string receiver)
  (let ((end (string-length string)))
    (let ((dot (substring-find-previous-char string 0 end #\.)))
      (if (or (not dot)
	      (= dot 0)
	      (= dot (- end 1))
	      (char=? #\. (string-ref string (- dot 1))))
	  (receiver (cond ((= end 0) #f)
			  ((string=? "*" string) 'WILD)
			  (else string))
		    #f)
	  (receiver (extract string 0 dot)
		    (extract string (+ dot 1) end))))))

(define (extract string start end)
  (if (substring=? string start end "*" 0 1)
      'WILD
      (substring string start end)))

;;;; Pathname Unparser

(define (unix/pathname->namestring pathname)
  (string-append (unparse-directory (%pathname-directory pathname))
		 (unparse-name (%pathname-name pathname)
			       (%pathname-type pathname))))

(define (unparse-directory directory)
  (cond ((not directory)
	 "")
	((pair? directory)
	 (string-append
	  (if (eq? (car directory) 'ABSOLUTE) "/" "")
	  (let loop ((directory (cdr directory)))
	    (if (null? directory)
		""
		(string-append (unparse-directory-component (car directory))
			       "/"
			       (loop (cdr directory)))))))
	(else
	 (error:illegal-pathname-component directory "directory"))))

(define (unparse-directory-component component)
  (cond ((eq? component 'UP) "..")
	((string? component) component)
	(else
	 (error:illegal-pathname-component component "directory component"))))

(define (unparse-name name type)
  (let ((name (or (unparse-component name) ""))
	(type (unparse-component type)))
    (if type
	(string-append name "." type)
	name)))

(define (unparse-component component)
  (cond ((or (not component) (string? component)) component)
	((eq? component 'WILD) "*")
	(else (error:illegal-pathname-component component "component"))))

;;;; Pathname Constructors

(define (unix/make-pathname host device directory name type version)
  (%make-pathname
   host
   (if (memq device '(#F UNSPECIFIC))
       'UNSPECIFIC
       (error:illegal-pathname-component device "device"))
   (cond ((not directory)
	  directory)
	 ((and (pair? directory)
	       (memq (car directory) '(RELATIVE ABSOLUTE))
	       (list-of-type? (cdr directory)
		 (lambda (element)
		   (if (string? element)
		       (not (string-null? element))
		       (eq? element 'UP)))))
	  (simplify-directory directory))
	 (else
	  (error:illegal-pathname-component directory "directory")))
   (if (or (memq name '(#F WILD))
	   (and (string? name) (not (string-null? name))))
       name
       (error:illegal-pathname-component name "name"))
   (if (or (memq type '(#F WILD))
	   (and (string? type) (not (string-null? type))))
       type
       (error:illegal-pathname-component type "type"))
   (if (memq version '(#F UNSPECIFIC WILD NEWEST))
       'UNSPECIFIC
       (error:illegal-pathname-component version "version"))))

(define (unix/directory-pathname? pathname)
  (and (not (%pathname-name pathname))
       (not (%pathname-type pathname))))

(define (unix/directory-pathname pathname)
  (%make-pathname (%pathname-host pathname)
		  (%pathname-device pathname)
		  (%pathname-directory pathname)
		  #f
		  #f
		  'UNSPECIFIC))

(define (unix/file-pathname pathname)
  (%make-pathname (%pathname-host pathname)
		  'UNSPECIFIC
		  #f
		  (%pathname-name pathname)
		  (%pathname-type pathname)
		  (%pathname-version pathname)))

(define (unix/pathname-as-directory pathname)
  (let ((name (%pathname-name pathname))
	(type (%pathname-type pathname)))
    (if (or name type)
	(%make-pathname
	 (%pathname-host pathname)
	 'UNSPECIFIC
	 (let ((directory (%pathname-directory pathname))
	       (component
		(parse-directory-component (unparse-name name type))))
	   (cond ((not (pair? directory))
		  (list 'RELATIVE component))
		 ((equal? component ".")
		  directory)
		 (else
		  (append directory (list component)))))
	 #f
	 #f
	 'UNSPECIFIC)
	pathname)))

(define (unix/directory-pathname-as-file pathname)
  (let ((directory (%pathname-directory pathname)))
    (if (not (and (pair? directory)
		  (or (eq? 'ABSOLUTE (car directory))
		      (pair? (cdr directory)))))
	(error:bad-range-argument pathname 'DIRECTORY-PATHNAME-AS-FILE))
    (if (or (%pathname-name pathname)
	    (%pathname-type pathname)
	    (null? (cdr directory)))
	;; Root directory can't be represented as a file, because the
	;; name field of a pathname must be a non-null string.  We
	;; could signal an error here, but instead we'll just return
	;; the original pathname and leave it to the caller to deal
	;; with any problems this might cause.
	pathname
	(parse-name (unparse-directory-component (car (last-pair directory)))
	  (lambda (name type)
	    (%make-pathname (%pathname-host pathname)
			    'UNSPECIFIC
			    (simplify-directory (except-last-pair directory))
			    name
			    type
			    'UNSPECIFIC))))))

;;;; Miscellaneous

(define (unix/pathname-wild? pathname)
  (or (eq? 'WILD (%pathname-name pathname))
      (eq? 'WILD (%pathname-type pathname))))

(define (unix/pathname->truename pathname)
  (if (file-exists-direct? pathname)
      pathname
      (unix/pathname->truename
       (error:file-operation pathname "find" "file" "file does not exist"
			     unix/pathname->truename (list pathname)))))

(define (unix/user-homedir-pathname host)
  (and (eq? host local-host)
       (pathname-as-directory (current-home-directory))))

(define (unix/init-file-pathname host)
  (let ((pathname
	 (merge-pathnames ".scheme.init" (unix/user-homedir-pathname host))))
    (and (file-exists? pathname)
	 pathname)))

(define (unix/pathname-simplify pathname)
  (if (pair? (pathname-directory pathname))
      (let loop ((pathname pathname) (np 1))
	(let ((directory (pathname-directory pathname)))
	  (let scan ((p (list-tail directory np)) (np np))
	    (if (pair? p)
		(if (and (not (eq? (car p) 'UP))
			 (pair? (cdr p))
			 (eq? (cadr p) 'UP))
		    (let ((pathname*
			   (pathname-new-directory pathname
						   (delete-up directory p))))
		      (if (file-eq? (directory-pathname pathname)
				    (directory-pathname pathname*))
			  (loop pathname* np)
			  (scan (cddr p) (+ np 2))))
		    (scan (cdr p) (+ np 1)))
		pathname))))
      pathname))

(define (delete-up directory p)
  (let loop ((p* directory))
    (if (eq? p* p)
	(cddr p*)
	(cons (car p*) (loop (cdr p*))))))

(define (file-eq? p1 p2)
  ((ucode-primitive file-eq? 2) (->namestring (merge-pathnames p1))
				(->namestring (merge-pathnames p2))))
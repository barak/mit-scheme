#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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
		  'unix
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
  (add-pathname-host-type! 'unix make-unix-host-type))

;;;; Pathname Parser

(define (unix/parse-namestring string host)
  (let ((end (string-length string)))
    (let ((components
	   (expand-directory-prefixes
	    (substring-components string 0 end #\/))))
      (parse-name (car (last-pair components))
	(lambda (name type)
	  (%make-pathname host
			  'unspecific
			  (let ((components (except-last-pair components)))
			    (and (pair? components)
				 (simplify-directory
				  (if (fix:= 0
					     (string-length (car components)))
				      (cons 'absolute
					    (parse-directory-components
					     (cdr components)))
				      (cons 'relative
					    (parse-directory-components
					     components))))))
			  name
			  type
			  'unspecific))))))

(define (expand-directory-prefixes components)
  (let ((string (car components))
	(replace-head
	 (lambda (string)
	   (append (string-components string #\/)
		   (cdr components)))))
    (let ((end (string-length string)))
      (if (or (fix:= 0 end)
	      (not (*expand-directory-prefixes?*)))
	  components
	  (case (string-ref string 0)
	    ((#\$)
	     (if (fix:= 1 end)
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
		       (if (fix:= 1 end)
			   (current-home-directory)
			   (user-home-directory (substring string 1 end)))))))
	       (if (condition? expansion)
		   components
		   (replace-head (->namestring expansion)))))
	    (else components))))))

(define (simplify-directory directory)
  (if (and (eq? (car directory) 'relative) (null? (cdr directory)))
      #f
      directory))

(define (parse-directory-components components)
  (map parse-directory-component
       (remove (lambda (component)
		 (fix:= 0 (string-length component)))
	       components)))

(define (parse-directory-component component)
  (cond ((string=? ".." component) 'up)
	((string=? "." component) 'here)
	(else component)))

(define (string-components string delimiter)
  (substring-components string 0 (string-length string) delimiter))

(define (substring-components string start end delimiter)
  (let loop ((start start))
    (let ((index (string-find-next-char string delimiter start end)))
      (if index
	  (cons (substring string start index) (loop (fix:+ index 1)))
	  (list (substring string start end))))))

(define (parse-name string receiver)
  (let ((end (string-length string)))
    (let ((dot (string-find-previous-char string #\.)))
      (if (or (not dot)
	      (fix:= dot 0)
	      (fix:= dot (fix:- end 1))
	      (char=? #\. (string-ref string (fix:- dot 1))))
	  (receiver (cond ((fix:= end 0) #f)
			  ((string=? "*" string) 'wild)
			  (else string))
		    #f)
	  (receiver (extract string 0 dot)
		    (extract string (+ dot 1) end))))))

(define (extract string start end)
  (if (and (fix:= 1 (fix:- end start))
	   (char=? #\* (string-ref string start)))
      'wild
      (substring string start end)))

;;;; Pathname print

(define (unix/pathname->namestring pathname)
  (string-append (print-directory (%pathname-directory pathname))
		 (print-name (%pathname-name pathname)
			     (%pathname-type pathname))))

(define (print-directory directory)
  (cond ((not directory)
	 "")
	((pair? directory)
	 (string-append
	  (if (eq? (car directory) 'absolute) "/" "")
	  (let loop ((directory (cdr directory)))
	    (if (not (pair? directory))
		""
		(string-append (print-directory-component (car directory))
			       "/"
			       (loop (cdr directory)))))))
	(else
	 (error:illegal-pathname-component directory "directory"))))

(define (print-directory-component component)
  (cond ((eq? component 'up) "..")
	((eq? component 'here) ".")
	((string? component) component)
	(else
	 (error:illegal-pathname-component component "directory component"))))

(define (print-name name type)
  (let ((name (or (print-component name) ""))
	(type (print-component type)))
    (if type
	(string-append name "." type)
	name)))

(define (print-component component)
  (cond ((or (not component) (string? component)) component)
	((eq? component 'wild) "*")
	(else (error:illegal-pathname-component component "component"))))

;;;; Pathname Constructors

(define (unix/make-pathname host device directory name type version)
  (%make-pathname
   host
   (if (memq device '(#f unspecific))
       'unspecific
       (error:illegal-pathname-component device "device"))
   (cond ((not directory)
	  directory)
	 ((and (pair? directory)
	       (memq (car directory) '(relative absolute))
	       (list-of-type? (cdr directory)
		 (lambda (element)
		   (if (string? element)
		       (not (fix:= 0 (string-length element)))
		       (memq element '(up here))))))
	  (simplify-directory directory))
	 (else
	  (error:illegal-pathname-component directory "directory")))
   (if (or (memq name '(#f wild))
	   (and (string? name) (not (fix:= 0 (string-length name)))))
       name
       (error:illegal-pathname-component name "name"))
   (if (or (memq type '(#f wild))
	   (and (string? type) (not (fix:= 0 (string-length type)))))
       type
       (error:illegal-pathname-component type "type"))
   (if (memq version '(#f unspecific wild newest))
       'unspecific
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
		  'unspecific))

(define (unix/file-pathname pathname)
  (%make-pathname (%pathname-host pathname)
		  'unspecific
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
	 'unspecific
	 (let ((directory (%pathname-directory pathname))
	       (component
		(parse-directory-component (print-name name type))))
	   (cond ((not (pair? directory))
		  (list 'relative component))
		 ((equal? component ".")
		  directory)
		 (else
		  (append directory (list component)))))
	 #f
	 #f
	 'unspecific)
	pathname)))

(define (unix/directory-pathname-as-file pathname)
  (let ((directory (%pathname-directory pathname)))
    (if (not (and (pair? directory)
		  (or (eq? 'absolute (car directory))
		      (pair? (cdr directory)))))
	(error:bad-range-argument pathname 'directory-pathname-as-file))
    (if (or (%pathname-name pathname)
	    (%pathname-type pathname)
	    (not (pair? (cdr directory))))
	;; Root directory can't be represented as a file, because the
	;; name field of a pathname must be a non-null string.  We
	;; could signal an error here, but instead we'll just return
	;; the original pathname and leave it to the caller to deal
	;; with any problems this might cause.
	pathname
	(parse-name (print-directory-component (car (last-pair directory)))
	  (lambda (name type)
	    (%make-pathname (%pathname-host pathname)
			    'unspecific
			    (simplify-directory (except-last-pair directory))
			    name
			    type
			    'unspecific))))))

;;;; Miscellaneous

(define (unix/pathname-wild? pathname)
  (or (eq? 'wild (%pathname-name pathname))
      (eq? 'wild (%pathname-type pathname))))

(define (unix/pathname->truename pathname)
  (if (file-exists-direct? pathname)
      pathname
      (unix/pathname->truename
       (error:file-operation 0 "find" "file" "file does not exist"
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
		(cond ((and (not (eq? (car p) 'up))
			    (pair? (cdr p))
			    (eq? (cadr p) 'up))
		       (let ((pathname*
			      (pathname-new-directory pathname
						      (delete-up directory p))))
			 (if (file-eq? (directory-pathname pathname)
				       (directory-pathname pathname*))
			     (loop pathname* np)
			     (scan (cddr p) (+ np 2)))))
		      ((eq? (car p) 'here)
		       (let ((pathname*
			      (pathname-new-directory pathname
						      (delete-here directory p))))
			 (loop pathname* np)))
		      (else
		       (scan (cdr p) (+ np 1))))
		pathname))))
      pathname))

(define (delete-up directory p)
  (let loop ((p* directory))
    (if (eq? p* p)
	(cddr p*)
	(cons (car p*) (loop (cdr p*))))))

(define (delete-here directory p)
  (let loop ((p* directory))
    (if (eq? p* p)
	(cdr p)
	(cons (car p*) (loop (cdr p*))))))

(define (file-eq? p1 p2)
  ((ucode-primitive file-eq? 2)
   (string-for-primitive (->namestring (merge-pathnames p1)))
   (string-for-primitive (->namestring (merge-pathnames p2)))))
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

;;;; Dos Pathnames (originally based on unxpth version 14.9)
;;; package: (runtime pathname dos)

(declare (usual-integrations))

(define sub-directory-delimiters
  ;; Allow forward slashes as well as backward slashes so that
  ;; - improperly-written scripts (e.g. compiler/comp.sf) will work
  ;; - laziness when typing file names since the backward slash
  ;;   must be quoted by another.
  (char-set #\\ #\/))

(define sub-directory-delimiter-string
  "\\")

(define init-file-name "scheme.ini")

(define (make-dos-host-type index)
  (make-host-type index
		  'dos
		  dos/parse-namestring
		  dos/pathname->namestring
		  dos/make-pathname
		  dos/pathname-wild?
		  dos/directory-pathname?
		  dos/directory-pathname
		  dos/file-pathname
		  dos/pathname-as-directory
		  dos/directory-pathname-as-file
		  dos/pathname->truename
		  dos/user-homedir-pathname
		  dos/init-file-pathname
		  dos/pathname-simplify))

(define (initialize-package!)
  (add-pathname-host-type! 'dos make-dos-host-type))

;;;; Pathname Parser

(define (dos/parse-namestring string host)
  (call-with-values
      (lambda ()
	(parse-device-and-path
	 (map string-downcase
	      (expand-directory-prefixes
	       (string-components string sub-directory-delimiters)))))
    (lambda (device components)
      (call-with-values (lambda () (parse-name (car (last-pair components))))
	(lambda (name type)
	  (dos/make-pathname
	   host
	   device
	   (let ((components (except-last-pair components)))
	     (and (not (null? components))
		  (simplify-directory
		   (if (fix:= 0 (string-length (car components)))
		       (cons 'absolute
			     (if (and (pair? (cdr components))
				      (fix:= 0
					     (string-length
					      (cadr components))))
				 ;; Handle "\\foo\bar" notation here:
				 ;; the "\\foo" isn't part of the
				 ;; directory path.
				 (cons (cadr components)
				       (parse-directory-components
					(cddr components)))
				 (parse-directory-components
				  (cdr components))))
		       (cons 'relative
			     (parse-directory-components components))))))
	   name
	   type
	   'unspecific))))))

(define (expand-directory-prefixes components)
  (let ((string (car components))
	(replace-head
	 (lambda (string)
	   ;; If STRING has a trailing slash, and it's followed by a
	   ;; slash, drop the trailing slash to avoid doubling.
	   (let ((head (string-components string sub-directory-delimiters)))
	     (append (if (and (pair? (cdr components))
			      (pair? (cdr head))
			      (fix:= 0 (string-length (car (last-pair head)))))
			 (except-last-pair head)
			 head)
		     (cdr components))))))
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
		       (if (= 1 end)
			   (current-home-directory)
			   (user-home-directory (substring string 1 end)))))))
	       (if (condition? expansion)
		   components
		   (replace-head (->namestring expansion)))))
	    (else components))))))

(define (parse-device-and-path components)
  (let ((string (car components)))
    (if (and (fix:= 2 (string-length string))
	     (char=? #\: (string-ref string 1))
	     (char-alphabetic? (string-ref string 0)))
	(values (string-head string 1) (cons "" (cdr components)))
	(values #f components))))

(define (simplify-directory directory)
  (cond ((and (eq? (car directory) 'relative) (null? (cdr directory))) #f)
	((equal? '(absolute up) directory) '(absolute))
	(else directory)))

(define (parse-directory-components components)
  (if (any (lambda (component)
	     (fix:= 0 (string-length component)))
	   components)
      (error "Directory contains null component:" components))
  (map parse-directory-component components))

(define (parse-directory-component component)
  (if (string=? ".." component)
      'up
      component))

(define (string-components string delimiters)
  (substring-components string 0 (string-length string) delimiters))

(define (substring-components string start end delimiters)
  (let loop ((start start))
    (let ((index (string-find-next-char-in-set string delimiters start end)))
      (if index
	  (cons (substring string start index) (loop (fix:+ index 1)))
	  (list (substring string start end))))))

(define (parse-name string)
  (let ((dot (string-find-previous-char string #\.))
	(end (string-length string)))
    (if (or (not dot)
	    (fix:= dot 0)
	    (fix:= dot (fix:- end 1))
	    (char=? #\. (string-ref string (fix:- dot 1))))
	(values (cond ((fix:= end 0) #f)
		      ((string=? "*" string) 'wild)
		      (else string))
		#f)
	(values (extract string 0 dot)
		(extract string (fix:+ dot 1) end)))))

(define (extract string start end)
  (if (and (fix:= 1 (fix:- end start))
	   (char=? #\* (string-ref string start)))
      'wild
      (substring string start end)))

;;;; Pathname printer

(define (dos/pathname->namestring pathname)
  (string-append (print-device (%pathname-device pathname))
		 (print-directory (%pathname-directory pathname))
		 (print-name (%pathname-name pathname)
			     (%pathname-type pathname))))

(define (print-device device)
  (if (or (not device) (eq? device 'unspecific))
      ""
      (string-append device ":")))

(define (print-directory directory)
  (cond ((or (not directory) (eq? directory 'unspecific))
	 "")
	((pair? directory)
	 (string-append
	  (if (eq? (car directory) 'absolute)
              sub-directory-delimiter-string
              "")
	  (let loop ((directory (cdr directory)))
	    (if (null? directory)
		""
		(string-append (print-directory-component (car directory))
			       sub-directory-delimiter-string
			       (loop (cdr directory)))))))
	(else
	 (error:illegal-pathname-component directory "directory"))))

(define (print-directory-component component)
  (cond ((eq? component 'up) "..")
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

(define (dos/make-pathname host device directory name type version)
  (%%make-pathname
   host
   (cond ((string? device) device)
	 ((memq device '(#f unspecific)) device)
	 (else (error:illegal-pathname-component device "device")))
   (cond ((or (not directory) (eq? directory 'unspecific))
	  directory)
	 ((and (list? directory)
	       (not (null? directory))
	       (memq (car directory) '(relative absolute))
	       (every (lambda (element)
			(if (string? element)
			    (not (fix:= 0 (string-length element)))
			    (eq? element 'up)))
		      (if (server-directory? directory)
			  (cddr directory)
			  (cdr directory))))
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

(define (%%make-pathname host device directory name type version)
  ;; This is a kludge to make the \\foo\bar notation work correctly.
  ;; This kludge does not distinguish the \\foo component from any
  ;; other directory component, as some rare programs might wish,
  ;; because doing so is a more pervasive change.  Until someone has
  ;; the energy to fix it correctly, this will have to do.
  (%make-pathname host
		  (if (server-directory? directory) 'unspecific device)
		  directory
		  name
		  type
		  version))

(define (server-directory? directory)
  (and (pair? directory)
       (eq? (car directory) 'absolute)
       (pair? (cdr directory))
       (string? (cadr directory))
       (fix:= 0 (string-length (cadr directory)))))

(define (dos/directory-pathname? pathname)
  (and (not (%pathname-name pathname))
       (not (%pathname-type pathname))))

(define (dos/directory-pathname pathname)
  (%%make-pathname (%pathname-host pathname)
		   (%pathname-device pathname)
		   (%pathname-directory pathname)
		   #f
		   #f
		   'unspecific))

(define (dos/file-pathname pathname)
  (%%make-pathname (%pathname-host pathname)
		   #f
		   #f
		   (%pathname-name pathname)
		   (%pathname-type pathname)
		   (%pathname-version pathname)))

(define (dos/pathname-as-directory pathname)
  (let ((name (%pathname-name pathname))
	(type (%pathname-type pathname)))
    (if (or name type)
	(%%make-pathname
	 (%pathname-host pathname)
	 (%pathname-device pathname)
	 (simplify-directory
	  (let ((directory (%pathname-directory pathname))
		(component
		 (parse-directory-component (print-name name type))))
	    (cond ((not (pair? directory)) (list 'relative component))
		  ((equal? component ".") directory)
		  (else (append directory (list component))))))
	 #f
	 #f
	 'unspecific)
	pathname)))

(define (dos/directory-pathname-as-file pathname)
  (let ((directory (%pathname-directory pathname)))
    (if (not (and (pair? directory)
		  (or (eq? 'absolute (car directory))
		      (pair? (cdr directory)))))
	(error:bad-range-argument pathname 'directory-pathname-as-file))
    (if (or (%pathname-name pathname)
	    (%pathname-type pathname)
	    (null? (cdr directory)))
	pathname
	(call-with-values
	    (lambda ()
	      (parse-name
	       (print-directory-component (car (last-pair directory)))))
	  (lambda (name type)
	    (%%make-pathname (%pathname-host pathname)
			     (%pathname-device pathname)
			     (simplify-directory (except-last-pair directory))
			     name
			     type
			     'unspecific))))))

;;;; Miscellaneous

(define (dos/pathname-wild? pathname)
  (let ((namestring (file-namestring pathname)))
    (or (string-find-next-char namestring #\*)
	(string-find-next-char namestring #\?))))

(define (dos/pathname->truename pathname)
  (if (file-exists-direct? pathname)
      pathname
      (dos/pathname->truename
       (error:file-operation 0 "find" "file" "file does not exist"
			     dos/pathname->truename (list pathname)))))

(define (dos/user-homedir-pathname host)
  (and (eq? host local-host)
       (pathname-as-directory (current-home-directory))))

(define (dos/init-file-pathname host)
  (let ((pathname
	 (merge-pathnames init-file-name (dos/user-homedir-pathname host))))
    (and (file-exists? pathname)
	 pathname)))

(define (dos/pathname-simplify pathname)
  (let ((directory (pathname-directory pathname)))
    (or (and (pair? directory)
	     (let ((directory*
		    (cons (car directory)
			  (reverse!
			   (let loop ((elements (reverse (cdr directory))))
			     (if (null? elements)
				 '()
				 (let ((head (car elements))
				       (tail (loop (cdr elements))))
				   (if (and (eq? head 'up)
					    (not (null? tail))
					    (not (eq? (car tail) 'up)))
				       (cdr tail)
				       (cons head tail)))))))))
	       (and (not (equal? directory directory*))
		    (let ((pathname*
			   (pathname-new-directory pathname directory*)))
		      (and ((ucode-primitive file-eq? 2)
			    (string-for-primitive (->namestring pathname))
			    (string-for-primitive (->namestring pathname*)))
			   pathname*)))))
	pathname)))
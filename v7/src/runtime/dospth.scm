#| -*-Scheme-*-

$Id: dospth.scm,v 1.47 2008/01/30 20:02:29 cph Exp $

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
		  'DOS
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
  (add-pathname-host-type! 'DOS make-dos-host-type))

;;;; Pathname Parser

(define (dos/parse-namestring string host)
  (call-with-values
      (lambda ()
	(let ((components
	       (expand-directory-prefixes
		(string-components string sub-directory-delimiters))))
	  (for-each string-downcase! components)
	  (parse-device-and-path components)))
    (lambda (device components)
      (call-with-values (lambda () (parse-name (car (last-pair components))))
	(lambda (name type)
	  (dos/make-pathname
	   host
	   device
	   (let ((components (except-last-pair components)))
	     (and (not (null? components))
		  (simplify-directory
		   (if (string-null? (car components))
		       (cons 'ABSOLUTE
			     (if (and (pair? (cdr components))
				      (string-null? (cadr components)))
				 ;; Handle "\\foo\bar" notation here:
				 ;; the "\\foo" isn't part of the
				 ;; directory path.
				 (cons (cadr components)
				       (parse-directory-components
					(cddr components)))
				 (parse-directory-components
				  (cdr components))))
		       (cons 'RELATIVE
			     (parse-directory-components components))))))
	   name
	   type
	   'UNSPECIFIC))))))

(define (expand-directory-prefixes components)
  (let ((string (car components))
	(replace-head
	 (lambda (string)
	   ;; If STRING has a trailing slash, and it's followed by a
	   ;; slash, drop the trailing slash to avoid doubling.
	   (let ((head (string-components string sub-directory-delimiters)))
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

(define (parse-device-and-path components)
  (let ((string (car components)))
    (if (and (fix:= (string-length string) 2)
	     (char=? #\: (string-ref string 1))
	     (char-alphabetic? (string-ref string 0)))
	(values (string-head string 1) (cons "" (cdr components)))
	(values #f components))))

(define (simplify-directory directory)
  (cond ((and (eq? (car directory) 'RELATIVE) (null? (cdr directory))) #f)
	((equal? '(ABSOLUTE UP) directory) '(ABSOLUTE))
	(else directory)))

(define (parse-directory-components components)
  (if (there-exists? components string-null?)
      (error "Directory contains null component:" components))
  (map parse-directory-component components))

(define (parse-directory-component component)
  (if (string=? ".." component)
      'UP
      component))

(define (string-components string delimiters)
  (substring-components string 0 (string-length string) delimiters))

(define (substring-components string start end delimiters)
  (let loop ((start start))
    (let ((index
	   (substring-find-next-char-in-set string start end delimiters)))
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
		      ((string=? "*" string) 'WILD)
		      (else string))
		#f)
	(values (extract string 0 dot)
		(extract string (fix:+ dot 1) end)))))

(define (extract string start end)
  (if (substring=? string start end "*" 0 1)
      'WILD
      (substring string start end)))

;;;; Pathname Unparser

(define (dos/pathname->namestring pathname)
  (string-append (unparse-device (%pathname-device pathname))
		 (unparse-directory (%pathname-directory pathname))
		 (unparse-name (%pathname-name pathname)
			       (%pathname-type pathname))))

(define (unparse-device device)
  (if (or (not device) (eq? device 'UNSPECIFIC))
      ""
      (string-append device ":")))

(define (unparse-directory directory)
  (cond ((or (not directory) (eq? directory 'UNSPECIFIC))
	 "")
	((pair? directory)
	 (string-append
	  (if (eq? (car directory) 'ABSOLUTE) 
              sub-directory-delimiter-string
              "")
	  (let loop ((directory (cdr directory)))
	    (if (null? directory)
		""
		(string-append (unparse-directory-component (car directory))
			       sub-directory-delimiter-string
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

(define (dos/make-pathname host device directory name type version)
  (%%make-pathname
   host
   (cond ((string? device) device)
	 ((memq device '(#F UNSPECIFIC)) device)
	 (else (error:illegal-pathname-component device "device")))
   (cond ((or (not directory) (eq? directory 'UNSPECIFIC))
	  directory)
	 ((and (list? directory)
	       (not (null? directory))
	       (memq (car directory) '(RELATIVE ABSOLUTE))
	       (for-all? (if (server-directory? directory)
			     (cddr directory)
			     (cdr directory))
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

(define (%%make-pathname host device directory name type version)
  ;; This is a kludge to make the \\foo\bar notation work correctly.
  ;; This kludge does not distinguish the \\foo component from any
  ;; other directory component, as some rare programs might wish,
  ;; because doing so is a more pervasive change.  Until someone has
  ;; the energy to fix it correctly, this will have to do.
  (%make-pathname host
		  (if (server-directory? directory) 'UNSPECIFIC device)
		  directory
		  name
		  type
		  version))

(define (server-directory? directory)
  (and (pair? directory)
       (eq? (car directory) 'ABSOLUTE)
       (pair? (cdr directory))
       (string? (cadr directory))
       (string-null? (cadr directory))))

(define (dos/directory-pathname? pathname)
  (and (not (%pathname-name pathname))
       (not (%pathname-type pathname))))

(define (dos/directory-pathname pathname)
  (%%make-pathname (%pathname-host pathname)
		   (%pathname-device pathname)
		   (%pathname-directory pathname)
		   #f
		   #f
		   'UNSPECIFIC))

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
		 (parse-directory-component (unparse-name name type))))
	    (cond ((not (pair? directory)) (list 'RELATIVE component))
		  ((equal? component ".") directory)
		  (else (append directory (list component))))))
	 #f
	 #f
	 'UNSPECIFIC)
	pathname)))

(define (dos/directory-pathname-as-file pathname)
  (let ((directory (%pathname-directory pathname)))
    (if (not (and (pair? directory)
		  (or (eq? 'ABSOLUTE (car directory))
		      (pair? (cdr directory)))))
	(error:bad-range-argument pathname 'DIRECTORY-PATHNAME-AS-FILE))
    (if (or (%pathname-name pathname)
	    (%pathname-type pathname)
	    (null? (cdr directory)))
	pathname
	(call-with-values
	    (lambda ()
	      (parse-name
	       (unparse-directory-component (car (last-pair directory)))))
	  (lambda (name type)
	    (%%make-pathname (%pathname-host pathname)
			     (%pathname-device pathname)
			     (simplify-directory (except-last-pair directory))
			     name
			     type
			     'UNSPECIFIC))))))

;;;; Miscellaneous

(define (dos/pathname-wild? pathname)
  (let ((namestring (file-namestring pathname)))
    (or (string-find-next-char namestring #\*)
	(string-find-next-char namestring #\?))))

(define (dos/pathname->truename pathname)
  (if (file-exists-direct? pathname)
      pathname
      (dos/pathname->truename
       (error:file-operation pathname "find" "file" "file does not exist"
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
				   (if (and (eq? head 'UP)
					    (not (null? tail))
					    (not (eq? (car tail) 'UP)))
				       (cdr tail)
				       (cons head tail)))))))))
	       (and (not (equal? directory directory*))
		    (let ((pathname*
			   (pathname-new-directory pathname directory*)))
		      (if (eq? 'OS/2 microcode-id/operating-system)
			  pathname*
			  (and ((ucode-primitive file-eq? 2)
				(->namestring pathname)
				(->namestring pathname*))
			       pathname*))))))
	pathname)))
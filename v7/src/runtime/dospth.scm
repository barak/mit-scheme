#| -*-Scheme-*-

$Id: dospth.scm,v 1.30 1995/10/18 05:00:30 cph Exp $

Copyright (c) 1992-95 Massachusetts Institute of Technology

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

;;;; Dos Pathnames (originally based on unxpth version 14.9)
;;; package: (runtime pathname dos)

(declare (usual-integrations))

(define hook/dos/end-of-line-string)

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
		  dos/pathname-as-directory
		  dos/directory-pathname-as-file
		  dos/pathname->truename
		  dos/user-homedir-pathname
		  dos/init-file-pathname
		  dos/pathname-simplify
		  dos/end-of-line-string))

(define (initialize-package!)
  (set! hook/dos/end-of-line-string default/dos/end-of-line-string)
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
	  (dos/make-pathname host
			     device
			     (let ((components (except-last-pair components)))
			       (and (not (null? components))
				    (simplify-directory
				     (if (string=? "" (car components))
					 (cons 'ABSOLUTE
					       (map parse-directory-component
						    (cdr components)))
					 (cons 'RELATIVE
					       (map parse-directory-component
						    components))))))
			     name
			     type
			     'UNSPECIFIC))))))

(define (expand-directory-prefixes components)
  (let ((string (car components)))
    (if (or (string-null? string)
	    (not *expand-directory-prefixes?*))
	components
	(case (string-ref string 0)
	  ((#\$)
	   (let ((value (get-environment-variable (string-tail string 1))))
	     (if (not value)
		 components
		 (append (string-components value sub-directory-delimiters)
			 (cdr components)))))
	  ((#\~)
	   (append
	    (string-components (->namestring
				(directory-pathname-as-file
				 (let ((user-name (string-tail string 1)))
				   (if (string-null? user-name)
				       (current-home-directory)
				       (user-home-directory user-name)))))
			       sub-directory-delimiters)
	    (cdr components)))
	  (else components)))))

(define (parse-device-and-path components)
  (let ((string (car components)))
    (let ((colon (string-find-next-char string #\:)))
      (if (not colon)
	  (values #f components)
	  (values (string-head string colon)
		  (cons (string-tail string (+ colon 1))
			(cdr components)))))))

(define (simplify-directory directory)
  (cond ((and (eq? (car directory) 'RELATIVE) (null? (cdr directory))) #f)
	((equal? '(ABSOLUTE UP) directory) '(ABSOLUTE))
	(else directory)))

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
  (if (eq? #t (file-exists? pathname))
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

(define (dos/end-of-line-string pathname)
  (hook/dos/end-of-line-string pathname))

(define (default/dos/end-of-line-string pathname)
  pathname				; ignored
  (os/default-end-of-line-translation))
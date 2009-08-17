#| -*-Scheme-*-

$Id: ed8f695ab6537636dbc804608260792e10698518 $

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

;;;; Pathnames
;;; package: (runtime pathname)

(declare (usual-integrations))

#|

When examining pathname components, programs must be prepared to
encounter any of the following situations:

* The host can be a host object.

* Any component except the host can be #F, which means the component
  has not been specified.

* Any component except the can be 'UNSPECIFIC, which means the
  component has no meaning in this particular pathname.

* The device, name, and type can be non-null strings.

* The directory can be a non-empty list of non-null strings and
  symbols, whose first element is either 'ABSOLUTE or 'RELATIVE.

* The version can be any symbol or any positive exact integer.  The
  symbol 'NEWEST refers to the largest version number that already
  exists in the file system when reading, overwriting, appending,
  superseding, or directory-listing an existing file; it refers to the
  smallest version number greater than any existing version number
  when creating a new file.

When examining wildcard components of a wildcard pathname, programs
must be prepared to encounter any of the following additional values
in any component (except the host) or any element of a list that is
the directory component:

* The symbol 'WILD, which matches anything.

* A string containing implementation-dependent special wildcard
  characters.

* Any object, representing an implementation-dependent wildcard
  pattern.

When constructing a pathname from components, programs must follow
these rules:

* Any component may be #F.  Specifying #F for the host results in
  using a default host rather than an actual #F value.

* The host may be a host object.

* The device, name, and type may be strings.  There are
  implementation-dependent limits on the number and type of characters
  in these strings.  A plausible assumption is that letters (of a
  single case) and digits are acceptable to most file system.

* The directory may be a list of strings and symbols whose first
  element is either 'ABSOLUTE or 'RELATIVE.  There are
  implementation-dependent limits on the length and contents of the
  list.

* The version may be 'NEWEST.

* Any component may be taken from the corresponding component of
  another pathname.  When the two pathnames are for different file
  systems, an appropriate translation occurs.  If no meaningful
  translation is possible, an error is signalled.

* When constructing a wildcard pathname, the name, type, or version
  may be 'WILD, which matches anything.

|#

(define-structure (pathname
		   (type vector)
		   (named '|#[(runtime pathname)pathname]|)
		   (constructor %make-pathname)
		   (conc-name %pathname-)
		   (print-procedure
		    (simple-unparser-method 'PATHNAME
		      (lambda (pathname)
			(list (->namestring pathname))))))
  (host #f read-only #t)
  (device #f read-only #t)
  (directory #f read-only #t)
  (name #f read-only #t)
  (type #f read-only #t)
  (version #f read-only #t))

(define-guarantee pathname "pathname")

(define pathname-parser-method
  (simple-parser-method
   (lambda (objects)
     (and (pair? objects)
	  (->pathname (car objects))))))

(define (->pathname object)
  (pathname-arg object #f '->PATHNAME))

(define (pathname-arg object defaults operator)
  (cond ((pathname? object) object)
	((string? object) (parse-namestring object #f defaults))
	(else (error:wrong-type-argument object "pathname" operator))))

(define (make-pathname host device directory name type version)
  (let ((host (if host (guarantee-host host 'MAKE-PATHNAME) local-host)))
    ((host-type/operation/make-pathname (host/type host))
     host device directory name type version)))

(define (pathname-host pathname)
  (%pathname-host (->pathname pathname)))

(define (pathname-device pathname)
  (%pathname-device (->pathname pathname)))

(define (pathname-directory pathname)
  (%pathname-directory (->pathname pathname)))

(define (pathname-name pathname)
  (%pathname-name (->pathname pathname)))

(define (pathname-type pathname)
  (%pathname-type (->pathname pathname)))

(define (pathname-version pathname)
  (%pathname-version (->pathname pathname)))

(define (pathname=? x y)
  (let ((x (->pathname x))
	(y (->pathname y)))
    (and (host=? (%pathname-host x) (%pathname-host y))
	 (equal? (%pathname-device x) (%pathname-device y))
	 (equal? (%pathname-directory x) (%pathname-directory y))
	 (equal? (%pathname-name x) (%pathname-name y))
	 (equal? (%pathname-type x) (%pathname-type y))
	 (equal? (%pathname-version x) (%pathname-version y)))))

(define (pathname-absolute? pathname)
  (let ((directory (pathname-directory pathname)))
    (and (pair? directory)
	 (eq? (car directory) 'ABSOLUTE))))

(define (pathname-relative? pathname)
  (let ((directory (pathname-directory pathname)))
    (and (pair? directory)
	 (eq? (car directory) 'RELATIVE))))

(define (pathname-wild? pathname)
  (let ((pathname (->pathname pathname)))
    ((host-type/operation/pathname-wild?
      (host/type (%pathname-host pathname)))
     pathname)))

(define (directory-pathname? pathname)
  (let ((pathname (->pathname pathname)))
    ((host-type/operation/directory-pathname?
      (host/type (%pathname-host pathname)))
     pathname)))

(define (pathname-simplify pathname)
  (let ((pathname (->pathname pathname)))
    ((host-type/operation/pathname-simplify
      (host/type (%pathname-host pathname)))
     pathname)))

(define (directory-pathname pathname)
  (let ((pathname (->pathname pathname)))
    ((host-type/operation/directory-pathname
      (host/type (%pathname-host pathname)))
     pathname)))

(define (file-pathname pathname)
  (let ((pathname (->pathname pathname)))
    ((host-type/operation/file-pathname
      (host/type (%pathname-host pathname)))
     pathname)))

(define (pathname-as-directory pathname)
  (let ((pathname (->pathname pathname)))
    ((host-type/operation/pathname-as-directory
      (host/type (%pathname-host pathname)))
     pathname)))

(define (directory-pathname-as-file pathname)
  (let ((pathname (->pathname pathname)))
    ((host-type/operation/directory-pathname-as-file
      (host/type (%pathname-host pathname)))
     pathname)))

(define (pathname-new-device pathname device)
  (let ((pathname (->pathname pathname)))
    (%make-pathname (%pathname-host pathname)
		    device
		    (%pathname-directory pathname)
		    (%pathname-name pathname)
		    (%pathname-type pathname)
		    (%pathname-version pathname))))

(define (pathname-new-directory pathname directory)
  (let ((pathname (->pathname pathname)))
    (%make-pathname (%pathname-host pathname)
		    (%pathname-device pathname)
		    directory
		    (%pathname-name pathname)
		    (%pathname-type pathname)
		    (%pathname-version pathname))))

(define (pathname-new-name pathname name)
  (let ((pathname (->pathname pathname)))
    (%make-pathname (%pathname-host pathname)
		    (%pathname-device pathname)
		    (%pathname-directory pathname)
		    name
		    (%pathname-type pathname)
		    (%pathname-version pathname))))

(define (pathname-new-type pathname type)
  (let ((pathname (->pathname pathname)))
    (%make-pathname (%pathname-host pathname)
		    (%pathname-device pathname)
		    (%pathname-directory pathname)
		    (%pathname-name pathname)
		    type
		    (%pathname-version pathname))))

(define (pathname-new-version pathname version)
  (let ((pathname (->pathname pathname)))
    (%make-pathname (%pathname-host pathname)
		    (%pathname-device pathname)
		    (%pathname-directory pathname)
		    (%pathname-name pathname)
		    (%pathname-type pathname)
		    version)))

(define (pathname-default-device pathname device)
  (let ((pathname (->pathname pathname)))
    (if (%pathname-device pathname)
	pathname
	(pathname-new-device pathname device))))

(define (pathname-default-directory pathname directory)
  (let ((pathname (->pathname pathname)))
    (if (%pathname-directory pathname)
	pathname
	(pathname-new-directory pathname directory))))

(define (pathname-default-name pathname name)
  (let ((pathname (->pathname pathname)))
    (if (%pathname-name pathname)
	pathname
	(pathname-new-name pathname name))))

(define (pathname-default-type pathname type)
  (let ((pathname (->pathname pathname)))
    (if (%pathname-type pathname)
	pathname
	(pathname-new-type pathname type))))

(define (pathname-default-version pathname version)
  (let ((pathname (->pathname pathname)))
    (if (%pathname-version pathname)
	pathname
	(pathname-new-version pathname version))))

(define (pathname-default pathname device directory name type version)
  (let ((pathname (->pathname pathname)))
    (%make-pathname (%pathname-host pathname)
		    (or (%pathname-device pathname) device)
		    (or (%pathname-directory pathname) directory)
		    (or (%pathname-name pathname) name)
		    (or (%pathname-type pathname) type)
		    (or (%pathname-version pathname) version))))

(define (pathname->uri pathname)
  (let ((pathname (->pathname pathname)))
    (make-uri (if (pathname-absolute? pathname) 'file #f)
	      #f
	      (map (lambda (x)
		     (cond ((eq? x 'WILD) "*")
			   ((eq? x 'UP) "..")
			   (else (string->utf8-string x))))
		   (append (if (pathname-absolute? pathname)
			       (list "")
			       '())
			   (let ((device (pathname-device pathname))
				 (directory (pathname-directory pathname)))
			     (if (missing-component? device)
				 (if (missing-component? directory)
				     '()
				     (cdr directory))
				 (cons device (cdr directory))))
			   (let ((name (file-namestring pathname)))
			     (if (missing-component? name)
				 (if (pathname-absolute? pathname)
				     (list "")
				     '())
				 (list name)))))
	      #f
	      #f)))

(define (uri->pathname uri #!optional error?)
  (let ((uri (->uri uri (and error? 'URI->PATHNAME)))
	(defaults *default-pathname-defaults*)
	(finish
	 (lambda (device path keyword)
	   (receive (directory name type)
	       (if (pair? path)
		   (let ((d (cons keyword (except-last-pair path)))
			 (s (car (last-pair path))))
		     (if (string-null? s)
			 (values d #f #f)
			 (let ((pn (parse-namestring s)))
			   (values d
				   (pathname-name pn)
				   (pathname-type pn)))))
		   (values (list keyword) #f #f))
	     (make-pathname #f device directory name type #f)))))
    (let ((scheme (uri-scheme uri))
	  (path
	   (map (lambda (x)
		  (cond ((string=? x "*") 'WILD)
			((string=? x "..") 'UP)
			(else (utf8-string->string x))))
		(uri-path uri)))
	  (lose
	   (lambda ()
	     (if error? (error:bad-range-argument uri 'URI->PATHNAME))
	     #f)))
      (case scheme
	((file)
	 (if (and (pair? path)
		  (string-null? (car path)))
	     (let ((path (cdr path)))
	       (receive (device path)
		   (let ((device (pathname-device defaults)))
		     (if (and (pair? path)
			      (not (missing-component? device)))
			 (values (car path) (cdr path))
			 (values device path)))
		 (if (pair? path)
		     (finish device path 'ABSOLUTE)
		     (lose))))
	     (lose)))
	((#f) (finish #f path 'RELATIVE))
	(else (lose))))))

(define (missing-component? x)
  (or (not x)
      (eq? x 'UNSPECIFIC)))

;;;; Pathname Syntax

(define (parse-namestring namestring #!optional host defaults)
  (let ((host
	 (if (and (not (default-object? host)) host)
	     (begin
	       (if (not (host? host))
		   (error:wrong-type-argument host "host" 'PARSE-NAMESTRING))
	       host)
	     (pathname-host
	      (if (and (not (default-object? defaults)) defaults)
		  defaults
		  *default-pathname-defaults*)))))
    (cond ((string? namestring)
	   ((host-type/operation/parse-namestring (host/type host))
	    namestring host))
	  ((pathname? namestring)
	   (if (not (host=? host (pathname-host namestring)))
	       (error:bad-range-argument namestring 'PARSE-NAMESTRING))
	   namestring)
	  (else
	   (error:wrong-type-argument namestring "namestring"
				      'PARSE-NAMESTRING)))))

(define (->namestring pathname)
  (let ((pathname (->pathname pathname)))
    (string-append (host-namestring pathname)
		   (pathname->namestring pathname))))

(define (file-namestring pathname)
  (pathname->namestring (file-pathname pathname)))

(define (directory-namestring pathname)
  (pathname->namestring (directory-pathname pathname)))

(define (host-namestring pathname)
  (let ((host (host/name (pathname-host pathname))))
    (if host
	(string-append host "::")
	"")))

(define (enough-namestring pathname #!optional defaults)
  (let ((defaults
	  (if (and (not (default-object? defaults)) defaults)
	      (->pathname defaults)
	      *default-pathname-defaults*)))
    (let ((pathname (enough-pathname pathname defaults)))
      (let ((namestring (pathname->namestring pathname)))
	(if (host=? (%pathname-host pathname) (%pathname-host defaults))
	    namestring
	    (string-append (host-namestring pathname) namestring))))))

(define (pathname->namestring pathname)
  ((host-type/operation/pathname->namestring
    (host/type (%pathname-host pathname)))
   pathname))

;;;; Pathname Merging

(define *default-pathname-defaults*)

(define (merge-pathnames pathname #!optional defaults default-version)
  (let* ((defaults
	   (if (and (not (default-object? defaults)) defaults)
	       (->pathname defaults)
	       *default-pathname-defaults*))
	 (pathname (pathname-arg pathname defaults 'MERGE-PATHNAMES)))
    (make-pathname
     (or (%pathname-host pathname) (%pathname-host defaults))
     (or (%pathname-device pathname)
	 (and (%pathname-host pathname)
	      (host=? (%pathname-host pathname) (%pathname-host defaults))
	      (%pathname-device defaults)))
     (let ((directory (%pathname-directory pathname))
	   (default (%pathname-directory defaults)))
       (cond ((not directory)
	      default)
	     ((and (pair? directory)
		   (eq? (car directory) 'RELATIVE)
		   (pair? default))
	      (append default (cdr directory)))
	     (else
	      directory)))
     (or (%pathname-name pathname) (%pathname-name defaults))
     (or (%pathname-type pathname) (%pathname-type defaults))
     (or (%pathname-version pathname)
	 (and (not (%pathname-name pathname)) (%pathname-version defaults))
	 (if (default-object? default-version)
	     'NEWEST
	     default-version)))))

(define (enough-pathname pathname #!optional defaults)
  (let* ((defaults
	   (if (and (not (default-object? defaults)) defaults)
	       (->pathname defaults)
	       *default-pathname-defaults*))
	 (pathname (pathname-arg pathname defaults 'ENOUGH-PATHNAME)))
    (let ((usual
	   (lambda (component default)
	     (and (or (symbol? component)
		      (not (equal? component default)))
		  component))))
      (make-pathname
       (and (or (symbol? (%pathname-host pathname))
		(not (host=? (%pathname-host pathname)
			     (%pathname-host defaults))))
	    (%pathname-host pathname))
       (let ((device (%pathname-device pathname)))
	 (and (or (symbol? device)
		  (not (equal? device (%pathname-device defaults)))
		  (not (host=? (%pathname-host pathname)
			       (%pathname-host defaults))))
	      device))
       (let ((directory (%pathname-directory pathname))
	     (default (%pathname-directory defaults)))
	 (if (or (not directory)
		 (symbol? directory)
		 (not (eq? (car directory) (car default)))
		 ;; Detect the case where directory starts with "//"
		 ;; and default does not, or vice versa.  This is a
		 ;; kludge to make network devices work properly in
		 ;; DOS-like pathnames.
		 (and (eq? (car directory) 'ABSOLUTE)
		      (not (boolean=? (and (pair? (cdr directory))
					   (equal? (cadr directory) ""))
				      (and (pair? (cdr default))
					   (equal? (cadr default) ""))))))
	     directory
	     (let loop
		 ((components (cdr directory)) (components* (cdr default)))
	       (cond ((null? components*)
		      (cons 'RELATIVE components))
		     ((and (not (null? components))
			   (equal? (car components) (car components*)))
		      (loop (cdr components) (cdr components*)))
		     (else
		      directory)))))
       (usual (%pathname-name pathname) (%pathname-name defaults))
       (usual (%pathname-type pathname) (%pathname-type defaults))
       (let ((version (%pathname-version pathname)))
	 (and (or (symbol? version)
		  (not (equal? version (%pathname-version defaults)))
		  (%pathname-name pathname))
	      version))))))

;;;; Host Abstraction
;;;  A lot of hair to make pathnames fasdumpable.

(define host-types)
(define local-host)

(define-structure (host-type (conc-name host-type/))
  (index #f read-only #t)
  (name #f read-only #t)
  (operation/parse-namestring #f read-only #t)
  (operation/pathname->namestring #f read-only #t)
  (operation/make-pathname #f read-only #t)
  (operation/pathname-wild? #f read-only #t)
  (operation/directory-pathname? #f read-only #t)
  (operation/directory-pathname #f read-only #t)
  (operation/file-pathname #f read-only #t)
  (operation/pathname-as-directory #f read-only #t)
  (operation/directory-pathname-as-file #f read-only #t)
  (operation/pathname->truename #f read-only #t)
  (operation/user-homedir-pathname #f read-only #t)
  (operation/init-file-pathname #f read-only #t)
  (operation/pathname-simplify #f read-only #t))

(define-structure (host (type vector)
			(named ((ucode-primitive string->symbol)
				"#[(runtime pathname)host]"))
			(constructor %make-host)
			(conc-name host/))
  (type-index #f read-only #t)
  (name #f read-only #t))

(define (make-host type name)
  (%make-host (host-type/index type) name))

(define (host/type host)
  (vector-ref host-types (host/type-index host)))

(define (host/type-name host)
  (host-type/name (host/type host)))

(define (host=? x y)
  (and (= (host/type-index x) (host/type-index y))
       (equal? (host/name x) (host/name y))))

(define (guarantee-host host operation)
  (if (not (host? host)) (error:wrong-type-argument host "host" operation))
  host)

;;;; File System Stuff

(define (->truename pathname)
  (let ((pathname (merge-pathnames pathname)))
    ((host-type/operation/pathname->truename
      (host/type (%pathname-host pathname)))
     pathname)))

(define (user-homedir-pathname #!optional host)
  (let ((host
	 (if (and (not (default-object? host)) host)
	     (guarantee-host host 'USER-HOMEDIR-PATHNAME)
	     local-host)))
    ((host-type/operation/user-homedir-pathname (host/type host)) host)))

(define (init-file-pathname #!optional host)
  (let ((host
	 (if (and (not (default-object? host)) host)
	     (guarantee-host host 'INIT-FILE-PATHNAME)
	     local-host)))
    ((host-type/operation/init-file-pathname (host/type host)) host)))

(define (system-library-pathname pathname #!optional required?)
  (let ((pathname* (merge-pathnames pathname (%find-library-directory)))
	(required? (if (default-object? required?) #t required?)))
    (if (and required? (not (file-exists? pathname*)))
	(system-library-pathname
	 (error:file-operation pathname*
			       "find"
			       "file"
			       "no such file in system library path"
			       system-library-pathname
			       (list pathname required?)))
	pathname*)))

(define (system-library-directory-pathname #!optional pathname required?)
  (if (if (default-object? pathname) #f pathname)
      (let ((dir (system-library-pathname pathname #f)))
	(cond ((file-directory? dir)
	       (pathname-as-directory dir))
	      ((if (default-object? required?) #f required?)
	       (system-library-directory-pathname
		(error:file-operation
		 pathname
		 "find"
		 "directory"
		 "no such directory in system library path"
		 system-library-directory-pathname
		 (list pathname required?))
		required?))
	      (else #f)))
      (%find-library-directory)))

(define (%find-library-directory)
  (pathname-simplify
   (or (find-matching-item library-directory-path file-directory?)
       (error "Can't find library directory."))))

(define library-directory-path)

(define known-host-types
  '((0 UNIX)
    (1 DOS NT OS/2)
    (2 VMS)))

(define (host-name->index name)
  (let loop ((entries known-host-types))
    (if (null? entries)
	(error "Unknown host type:" name))
    (if (memq name (cdar entries))
	(caar entries)
	(loop (cdr entries)))))

(define (host-index->name index)
  (let ((entry (assv index known-host-types)))
    (and entry
	 (cadr entry))))

(define available-host-types
  '())

(define (host-name->type name)
  (host-index->type (host-name->index name)))

(define (host-index->type index)
  (let ((entry (assv index available-host-types)))
    (if (not entry)
	(error "Missing host type for index:" index))
    (cdr entry)))

(define (add-pathname-host-type! name constructor)
  (let ((index (host-name->index name)))
    (let ((host-type (constructor index))
	  (place (assv index available-host-types)))
      (if place
	  (set-cdr! place host-type)
	  (begin
	    (set! available-host-types
		  (cons (cons index host-type)
			available-host-types))
	    unspecific)))))

(define (make-unimplemented-host-type index)
  (let ((name (or (host-index->name index) 'UNKNOWN)))
    (let ((fail
	   (lambda arguments
	     (error "Unimplemented host type:" name arguments))))
      (make-host-type index name fail fail fail fail fail fail fail fail fail
		      fail fail fail fail))))

(define (reset-package!)
  (let ((host-type (host-name->type microcode-id/operating-system))
	(n-types (+ (apply max (map car known-host-types)) 1)))
    (let ((types (make-vector n-types #f)))
      (for-each (lambda (type) (vector-set! types (car type) (cdr type)))
		available-host-types)
      (do ((index 0 (+ index 1)))
	  ((= index n-types))
	(if (not (vector-ref types index))
	    (vector-set! types index (make-unimplemented-host-type index))))
      (set! host-types types)
      (set! local-host (make-host host-type #f))))
  (set! *default-pathname-defaults*
	(make-pathname local-host #f #f #f #f #f))
  (set! library-directory-path
	(map pathname-as-directory
	     (vector->list ((ucode-primitive microcode-library-path 0)))))
  unspecific)

(define (initialize-package!)
  (reset-package!)
  (add-event-receiver! event:after-restore reset-package!))

(define (initialize-parser-method!)
  (define-bracketed-object-parser-method 'PATHNAME pathname-parser-method))
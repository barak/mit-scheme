#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/pathnm.scm,v 14.18 1991/11/05 20:37:02 cph Exp $

Copyright (c) 1988-91 Massachusetts Institute of Technology

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
		   (named (string->symbol "#[(runtime pathname)pathname]"))
		   (constructor %make-pathname)
		   (conc-name %pathname-)
		   (print-procedure
		    (unparser/standard-method 'PATHNAME
		      (lambda (state pathname)
			(unparse-object state (->namestring pathname))))))
  (host false read-only true)
  (device false read-only true)
  (directory false read-only true)
  (name false read-only true)
  (type false read-only true)
  (version false read-only true))

(define (->pathname object)
  (pathname-arg object false '->PATHNAME))

(define (pathname-arg object defaults operator)
  (cond ((pathname? object) object)
	((string? object) (parse-namestring object false defaults))
	(else (error:wrong-type-argument object "pathname" operator))))

(define (make-pathname host device directory name type version)
  (let ((host (if host (guarantee-host host 'MAKE-PATHNAME) local-host)))
    ((host-operation/make-pathname host)
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

(define (pathname-wild? pathname)
  (let ((pathname (->pathname pathname)))
    ((host-operation/pathname-wild? (%pathname-host pathname)) pathname)))

(define (pathname-simplify pathname)
  (let ((pathname (->pathname pathname)))
    ((host-operation/pathname-simplify (%pathname-host pathname)) pathname)))

(define (directory-pathname pathname)
  (let ((pathname (->pathname pathname)))
    (%make-pathname (%pathname-host pathname)
		    (%pathname-device pathname)
		    (%pathname-directory pathname)
		    false
		    false
		    false)))

(define (file-pathname pathname)
  (let ((pathname (->pathname pathname)))
    (%make-pathname (%pathname-host pathname)
		    false
		    false
		    (%pathname-name pathname)
		    (%pathname-type pathname)
		    (%pathname-version pathname))))

(define (pathname-as-directory pathname)
  (let ((pathname (->pathname pathname)))
    ((host-operation/pathname-as-directory (%pathname-host pathname))
     pathname)))

(define (directory-pathname-as-file pathname)
  (let ((pathname (->pathname pathname)))
    ((host-operation/directory-pathname-as-file (%pathname-host pathname))
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
	   ((host-operation/parse-namestring host) namestring host))
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
  ((host-operation/pathname->namestring (%pathname-host pathname)) pathname))

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
		 (not (eq? (car directory) (car default))))
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
  (index false read-only true)
  (name false read-only true)
  (operation/parse-namestring false read-only true)
  (operation/pathname->namestring false read-only true)
  (operation/make-pathname false read-only true)
  (operation/pathname-wild? false read-only true)
  (operation/pathname-as-directory false read-only true)
  (operation/directory-pathname-as-file false read-only true)
  (operation/pathname->truename false read-only true)
  (operation/user-homedir-pathname false read-only true)
  (operation/init-file-pathname false read-only true)
  (operation/pathname-simplify false read-only true))

(define-structure (host
		   (named (string->symbol "#[(runtime pathname)host]"))
		   (constructor %make-host)
		   (conc-name host/))
  (type-index false read-only true)
  (name false read-only true))

(define (make-host type name)
  (%make-host (host-type/index type) name))

(define (host/type host)
  (vector-ref host-types (host/type-index host)))

(define (host=? x y)
  (and (= (host/type-index x) (host/type-index y))
       (equal? (host/name x) (host/name y))))

(define (guarantee-host host operation)
  (if (not (host? host))
      (error:wrong-type-argument host "host" operation))
  host)

(define (host-operation/parse-namestring host)
  (host-type/operation/parse-namestring (host/type host)))

(define (host-operation/pathname->namestring host)
  (host-type/operation/pathname->namestring (host/type host)))

(define (host-operation/make-pathname host)
  (host-type/operation/make-pathname (host/type host)))

(define (host-operation/pathname-wild? host)
  (host-type/operation/pathname-wild? (host/type host)))

(define (host-operation/pathname-as-directory host)
  (host-type/operation/pathname-as-directory (host/type host)))

(define (host-operation/directory-pathname-as-file host)
  (host-type/operation/directory-pathname-as-file (host/type host)))

(define (host-operation/pathname->truename host)
  (host-type/operation/pathname->truename (host/type host)))

(define (host-operation/user-homedir-pathname host)
  (host-type/operation/user-homedir-pathname (host/type host)))

(define (host-operation/init-file-pathname host)
  (host-type/operation/init-file-pathname (host/type host)))

(define (host-operation/pathname-simplify host)
  (host-type/operation/pathname-simplify (host/type host)))

;;;; File System Stuff

(define (->truename pathname)
  (let ((pathname (merge-pathnames pathname)))
    ((host-operation/pathname->truename (%pathname-host pathname)) pathname)))

(define (user-homedir-pathname #!optional host)
  (let ((host
	 (if (and (not (default-object? host)) host)
	     (guarantee-host host 'USER-HOMEDIR-PATHNAME)
	     local-host)))
    ((host-operation/user-homedir-pathname host) host)))

(define (init-file-pathname #!optional host)
  (let ((host
	 (if (and (not (default-object? host)) host)
	     (guarantee-host host 'INIT-FILE-PATHNAME)
	     local-host)))
    ((host-operation/init-file-pathname host) host)))

(define (system-library-pathname pathname)
  (let ((try-directory
	 (lambda (directory)
	   (let ((pathname (merge-pathnames pathname directory)))
	     (and (file-exists? pathname)
		  pathname))))
	(loser
	 (lambda ()
	   (system-library-pathname
	    (->pathname
	     (error:file-operation pathname
				   "find"
				   "file"
				   "no such file in system library path"
				   system-library-pathname
				   (list pathname)))))))
    (if (pathname-absolute? pathname)
	(if (file-exists? pathname) pathname (loser))
	(let loop ((directories library-directory-path))
	  (if (null? directories)
	      (loser)
	      (or (try-directory (car directories))
		  (loop (cdr directories))))))))

(define (system-library-directory-pathname pathname)
  (if (not pathname)
      (let ((pathname
	     (list-search-positive library-directory-path file-directory?)))
	(if (not pathname)
	    (error "can't find system library directory"))
	(pathname-as-directory pathname))
      (let loop ((directories library-directory-path))
	(and (not (null? directories))
	     (let ((pathname (merge-pathnames pathname (car directories))))
	       (if (file-directory? pathname)
		   (pathname-as-directory pathname)
		   (loop (cdr directories))))))))

(define library-directory-path)

(define (initialize-package!)
  (reset-package!)
  (add-event-receiver! event:after-restore reset-package!))

(define (reset-package!)
  (let ((unix-host-type (make-unix-host-type 0)))
    (set! host-types (vector unix-host-type))
    (set! local-host (make-host unix-host-type false)))
  (set! *default-pathname-defaults*
	(make-pathname local-host false false false false false))
  (set! library-directory-path
	(map pathname-as-directory
	     (vector->list ((ucode-primitive microcode-library-path 0)))))
  unspecific)
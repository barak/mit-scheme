#| -*-Scheme-*-

$Id: ntprm.scm,v 1.14 1997/11/11 13:20:21 cph Exp $

Copyright (c) 1992-97 Massachusetts Institute of Technology

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

;;;; Miscellaneous Win32 Primitives
;;; package: ()

(declare (usual-integrations))

(define (file-directory? filename)
  ((ucode-primitive file-directory? 1)
   (->namestring (merge-pathnames filename))))

(define (file-symbolic-link? filename)
  filename				; ignored
  false)

(define (file-modes filename)
  ((ucode-primitive file-modes 1)
   (->namestring (merge-pathnames filename))))

(define (set-file-modes! filename modes)
  ((ucode-primitive set-file-modes! 2)
   (->namestring (merge-pathnames filename))
   modes))

(define-integrable nt-file-mode/read-only  #x001)
(define-integrable nt-file-mode/hidden     #x002)
(define-integrable nt-file-mode/system     #x004)
(define-integrable nt-file-mode/directory  #x010)
(define-integrable nt-file-mode/archive    #x020)
(define-integrable nt-file-mode/normal     #x080)
(define-integrable nt-file-mode/temporary  #x100)
(define-integrable nt-file-mode/compressed #x800)

(define (file-access filename amode)
  ((ucode-primitive file-access 2)
   (->namestring (merge-pathnames filename))
   amode))

;; upwards compatability
(define dos/file-access file-access)

(define (file-readable? filename)
  (file-access filename 4))

(define (file-writable? filename)
  (let ((pathname (merge-pathnames filename)))
    (let ((filename (->namestring pathname)))
      (or ((ucode-primitive file-access 2) filename 2)
	  (and (not ((ucode-primitive file-exists? 1) filename))
	       ((ucode-primitive file-access 2)
		(directory-namestring pathname)
		2))))))

(define (temporary-file-pathname)
  (let ((root (merge-pathnames "_scm_tmp" (temporary-directory-pathname))))
    (let loop ((ext 0))
      (let ((pathname (pathname-new-type root (number->string ext))))
	(if (allocate-temporary-file pathname)
	    pathname
	    (begin
	      (if (> ext 999)
		  (error "Can't find unique temporary pathname:" root))
	      (loop (+ ext 1))))))))

(define (temporary-directory-pathname)
  (let ((try-directory
	 (lambda (directory)
	   (let ((directory
		  (pathname-as-directory (merge-pathnames directory))))
	     (and (file-directory? directory)
		  (file-writable? directory)
		  directory)))))
    (let ((try-variable
	   (lambda (name)
	     (let ((value (get-environment-variable name)))
	       (and value
		    (try-directory value))))))
      (or (try-variable "TEMP")
	  (try-variable "TMP")
	  (try-directory "/tmp")
	  (try-directory "c:/")
	  (try-directory ".")
	  (try-directory "/")
	  (error "Can't find temporary directory.")))))

(define (file-attributes filename)
  ((ucode-primitive file-attributes 1)
   (->namestring (merge-pathnames filename))))
(define file-attributes-direct file-attributes)
(define file-attributes-indirect file-attributes)

(define-structure (file-attributes (type vector)
				   (constructor #f)
				   (conc-name file-attributes/))
  (type #f read-only #t)
  (n-links #f read-only #t)
  (uid #f read-only #t)
  (gid #f read-only #t)
  (access-time #f read-only #t)
  (modification-time #f read-only #t)
  (change-time #f read-only #t)
  (length #f read-only #t)
  (mode-string #f read-only #t)
  (inode-number #f read-only #t)
  (modes #f read-only #t))

(define (file-length namestring)
  (let ((attr (file-attributes namestring)))
    (and attr
	 (file-attributes/length attr))))

(define (file-modification-time filename)
  ((ucode-primitive file-mod-time 1)
   (->namestring (merge-pathnames filename))))
(define file-modification-time-direct file-modification-time)
(define file-modification-time-indirect file-modification-time)

(define (file-access-time namestring)
  (let ((attr (file-attributes namestring)))
    (and attr
	 (file-attributes/access-time attr))))
(define file-access-time-direct file-modification-time-direct)
(define file-access-time-indirect file-modification-time-indirect)

(define (set-file-times! filename access-time modification-time)
  (let ((filename (->namestring (merge-pathnames filename))))
    ((ucode-primitive set-file-times! 3)
     filename
     (or access-time (file-access-time filename))
     (or modification-time (file-modification-time filename)))))

(define (decode-file-time time)
  (decode-universal-time (file-time->universal-time time)))

(define (encode-file-time dt)
  (universal-time->file-time (encode-universal-time dt)))

(define (file-time->universal-time time) (+ time epoch))
(define (universal-time->file-time time) (- time epoch))

(define get-environment-variable)
(define set-environment-variable!)
(define set-environment-variable-default!)
(define delete-environment-variable!)
(define reset-environment-variables!)
(let ((environment-variables '())
      (environment-defaults '()))

  ;; Kludge: since getenv returns false for unbound,
  ;; that can also be the marker for a deleted variable
  (define-integrable *variable-deleted* false)

  (define (env-error proc var)
    (error "Variable must be a string:" var proc))

  (define (default-variable! var val)
    (if (and (not (assoc var environment-variables))
	     (not ((ucode-primitive get-environment-variable 1) var)))
	(set! environment-variables
	      (cons (cons var (if (procedure? val) (val) val))
		    environment-variables)))
    unspecific)

  (set! get-environment-variable
	(lambda (variable)
	  (if (not (string? variable))
	      (env-error 'GET-ENVIRONMENT-VARIABLE variable))
	  (let ((variable (string-upcase variable)))
	    (cond ((assoc variable environment-variables)
		   => cdr)
		  (else
		   ((ucode-primitive get-environment-variable 1) variable))))))

  (set! set-environment-variable!
	(lambda (variable value)
	  (if (not (string? variable))
	      (env-error 'SET-ENVIRONMENT-VARIABLE! variable))
	  (let ((variable (string-upcase variable)))
	    (cond ((assoc variable environment-variables)
		   => (lambda (pair) (set-cdr! pair value)))
		  (else
		   (set! environment-variables
			 (cons (cons variable value) environment-variables)))))
	  unspecific))

  (set! delete-environment-variable!
	(lambda (variable)
	  (if (not (string? variable))
	      (env-error 'DELETE-ENVIRONMENT-VARIABLE! variable))
	  (set-environment-variable! variable *variable-deleted*)))

  (set! reset-environment-variables!
	(lambda ()
	  (set! environment-variables '())
	  (for-each (lambda (def) (default-variable! (car def) (cdr def)))
		    environment-defaults)))

  (set! set-environment-variable-default!
	(lambda (var val)
	  (if (not (string? var))
	      (env-error 'SET-ENVIRONMENT-VARIABLE-DEFAULT! var))
	  (let ((var (string-upcase var)))
	    (cond ((assoc var environment-defaults)
		   => (lambda (pair) (set-cdr! pair val)))
		  (else
		   (set! environment-defaults
			 (cons (cons var val) environment-defaults))))
	    (default-variable! var val))))

)				; End LET

(define (current-home-directory)
  (or (nt/current-home-directory)
      (user-home-directory (current-user-name))))

(define (current-user-name)
  (or (get-environment-variable "USERNAME")
      (get-environment-variable "USER")
      (let ((homedir (nt/current-home-directory)))
	(and homedir
	     (pathname-name
	      (directory-pathname-as-file (directory-pathname homedir)))))
      (error "Unable to determine current user name.")))

(define (user-home-directory user-name)
  (let ((try
	 (lambda (directory)
	   (pathname-as-directory
	    (merge-pathnames user-name directory)))))
    (cond ((get-environment-variable "USERDIR")
	   => (lambda (userdir)
		(try (pathname-as-directory (merge-pathnames userdir)))))
	  ((nt/current-home-directory)
	   => (lambda (homedir)
		(if (string=? user-name (current-user-name))
		    homedir
		    (try (directory-pathname-as-file homedir)))))
	  (else
	   (error "Can't find user's home directory:" user-name)))))

(define (nt/current-home-directory)
  (let ((homedrive (get-environment-variable "HOMEDRIVE"))
	(homepath (get-environment-variable "HOMEPATH"))
	(home (get-environment-variable "HOME")))
    (and (or homepath home)
	 (pathname-as-directory
	  (merge-pathnames (or homepath home) homedrive)))))

(define dos/user-home-directory user-home-directory)
(define dos/current-user-name current-user-name)
(define dos/current-home-directory current-home-directory)

(define (file-touch filename)
  ((ucode-primitive file-touch 1)
   (->namestring (merge-pathnames filename))))

(define (make-directory name)
  ((ucode-primitive directory-make 1)
   (->namestring (directory-pathname-as-file (merge-pathnames name)))))

(define (delete-directory name)
  ((ucode-primitive directory-delete 1)
   (->namestring (directory-pathname-as-file (merge-pathnames name)))))

(define (os/file-end-of-line-translation pathname)
  (if (let ((type (dos/fs-drive-type pathname)))
	(or (string=? "NFS" (car type))
	    (string=? "NtNfs" (car type))
	    (string=? "Samba" (car type))))
      #f
      "\r\n"))

(define (os/default-end-of-line-translation)
  "\r\n")

(define (initialize-system-primitives!)
  (let ((reset!
	 (lambda ()
	   (reset-environment-variables!)
	   (cache-console-channel-descriptor!))))
    (reset!)
    (add-event-receiver! event:after-restart reset!))
  (set! nt/hide-subprocess-windows? #t)
  (set! nt/subprocess-argument-quote-char #f)
  (set! nt/subprocess-argument-escape-char #f)
  unspecific)

(define (dos/fs-drive-type pathname)
  ;; (system-name . [nfs-]mount-point)
  (cons (nt-volume-info/file-system-name (nt-volume-info pathname)) ""))

(define (dos/fs-long-filenames? pathname)
  ;; Currently we have a problem with long filenames on FAT systems because
  ;; the invented backup names may clash: FOO.SCM and FOO.SCM~ are confused.
  ;; The temporary fix is to treat backup names on FAT systems like the short
  ;; version, even if the VFAT driver is being used to provide long file names.
  (let* ((volume-info (nt-volume-info pathname))
	 (fs-type     (nt-volume-info/file-system-name volume-info)))
    (cond ((string-ci=? fs-type "VFAT")
	   'VFAT)			; ``kind of''
	  ((string-ci=? fs-type "FAT")
	   #F)
	  ((> (nt-volume-info/max-component-length volume-info) 32)
	   ;; 32 is random -- FAT is 12 and everything else is much larger.
	   #T)				; NTFS HPFS
	  (else #F))))			; FAT

(define (nt-volume-info pathname)
  (let ((root
	 (pathname-new-directory
	  (directory-pathname (merge-pathnames pathname))
	  '(ABSOLUTE))))
    (let ((info
	   ((ucode-primitive nt-get-volume-information 1)
	    (->namestring root))))
      (if (not info)
	  (error "Error reading volume information:" root))
      info)))

(define-structure (nt-volume-info (type vector)
				  (constructor #f)
				  (conc-name nt-volume-info/))
  (name #f read-only #t)
  (serial-number #f read-only #t)
  (max-component-length #f read-only #t)
  (file-system-flags #f read-only #t)
  (file-system-name #f read-only #t))

(define nt-fs-flag/case-sensitive-search	#x00000001)
(define nt-fs-flag/case-preserved-names		#x00000002)
(define nt-fs-flag/unicode-on-disk		#x00000004)
(define nt-fs-flag/persistent-acls		#x00000008)
(define nt-fs-flag/file-compression		#x00000010)
(define nt-fs-flag/volume-is-compressed		#x00008000)

(define (copy-file from to)
  ((ucode-primitive nt-copy-file 2) (->namestring (merge-pathnames from))
				    (->namestring (merge-pathnames to))))

(define (init-file-specifier->pathname specifier)

  (define (read-fat-init-file-map port)
    (let loop ((result '()))
      (let ((item (read port)))
	(if (eof-object? item)
	    result
	    (begin
	      (if (not (and (pair? item)
			    (init-file-specifier? (car item))
			    (string? (cdr item))))
		  (error "Malformed init-file map item:" item))
	      (loop (cons item result)))))))

  (define (generate-fat-init-file directory)
    (let loop ((index 1))
      (let ((filename
	     (string-append "ini"
			    (string-pad-left (number->string index) 5 #\0)
			    ".dat")))
	(if (file-exists? (merge-pathnames filename directory))
	    (loop (+ index 1))
	    filename))))

  (guarantee-init-file-specifier specifier 'INIT-FILE-SPECIFIER->PATHNAME)
  (let ((long-base (merge-pathnames ".mit-scheme/" (user-homedir-pathname))))
    (if (dos/fs-long-filenames? long-base)
	(if (null? specifier)
	    (directory-pathname-as-file long-base)
	    (merge-pathnames
	     (apply string-append
		    (cons (car specifier)
			  (append-map (lambda (string) (list "/" string))
				      (cdr specifier))))
	     long-base))
	(let ((short-base
	       (merge-pathnames "mitschem.ini/" (user-homedir-pathname))))
	  (let ((file-map-pathname (merge-pathnames "filemap.dat" short-base)))
	    (let ((port #f))
	      (dynamic-wind
	       (lambda ()
		 (set! port (open-i/o-file file-map-pathname))
		 unspecific)
	       (lambda ()
		 (merge-pathnames
		  (or (let ((entry
			     (assoc specifier (read-fat-init-file-map port))))
			(and entry
			     (cdr entry)))
		      (let ((filename (generate-fat-init-file short-base)))
			(let ((channel (port/output-channel port)))
			  (channel-file-set-position
			   channel
			   (channel-file-length channel)))
			(write (cons specifier filename) port)
			(newline port)
			filename))
		  short-base))
	       (lambda ()
		 (if port
		     (begin
		       (close-port port)
		       (set! port #f)
		       unspecific))))))))))

(define-structure (nt-select-registry (conc-name nt-select-registry/))
  descriptors)

(define (make-select-registry . descriptors)
  (make-nt-select-registry descriptors))

(define (add-to-select-registry! registry descriptor)
  (if (not (memv descriptor (nt-select-registry/descriptors registry)))
      (set-nt-select-registry/descriptors!
       registry
       (cons descriptor (nt-select-registry/descriptors registry)))))

(define (remove-from-select-registry! registry descriptor)
  (set-nt-select-registry/descriptors!
   registry
   (delv! descriptor (nt-select-registry/descriptors registry))))

(define (select-registry-test registry block?)
  (let ((descriptors (list->vector (nt-select-registry/descriptors registry))))
    (let ((result
	   ((ucode-primitive nt:waitformultipleobjects 3)
	    descriptors #f block?)))
      (cond ((and (fix:<= 0 result) (fix:< result (vector-length descriptors)))
	     (list (vector-ref descriptors result)))
	    ((fix:= result -1) #f)
	    ((fix:= result -2) 'INTERRUPT)
	    ((fix:= result -3) 'PROCESS-STATUS-CHANGE)
	    (else (error "Illegal result from select-internal:" result))))))

(define (select-descriptor descriptor block?)
  (let ((result
	 ((ucode-primitive nt:waitformultipleobjects 3)
	  (vector descriptor) #f block?)))
    (case result
      ((0) 'INPUT-AVAILABLE)
      ((-1) #f)
      ((-2) 'INTERRUPT)
      ((-3) 'PROCESS-STATUS-CHANGE)
      (else (error "Illegal result from select-internal:" result)))))

(define console-channel-descriptor)

(define (cache-console-channel-descriptor!)
  (set! console-channel-descriptor
	(channel-descriptor-for-select (tty-input-channel)))
  unspecific)

(define nt/hide-subprocess-windows?)
(define nt/subprocess-argument-quote-char)
(define nt/subprocess-argument-escape-char)

(define (os/make-subprocess filename arguments environment working-directory
			    ctty stdin stdout stderr)
  (if ctty
      (error "Can't manipulate controlling terminal of subprocess:" ctty))
  ((ucode-primitive nt-make-subprocess 8)
   filename
   (nt/rewrite-subprocess-arguments filename (vector->list arguments))
   (and environment
	(nt/rewrite-subprocess-environment (vector->list environment)))
   working-directory
   stdin
   stdout
   stderr
   (vector nt/hide-subprocess-windows?)))

(define (nt/rewrite-subprocess-environment strings)
  (let ((strings
	 (map car
	      (sort (map (lambda (binding)
			   (cons binding
				 (or (string-find-next-char binding #\=)
				     (string-length binding))))
			 strings)
		    (lambda (s1 s2)
		      (substring<? (car s1) 0 (cdr s1)
				   (car s2) 0 (cdr s2)))))))
    (let ((result
	   (make-string
	    (reduce +
		    0
		    (map (lambda (s) (fix:+ (string-length s) 1))
			 strings)))))
      (let loop ((strings strings) (index 0))
	(if (not (null? strings))
	    (let ((n (string-length (car strings))))
	      (substring-move-left! (car strings) 0 n result index)
	      (let ((index* (fix:+ index n)))
		(string-set! result index* #\NUL)
		(loop (cdr strings) (fix:+ index* 1))))))
      result)))

(define (nt/rewrite-subprocess-arguments program strings)
  ;; PROGRAM will eventually be used to determine the appropriate
  ;; escape character -- strangely enough, this depends on what
  ;; runtime library PROGRAM is linked with.
  program
  (let ((quote-char nt/subprocess-argument-quote-char)
	(escape-char nt/subprocess-argument-escape-char))
    (if (not quote-char)
	(nt/rewrite-subprocess-arguments/no-quoting strings)
	(nt/rewrite-subprocess-arguments/quoting strings
						 quote-char escape-char))))

(define (nt/rewrite-subprocess-arguments/no-quoting strings)
  (if (null? strings)
      ""
      (let ((result
	     (make-string
	      (fix:+ (reduce +
			     0
			     (map (lambda (s) (string-length s)) strings))
		     (fix:- (length strings) 1)))))
	(let ((n (string-length (car strings))))
	  (substring-move-left! (car strings) 0 n result 0)
	  (let loop ((strings (cdr strings)) (index n))
	    (if (not (null? strings))
		(let ((n (string-length (car strings))))
		  (string-set! result index #\space)
		  (substring-move-left! (car strings) 0 n
					result (fix:+ index 1))
		  (loop (cdr strings) (fix:+ (fix:+ index 1) n))))))
	result)))

(define (nt/rewrite-subprocess-arguments/quoting strings
						 quote-char escape-char)
  (define (analyze-arg s)
    (let ((need-quotes? #f)
	  (n (string-length s)))
      (do ((i 0 (fix:+ i 1))
	   (j 0
	      (fix:+ j
		     (let ((c (string-ref s i)))
		       (if (or (char=? quote-char c)
			       (char=? escape-char c))
			   (begin
			     (set! need-quotes? #t)
			     2)
			   (begin
			     (if (or (char=? #\space c)
				     (char=? #\tab c))
				 (set! need-quotes? #t))
			     1))))))
	  ((fix:= i n)
	   (cons (if need-quotes? (fix:+ j 2) j)
		 need-quotes?)))))
  (let ((analyses (map analyze-arg strings)))
    (let ((result (make-string (reduce + 0 (map car analyses)))))
      (define (do-arg index s analysis)
	(if (cdr analysis)
	    (begin
	      (vector-set! result index quote-char)
	      (let ((index (do-arg-1 index s)))
		(vector-set! result index quote-char)
		(fix:+ index 1)))
	    (do-arg-1 index s)))
      (define (do-arg-1 index s)
	(let ((n (string-length s)))
	  (do ((i 0 (fix:+ i 1))
	       (index index
		      (let ((c (string-ref s i)))
			(if (or (char=? quote-char c)
				(char=? escape-char c))
			    (begin
			      (vector-set! result index escape-char)
			      (vector-set! result (fix:+ index 1) c)
			      (fix:+ index 2))
			    (begin
			      (vector-set! result index c)
			      (fix:+ index 1))))))
	      ((fix:= i n) index))))
      (let loop ((index 0) (strings strings) (analyses analyses))
	(if (not (null? strings))
	    (loop (do-arg index (car strings) (car analyses))
		  (cdr strings)
		  (cdr analyses))))
      result)))
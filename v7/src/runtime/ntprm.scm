#| -*-Scheme-*-

$Id: ntprm.scm,v 1.4 1996/04/09 20:13:30 adams Exp $

Copyright (c) 1992-96 Massachusetts Institute of Technology

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

(define file-attributes-direct
  file-attributes)

(define file-attributes-indirect
  file-attributes)

(define-structure (file-attributes
		   (type vector)
		   (constructor false)
		   (conc-name file-attributes/))
  (type false read-only true)
  (n-links false read-only true)
  (uid false read-only true)
  (gid false read-only true)
  (access-time false read-only true)
  (modification-time false read-only true)
  (change-time false read-only true)
  (length false read-only true)
  (mode-string false read-only true)
  (inode-number false read-only true))

(define (file-length filename)
  (file-attributes/length (file-attributes filename)))

(define (file-modification-time filename)
  ((ucode-primitive file-mod-time 1)
   (->namestring (merge-pathnames filename))))

(define file-modification-time-direct
  file-modification-time)

(define file-modification-time-indirect
  file-modification-time)

;; These are obviously incorrect, but there is no alternative.
;; DOS only keeps track of modification times.

(define file-access-time-direct
  file-modification-time-direct)

(define file-access-time-indirect
  file-modification-time-indirect)

(define file-access-time
  file-modification-time)

(define (set-file-times! filename access-time modification-time)
  (let ((filename (->namestring (merge-pathnames filename)))
	(time (or modification-time
		  access-time
		  (file-modification-time-direct filename))))
    ((ucode-primitive set-file-times! 3)
     filename
     (or access-time time)
     (or modification-time time))))

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
      "nouser"))

(define (user-home-directory user-name)
  (or (and user-name
	   (let ((try
		  (lambda (directory)
		    (pathname-as-directory
		     (merge-pathnames user-name directory)))))
	     (cond ((get-environment-variable "USERDIR")
		    => (lambda (userdir)
			 (try (pathname-as-directory
			       (merge-pathnames userdir)))))
		   ((nt/current-home-directory)
		    => (lambda (homedir)
			 (try (directory-pathname-as-file homedir))))
		   (else #f))))
      (merge-pathnames "\\")))

(define (nt/current-home-directory)
  (let ((homedrive (get-environment-variable "HOMEDRIVE"))
	(homepath (get-environment-variable "HOMEPATH"))
	(home (get-environment-variable "HOME")))
    (and (or homepath home)
	 (pathname-as-directory
	  (merge-pathnames (or homepath home) homedrive)))))

(define (file-time->string time)
  (or ((ucode-primitive file-time->string 1) time)
      "Thu Jan  1 00:00:00 1970"))

(define (decode-file-time time) (decode-universal-time time))
(define (encode-file-time dt) (encode-universal-time dt))
(define (file-time->universal-time time) time)
(define (universal-time->file-time time) time)

(define dos/user-home-directory user-home-directory)
(define dos/current-user-name current-user-name)
(define dos/current-home-directory current-home-directory)
(define dos/file-time->string file-time->string)

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
  pathname
  "\r\n")

(define (os/default-end-of-line-translation)
  "\r\n")

(define (initialize-system-primitives!)
  (let ((reset!
	 (lambda ()
	   (reset-environment-variables!)
	   (cache-console-channel-descriptor!))))
    (reset!)
    (add-event-receiver! event:after-restart reset!)))

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

(define (copy-file from to)
  ((ucode-primitive nt-copy-file 2) (->namestring (merge-pathnames from))
				    (->namestring (merge-pathnames to))))

(define (select-internal console? handles block?)
  (let* ((nt/qs-allinput #xff)
	 (select
	  (if console?
	      (lambda (period)
		((ucode-primitive nt:msgwaitformultipleobjects 4)
		 handles #f period nt/qs-allinput))
	      (lambda (period)
		((ucode-primitive nt:waitformultipleobjects 3)
		 handles #f period)))))
    (if (not block?)
	(select 0)
	(let loop ()
	  (let ((res (select 20)))
	    (if (zero? res)
		(loop)
		res))))))
	       
(define console-channel-descriptor)

(define (cache-console-channel-descriptor!)
  (set! console-channel-descriptor ((ucode-primitive get-handle 1) 1))
  unspecific)

(define (select-descriptor descriptor block?)
  (define (select-result result)
    (cond ((fix:> result 0)
	   'INPUT-AVAILABLE)
	  ((fix:< result 0)
	   (error "Illegal result from select-internal" result))
	  (else
	   #f)))

  (select-result
   (if (= descriptor console-channel-descriptor)
       (select-internal true '#() block?)
       (select-internal false (vector descriptor) block?))))

(define-structure (nt-select-registry
		   (conc-name nt-select-registry/)
		   (constructor nt-select-registry/make))
  console
  descriptors)

(define-integrable (find-descriptor df dl)
  (list-search-positive dl
    (lambda (d)
      (= d df))))

(define (make-select-registry . descriptors)
  (cond ((find-descriptor console-channel-descriptor descriptors)
	 => (lambda (ccd)
	      (nt-select-registry/make console-channel-descriptor
				       (delq! ccd descriptors))))
	(else
	 (nt-select-registry/make false descriptors))))

(define (add-to-select-registry! registry descriptor)
  (cond ((= descriptor console-channel-descriptor)
	 (set-nt-select-registry/console! registry console-channel-descriptor))
	((not (find-descriptor descriptor
			       (nt-select-registry/descriptors registry)))
	 (set-nt-select-registry/descriptors!
	  registry
	  (cons descriptor (nt-select-registry/descriptors registry))))))

(define (remove-from-select-registry! registry descriptor)
  (cond ((= descriptor console-channel-descriptor)
	 (set-nt-select-registry/console! registry false))
	((find-descriptor descriptor (nt-select-registry/descriptors registry))
	 => (lambda (dr)
	      (set-nt-select-registry/descriptors!
	       registry
	       (delq! dr (nt-select-registry/descriptors registry)))))))

(define (select-registry-test registry block?)
  (let* ((handles (list->vector (nt-select-registry/descriptors registry)))
	 (result (select-internal (nt-select-registry/console registry)
				  handles
				  block?)))
    (cond ((fix:< result 0)
	   (error "Illegal result from select-internal" result))
	  ((fix:= result 0)
	   #f)
	  ((fix:> result (vector-length handles))
	   (list (nt-select-registry/console registry)))
	  (else
	   (list (vector-ref handles (fix:- result 1)))))))
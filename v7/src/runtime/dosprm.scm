#| -*-Scheme-*-

$Id: dosprm.scm,v 1.28 1995/04/23 05:18:58 cph Exp $

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

;;;; Miscellaneous DOS Primitives (emulation of unxprm version 1.16)
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
    (error "Variable must be a string" proc var))

  (define (default-variable! var val)
    (if (and (not (assoc var environment-variables))
	     (not ((ucode-primitive get-environment-variable 1)
		   var)))
	(set! environment-variables
	      (cons (cons var
			  (if (procedure? val)
			      (val)
			      val))
		    environment-variables)))
    unspecific)

  (set! get-environment-variable
	(lambda (variable)
	  (if (not (string? variable))
	      (env-error 'GET-ENVIRONMENT-VARIABLE variable)
	      (let ((variable (string-upcase variable)))
		(cond ((assoc variable environment-variables) => cdr)
		      (else ((ucode-primitive get-environment-variable 1)
			     variable)))))))

  (set! set-environment-variable!
	(lambda (variable value)
	  (if (not (string? variable))
	      (env-error 'SET-ENVIRONMENT-VARIABLE! variable)
	      (let ((variable (string-upcase variable)))
		(cond ((assoc variable environment-variables)
		       =>
		       (lambda (pair)
			 (set-cdr! pair value)))
		      (else
		       (set! environment-variables
			     (cons (cons variable value)
				   environment-variables))))))
	  unspecific))

  (set! delete-environment-variable!
	(lambda (variable)
	  (if (not (string? variable))
	      (env-error 'DELETE-ENVIRONMENT-VARIABLE! variable)
	      (set-environment-variable! variable *variable-deleted*))))

  (set! reset-environment-variables!
	(lambda ()
	  (set! environment-variables '())
	  (for-each (lambda (def)
		      (default-variable! (car def) (cdr def)))
		    environment-defaults)
	  unspecific))

  (set! set-environment-variable-default!
	(lambda (var val)
	  (if (not (string? var))
	      (env-error 'SET-ENVIRONMENT-VARIABLE-DEFAULT! var)
	      (let ((var (string-upcase var)))
		(cond ((assoc var environment-defaults)
		       => (lambda (pair)
			    (set-cdr! pair val)))
		      (else
		       (set! environment-defaults
			     (cons (cons var val)
				   environment-defaults))))
		(default-variable! var val)))))

  unspecific)				; End LET

(define (user-home-directory user-name)
  (or (and user-name
	   (let ((directory (get-environment-variable "USERDIR")))
	     (and directory
		  (pathname-new-name
		   (pathname-as-directory (merge-pathnames directory))
		   user-name))))
      "\\"))

(define (current-user-name)
  (or (get-environment-variable "USER")
      "nouser"))

(define (current-home-directory)
  (or (get-environment-variable "HOME")
      (user-home-directory (current-user-name))))

(define file-time->string
  (ucode-primitive file-time->string 1))

(define (decode-file-time time)
  (decode-universal-time time))

(define (encode-file-time dt)
  (encode-universal-time dt))

(define dos/user-home-directory user-home-directory)
(define dos/current-user-name current-user-name)
(define dos/current-home-directory current-home-directory)
(define dos/file-time->string file-time->string)

(define (file-touch filename)
  ((ucode-primitive file-touch 1)
   (->namestring (merge-pathnames filename))))

(define (make-directory name)
  ;; No pathname-as-directory here because DOS does not know how
  ;; to handle the trailing back-slash.
  ((ucode-primitive directory-make 1)
   (->namestring (merge-pathnames name))))

(define (delete-directory name)
  ;; No pathname-as-directory here because DOS does not know how
  ;; to handle the trailing back-slash.
  ((ucode-primitive directory-delete 1)
   (->namestring (merge-pathnames name))))

(define (os/default-end-of-line-translation)
  "\r\n")

(define (initialize-system-primitives!)
  (let ((reset!
	 (lambda ()
	   (reset-environment-variables!)
	   (cache-console-channel-descriptor!))))
    (reset!)
    (add-event-receiver! event:after-restart reset!)))

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
  (set! console-channel-descriptor
	(if (eq? 'DOS microcode-id/operating-system)
	    -1
	    ((ucode-primitive get-handle 1) 1)))
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
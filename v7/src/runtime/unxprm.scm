#| -*-Scheme-*-

$Id: unxprm.scm,v 1.21 1992/09/18 16:30:34 jinx Exp $

Copyright (c) 1988-1992 Massachusetts Institute of Technology

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

;;;; Miscellaneous Unix Primitives
;;; package: ()

(declare (usual-integrations))

(define (file-directory? filename)
  ((ucode-primitive file-directory? 1)
   (->namestring (merge-pathnames filename))))

(define (file-symbolic-link? filename)
  ((ucode-primitive file-symlink? 1)
   (->namestring (merge-pathnames filename))))

(define (file-modes filename)
  ((ucode-primitive file-modes 1) (->namestring (merge-pathnames filename))))

(define-integrable (set-file-modes! filename modes)
  ((ucode-primitive set-file-modes! 2)
   (->namestring (merge-pathnames filename))
   modes))

(define (file-access filename amode)
  ((ucode-primitive file-access 2)
   (->namestring (merge-pathnames filename))
   amode))

;; upwards compatability
(define unix/file-access file-access)

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

(define (call-with-temporary-filename receiver)
  (let find-eligible-directory
      ((eligible-directories 
	(let ((tmp (or (get-environment-variable "TEMP")
		       (get-environment-variable "TMP")))
	      (others '("." "/tmp" "/usr/tmp")))
	  (if (not tmp) others (cons tmp others)))))
    (if (null? eligible-directories)
	(error "Can't locate directory for temporary file")
	(let ((dir (->namestring
		    (pathname-as-directory
		     (merge-pathnames (car eligible-directories))))))
	  (if (and (file-directory? dir) (file-writable? dir))
	      (let ((base-name
		     (string-append dir "_" (unix/current-user-name) "_scm")))
		(let unique-file ((ext 0))
		  (let ((name (string-append base-name (number->string ext))))
		    (if (or (file-exists? name)
			    (not (file-touch name)))
			(if (fix:> ext 999) ; don't get rediculous here
			    (error "Cannot find unique temp file name"
				   base-name)
			    (unique-file (fix:+ ext 1)))
			(dynamic-wind
			 (lambda () unspecific)
			 (lambda () (receiver name))
			 (lambda () (if (file-exists? name)
					(delete-file name))))))))
	      (find-eligible-directory (cdr eligible-directories)))))))

(define (file-attributes-direct filename)
  ((ucode-primitive file-attributes 1)
   (->namestring (merge-pathnames filename))))

(define (file-attributes-indirect filename)
  ((ucode-primitive file-attributes-indirect 1)
   (->namestring (merge-pathnames filename))))

(define file-attributes
  file-attributes-direct)

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

(define (file-modification-time-direct filename)
  ((ucode-primitive file-mod-time 1)
   (->namestring (merge-pathnames filename))))

(define (file-modification-time-indirect filename)
  ((ucode-primitive file-mod-time-indirect 1)
   (->namestring (merge-pathnames filename))))

(define file-modification-time
  file-modification-time-indirect)

(define (file-access-time-direct filename)
  ((ucode-primitive file-access-time 1)
   (->namestring (merge-pathnames filename))))

(define (file-access-time-indirect filename)
  ((ucode-primitive file-access-time-indirect 1)
   (->namestring (merge-pathnames filename))))

(define file-access-time
  file-access-time-indirect)

(define (set-file-times! filename access-time modification-time)
  (let ((filename (->namestring (merge-pathnames filename))))
    ((ucode-primitive set-file-times! 3)
     filename
     (or access-time (file-access-time-direct filename))
     (or modification-time (file-modification-time-direct filename)))))

(define get-environment-variable)
(define set-environment-variable!)
(define delete-environment-variable!)
(define reset-environment-variables!)

(let ((environment-variables '()))
  ;; Kludge: since getenv returns false for unbound,
  ;; that can also be the marker for a deleted variable
  (define-integrable *variable-deleted* false)

  (set! get-environment-variable
	(lambda (variable)
	  (cond ((not (string? variable))
		 (error "GET-ENVIRONMENT-VARIABLE: Variable must be a string"
			variable))
		((assoc variable environment-variables)
		 =>
		 cdr)
		(else ((ucode-primitive get-environment-variable 1)
		       variable)))))

  (set! set-environment-variable!
	(lambda (variable value)
	  (cond ((not (string? variable))
		 (error "SET-ENVIRONMENT-VARIABLE!: Variable must be a string"
			variable value))
		((assoc variable environment-variables)
		 =>
		 (lambda (pair)
		   (set-cdr! pair value)))
		(else
		 (set! environment-variables
		       (cons (cons variable value)
			     environment-variables))))
	  unspecific))

  (set! delete-environment-variable!
	(lambda (variable)
	  (set-environment-variable! variable *variable-deleted*)))

  (set! reset-environment-variables!
	(lambda () (set! environment-variables '())))
) ; End LET

(define (unix/user-home-directory user-name)
  (let ((directory ((ucode-primitive get-user-home-directory 1) user-name)))
    (if (not directory)
	(error "Can't find user's home directory:" user-name))
    directory))

(define (unix/current-home-directory)
  (or (get-environment-variable "HOME")
      (unix/user-home-directory (unix/current-user-name))))

(define-integrable unix/current-user-name
  (ucode-primitive current-user-name 0))

(define-integrable unix/current-uid
  (ucode-primitive current-uid 0))

(define-integrable unix/current-gid
  (ucode-primitive current-gid 0))

(define-integrable unix/current-file-time
  (ucode-primitive current-file-time 0))

(define-integrable unix/file-time->string
  (ucode-primitive file-time->string 1))

(define (unix/uid->string uid)
  (or ((ucode-primitive uid->string 0) uid)
      (number->string uid 10)))

(define (unix/gid->string gid)
  (or ((ucode-primitive gid->string 0) gid)
      (number->string gid 10)))

(define-integrable unix/system
  (ucode-primitive system 1))

(define (file-touch filename)
  ((ucode-primitive file-touch 1) (->namestring (merge-pathnames filename))))

(define (make-directory name)
  ((ucode-primitive directory-make 1)
   (->namestring (pathname-as-directory (merge-pathnames name)))))

(define (delete-directory name)
  ((ucode-primitive directory-delete 1)
   (->namestring (pathname-as-directory (merge-pathnames name)))))

;;; Queues after-restart daemon to clean up environment space

(define (initialize-system-primitives!)
  (add-event-receiver! event:after-restart reset-environment-variables!))
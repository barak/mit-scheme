#| -*-Scheme-*-

$Id: dosprm.scm,v 1.10 1992/09/17 00:41:34 jinx Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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
  ((ucode-primitive file-directory?)
   (->namestring (merge-pathnames filename))))

(define (file-symbolic-link? filename)
  ((ucode-primitive file-symlink?) (->namestring (merge-pathnames filename))))

(define (file-modes filename)
  ((ucode-primitive file-modes) (->namestring (merge-pathnames filename))))

(define-integrable (set-file-modes! filename modes)
  ((ucode-primitive set-file-modes!) (->namestring (merge-pathnames filename))
				     modes))

(define (file-access filename amode)
  ((ucode-primitive file-access) (->namestring (merge-pathnames filename))
				 amode))

;; upwards compatability
(define dos/file-access file-access)

(define (file-readable? filename)
  (file-access filename 4))

(define (file-writable? filename)
  (let ((pathname (merge-pathnames filename)))
    (let ((filename (->namestring pathname)))
      (or ((ucode-primitive file-access) filename 2)
	  (and (not ((ucode-primitive file-exists?) filename))
	       ((ucode-primitive file-access) (directory-namestring pathname)
					      2))))))

(define (call-with-temporary-filename receiver)
  (let find-eligible-directory
      ((eligible-directories 
	(let ((tmp (or (get-environment-variable "TEMP")
		       (get-environment-variable "TMP")))
	      (others '("/tmp" "c:/" "." "/")))
	  (if (not tmp) others (cons tmp others)))))
    (if (null? eligible-directories)
	(error "Can't locate directory for temporary file")
	(let ((dir (->namestring
		    (pathname-as-directory
		     (merge-pathnames (car eligible-directories))))))
	  (if (and (file-directory? dir) (file-writable? dir))
	      (let ((base-name (string-append dir "_scm_tmp.")))
		(let unique-file ((ext 0))
		  (let ((name (string-append base-name (number->string ext))))
		    (if (or (file-exists? name) (not (file-touch name)))
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
  ((ucode-primitive file-attributes)
   (->namestring (merge-pathnames filename))))

(define (file-attributes-indirect filename)
  ((ucode-primitive file-attributes-indirect)
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
    ((ucode-primitive set-file-times!)
     filename
     (or access-time time)
     (or modification-time time))))

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
	  (if (not (string? variable))
	      (error "GET-ENVIRONMENT-VARIABLE: Variable must be a string"
		     variable)
	      (let ((variable (string-upcase variable)))
		(cond ((assoc variable environment-variables) => cdr)
		      (else ((ucode-primitive get-environment-variable)
			     variable)))))))
  (set! set-environment-variable!
	(lambda (variable value)
	  (if (string? variable)
	      (let ((variable (string-upcase variable)))
		(cond ((assoc variable environment-variables)
		       =>
		       (lambda (pair)
			 (set-cdr! pair value)))
		      (else
		       (set! environment-variables
			     (cons (cons variable value)
				   environment-variables)))))
	      (error "SET-ENVIRONMENT-VARIABLE!: Variable must be a string"
		     variable value))
	  unspecific))
  (set! delete-environment-variable!
	(lambda (variable)
	  (set-environment-variable! variable *variable-deleted*)))
  (set! reset-environment-variables!
	(lambda () (set! environment-variables '())))
) ; End LET

(define (dos/user-home-directory user-name)
  (let ((directory ((ucode-primitive get-user-home-directory) user-name)))
    (if (not directory)
	(error "Can't find user's home directory:" user-name))
    directory))

(define (dos/current-home-directory)
  (or (get-environment-variable "HOME")
      (dos/user-home-directory (dos/current-user-name))))

(define-integrable dos/current-user-name
  (ucode-primitive current-user-name))

(define-integrable dos/current-file-time
  (ucode-primitive current-file-time))

(define-integrable dos/file-time->string
  (ucode-primitive file-time->string))

(define (file-touch filename)
  ((ucode-primitive file-touch)
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

;;; Queues after-restart daemon to clean up environment space

(define (initialize-system-primitives!)
  (add-event-receiver! event:after-restart reset-environment-variables!))
#| -*-Scheme-*-

$Id: unxprm.scm,v 1.33 1995/01/31 19:34:50 cph Exp $

Copyright (c) 1988-95 Massachusetts Institute of Technology

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
  ((ucode-primitive file-access 2)
   (let ((pathname (merge-pathnames filename)))
     (let ((filename (->namestring pathname)))
       (if ((ucode-primitive file-exists? 1) filename)
	   filename
	   (directory-namestring pathname))))
   2))

(define (file-executable? filename)
  (file-access filename 1))

(define (temporary-file-pathname)
  (let ((root-string
	 (string-append "sch"
			(string-pad-left (number->string (unix/current-pid))
					 6
					 #\0)
			"_"))
	(directory (temporary-directory-pathname)))
    (let loop ((ext 0))
      (let ((pathname
	     (merge-pathnames (string-append root-string (number->string ext))
			      directory)))
	(if (allocate-temporary-file pathname)
	    pathname
	    (begin
	      (if (> ext 999)
		  (error "Can't find unique temporary pathname:"
			 (merge-pathnames root-string directory)))
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
	  (try-directory "/usr/tmp")
	  (error "Can't find temporary directory.")))))

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

(define (file-length filename)
  (file-attributes/length (file-attributes-direct filename)))

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

(define-integrable unix/file-time->string
  (ucode-primitive file-time->string 1))

(define (unix/uid->string uid)
  (or ((ucode-primitive uid->string 1) uid)
      (number->string uid 10)))

(define (unix/gid->string gid)
  (or ((ucode-primitive gid->string 1) gid)
      (number->string gid 10)))

(define-integrable unix/current-pid
  (ucode-primitive current-pid 0))

(define (unix/system string)
  (let ((wd-inside (->namestring (working-directory-pathname)))
	(wd-outside)
	(ti-outside))
    (dynamic-wind
     (lambda ()
       (set! wd-outside ((ucode-primitive working-directory-pathname 0)))
       ((ucode-primitive set-working-directory-pathname! 1) wd-inside)
       (set! ti-outside (thread-timer-interval))
       (set-thread-timer-interval! #f))
     (lambda ()
       ((ucode-primitive system 1) string))
     (lambda ()
       ((ucode-primitive set-working-directory-pathname! 1) wd-outside)
       (set! wd-outside)
       (set-thread-timer-interval! ti-outside)
       (set! ti-outside)
       unspecific))))

(define (file-touch filename)
  ((ucode-primitive file-touch 1) (->namestring (merge-pathnames filename))))

(define (make-directory name)
  ((ucode-primitive directory-make 1)
   (->namestring (pathname-as-directory (merge-pathnames name)))))

(define (delete-directory name)
  ((ucode-primitive directory-delete 1)
   (->namestring (pathname-as-directory (merge-pathnames name)))))

(define (os/default-end-of-line-translation)
  #f)

;;; Queues after-restart daemon to clean up environment space

(define (initialize-system-primitives!)
  (add-event-receiver! event:after-restart reset-environment-variables!)
  (discard-select-registry-result-vectors!)
  (add-event-receiver! event:after-restart
		       discard-select-registry-result-vectors!))

(define (make-select-registry . descriptors)
  (let ((registry (make-string ((ucode-primitive select-registry-size 0)))))
    ((ucode-primitive select-registry-clear-all 1) registry)
    (do ((descriptors descriptors (cdr descriptors)))
	((null? descriptors))
      ((ucode-primitive select-registry-set 2) registry (car descriptors)))
    registry))

(define (add-to-select-registry! registry descriptor)
  ((ucode-primitive select-registry-set 2) registry descriptor))

(define (remove-from-select-registry! registry descriptor)
  ((ucode-primitive select-registry-clear 2) registry descriptor))

(define (select-descriptor descriptor block?)
  (let ((result ((ucode-primitive select-descriptor 2) descriptor block?)))
    (case result
      ((0)
       #f)
      ((1)
       'INPUT-AVAILABLE)
      ((-1)
       (subprocess-global-status-tick)
       'PROCESS-STATUS-CHANGE)
      ((-2)
       'INTERRUPT)
      (else
       (error "Illegal result from CHANNEL-SELECT:" result)))))

(define (select-registry-test registry block?)
  (let ((result-vector (allocate-select-registry-result-vector)))
    (let ((result
	   ((ucode-primitive select-registry-test 3) registry block?
						     result-vector)))
      (if (fix:> result 0)
	  (let loop ((index (fix:- result 1)) (descriptors '()))
	    (let ((descriptors
		   (cons (vector-ref result-vector index) descriptors)))
	      (if (fix:= 0 index)
		  (begin
		    (deallocate-select-registry-result-vector result-vector)
		    descriptors)
		  (loop (fix:- index 1) descriptors))))
	  (begin
	    (deallocate-select-registry-result-vector result-vector)
	    (cond ((fix:= 0 result)
		   #f)
		  ((fix:= -1 result)
		   (subprocess-global-status-tick)
		   'PROCESS-STATUS-CHANGE)
		  ((fix:= -2 result)
		   'INTERRUPT)
		  (else
		   (error "Illegal result from SELECT-REGISTRY-TEST:"
			  result))))))))

(define select-registry-result-vectors)

(define (discard-select-registry-result-vectors!)
  (set! select-registry-result-vectors '())
  unspecific)

(define (allocate-select-registry-result-vector)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (let ((v
	   (let loop ((rv select-registry-result-vectors))
	     (cond ((null? rv)
		    (make-vector ((ucode-primitive select-registry-lub 0)) #f))
		   ((car rv)
		    => (lambda (v) (set-car! rv #f) v))
		   (else
		    (loop (cdr rv)))))))
      (set-interrupt-enables! interrupt-mask)
      v)))

(define (deallocate-select-registry-result-vector v)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (let loop ((rv select-registry-result-vectors))
      (cond ((null? rv)
	     (set! select-registry-result-vectors
		   (cons v select-registry-result-vectors)))
	    ((car rv)
	     (loop (cdr rv)))
	    (else
	     (set-car! rv v))))
    (set-interrupt-enables! interrupt-mask)))
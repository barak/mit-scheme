#| -*-Scheme-*-

$Id: unxprm.scm,v 1.61 2001/05/09 03:17:14 cph Exp $

Copyright (c) 1988-2001 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.
|#

;;;; Miscellaneous Unix Primitives
;;; package: ()

(declare (usual-integrations))

(define (file-modes filename)
  ((ucode-primitive file-modes 1) (->namestring (merge-pathnames filename))))

(define-integrable (set-file-modes! filename modes)
  ((ucode-primitive set-file-modes! 2)
   (->namestring (merge-pathnames filename))
   modes))

(define unix/file-access file-access)	;upwards compatability

(define (temporary-file-pathname #!optional directory)
  (let ((root-string
	 (string-append
	  "sch"
	  (string-pad-left (number->string (unix/current-pid)) 6 #\0)
	  "_"))
	(directory
	 (if (or (default-object? directory) (not directory))
	     (temporary-directory-pathname)
	     (pathname-as-directory directory))))
    (let loop ((ext 0))
      (let ((pathname
	     (merge-pathnames (string-append root-string (number->string ext))
			      directory)))
	(if (allocate-temporary-file pathname)
	    (begin
	      ;; Make sure file isn't readable or writeable by anyone
	      ;; other than the owner.
	      (set-file-modes! pathname
			       (fix:and (file-modes pathname)
					#o0700))
	      pathname)
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
		  (file-writeable? directory)
		  directory)))))
    (let ((try-variable
	   (lambda (name)
	     (let ((value (get-environment-variable name)))
	       (and value
		    (try-directory value))))))
      (or (try-variable "TMPDIR")
	  (try-variable "TEMP")
	  (try-variable "TMP")
	  (try-directory "/var/tmp")
	  (try-directory "/usr/tmp")
	  (try-directory "/tmp")
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

(define (user-home-directory user-name)
  (let ((directory ((ucode-primitive get-user-home-directory 1) user-name)))
    (if (not directory)
	(error "Can't find user's home directory:" user-name))
    (pathname-as-directory directory)))

(define (current-home-directory)
  (let ((home (get-environment-variable "HOME")))
    (if home
	(pathname-as-directory home)
	(user-home-directory (current-user-name)))))

(define (current-user-name)
  (or (get-environment-variable "USER")
      ((ucode-primitive current-user-name 0))))

(define (file-time->local-decoded-time time)
  (universal-time->local-decoded-time (file-time->universal-time time)))

(define (file-time->global-decoded-time time)
  (universal-time->global-decoded-time (file-time->universal-time time)))

(define (decoded-time->file-time dt)
  (universal-time->file-time (decoded-time->universal-time dt)))

(define (file-time->universal-time time) (+ time epoch))
(define (universal-time->file-time time) (- time epoch))

(define decode-file-time file-time->local-decoded-time)
(define encode-file-time decoded-time->file-time)
(define unix/user-home-directory user-home-directory)
(define unix/current-home-directory current-home-directory)
(define unix/current-user-name current-user-name)

(define-integrable unix/current-uid (ucode-primitive current-uid 0))
(define-integrable unix/current-gid (ucode-primitive current-gid 0))
(define-integrable unix/current-pid (ucode-primitive current-pid 0))

(define (unix/uid->string uid)
  (or ((ucode-primitive uid->string 1) uid)
      (number->string uid 10)))

(define (unix/gid->string gid)
  (or ((ucode-primitive gid->string 1) gid)
      (number->string gid 10)))

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

(define (os/file-end-of-line-translation pathname)
  ;; This works because the line translation is harmless when not
  ;; needed.  We can't tell when it is needed, because FAT and HPFS
  ;; filesystems can be mounted with automatic translation (in the
  ;; Linux kernel), and ISO9660 can be either DOS or unix format.
  (let ((type
	 ((ucode-primitive file-system-type 1)
	  (->namestring
	   (let loop ((pathname (merge-pathnames pathname)))
	     (if (file-exists? pathname)
		 pathname
		 (loop (directory-pathname-as-file
			(directory-pathname pathname)))))))))
    (if (or (string-ci=? "fat" type)
	    (string-ci=? "hpfs" type)
	    (string-ci=? "iso9660" type)
	    (string-ci=? "ntfs" type)
	    (string-ci=? "smb" type))
	"\r\n"
	#f)))

(define (os/default-end-of-line-translation)
  #f)

(define (copy-file from to)
  (let ((input-filename (->namestring (merge-pathnames from)))
	(output-filename (->namestring (merge-pathnames to))))
    (let ((input-channel false)
	  (output-channel false))
      (dynamic-wind
       (lambda ()
	 (set! input-channel (file-open-input-channel input-filename))
	 (set! output-channel (file-open-output-channel output-filename))
	 unspecific)
       (lambda ()
	 (let ((source-length (channel-file-length input-channel))
	       (buffer-length 8192))
	   (if (zero? source-length)
	       0
	       (let* ((buffer (make-string buffer-length))
		      (transfer
		       (lambda (length)
			 (let ((n-read
				(channel-read-block input-channel
						    buffer
						    0
						    length)))
			   (if (positive? n-read)
			       (channel-write-block output-channel
						    buffer
						    0
						    n-read))
			   n-read))))
		 (let loop ((source-length source-length))
		   (if (< source-length buffer-length)
		       (transfer source-length)
		       (let ((n-read (transfer buffer-length)))
			 (if (= n-read buffer-length)
			     (+ (loop (- source-length buffer-length))
				buffer-length)
			     n-read))))))))
       (lambda ()
	 (if output-channel (channel-close output-channel))
	 (if input-channel (channel-close input-channel)))))
    (set-file-times! output-filename
		     #f
		     (file-modification-time input-filename))
    (set-file-modes! output-filename (file-modes input-filename))))

(define (init-file-specifier->pathname specifier)
  (guarantee-init-file-specifier specifier 'INIT-FILE-SPECIFIER->PATHNAME)
  (merge-pathnames (apply string-append
			  (cons ".mit-scheme"
				(append-map (lambda (string) (list "/" string))
					    specifier)))
		   (user-homedir-pathname)))

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

;;;; Subprocess/Shell Support

(define (os/make-subprocess filename arguments environment working-directory
			    ctty stdin stdout stderr)
  ((ucode-primitive ux-make-subprocess 8)
   filename arguments environment working-directory
   ctty stdin stdout stderr))

(define (os/find-program program default-directory #!optional exec-path error?)
  (let ((namestring
	 (let ((exec-path
		(if (default-object? exec-path)
		    (os/exec-path)
		    exec-path)))
	   (let ((try
		  (lambda (pathname)
		    (and (file-access pathname 1)
			 (->namestring pathname)))))
	     (cond ((pathname-absolute? program)
		    (try program))
		   ((not default-directory)
		    (let loop ((path exec-path))
		      (and (not (null? path))
			   (or (and (car path)
				    (pathname-absolute? (car path))
				    (try (merge-pathnames program (car path))))
			       (loop (cdr path))))))
		   (else
		    (let ((default-directory
			    (merge-pathnames default-directory)))
		      (let loop ((path exec-path))
			(and (not (null? path))
			     (or (try (merge-pathnames
				       program
				       (if (car path)
					   (merge-pathnames (car path)
							    default-directory)
					   default-directory)))
				 (loop (cdr path))))))))))))
    (if (and (not namestring)
	     (if (default-object? error) #t error?))
	(error "Can't find program:" (->namestring program)))
    namestring))

(define (os/exec-path)
  (os/parse-path-string
   (let ((path (get-environment-variable "PATH")))
     (if (not path)
	 (error "Can't find PATH environment variable."))
     path)))

(define (os/parse-path-string string)
  (let ((end (string-length string))
	(substring
	 (lambda (string start end)
	   (pathname-as-directory (substring string start end)))))
    (let loop ((start 0))
      (if (< start end)
	  (let ((index (substring-find-next-char string start end #\:)))
	    (if index
		(cons (if (= index start)
			  false
			  (substring string start index))
		      (loop (+ index 1)))
		(list (substring string start end))))
	  '()))))

(define (os/shell-file-name)
  (or (get-environment-variable "SHELL")
      "/bin/sh"))

(define (os/form-shell-command command)
  (list "-c" command))

(define (os/executable-pathname-types)
  '())
#| -*-Scheme-*-

$Id: unxprm.scm,v 1.67 2004/10/18 05:05:52 cph Exp $

Copyright 1988,1989,1990,1991,1992,1993 Massachusetts Institute of Technology
Copyright 1994,1995,1997,1998,1999,2000 Massachusetts Institute of Technology
Copyright 2001,2003,2004 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Miscellaneous Unix Primitives
;;; package: (runtime os-primitives)

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
  (inode-number #f read-only #t))

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
  ;; Kludge: since getenv returns #f for unbound,
  ;; that can also be the marker for a deleted variable
  (define-integrable *variable-deleted* #f)

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
	(lambda () (set! environment-variables '()))))

(define (initialize-system-primitives!)
  (add-event-receiver! event:after-restart reset-environment-variables!))

(define (os/pathname-mime-type pathname)
  (let ((suffix (pathname-type pathname)))
    (and (string? suffix)
	 (or (search-mime-types-file "~/.mime.types" suffix)
	     (search-mime-types-file "/etc/mime.types" suffix)))))

(define (search-mime-types-file pathname suffix)
  (and (file-readable? pathname)
       (call-with-input-file pathname
	 (lambda (port)
	   (let loop ()
	     (let ((line (read-line port)))
	       (and (not (eof-object? line))
		    (let ((line (string-trim line)))
		      (if (or (string-null? line)
			      (char=? (string-ref line 0) #\#))
			  (loop)
			  (let ((tokens
				 (burst-string line char-set:whitespace #t)))
			    (if (there-exists? (cdr tokens)
				  (lambda (suffix*)
				    (string=? suffix* suffix)))
				(car tokens)
				(loop))))))))))))

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

(define (file-line-ending pathname)
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
	'CRLF
	'LF)))

(define (default-line-ending)
  'LF)

(define (copy-file from to)
  (let ((input-filename (->namestring (merge-pathnames from)))
	(output-filename (->namestring (merge-pathnames to))))
    (let ((input-channel #f)
	  (output-channel #f))
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
		      (and (pair? path)
			   (or (and (car path)
				    (pathname-absolute? (car path))
				    (try (merge-pathnames program (car path))))
			       (loop (cdr path))))))
		   (else
		    (let ((default-directory
			    (merge-pathnames default-directory)))
		      (let loop ((path exec-path))
			(and (pair? path)
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
			  #f
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
#| -*-Scheme-*-

$Id: os2prm.scm,v 1.50 2003/01/22 02:05:21 cph Exp $

Copyright 1994,1995,1997,1998,1999,2000 Massachusetts Institute of Technology
Copyright 2001,2003 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; Miscellaneous OS/2 Primitives
;;; package: (runtime os-primitives)

(declare (usual-integrations))

(define (file-modes filename)
  ((ucode-primitive file-attributes 1)
   (->namestring (merge-pathnames filename))))

(define (set-file-modes! filename modes)
  ((ucode-primitive set-file-attributes! 2)
   (->namestring (merge-pathnames filename))
   modes))

(define-integrable os2-file-mode/read-only #x01)
(define-integrable os2-file-mode/hidden    #x02)
(define-integrable os2-file-mode/system    #x04)
(define-integrable os2-file-mode/directory #x10)
(define-integrable os2-file-mode/archived  #x20)

(define (file-length filename)
  ((ucode-primitive file-length 1)
   (->namestring (merge-pathnames filename))))

(define (file-modification-time filename)
  ((ucode-primitive file-mod-time 1)
   (->namestring (merge-pathnames filename))))
(define file-modification-time-direct file-modification-time)
(define file-modification-time-indirect file-modification-time)

(define (file-access-time filename)
  ((ucode-primitive file-access-time 1)
   (->namestring (merge-pathnames filename))))
(define file-access-time-direct file-access-time)
(define file-access-time-indirect file-access-time)

(define (set-file-times! filename access-time modification-time)
  ((ucode-primitive set-file-times! 3)
   (->namestring (merge-pathnames filename))
   access-time
   modification-time))

(define (file-time->local-decoded-time time)
  (let* ((twosecs (remainder time 32)) (time (quotient time 32))
	 (minutes (remainder time 64)) (time (quotient time 64))
	 (hours   (remainder time 32)) (time (quotient time 32))
	 (day     (remainder time 32)) (time (quotient time 32))
	 (month   (remainder time 16)) (year (quotient time 16)))
    (make-decoded-time (* twosecs 2) minutes hours day month (+ 1980 year))))

(define (file-time->global-decoded-time time)
  (universal-time->global-decoded-time (file-time->universal-time time)))

(define (decoded-time->file-time dt)
  (let ((f (lambda (i j k) (+ (* i j) k))))
    (f (f (f (f (f (let ((year (decoded-time/year dt)))
		     (if (< year 1980)
			 (error "Can't encode years earlier than 1980:" year))
		     year)
		   16 (decoded-time/month dt))
		32 (decoded-time/day dt))
	     32 (decoded-time/hour dt))
	  64 (decoded-time/minute dt))
       32 (quotient (decoded-time/second dt) 2))))

(define decode-file-time file-time->local-decoded-time)
(define encode-file-time decoded-time->file-time)

(define (file-time->universal-time time)
  (decoded-time->universal-time (file-time->local-decoded-time time)))

(define (universal-time->file-time time)
  (decoded-time->file-time (universal-time->local-decoded-time time)))

(define (file-attributes filename)
  ((ucode-primitive file-info 1)
   (->namestring (merge-pathnames filename))))
(define file-attributes-direct file-attributes)
(define file-attributes-indirect file-attributes)

(define-structure (file-attributes (type vector)
				   (constructor #f)
				   (conc-name file-attributes/))
  (type #f read-only #t)
  (access-time #f read-only #t)
  (modification-time #f read-only #t)
  (change-time #f read-only #t)
  (length #f read-only #t)
  (mode-string #f read-only #t)
  (modes #f read-only #t)
  (allocated-length #f read-only #t))

(define (file-attributes/n-links attributes) attributes 1)

(define (get-environment-variable name)
  ((ucode-primitive get-environment-variable 1) name))

(define (temporary-file-pathname #!optional directory)
  (let ((root
	 (let ((directory
		(if (or (default-object? directory) (not directory))
		    (temporary-directory-pathname)
		    (pathname-as-directory directory))))
	   (merge-pathnames
	    (if (dos/fs-long-filenames? directory)
		(string-append
		 "sch"
		 (string-pad-left (number->string (os2/current-pid)) 6 #\0))
		"_scm_tmp")
	    directory))))
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
		  (file-writeable? directory)
		  directory)))))
    (let ((try-variable
	   (lambda (name)
	     (let ((value (get-environment-variable name)))
	       (and value
		    (try-directory value)))))
	  (try-system-directory
	   (lambda (directory)
	     (try-directory
	      (merge-pathnames directory (os2/system-root-directory))))))
      (or (try-variable "TMPDIR")
	  (try-variable "TEMP")
	  (try-variable "TMP")
	  (try-system-directory "\\temp")
	  (try-system-directory "\\tmp")
	  (try-system-directory "")
	  (try-directory ".")
	  (error "Can't find temporary directory.")))))

(define (os2/system-root-directory)
  (let ((system.ini (get-environment-variable "SYSTEM_INI")))
    (if (not (file-exists? system.ini))
	(error "Unable to find OS/2 system.ini file:" system.ini))
    (pathname-new-directory (directory-pathname system.ini) '(ABSOLUTE))))

(define-integrable os2/current-pid
  (ucode-primitive current-pid 0))

(define current-user-name)
(define current-home-directory)
(define user-home-directory)
(letrec
    ((trydir
      (lambda (directory)
	(and directory
	     (file-directory? directory)
	     (pathname-as-directory directory))))
     (%current-user-name
      (lambda ()
	(get-environment-variable "USER")))
     (%current-home-directory
      (lambda ()
	(trydir (get-environment-variable "HOME"))))
     (%users-directory
      (lambda ()
	(trydir (get-environment-variable "USERDIR")))))

  (set! current-user-name
	(lambda ()
	  (or (%current-user-name)
	      ;; If the home directory is defined, use the last part of the
	      ;; path as the user's name.  If the home directory is the root
	      ;; of a drive, this won't do anything.
	      (let ((homedir (%current-home-directory)))
		(and homedir
		     (pathname-name (directory-pathname-as-file homedir))))
	      (error "Unable to determine current user name."))))

  (set! current-home-directory
	(lambda ()
	  (or (%current-home-directory)
	      ;; If home directory not defined, look for directory
	      ;; with user's name in users directory and in root
	      ;; directory of system drive.  If still nothing, use
	      ;; root directory of system drive.
	      (let ((user-name (%current-user-name))
		    (rootdir (os2/system-root-directory)))
		(or (and user-name
			 (or (let ((usersdir (%users-directory)))
			       (and usersdir
				    (trydir
				     (merge-pathnames user-name usersdir))))
			     (trydir (merge-pathnames user-name rootdir))))
		    rootdir)))))

  (set! user-home-directory
	(lambda (user-name)
	  (let ((homedir (%current-home-directory)))
	    ;; If USER-NAME is current user, use current home
	    ;; directory.
	    (or (let ((user-name* (%current-user-name)))
		  (and user-name*
		       (string=? user-name user-name*)
		       homedir))
		;; Look for USER-NAME in users directory.
		(let ((usersdir (%users-directory)))
		  (and usersdir
		       (trydir (merge-pathnames user-name usersdir))))
		;; Look for USER-NAME in same directory as current
		;; user's home directory.
		(and homedir
		     (trydir (merge-pathnames
			      user-name
			      (directory-pathname-as-file homedir))))
		;; Look for USER-NAME in root directory of system
		;; drive.
		(trydir
		 (merge-pathnames user-name (os2/system-root-directory)))
		;; OK, give up:
		(error "Can't find user's home directory:" user-name))))))

(define (dos/fs-drive-type pathname)
  (let ((type
	 ((ucode-primitive drive-type 1)
	  (pathname-device (merge-pathnames pathname)))))
    (let ((colon (string-find-next-char type #\:)))
      (if colon
	  (cons (string-head type colon) (string-tail type (fix:+ colon 1)))
	  (cons type "")))))

(define (dos/fs-long-filenames? pathname)
  (not (string-ci=? "fat" (car (dos/fs-drive-type pathname)))))

(define (os/file-end-of-line-translation pathname)
  (let ((type (dos/fs-drive-type pathname)))
    ;; "ext2" is the Linux ext2 file-system driver.  "NFS" is the IBM
    ;; TCP/IP NFS driver, which we further qualify by examining the
    ;; mount info -- if the directory starts with a "/", we assume
    ;; that it is a unix system.
    (if (or (string=? "ext2" (car type))
	    (and (string=? "NFS" (car type))
		 (let* ((mount (cdr type))
			(colon (string-find-next-char mount #\:)))
		   (and colon
			(fix:< (fix:+ colon 1) (string-length mount))
			(char=? #\/ (string-ref mount (fix:+ colon 1)))))))
	#f
	"\r\n")))

(define (os/default-end-of-line-translation)
  "\r\n")

(define (copy-file from to)
  ((ucode-primitive os2-copy-file 2) (->namestring (merge-pathnames from))
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
	(if (pair? specifier)
	    (merge-pathnames
	     (apply string-append
		    (cons (car specifier)
			  (append-map (lambda (string) (list "/" string))
				      (cdr specifier))))
	     long-base)
	    (directory-pathname-as-file long-base))
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

(define (initialize-system-primitives!)
  unspecific)

;;;; Subprocess/Shell Support

(define (os/make-subprocess filename arguments environment working-directory
			    ctty stdin stdout stderr)
  (if ctty
      (error "Can't manipulate controlling terminal of subprocess:" ctty))
  ((ucode-primitive os2-make-subprocess 7)
   filename
   (os2/rewrite-subprocess-arguments (vector->list arguments))
   (and environment
	(os2/rewrite-subprocess-environment (vector->list environment)))
   working-directory
   stdin
   stdout
   stderr))

(define (os2/rewrite-subprocess-arguments strings)
  (let ((strings
	 (if (pair? strings)
	     (if (pair? (cdr strings))
		 strings
		 (list (car strings) ""))
	     (list "" ""))))
    (let ((result
	   (make-string
	    (reduce +
		    0
		    (map (lambda (s) (fix:+ (string-length s) 1)) strings)))))
      (let ((n (string-length (car strings))))
	(substring-move! (car strings) 0 n result 0)
	(string-set! result n #\NUL)
	(let loop ((strings (cdr strings)) (index (fix:+ n 1)))
	  (let ((n (string-length (car strings))))
	    (substring-move! (car strings) 0 n result index)
	    (if (pair? (cdr strings))
		(begin
		  (string-set! result (fix:+ index n) #\space)
		  (loop (cdr strings) (fix:+ (fix:+ index n) 1)))
		(string-set! result (fix:+ index n) #\NUL)))))
      result)))

(define (os2/rewrite-subprocess-environment strings)
  (let ((result
	 (make-string
	  (reduce +
		  0
		  (map (lambda (s) (fix:+ (string-length s) 1)) strings)))))
    (let loop ((strings strings) (index 0))
      (if (pair? strings)
	  (let ((n (string-length (car strings))))
	    (substring-move! (car strings) 0 n result index)
	    (string-set! result (fix:+ index n) #\NUL)
	    (loop (cdr strings) (fix:+ (fix:+ index n) 1)))))
    result))

(define (os/find-program program default-directory #!optional exec-path error?)
  (let ((namestring
	 (let* ((exec-path
		 (if (default-object? exec-path)
		     (os/exec-path)
		     exec-path))
		(try
		 (let ((types (os/executable-pathname-types)))
		   (lambda (pathname)
		     (let ((type (pathname-type pathname)))
		       (if type
			   (and (member type types)
				(file-exists? pathname)
				(->namestring pathname))
			   (let loop ((types types))
			     (and (pair? types)
				  (let ((p
					 (pathname-new-type pathname
							    (car types))))
				    (if (file-exists? p)
					(->namestring p)
					(loop (cdr types)))))))))))
		(try-dir
		 (lambda (directory)
		   (try (merge-pathnames program directory)))))
	   (cond ((pathname-absolute? program)
		  (try program))
		 ((not default-directory)
		  (let loop ((path exec-path))
		    (and (pair? path)
			 (or (and (pathname-absolute? (car path))
				  (try-dir (car path)))
			     (loop (cdr path))))))
		 (else
		  (let ((default-directory
			  (merge-pathnames default-directory)))
		    (let loop ((path exec-path))
		      (and (pair? path)
			   (or (try-dir
				(merge-pathnames (car path) default-directory))
			       (loop (cdr path)))))))))))
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
	  (let ((index (substring-find-next-char string start end #\;)))
	    (if index
		(if (= index start)
		    (loop (+ index 1))
		    (cons (substring string start index)
			  (loop (+ index 1))))
		(list (substring string start end))))
	  '()))))

(define (os/shell-file-name)
  (or (get-environment-variable "SHELL")
      (get-environment-variable "COMSPEC")
      "cmd.exe"))

(define (os/form-shell-command command)
  (list "/c" command))

(define (os/executable-pathname-types)
  '("exe" "com" "bat" "cmd" "btm"))
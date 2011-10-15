#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; Miscellaneous Win32 Primitives
;;; package: (runtime os-primitives)

(declare (usual-integrations))

(define (file-modes filename)
  ((ucode-primitive file-modes 1) (->namestring (merge-pathnames filename))))

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

(define (copy-file from to)
  ((ucode-primitive nt-copy-file 2) (->namestring (merge-pathnames from))
				    (->namestring (merge-pathnames to))))

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

(define (file-time->local-decoded-time time)
  (universal-time->local-decoded-time (file-time->universal-time time)))

(define (file-time->global-decoded-time time)
  (universal-time->global-decoded-time (file-time->universal-time time)))

(define (decoded-time->file-time dt)
  (universal-time->file-time (decoded-time->universal-time dt)))

(define decode-file-time file-time->local-decoded-time)
(define encode-file-time decoded-time->file-time)

(define (file-time->universal-time time) (+ time epoch))
(define (universal-time->file-time time) (- time epoch))

(define (os/suffix-mime-type suffix)
  (let* ((name (string-append "HKEY_CLASSES_ROOT\\." suffix))
	 (key (win32-registry/open-key name #f)))
    (and key
	 (receive (type value)
	     (win32-registry/get-value key "Content Type")
	   (and type
		(begin
		  (if (not (eq? type 'REG_SZ))
		      (error "Wrong value type in registry entry:"
			     name))
		  value))))))

(define get-environment-variable)
(define set-environment-variable!)
(define set-environment-variable-default!)
(define delete-environment-variable!)
(define reset-environment-variables!)
(let ((environment-variables '())
      (environment-defaults '()))

  ;; Kludge: since getenv returns #f for unbound,
  ;; that can also be the marker for a deleted variable
  (define-integrable *variable-deleted* #f)

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

  )

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
	(or (get-environment-variable "USERNAME")
	    (get-environment-variable "USER"))))
     (%current-home-directory
      (lambda ()
	(or (let ((homedrive (get-environment-variable "HOMEDRIVE"))
		  (homepath (get-environment-variable "HOMEPATH")))
	      (and homedrive
		   homepath
		   (trydir (merge-pathnames homepath homedrive))))
	    (trydir (get-environment-variable "HOME")))))
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
	      (let ((user-name (%current-user-name)))
		;; If home directory not defined, look for directory
		;; with user's name in users directory and in root
		;; directory of system drive.  If still nothing, use
		;; root directory of system drive.
		(or (let ((usersdir (%users-directory)))
		      (and user-name
			   usersdir
			   (trydir (merge-pathnames user-name usersdir))))
		    (let ((rootdir (nt/system-root-directory)))
		      (or (and user-name
			       (trydir (merge-pathnames user-name rootdir)))
			  rootdir)))))))
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
		(trydir (merge-pathnames user-name (nt/system-root-directory)))
		;; OK, give up:
		(error "Can't find user's home directory:" user-name))))))

(define dos/user-home-directory user-home-directory)
(define dos/current-user-name current-user-name)
(define dos/current-home-directory current-home-directory)

(define (temporary-file-pathname #!optional directory)
  (let ((root
	 (merge-pathnames "_scm_tmp"
			  (if (or (default-object? directory) (not directory))
			      (temporary-directory-pathname)
			      (pathname-as-directory directory)))))
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
	      (merge-pathnames directory (nt/system-root-directory))))))
      (or (try-variable "TMPDIR")
	  (try-variable "TEMP")
	  (try-variable "TMP")
	  (try-system-directory "\\temp")
	  (try-system-directory "\\tmp")
	  (try-system-directory "")
	  (try-directory ".")
	  (error "Can't find temporary directory.")))))

(define (nt/system-root-directory)
  (let ((trydir
	 (lambda (directory)
	   (and directory
		(file-directory? directory)
		directory))))
    (let ((sysroot
	   (or (trydir (get-environment-variable "SystemRoot"))
	       (trydir (get-environment-variable "windir"))
	       (trydir (get-environment-variable "winbootdir")))))
      (if (not sysroot)
	  (error "Unable to find Windows system root."))
      (pathname-new-directory (pathname-as-directory sysroot) '(ABSOLUTE)))))

(define (file-line-ending pathname)
  (if (let ((type (dos/fs-drive-type pathname)))
	(or (string=? "NFS" (car type))
	    (string=? "NtNfs" (car type))
	    (string=? "Samba" (car type))))
      'LF
      'CRLF))

(define (default-line-ending)
  'CRLF)

(define (dos/fs-drive-type pathname)
  ;; (system-name . [nfs-]mount-point)
  (cons (let ((info (nt-volume-info pathname)))
	  (let ((name (nt-volume-info/file-system-name info)))
	    ;; Samba normally advertises itself as NTFS, except that
	    ;; it doesn't claim to store Unicode on the disk.
	    (if (and (string-ci=? name "NTFS")
		     (even? (quotient (nt-volume-info/file-system-flags info)
				      nt-fs-flag/unicode-on-disk)))
		"Samba"
		name)))
	""))

(define (dos/fs-long-filenames? pathname)
  ;; Currently we have a problem with long filenames on FAT systems because
  ;; the invented backup names may clash: FOO.SCM and FOO.SCM~ are confused.
  ;; The temporary fix is to treat backup names on FAT systems like the short
  ;; version, even if the VFAT driver is being used to provide long file names.
  (let* ((volume-info (nt-volume-info pathname))
	 (fs-type     (nt-volume-info/file-system-name volume-info)))
    (cond ((or (string-ci=? fs-type "VFAT")
	       (string-ci=? fs-type "FAT32"))
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

;;;; Subprocess/Shell Support

(define console-channel-descriptor)
(define nt/hide-subprocess-windows?)
(define nt/subprocess-argument-quote-char)
(define nt/subprocess-argument-escape-char)

(define (initialize-system-primitives!)
  (let ((reset!
	 (lambda ()
	   (reset-environment-variables!)
	   (set! console-channel-descriptor
		 (channel-descriptor-for-select (tty-input-channel)))
	   unspecific)))
    (reset!)
    (add-event-receiver! event:after-restart reset!))
  (set! nt/hide-subprocess-windows? #t)
  (set! nt/subprocess-argument-quote-char #\")
  (set! nt/subprocess-argument-escape-char #\\)
  unspecific)

(define (os/make-subprocess filename arguments environment working-directory
			    ctty stdin stdout stderr)
  (if ctty
      (error "Can't manipulate controlling terminal of subprocess:" ctty))
  ((ucode-primitive nt-make-subprocess 8)
   filename
   (rewrite-args filename (vector->list arguments))
   (and environment
	(rewrite-env (vector->list environment)))
   working-directory
   stdin
   stdout
   stderr
   (vector nt/hide-subprocess-windows?)))

(define (rewrite-env strings)
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
	(if (pair? strings)
	    (let ((n (string-length (car strings))))
	      (substring-move! (car strings) 0 n result index)
	      (let ((index* (fix:+ index n)))
		(string-set! result index* #\NUL)
		(loop (cdr strings) (fix:+ index* 1))))))
      result)))

(define (rewrite-args program strings)
  ;; PROGRAM will eventually be used to determine the appropriate
  ;; escape character -- strangely enough, this depends on what
  ;; runtime library PROGRAM is linked with.
  program
  (let ((quote-char nt/subprocess-argument-quote-char)
	(escape-char nt/subprocess-argument-escape-char))
    (if (not quote-char)
	(rewrite-args/no-quoting strings)
	(rewrite-args/quoting strings quote-char escape-char))))

(define (rewrite-args/no-quoting strings)
  (if (pair? strings)
      (let ((result
	     (make-string
	      (fix:+ (reduce +
			     0
			     (map (lambda (s) (string-length s)) strings))
		     (fix:- (length strings) 1)))))
	(let ((n (string-length (car strings))))
	  (substring-move! (car strings) 0 n result 0)
	  (let loop ((strings (cdr strings)) (index n))
	    (if (not (null? strings))
		(let ((n (string-length (car strings))))
		  (string-set! result index #\space)
		  (substring-move! (car strings) 0 n result (fix:+ index 1))
		  (loop (cdr strings) (fix:+ (fix:+ index 1) n))))))
	result)
      ""))

(define (rewrite-args/quoting strings quote-char escape-char)
  (define (analyze-arg s)
    (let ((need-quotes? #f)
	  (n (string-length s)))
      (do ((i 0 (fix:+ i 1))
	   (j 0 (if (char=? escape-char (string-ref s i)) (fix:+ j 1) 0))
	   (k 0
	      (fix:+ k
		     (let ((c (string-ref s i)))
		       (if (char=? quote-char c)
			   (begin
			     (set! need-quotes? #t)
			     ;; Double preceding escape chars.
			     (fix:+ j 2))
			   (begin
			     (if (or (char=? #\space c)
				     (char=? #\tab c))
				 (set! need-quotes? #t))
			     1))))))
	  ((fix:= i n)
	   (cons (if need-quotes? (fix:+ k 2) k)
		 need-quotes?)))))
  (let ((analyses (map analyze-arg strings)))
    (let ((result (make-string (reduce + 0 (map car analyses)))))
      (define (do-arg index s analysis)
	(if (cdr analysis)
	    (begin
	      (string-set! result index quote-char)
	      (let ((index (do-arg-1 (fix:+ index 1) s)))
		(string-set! result index quote-char)
		(fix:+ index 1)))
	    (do-arg-1 index s)))
      (define (do-arg-1 index s)
	(let ((n (string-length s)))
	  (do ((i 0 (fix:+ i 1))
	       (j 0 (if (char=? escape-char (string-ref s i)) (fix:+ j 1) 0))
	       (index index
		      (let ((c (string-ref s i)))
			(if (char=? quote-char c)
			    (let ((index* (fix:+ index (fix:+ j 1))))
			      ;; Double preceding escape chars.
			      (substring-fill! result index index* escape-char)
			      (string-set! result index* c)
			      (fix:+ index* 1))
			    (begin
			      (string-set! result index c)
			      (fix:+ index 1))))))
	      ((fix:= i n) index))))
      (let loop ((index 0) (strings strings) (analyses analyses))
	(if (pair? strings)
	    (loop (do-arg index (car strings) (car analyses))
		  (cdr strings)
		  (cdr analyses))))
      result)))

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
	   (if (pathname-absolute? program)
	       (try program)
	       (or (let ((ns (nt/scheme-executable-pathname)))
		     (and ns
			  (try-dir (directory-pathname ns))))
		   (if (not default-directory)
		       (let loop ((path exec-path))
			 (and (pair? path)
			      (or (and (pathname-absolute? (car path))
				       (try-dir (car path)))
				  (loop (cdr path)))))
		       (let ((default-directory
			       (merge-pathnames default-directory)))
			 (let loop ((path exec-path))
			   (and (pair? path)
				(or (try-dir
				     (merge-pathnames (car path)
						      default-directory))
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
  (map (lambda (string)
	 (let ((input (open-input-string string))
	       (output (open-output-string)))
	   (let loop ()
	     (let ((char (read-char input)))
	       (cond ((eof-object? char)
		      (pathname-as-directory (get-output-string! output)))
		     ((or (char=? char #\")
			  (and (char=? char #\\)
			       (eqv? (peek-char input) #\\)))
		      (loop))
		     (else
		      (write-char char output)
		      (loop)))))))
       (burst-string string #\; #t)))

(define (nt/scheme-executable-pathname)
  (let ((env (->environment '(win32))))
    (let ((handle
	   ((access get-module-handle env)
	    (file-namestring
	     (pathname-default-type
	      ((make-primitive-procedure 'SCHEME-PROGRAM-NAME))
	      "exe"))))
	  (buf (make-string 256)))
      (substring buf 0 ((access get-module-file-name env) handle buf 256)))))

(define (os/shell-file-name)
  (or (get-environment-variable "SHELL")
      (get-environment-variable "COMSPEC")
      (if (eq? 'WINNT (nt/windows-type))
	  "cmd.exe"
	  "command.com")))

(define (nt/windows-type)
  (cond ((string-prefix? "Microsoft Windows NT"
			 microcode-id/operating-system-variant)
	 'WINNT)
	((string-prefix? "Microsoft Windows 9"
			 microcode-id/operating-system-variant)
	 'WIN9X)
	((string-prefix? "Microsoft Windows"
			 microcode-id/operating-system-variant)
	 'WIN3X)
	(else #f)))

(define (os/form-shell-command command)
  (list "/c" command))

(define (os/executable-pathname-types)
  '("exe" "com" "bat" "btm"))
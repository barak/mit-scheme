;;; -*-Scheme-*-
;;;
;;;	$Id: os2.scm,v 1.10 1995/04/10 20:22:42 cph Exp $
;;;
;;;	Copyright (c) 1994-95 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;
;;; NOTE: Parts of this program (Edwin) were created by translation
;;; from corresponding parts of GNU Emacs.  Users should be aware that
;;; the GNU GENERAL PUBLIC LICENSE may apply to these parts.  A copy
;;; of that license should have been included along with this file.

;;;; OS/2 Customizations for Edwin

(declare (usual-integrations))

(define-variable version-control
  "Control use of version numbers for backup files.
#T means make numeric backup versions unconditionally.
#F means make them for files that have some already.
'NEVER means do not make them."
  #f
  (lambda (object) (or (eq? object 'NEVER) (boolean? object))))

(define-variable kept-old-versions
  "Number of oldest versions to keep when a new numbered backup is made."
  2
  exact-nonnegative-integer?)

(define-variable kept-new-versions
  "Number of newest versions to keep when a new numbered backup is made.
Includes the new backup.  Must be > 0."
  2
  (lambda (n) (and (exact-integer? n) (> n 0))))

(define os2/encoding-pathname-types
  '("gz" #|"ky"|#))

(define os2/backup-suffixes
  (cons "~"
	(map (lambda (type) (string-append "~." type))
	     os2/encoding-pathname-types)))

(define-variable completion-ignored-extensions
  "Completion ignores filenames ending in any string in this list."
  (append (list ".bin" ".com" ".ext"
		".inf" ".bif" ".bsm" ".bci" ".bcs"
		".psb" ".moc" ".fni"
		".bco" ".bld" ".bad" ".glo" ".fre"
		".obj" ".exe" ".pif" ".grp"
		".dvi" ".toc" ".log" ".aux")
	  (list-copy os2/backup-suffixes))
  (lambda (extensions)
    (and (list? extensions)
	 (for-all? extensions
	   (lambda (extension)
	     (and (string? extension)
		  (not (string-null? extension))))))))

;;;; Filename I/O

(define (os/trim-pathname-string string)
  (let ((end (string-length string))
	(pattern (re-compile-pattern "[\\/]\\([\\/$~]\\|[a-zA-Z]:\\)" #t)))
    (let loop ((start 0))
      (cond ((re-search-substring-forward pattern #t #f string start end)
	     (loop (re-match-start-index 1)))
	    ((fix:= start 0) string)
	    (else (string-tail string start))))))

(define (os/pathname->display-string pathname)
  (or (let ((relative (enough-pathname pathname (user-homedir-pathname))))
	(and (not (pathname-device relative))
	     (not (pathname-absolute? relative))
	     (string-append "~\\" (->namestring relative))))
      (->namestring pathname)))

(define (os/truncate-filename-for-modeline filename width)
  (let ((length (string-length filename)))
    (if (< 0 width length)
	(let ((result
	       (substring
		filename
		(let ((index (- length width)))
		  (if (char=? #\\ (string-ref filename index))
		      index
		      (or (substring-find-next-char filename index length #\\)
			  (fix:+ index 1))))
		length)))
	  (string-set! result 0 #\$)
	  result)
	filename)))

;;;; Backup and Auto-Save Filenames

(define (os/buffer-backup-pathname truename)
  (call-with-values
      (lambda ()
	(if (os2/fs-long-filenames? truename)
	    (let ((type (pathname-type truename)))
	      (if (member type os2/encoding-pathname-types)
		  (values (pathname-new-type truename #f)
			  (string-append "~." type))
		  (values truename "~")))
	    (values truename "")))
    (lambda (truename suffix)
      (if (eq? 'NEVER (ref-variable version-control))
	  (values (os2/make-backup-pathname truename #f suffix) '())
	  (let ((prefix
		 (if (os2/fs-long-filenames? truename)
		     (string-append (file-namestring truename) ".~")
		     (string-append (pathname-name truename) "."))))
	    (let ((backups
		   (let loop
		       ((filenames
			 (os/directory-list-completions
			  (directory-namestring truename)
			  prefix))
			(backups '()))
		     (if (null? filenames)
			 (sort backups (lambda (x y) (< (cdr x) (cdr y))))
			 (loop (cdr filenames)
			       (let ((root.version
				      (os/numeric-backup-filename?
				       (car filenames))))
				 (if root.version
				     (cons (cons (car filenames)
						 (cdr root.version))
					   backups)
				     backups)))))))
	      (if (null? backups)
		  (values (os2/make-backup-pathname
			   truename
			   (and (ref-variable version-control) 1)
			   suffix)
			  '())
		  (values (os2/make-backup-pathname
			   truename
			   (+ (apply max (map cdr backups)) 1)
			   suffix)
			  (let ((start (ref-variable kept-old-versions))
				(end
				 (- (length backups)
				    (- (ref-variable kept-new-versions) 1))))
			    (if (< start end)
				(map car (sublist backups start end))
				'()))))))))))

(define (os2/make-backup-pathname pathname version suffix)
  (if (os2/fs-long-filenames? pathname)
      (string-append (->namestring pathname)
		     (if version
			 (string-append ".~" (number->string version) suffix)
			 suffix))
      (pathname-new-type pathname
			 (if (and version (< version 1000))
			     (let ((type (pathname-type pathname))
				   (vs (number->string version)))
			       (if (and (< version 100)
					(string? type)
					(not (string-null? type)))
				   (string-append (substring type 0 1)
						  (string-pad-left vs 2 #\0))
				   (string-pad-left vs 3 #\0)))
			     "bak"))))

(define (os/default-backup-filename)
  "$TMP\\edwin.bak")

(define (os/backup-filename? filename)
  (or (there-exists? os2/backup-suffixes
	(lambda (suffix)
	  (string-suffix? suffix filename)))
      (let ((type (pathname-type filename)))
	(and (string? type)
	     (or (string-ci=? "bak" type)
		 (re-match-string-forward (re-compile-pattern ".[0-9][0-9]" #f)
					  #f
					  #f
					  type))))))

(define (os/numeric-backup-filename? filename)
  (and (let ((try
	      (lambda (pattern)
		(re-search-string-forward (re-compile-pattern pattern #f)
					  #f
					  #f
					  filename))))
	 (or (try "^\\([^.]+\\)\\.\\([0-9][0-9][0-9]\\)$")
	     (try "^\\([^.]+\\.[^.]\\)\\([0-9][0-9]\\)$")
	     (there-exists? os2/backup-suffixes
	       (lambda (suffix)
		 (try (string-append "^\\(.+\\)\\.~\\([0-9]+\\)"
				     (re-quote-string suffix)
				     "$"))))))
       (let ((root-start (re-match-start-index 1))
	     (root-end (re-match-end-index 1))
	     (version-start (re-match-start-index 2))
	     (version-end (re-match-end-index 2)))
	 (let ((version
		(substring->number filename version-start version-end)))
	   (and (> version 0)
		(cons (substring filename root-start root-end)
		      version))))))

(define (os/auto-save-pathname pathname buffer)
  (let ((pathname
	 (or pathname
	     (let ((name (buffer-name buffer))
		   (directory (buffer-default-directory buffer)))
	       (merge-pathnames (if (os2/fs-long-filenames? directory)
				    (string-append "%" name)
				    "%buffer%")
				directory)))))
    (if (os2/fs-long-filenames? pathname)
	(merge-pathnames (string-append "#" (file-namestring pathname) "#")
			 (directory-pathname pathname))
	(pathname-new-type pathname "sav"))))

(define (os/auto-save-filename? filename)
  (or (re-match-string-forward (re-compile-pattern "^#.+#$" #f)
			       #f
			       #f
			       (file-namestring filename))
      (let ((type (pathname-type filename)))
	(and (string? type)
	     (string-ci=? "sav" type)))))

(define (os/precious-backup-pathname pathname)
  (if (os2/fs-long-filenames? pathname)
      (let ((directory (directory-pathname pathname)))
	(let loop ((i 0))
	  (let ((pathname
		 (merge-pathnames (string-append "#tmp#" (number->string i))
				  directory)))
	    (if (allocate-temporary-file pathname)
		(begin
		  (deallocate-temporary-file pathname)
		  pathname)
		(loop (+ i 1))))))
      (os/auto-save-pathname pathname #f)))

;;;; Miscellaneous

(define (os/backup-buffer? truename)
  (let ((attrs (file-attributes truename)))
    (and attrs
	 (eq? #f (file-attributes/type attrs)))))

(define (os/backup-by-copying? truename buffer)
  truename buffer
  #f)

(define (os/pathname-type-for-mode pathname)
  (let ((type (pathname-type pathname)))
    (if (member type os2/encoding-pathname-types)
	(pathname-type (->namestring (pathname-new-type pathname false)))
	type)))

(define (os/completion-ignore-filename? filename)
  (or (os/backup-filename? filename)
      (os/auto-save-filename? filename)
      (and (not (file-directory? filename))
	   (there-exists? (ref-variable completion-ignored-extensions)
   	     (lambda (extension)
	       (string-suffix? extension filename))))))

(define (os/file-type-to-major-mode)
  (alist-copy
   `(("asm" . midas)
     ("bat" . text)
     ("bib" . text)
     ("c" . c)
     ("h" . c)
     ("m4" . midas)
     ("pas" . pascal)
     ("s" . scheme)
     ("scm" . scheme)
     ("txi" . texinfo)
     ("txt" . text))))

(define (os/init-file-name)
  (let ((name "edwin.ini"))
    (let ((user-init-file (merge-pathnames name (user-homedir-pathname))))
      (if (file-exists? user-init-file)
	  user-init-file
	  (merge-pathnames name (system-library-directory-pathname #f))))))

(define (os/find-file-initialization-filename pathname)
  (or (and (equal? "scm" (pathname-type pathname))
	   (let ((pathname (pathname-new-type pathname "ffi")))
	     (and (file-exists? pathname)
		  pathname)))
      (let ((pathname
	     (merge-pathnames "edwin.ffi" (directory-pathname pathname))))
	(and (file-exists? pathname)
	     pathname))))

(define (os/scheme-can-quit?)
  #f)

(define (os/quit dir)
  dir
  (error "Can't quit."))

(define (os/set-file-modes-writable! pathname)
  (set-file-modes! pathname (fix:andc (file-modes pathname) #x0001)))

;;;; Dired customization

(define-variable dired-listing-switches
  "Dired listing format."
  "-l"
  string?)

(define-variable list-directory-brief-switches
  "list-directory brief listing format."
  ""
  string?)

(define-variable list-directory-verbose-switches
  "list-directory verbose listing format."
  "-l"
  string?)

(define (insert-directory! file switches mark type)
  ;; Insert directory listing for FILE at MARK.
  ;; SWITCHES are examined for the presence of "t".
  type
  (let ((mark (mark-left-inserting-copy mark)))
    (call-with-current-continuation
     (lambda (k)
       (bind-condition-handler (list condition-type:file-error)
	   (lambda (condition)
	     (insert-string (condition/report-string condition) mark)
	     (insert-newline mark)
	     (k unspecific))
	 (lambda ()
	   (for-each
	    (let ((nmonths
		   (lambda (time)
		     (let ((time (quotient time #x200000)))
		       (+ (* (quotient time 16) 12) (remainder time 16))))))
	      (let ((now (nmonths (current-file-time))))
		(lambda (entry)
		  (insert-string
		   (let ((name (car entry))
			 (attr (cdr entry)))
		     (let ((time (file-attributes/modification-time attr)))
		       (let ((time-string (file-time->string time)))
			 (string-append
			  (file-attributes/mode-string attr)
			  " "
			  (string-pad-left (number->string
					    (file-attributes/length attr))
					   10 #\Space)
			  " "
			  (substring time-string 0 6) ;month/day
			  " "
			  (if (<= -6 (- (nmonths time) now) 0)
			      (substring time-string 7 12) ;hour/minute
			      (substring time-string 15 20)) ;year
			  " "
			  name))))
		   mark)
		  (insert-newline mark))))
	    (sort (list-transform-positive
		      (map (lambda (pathname)
			     (cons (file-namestring pathname)
				   (file-attributes pathname)))
			   (directory-read file #f))
		    cdr)
		  (if (string-find-next-char switches #\t)
		      (lambda (x y)
			(> (file-attributes/modification-time (cdr x))
			   (file-attributes/modification-time (cdr y))))
		      (lambda (x y)
			(string-ci<? (car x) (car y))))))))))
    (mark-temporary! mark)))

;;;; Subprocess/Shell Support

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

(define (os/find-program program default-directory)
  (or (os2/find-program program (ref-variable exec-path) default-directory)
      (error "Can't find program:" (->namestring program))))

(define (os2/find-program program exec-path default-directory)
  (let* ((types '("exe" "cmd"))
	 (try
	  (lambda (pathname)
	    (let ((type (pathname-type pathname)))
	      (if type
		  (and (member type types)
		       (file-exists? pathname)
		       (->namestring pathname))
		  (let loop ((types types))
		    (and (not (null? types))
			 (let ((p
				(pathname-new-type pathname (car types))))
			   (if (file-exists? p)
			       (->namestring p)
			       (loop (cdr types)))))))))))
    (cond ((pathname-absolute? program)
	   (try program))
	  ((not default-directory)
	   (let loop ((path exec-path))
	     (and (not (null? path))
		  (or (and (pathname-absolute? (car path))
			   (try (merge-pathnames program (car path))))
		      (loop (cdr path))))))
	  (else
	   (let ((default-directory (merge-pathnames default-directory)))
	     (let loop ((path exec-path))
	       (and (not (null? path))
		    (or (try (merge-pathnames
			      program
			      (merge-pathnames (car path)
					       default-directory)))
			(loop (cdr path))))))))))

(define (os/shell-file-name)
  (or (get-environment-variable "SHELL")
      "cmd.exe"))

(define (os/form-shell-command command)
  (list "/c" command))

(define (os/shell-name pathname)
  (if (member (pathname-type pathname) '("exe" "cmd"))
      (pathname-name pathname)
      (file-namestring pathname)))

(define (os/default-shell-prompt-pattern)
  "^\\[[^]]*] *")

(define (os/default-shell-args)
  '())

(define (os/comint-filename-region start point end)
  (let ((chars "]\\\\A-Za-z0-9!#$%&'()+,.:;=@[^_`{}~---"))
    (let ((start (skip-chars-backward chars point start)))
      (make-region start (skip-chars-forward chars start end)))))

;;;; Compressed Files

(define (os/read-file-methods) (list maybe-read-compressed-file))

(define (os/write-file-methods) (list maybe-write-compressed-file))

(define-variable enable-compressed-files
  "If true, compressed files are automatically uncompressed when read,
and recompressed when written.  A compressed file is identified by the
filename suffix \".gz\"."
  #t
  boolean?)

(define (maybe-read-compressed-file pathname mark visit?)
  visit?
  (and (ref-variable enable-compressed-files mark)
       (equal? "gz" (pathname-type pathname))
       (begin
	 (read-compressed-file "gzip -d" pathname mark)
	 #t)))

(define (read-compressed-file program pathname mark)
  (let ((do-it
	 (lambda ()
	   (if (not (equal? '(EXITED . 0)
			    (shell-command #f
					   mark
					   (directory-pathname pathname)
					   #f
					   (string-append
					    program
					    " < "
					    (file-namestring pathname)))))
	       (error:file-operation pathname
				     program
				     "file"
				     "[unknown]"
				     read-compressed-file
				     (list pathname mark))))))
    (if (ref-variable read-file-message mark)
	(do-it)
	(begin
	  (temporary-message "Uncompressing file "
			     (->namestring pathname)
			     "...")
	  (do-it)
	  (append-message "done")))))

(define (maybe-write-compressed-file region pathname visit?)
  visit?
  (and (ref-variable enable-compressed-files (region-start region))
       (equal? "gz" (pathname-type pathname))
       (begin
	 (write-compressed-file "gzip" region pathname)
	 #t)))

(define (write-compressed-file program region pathname)
  (if (not (equal? '(EXITED . 0)
		   (shell-command region
				  #f
				  (directory-pathname pathname)
				  #f
				  (string-append program
						 " > "
						 (file-namestring pathname)))))
      (error:file-operation pathname
			    program
			    "file"
			    "[unknown]"
			    write-compressed-file
			    (list region pathname))))

;;;; Mail Customization

(define (os/sendmail-program)
  "sendmail")

(define (os/rmail-pop-procedure)
  (and (os2/find-program "popclient" (ref-variable exec-path) #f)
       (lambda (server user-name password directory)
	 (os2-pop-client server user-name password directory))))

(define (os2-pop-client server user-name password directory)
  (let ((target
	 (->namestring
	  (merge-pathnames (if (os2/fs-long-filenames? directory)
			       ".popmail"
			       "popmail.tmp")
			   directory))))
    (let ((buffer (temporary-buffer "*popclient*")))
      (let ((status.reason
	     (run-synchronous-process #f (buffer-end buffer) #f #f
				      "popclient"
				      (if (ref-variable rmail-pop-delete)
					  "-3"
					  "-3 -k")
				      "-u" user-name
				      "-p" password
				      "-o" target
				      server)))
	(if (not (and (eq? 'EXITED (car status.reason))
		      (memv (cdr status.reason) '(0 1))))
	    (begin
	      (pop-up-buffer buffer)
	      (error "Error getting mail from POP server:" status.reason)))))
    target))

(define-variable rmail-pop-delete
  "If true, messages are deleted from the POP server after being retrieved.
Otherwise, messages remain on the server and will be re-fetched later."
  #t
  boolean?)

;;;; Generic Stuff
;;; These definitions are OS-independent and references to them should
;;; be replaced in order to reduce the number of OS-dependent defs.

(define (os/directory-list directory)
  (let ((channel (directory-channel-open directory)))
    (let loop ((result '()))
      (let ((name (directory-channel-read channel)))
	(if name
	    (loop (cons name result))
	    (begin
	      (directory-channel-close channel)
	      result))))))

(define (os/directory-list-completions directory prefix)
  (let ((channel (directory-channel-open directory)))
    (let loop ((result '()))
      (let ((name (directory-channel-read-matching channel prefix)))
	(if name
	    (loop (cons name result))
	    (begin
	      (directory-channel-close channel)
	      result))))))
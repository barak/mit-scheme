;;; -*-Scheme-*-
;;;
;;;	$Id: os2.scm,v 1.6 1995/01/31 22:06:04 cph Exp $
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

(define-variable completion-ignored-extensions
  "Completion ignores filenames ending in any string in this list."
  (list ".bin" ".com" ".ext"
	".inf" ".bif" ".bsm" ".bci" ".bcs"
	".psb" ".moc" ".fni"
	".bco" ".bld" ".bad" ".glo" ".fre"
	".obj" ".exe" ".pif" ".grp"
	".dvi" ".toc" ".log" ".aux")
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
  (if (eq? 'NEVER (ref-variable version-control))
      (values (os2/make-backup-pathname truename #f) '())
      (let ((prefix
	     (if (os2/fs-long-filenames? truename)
		 (string-append (file-namestring truename) ".~")
		 (string-append (pathname-name truename) "."))))
	(let ((versions
	       (let loop
		   ((filenames
		     (os/directory-list-completions
		      (directory-namestring truename)
		      prefix))
		    (versions '()))
		 (if (null? filenames)
		     (sort versions <)
		     (loop (cdr filenames)
			   (let ((root.version
				  (os/numeric-backup-filename?
				   (car filenames))))
			     (if root.version
				 (cons (cdr root.version) versions)
				 versions)))))))
	  (if (null? versions)
	      (values (os2/make-backup-pathname
		       truename
		       (and (ref-variable version-control)
			    1))
		      '())
	      (values (os2/make-backup-pathname truename
						(+ (apply max versions) 1))
		      (let ((start (ref-variable kept-old-versions))
			    (end
			     (- (length versions)
				(- (ref-variable kept-new-versions) 1))))
			(if (< start end)
			    (map (lambda (version)
				   (os2/make-backup-pathname truename
							     version))
				 (sublist versions start end))
			    '()))))))))

(define (os2/make-backup-pathname pathname version)
  (if (os2/fs-long-filenames? pathname)
      (string-append (->namestring pathname)
		     (if version
			 (string-append ".~" (number->string version) "~")
			 "~"))
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
  (or (string-suffix? "~" filename)
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
	 (or (try "^\\(.+\\)\\.~\\([0-9]+\\)~$")
	     (try "^\\([^.]+\\)\\.\\([0-9][0-9][0-9]\\)$")
	     (try "^\\([^.]+\\.[^.]\\)\\([0-9][0-9]\\)$")))
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
		pathname
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

(define os/pathname-type-for-mode
  pathname-type)

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

(define (os/read-file-methods) '())

(define (os/write-file-methods) '())

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
  ;; TYPE can have one of three values:
  ;;   'WILDCARD means treat FILE as shell wildcard.
  ;;   'DIRECTORY means FILE is a directory and a full listing is expected.
  ;;   'FILE means FILE itself should be listed, and not its contents.
  ;; SWITCHES are examined for the presence of "t".
  (for-each
   (let ((nmonths
	  (lambda (time)
	    (let ((time (quotient time #x200000)))
	      (+ (* (quotient time 16) 12) (remainder time 16))))))
     (let ((now (nmonths (os2/current-file-time))))
       (lambda (entry)
	 (let ((string
		(let ((name (car entry))
		      (attr (cdr entry)))
		  (let ((time (file-attributes/modification-time attr)))
		    (let ((time-string (os2/file-time->string time)))
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
			   (substring time-string 7 12)	;hour/minute
			   (substring time-string 15 20)) ;year
		       " "
		       name))))))
	   (let ((mark (mark-left-inserting-copy mark)))
	     (insert-string string mark)
	     (insert-newline mark)
	     (mark-temporary! mark))))))
   (let ((pathname
	  (if (eq? 'DIRECTORY type) (pathname-as-directory file) file))
	 (read
	  (lambda (pathname sort?)
	    (list-transform-positive
		(map (lambda (pathname)
		       (cons (file-namestring pathname)
			     (file-attributes pathname)))
		     (directory-read pathname sort?))
	      cdr))))
     (if (string-find-next-char switches #\t)
	 (sort (read pathname #f)
	       (lambda (x y)
		 (> (file-attributes/modification-time (cdr x))
		    (file-attributes/modification-time (cdr y)))))
	 (read pathname #t)))))

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
  (or (let* ((types '("exe" "cmd"))
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
	       (let loop ((path (ref-variable exec-path)))
		 (and (not (null? path))
		      (or (and (pathname-absolute? (car path))
			       (try (merge-pathnames program (car path))))
			  (loop (cdr path))))))
	      (else
	       (let ((default-directory (merge-pathnames default-directory)))
		 (let loop ((path (ref-variable exec-path)))
		   (and (not (null? path))
			(or (try (merge-pathnames
				  program
				  (merge-pathnames (car path)
						   default-directory)))
			    (loop (cdr path)))))))))
      (error "Can't find program:" (->namestring program))))

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
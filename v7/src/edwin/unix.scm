;;; -*-Scheme-*-
;;;
;;;	$Id: unix.scm,v 1.46 1995/04/09 23:06:46 cph Exp $
;;;
;;;	Copyright (c) 1989-95 Massachusetts Institute of Technology
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
;;;

;;;; Unix Customizations for Edwin

(declare (usual-integrations))

(define-variable backup-by-copying-when-symlink
  "True means use copying to create backups for a symbolic name.
This causes the actual names to refer to the latest version as edited.
'QUERY means ask whether to backup by copying and write through, or rename.
This variable is relevant only if  backup-by-copying  is false."
  false)

(define-variable backup-by-copying-when-linked
  "True means use copying to create backups for files with multiple names.
This causes the alternate names to refer to the latest version as edited.
This variable is relevant only if  backup-by-copying  is false."
  false
  boolean?)

(define-variable backup-by-copying-when-mismatch
  "True means create backups by copying if this preserves owner or group.
Renaming may still be used (subject to control of other variables)
when it would not result in changing the owner or group of the file;
that is, for files which are owned by you and whose group matches
the default for a new file created there by you.
This variable is relevant only if  Backup By Copying  is false."
  false
  boolean?)

(define-variable version-control
  "Control use of version numbers for backup files.
#T means make numeric backup versions unconditionally.
#F means make them for files that have some already.
'NEVER means do not make them."
  false)

(define-variable kept-old-versions
  "Number of oldest versions to keep when a new numbered backup is made."
  2
  exact-nonnegative-integer?)

(define-variable kept-new-versions
  "Number of newest versions to keep when a new numbered backup is made.
Includes the new backup.  Must be > 0."
  2
  (lambda (n) (and (exact-integer? n) (> n 0))))

(define (os/trim-pathname-string string)
  (let ((end (string-length string)))
    (let loop ((index end))
      (let ((slash (substring-find-previous-char string 0 index #\/)))
	(cond ((not slash)
	       string)
	      ((and (< (1+ slash) end)
		    (memv (string-ref string (1+ slash)) '(#\~ #\$)))
	       (string-tail string (1+ slash)))
	      ((zero? slash)
	       string)
	      ((char=? #\/ (string-ref string (-1+ slash)))
	       (string-tail string slash))
	      (else
	       (loop (-1+ slash))))))))

(define (os/pathname->display-string pathname)
  (let ((pathname (enough-pathname pathname (user-homedir-pathname))))
    (if (pathname-absolute? pathname)
	(->namestring pathname)
	(string-append "~/" (->namestring pathname)))))

(define (os/auto-save-pathname pathname buffer)
  (let ((wrap
	 (lambda (name directory)
	   (merge-pathnames (string-append "#" name "#") directory))))
    (if (not pathname)
	(wrap (string-append "%" (buffer-name buffer))
	      (buffer-default-directory buffer))
	(wrap (file-namestring pathname)
	      (directory-pathname pathname)))))

(define (os/precious-backup-pathname pathname)
  (let ((directory (directory-pathname pathname)))
    (let loop ((i 0))
      (let ((pathname
	     (merge-pathnames (string-append "#tmp#" (number->string i))
			      directory)))
	(if (allocate-temporary-file pathname)
	    pathname
	    (loop (+ i 1)))))))

(define (os/backup-buffer? truename)
  (and (memv (string-ref (vector-ref (file-attributes truename) 8) 0)
	     '(#\- #\l))
       (not
	(let ((directory (pathname-directory truename)))
	  (and (pair? directory)
	       (eq? 'ABSOLUTE (car directory))
	       (pair? (cdr directory))
	       (eqv? "tmp" (cadr directory)))))))

(define (os/default-backup-filename)
  "~/%backup%~")

(define (os/truncate-filename-for-modeline filename width)
  (let ((length (string-length filename)))
    (if (< 0 width length)
	(let ((result
	       (substring
		filename
		(let ((index (- length width)))
		  (or (and (not (char=? #\/ (string-ref filename index)))
			   (substring-find-next-char filename index length
						     #\/))
		      (1+ index)))
		length)))
	  (string-set! result 0 #\$)
	  result)
	filename)))

(define (os/backup-by-copying? truename buffer)
  (let ((attributes (file-attributes truename)))
    (or (and (ref-variable backup-by-copying-when-linked buffer)
	     (> (file-attributes/n-links attributes) 1))
	(let ((flag (ref-variable backup-by-copying-when-symlink buffer)))
	  (and flag
	       (string? (file-attributes/type attributes))
	       (or (not (eq? flag 'QUERY))
		   (prompt-for-confirmation?
		    (string-append "Write through symlink to "
				   (->namestring
				    (enough-pathname
				     (pathname-simplify
				      (merge-pathnames
				       (file-attributes/type attributes)
				       (buffer-pathname buffer)))
				     (buffer-default-directory buffer))))))))
	(and (ref-variable backup-by-copying-when-mismatch buffer)
	     (not (and (= (file-attributes/uid attributes)
			  (unix/current-uid))
		       (= (file-attributes/gid attributes)
			  (unix/current-gid))))))))

(define (os/buffer-backup-pathname truename)
  (with-values
      (lambda ()
	;; Handle compressed files specially.
	(let ((type (pathname-type truename)))
	  (if (member type unix/encoding-pathname-types)
	      (values (->namestring (pathname-new-type truename false))
		      (string-append "~." type))
	      (values (->namestring truename) "~"))))
    (lambda (filename suffix)
      (let ((no-versions
	     (lambda ()
	       (values (->pathname (string-append filename suffix)) '()))))
	(if (eq? 'NEVER (ref-variable version-control))
	    (no-versions)
	    (let ((prefix (string-append (file-namestring filename) ".~")))
	      (let ((filenames
		     (os/directory-list-completions
		      (directory-namestring filename)
		      prefix))
		    (prefix-length (string-length prefix)))
		(let ((versions
		       (sort
			(let ((pattern
			       (re-compile-pattern
				(string-append "\\([0-9]+\\)"
					       (re-quote-string suffix)
					       "$")
				false)))
			  (let loop ((filenames filenames))
			    (cond ((null? filenames)
				   '())
				  ((re-match-substring-forward
				    pattern false false
				    (car filenames)
				    prefix-length
				    (string-length (car filenames)))
				   (let ((version
					  (string->number
					   (substring
					    (car filenames)
					    (re-match-start-index 1)
					    (re-match-end-index 1)))))
				     (cons version
					   (loop (cdr filenames)))))
				  (else
				   (loop (cdr filenames))))))
			<)))
		  (let ((high-water-mark (apply max (cons 0 versions))))
		    (if (or (ref-variable version-control)
			    (positive? high-water-mark))
			(let ((version->pathname
			       (let ((directory
				      (directory-pathname filename)))
				 (lambda (version)
				   (merge-pathnames
				    (string-append prefix
						   (number->string version)
						   suffix)
				    directory)))))
			  (values
			   (version->pathname (+ high-water-mark 1))
			   (let ((start (ref-variable kept-old-versions))
				 (end
				  (- (length versions)
				     (- (ref-variable kept-new-versions)
					1))))
			     (if (< start end)
				 (map version->pathname
				      (sublist versions start end))
				 '()))))
			(no-versions)))))))))))

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

(define unix/encoding-pathname-types
  '("Z" "gz" "KY"))

(define unix/backup-suffixes
  (cons "~"
	(map (lambda (type) (string-append "~." type))
	     unix/encoding-pathname-types)))

(define (os/backup-filename? filename)
  (let ((end (string-length filename)))
    (let loop ((suffixes unix/backup-suffixes))
      (and (not (null? suffixes))
	   (or (let ((suffix (car suffixes)))
		 (let ((start (fix:- end (string-length suffix))))
		   (and (fix:> start 0)
			(let loop ((suffix-index 0) (index start))
			  (if (fix:= index end)
			      start
			      (and (char=? (string-ref suffix suffix-index)
					   (string-ref filename index))
				   (loop (fix:+ suffix-index 1)
					 (fix:+ index 1))))))))
	       (loop (cdr suffixes)))))))

(define (os/numeric-backup-filename? filename)
  (let ((suffix (os/backup-filename? filename)))
    (and suffix
	 (fix:>= suffix 4)
	 (let loop ((index (fix:- suffix 2)))
	   (and (fix:>= index 2)
		(if (char-numeric? (string-ref filename index))
		    (loop (fix:- index 1))
		    (and (char=? (string-ref filename index) #\~)
			 (char=? (string-ref filename (fix:- index 1)) #\.)
			 (cons (string-head filename (fix:- index 1))
			       (substring->number filename
						  (fix:+ index 1)
						  suffix)))))))))

(define (os/pathname-type-for-mode pathname)
  (let ((type (pathname-type pathname)))
    (if (member type unix/encoding-pathname-types)
	(pathname-type (->namestring (pathname-new-type pathname false)))
	type)))

(define (os/completion-ignore-filename? filename)
  (and (not (file-directory? filename))
       (there-exists? (ref-variable completion-ignored-extensions)
         (lambda (extension)
	   (string-suffix? extension filename)))))

(define (os/completion-ignored-extensions)
  (append '(".o" ".elc" ".bin" ".lbin" ".fasl"
		 ".dvi" ".toc" ".log" ".aux"
		 ".lof" ".blg" ".bbl" ".glo" ".idx" ".lot")
	  (list-copy unix/backup-suffixes)))

(define-variable completion-ignored-extensions
  "Completion ignores filenames ending in any string in this list."
  (os/completion-ignored-extensions)
  (lambda (extensions)
    (and (list? extensions)
	 (for-all? extensions
	   (lambda (extension)
	     (and (string? extension)
		  (not (string-null? extension))))))))

(define (os/file-type-to-major-mode)
  (alist-copy
   `(("article" . text)
     ("asm" . midas)
     ("bib" . text)
     ("c" . c)
     ("cc" . c)
     ("h" . c)
     ("pas" . pascal)
     ("s" . scheme)
     ("scm" . scheme)
     ("text" . text)
     ("txi" . texinfo)
     ("txt" . text)
     ("y" . c))))

(define (os/init-file-name)
  "~/.edwin")

(define (os/find-file-initialization-filename pathname)
  (or (and (equal? "scm" (pathname-type pathname))
	   (let ((pathname (pathname-new-type pathname "ffi")))
	     (and (file-exists? pathname)
		  pathname)))
      (let ((pathname
	     (merge-pathnames ".edwin-ffi" (directory-pathname pathname))))
	(and (file-exists? pathname)
	     pathname))))

(define (os/auto-save-filename? filename)
  ;; This could be more sophisticated, but is what the edwin
  ;; code was originally doing.
  (and (string? filename)
       (string-find-next-char filename #\#)))

(define (os/read-file-methods)
  (list maybe-read-compressed-file
	maybe-read-encrypted-file))

(define (os/write-file-methods)
  (list maybe-write-compressed-file
	maybe-write-encrypted-file))

;;;; Compressed Files

(define-variable enable-compressed-files
  "If true, compressed files are automatically uncompressed when read,
and recompressed when written.  A compressed file is identified by one
of the filename suffixes \".gz\" or \".Z\"."
  true
  boolean?)

(define (maybe-read-compressed-file pathname mark visit?)
  visit?
  (and (ref-variable enable-compressed-files mark)
       (let ((type (pathname-type pathname)))
	 (cond ((equal? "gz" type)
		(read-compressed-file "gunzip" pathname mark)
		#t)
	       ((equal? "Z" type)
		(read-compressed-file "uncompress" pathname mark)
		#t)
	       (else
		#f)))))

(define (read-compressed-file program pathname mark)
  (let ((do-it
	 (lambda ()
	   (if (not (equal? '(EXITED . 0)
			    (shell-command false
					   mark
					   (directory-pathname pathname)
					   false
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
       (let ((type (pathname-type pathname)))
	 (cond ((equal? "gz" type)
		(write-compressed-file "gzip" region pathname)
		#t)
	       ((equal? "Z" type)
		(write-compressed-file "compress" region pathname)
		#t)
	       (else
		#f)))))

(define (write-compressed-file program region pathname)
  (if (not (equal? '(EXITED . 0)
		   (shell-command region
				  false
				  (directory-pathname pathname)
				  false
				  (string-append program
						 " > "
						 (file-namestring pathname)))))
      (error:file-operation pathname
			    program
			    "file"
			    "[unknown]"
			    write-compressed-file
			    (list region pathname))))

;;;; Encrypted files

(define-variable enable-encrypted-files
  "If true, encrypted files are automatically decrypted when read,
and recrypted when written.  An encrypted file is identified by the
filename suffix \".KY\"."
  true
  boolean?)

(define (maybe-read-encrypted-file pathname mark visit?)
  visit?
  (and (ref-variable enable-encrypted-files mark)
       (equal? "KY" (pathname-type pathname))
       (begin
	 (read-encrypted-file pathname mark)
	 true)))

(define (read-encrypted-file pathname mark)
  (let ((the-encrypted-file
	 (with-input-from-file pathname
	   (lambda ()
	     (read-string (char-set)))))
	(password 
	 (prompt-for-password "Password: ")))
    (insert-string
     (decrypt the-encrypted-file password
	      (lambda () 
		(kill-buffer (mark-buffer mark))
		(editor-error "krypt: Password error!"))
	      (lambda (x) 
		(editor-beep)
		(message "krypt: Checksum error!")
		x))
     mark)
    ;; Disable auto-save here since we don't want to
    ;; auto-save the unencrypted contents of the 
    ;; encrypted file.
    (define-variable-local-value! (mark-buffer mark)
	(ref-variable-object auto-save-default)
      #f)))

(define (maybe-write-encrypted-file region pathname visit?)
  visit?
  (and (ref-variable enable-encrypted-files (region-start region))
       (equal? "KY" (pathname-type pathname))
       (begin
	 (write-encrypted-file region pathname)
	 true)))

(define (write-encrypted-file region pathname)
  (let* ((password 
	  (prompt-for-confirmed-password))
	 (the-encrypted-file
	  (encrypt (extract-string (region-start region) (region-end region))
		   password)))
    (with-output-to-file pathname
      (lambda ()
	(write-string the-encrypted-file)))))

;;; End of encrypted files

;;;; Dired customization

(define-variable dired-listing-switches
  "Switches passed to ls for dired.  MUST contain the 'l' option.
CANNOT contain the 'F' option."
  "-al"
  string?)

(define-variable list-directory-brief-switches
  "Switches for list-directory to pass to `ls' for brief listing,"
  "-CF"
  string?)

(define-variable list-directory-verbose-switches
  "Switches for list-directory to pass to `ls' for verbose listing,"
  "-l"
  string?)

(define-variable insert-directory-program
  "Absolute or relative name of the `ls' program used by `insert-directory'."
  "ls"
  string?)

(define (insert-directory! file switches mark type)
  ;; Insert directory listing for FILE, formatted according to SWITCHES.
  ;; The listing is inserted at MARK.
  ;; TYPE can have one of three values:
  ;;   'WILDCARD means treat FILE as shell wildcard.
  ;;   'DIRECTORY means FILE is a directory and a full listing is expected.
  ;;   'FILE means FILE itself should be listed, and not its contents.
  ;; SWITCHES must not contain "-d".
  (let ((directory (directory-pathname (merge-pathnames file)))
	(program (ref-variable insert-directory-program mark))
	(switches
	 (if (eq? 'DIRECTORY type)
	     switches
	     (string-append-separated "-d" switches))))
    (if (eq? 'WILDCARD type)
	(shell-command #f mark directory #f
		       (string-append program
				      " "
				      switches
				      " "
				      (file-namestring file)))
	(apply run-synchronous-process
	       #f mark directory #f
	       (os/find-program program #f)
	       (append
		(split-unix-switch-string switches)
		(list
		 (if (eq? 'DIRECTORY type)
		     ;; If FILE is a symbolic link, this reads the
		     ;; directory that it points to.
		     (->namestring
		      (pathname-new-directory file
					      (append (pathname-directory file)
						      (list "."))))
		     (file-namestring file))))))))

(define (split-unix-switch-string switches)
  (let ((end (string-length switches)))
    (let loop ((start 0))
      (if (fix:< start end)
	  (let ((space (substring-find-next-char switches start end #\space)))
	    (if space
		(cons (substring switches start space)
		      (loop (fix:+ space 1)))
		(list (substring switches start end))))
	  '()))))

;;;; Subprocess/Shell Support

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

(define (os/find-program program default-directory)
  (->namestring
   (let ((lose
	  (lambda () (error "Can't find program:" (->namestring program)))))
     (cond ((pathname-absolute? program)
	    (if (not (file-access program 1)) (lose))
	    program)
	   ((not default-directory)
	    (let loop ((path (ref-variable exec-path)))
	      (if (null? path) (lose))
	      (or (and (car path)
		       (pathname-absolute? (car path))
		       (let ((pathname (merge-pathnames program (car path))))
			 (and (file-access pathname 1)
			      pathname)))
		  (loop (cdr path)))))
	   (else
	    (let ((default-directory (merge-pathnames default-directory)))
	      (let loop ((path (ref-variable exec-path)))
		(if (null? path) (lose))
		(let ((pathname
		       (merge-pathnames
			program
			(cond ((not (car path)) default-directory)
			      ((pathname-absolute? (car path)) (car path))
			      (else (merge-pathnames (car path)
						     default-directory))))))
		  (if (file-access pathname 1)
		      pathname
		      (loop (cdr path)))))))))))

(define (os/shell-file-name)
  (or (get-environment-variable "SHELL")
      "/bin/sh"))

(define (os/form-shell-command command)
  (list "-c" command))

(define (os/shell-name pathname)
  (file-namestring pathname))

(define (os/default-shell-prompt-pattern)
  "^[^#$>]*[#$>] *")

(define (os/default-shell-args)
  '("-i"))

(define-variable explicit-csh-args
  "Args passed to inferior shell by M-x shell, if the shell is csh.
Value is a list of strings."
  (if (string=? microcode-id/operating-system-variant "HP-UX")
      ;; -T persuades HP's csh not to think it is smarter
      ;; than us about what terminal modes to use.
      '("-i" "-T")
      '("-i")))

(define (os/comint-filename-region start point end)
  (let ((chars "~/A-Za-z0-9---_.$#,"))
    (let ((start (skip-chars-backward chars point start)))
      (make-region start (skip-chars-forward chars start end)))))

(define (os/scheme-can-quit?)
  (subprocess-job-control-available?))

(define (os/quit dir)
  dir					; ignored
  (%quit))

(define (os/set-file-modes-writable! pathname)
  (set-file-modes! pathname #o777))

(define (os/sendmail-program)
  (if (file-exists? "/usr/lib/sendmail")
      "/usr/lib/sendmail"
      "fakemail"))
;;; -*-Scheme-*-
;;;
;;;	$Id: dosfile.scm,v 1.20 1998/10/23 05:44:06 cph Exp $
;;;
;;;	Copyright (c) 1994-98 Massachusetts Institute of Technology
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

;;;; DOS-Syntax File Customizations

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

(define dos/encoding-pathname-types
  '("gz" "bf" "ky"))

(define dos/backup-suffixes
  (cons "~"
	(map (lambda (type) (string-append "~." type))
	     dos/encoding-pathname-types)))

(define-variable completion-ignored-extensions
  "Completion ignores filenames ending in any string in this list."
  (append (list ".bin" ".com" ".ext"
		".inf" ".bif" ".bsm" ".bci" ".bcs"
		".psb" ".moc" ".fni"
		".bco" ".bld" ".bad" ".glo" ".fre"
		".obj" ".exe" ".pif" ".grp"
		".dvi" ".toc" ".log" ".aux")
	  (list-copy dos/backup-suffixes))
  (lambda (extensions)
    (and (list? extensions)
	 (for-all? extensions
	   (lambda (extension)
	     (and (string? extension)
		  (not (string-null? extension))))))))

;;;; Filename I/O

(define (os/trim-pathname-string string prefix)
  (let ((index (string-match-forward prefix string)))
    (if (and index
	     (or (fix:= index (string-length prefix))
		 (and (fix:> index 0)
		      (or (char=? (string-ref prefix (fix:- index 1)) #\/)
			  (char=? (string-ref prefix (fix:- index 1)) #\\))))
	     (re-substring-match "[\\/$~]\\|[a-zA-Z]:"
				 string index (string-length string)))
	(string-tail string index)
	string)))

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

(define (os/directory-list directory)
  (let ((channel (directory-channel-open directory)))
    (let loop ((result '()))
      (let ((name (directory-channel-read channel)))
	(if name
	    (loop (cons (begin (string-downcase! name) name) result))
	    (begin
	      (directory-channel-close channel)
	      result))))))

(define (os/directory-list-completions directory prefix)
  (let ((channel (directory-channel-open directory)))
    (let loop ((result '()))
      (let ((name (directory-channel-read-matching channel prefix)))
	(if name
	    (loop (cons (begin (string-downcase! name) name) result))
	    (begin
	      (directory-channel-close channel)
	      result))))))

;;;; Dired customization

(define-variable dired-listing-switches
  "Dired listing format.
Recognized switches are:
    -a	show all files including system and hidden files
    -t	sort files according to modification time
    -l	ignored (but allowed for unix compatibility)
Switches may be concatenated, e.g. `-lt' is equivalent to `-l -t'."
  "-l"
  string?)

(define-variable list-directory-brief-switches
  "list-directory brief listing format.
Recognized switches are:
    -a	show all files including system and hidden files
    -t	sort files according to modification time
    -l	ignored (but allowed for unix compatibility)
Switches may be concatenated, e.g. `-lt' is equivalent to `-l -t'."
  ""
  string?)

(define-variable list-directory-verbose-switches
  "list-directory verbose listing format.
Recognized switches are:
    -a	show all files including system and hidden files
    -t	sort files according to modification time
    -l	ignored (but allowed for unix compatibility)
Switches may be concatenated, e.g. `-lt' is equivalent to `-l -t'."
  "-l"
  string?)

(define (insert-directory! file switches mark type)
  ;; Insert directory listing for FILE at MARK.
  ;; SWITCHES are examined for the presence of "a" and "t".
  ;; TYPE can have one of three values:
  ;;   'WILDCARD means treat FILE as shell wildcard.
  ;;   'DIRECTORY means FILE is a directory and a full listing is expected.
  ;;   'FILE means FILE itself should be listed, and not its contents.
  (let ((mark (mark-left-inserting-copy mark))
	(now (get-universal-time)))
    (catch-file-errors (lambda (c)
			 (insert-string (condition/report-string c) mark)
			 (insert-newline mark))
      (lambda ()
	(for-each
	 (lambda (entry)
	   (insert-string (dos/dired-line-string (car entry) (cdr entry) now)
			  mark)
	   (insert-newline mark))
	 (if (eq? 'FILE type)
	     (let ((attributes (file-attributes file)))
	       (if attributes
		   (list (cons (file-namestring file) attributes))
		   '()))
	     (sort (dos/read-dired-files file
					 (string-find-next-char switches #\a))
		   (if (string-find-next-char switches #\t)
		       (lambda (x y)
			 (> (file-attributes/modification-time (cdr x))
			    (file-attributes/modification-time (cdr y))))
		       (lambda (x y)
			 (string-ci<? (car x) (car y)))))))))
    (mark-temporary! mark)))

(define (dos/dired-line-string name attr now)
  (string-append
   (file-attributes/mode-string attr)
   " "
   (string-pad-left (number->string (file-attributes/length attr)) 10 #\space)
   " "
   (file-time->ls-string (file-attributes/modification-time attr) now)
   " "
   name))

(define dired-pathname-wild?
  pathname-wild?)

;;;; Backup and Auto-Save Filenames

(define (os/buffer-backup-pathname truename buffer)
  (call-with-values
      (lambda ()
	(if (dos/fs-long-filenames? truename)
	    (let ((type (pathname-type truename)))
	      (if (member type dos/encoding-pathname-types)
		  (values (pathname-new-type truename #f)
			  (string-append "~." type))
		  (values truename "~")))
	    (values truename "")))
    (lambda (truename suffix)
      (if (eq? 'NEVER (ref-variable version-control buffer))
	  (values (dos/make-backup-pathname truename #f suffix) '())
	  (let ((prefix
		 (if (dos/fs-long-filenames? truename)
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
		  (values (dos/make-backup-pathname
			   truename
			   (and (ref-variable version-control buffer) 1)
			   suffix)
			  '())
		  (values (dos/make-backup-pathname
			   truename
			   (+ (apply max (map cdr backups)) 1)
			   suffix)
			  (let ((start (ref-variable kept-old-versions buffer))
				(end
				 (- (length backups)
				    (- (ref-variable kept-new-versions buffer)
				       1))))
			    (if (< start end)
				(map (let ((dir (directory-pathname truename)))
				       (lambda (entry)
					 (merge-pathnames (car entry) dir)))
				     (sublist backups start end))
				'()))))))))))

(define (dos/make-backup-pathname pathname version suffix)
  (if (dos/fs-long-filenames? pathname)
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
  (or (there-exists? dos/backup-suffixes
	(lambda (suffix)
	  (string-suffix? suffix filename)))
      (let ((type (pathname-type filename)))
	(and (string? type)
	     (or (string-ci=? "bak" type)
		 (re-string-match ".[0-9][0-9]" type))))))

(define (os/numeric-backup-filename? filename)
  (and (let ((try (lambda (pattern) (re-string-search pattern filename))))
	 (or (try "^\\([^.]+\\)\\.\\([0-9][0-9][0-9]\\)$")
	     (try "^\\([^.]+\\.[^.]\\)\\([0-9][0-9]\\)$")
	     (there-exists? dos/backup-suffixes
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

(define (os/auto-save-filename? filename)
  (if (dos/fs-long-filenames? filename)
      (re-string-match "^#.+#$" (file-namestring filename))
      (let ((type (pathname-type filename)))
	(and (string? type)
	     (string-ci=? "sav" type)))))

(define (os/precious-backup-pathname pathname)
  (let ((directory (directory-pathname pathname)))
    (let loop ((i 0))
      (let ((pathname
	     (merge-pathnames (string-append "#tmp#" (number->string i))
			      directory)))
	(if (allocate-temporary-file pathname)
	    (begin
	      (deallocate-temporary-file pathname)
	      pathname)
	    (loop (+ i 1)))))))

(define (os/auto-save-pathname pathname buffer)
  (let ((pathname
	 (or pathname
	     (merge-pathnames (dos/buffer-auto-save-name buffer)
			      (buffer-default-directory buffer)))))
    (if (dos/fs-long-filenames? pathname)
	(merge-pathnames (string-append "#" (file-namestring pathname) "#")
			 (directory-pathname pathname))
	(pathname-new-type pathname "sav"))))

(define (dos/buffer-auto-save-name buffer)
  (string-append
   "%"
   (let ((directory (buffer-default-directory buffer)))
     (cond ((not (dos/fs-long-filenames? directory))
	    (let ((name (dos/buffer-short-name buffer char-set:valid-fat)))
	      (if (string-null? name)
		  "buffer%"
		  name)))
	   ((string-ci=? "hpfs" (car (dos/fs-drive-type directory)))
	    (dos/buffer-long-name buffer char-set:valid-hpfs))
	   (else
	    (dos/buffer-long-name buffer char-set:valid-windows-long))))))

(define (dos/buffer-long-name buffer valid-chars)
  (let ((name (buffer-name buffer)))
    (let ((length (string-length name)))
      (let ((copy (make-string length)))
	(do ((i 0 (fix:+ i 1)))
	    ((fix:= i length))
	  (string-set!
	   copy i
	   (let ((char (string-ref name i)))
	     (if (char-set-member? valid-chars char)
		 char
		 #\_))))
	copy))))

(define (dos/buffer-short-name buffer valid-chars)
  (let ((name
	 (list->string
	  (let loop ((chars (string->list (buffer-name buffer))))
	    (cond ((null? chars)
		   '())
		  ((char-set-member? valid-chars (car chars))
		   (cons (car chars) (loop (cdr chars))))
		  (else
		   (loop (cdr chars))))))))
    (let ((n (string-length name)))
      (if (fix:<= n 7)
	  name
	  (string-head name 7)))))

(define char-set:valid-hpfs)
(define char-set:valid-windows-long)
(let ((reserved-chars
       (char-set-union (string->char-set "\"/:<>\\|")
		       (string->char-set "*?"))))
  (set! char-set:valid-hpfs
	(char-set-difference (ascii-range->char-set #x21 #x7F)
			     reserved-chars))
  (set! char-set:valid-windows-long
	(char-set-difference (ascii-range->char-set #x20 #x100)
			     reserved-chars)))

(define char-set:valid-fat
  (char-set-union char-set:alphanumeric
		  (string->char-set "!#$%'()-@^_`{}~")))

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
    (if (member type dos/encoding-pathname-types)
	(pathname-type (->namestring (pathname-new-type pathname #f)))
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
   `(("article" . text)
     ("asm" . midas)
     ("bat" . text)
     ("bib" . text)
     ("c" . c)
     ("cc" . c)
     ("h" . c)
     ("java" . java)
     ("pas" . pascal)
     ("s" . scheme)
     ("scm" . scheme)
     ("text" . text)
     ("txi" . texinfo)
     ("txt" . text)
     ("y" . c))))

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

(define (os/newsrc-file-name server)
  (let ((homedir (user-homedir-pathname)))
    (if (dos/fs-long-filenames? homedir)
	(let ((specific
	       (merge-pathnames (string-append ".newsrc-" server) homedir)))
	  (if (file-exists? specific)
	      specific
	      (merge-pathnames ".newsrc" homedir)))
	(merge-pathnames "newsrc.ini" homedir))))

(define (os/info-default-directory-list)
  '())

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
  (or (dos/find-program program (ref-variable exec-path) default-directory)
      (error "Can't find program:" (->namestring program))))

(define (dos/find-program program exec-path default-directory)
  (let* ((try
	  (let ((types (os/executable-pathname-types)))
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
	 (try-dir
	  (lambda (directory)
	    (try (merge-pathnames program directory)))))
    (if (pathname-absolute? program)
	(try program)
	(or (and (eq? 'NT microcode-id/operating-system)
		 (let ((ns (nt/scheme-executable-pathname)))
		   (and ns
			(try-dir (directory-pathname ns)))))
	    (if (not default-directory)
		(let loop ((path exec-path))
		  (and (not (null? path))
		       (or (and (pathname-absolute? (car path))
				(try-dir (car path)))
			   (loop (cdr path)))))
		(let ((default-directory (merge-pathnames default-directory)))
		  (let loop ((path exec-path))
		    (and (not (null? path))
			 (or (try-dir (merge-pathnames (car path)
						       default-directory))
			     (loop (cdr path)))))))))))

(define (nt/scheme-executable-pathname)
  (let ((handle
	 (get-module-handle
	  (file-namestring
	   (pathname-default-type
	    ((make-primitive-procedure 'SCHEME-PROGRAM-NAME))
	    "exe"))))
	(buf (make-string 256)))
    (substring buf 0 (get-module-file-name handle buf 256))))

(define (os/shell-file-name)
  (or (get-environment-variable "SHELL")
      (get-environment-variable "COMSPEC")
      (dos/default-shell-file-name)))

(define (os/shell-name pathname)
  (if (member (pathname-type pathname) (os/executable-pathname-types))
      (pathname-name pathname)
      (file-namestring pathname)))

(define (os/form-shell-command command)
  (list "/c" command))

(define (os/executable-pathname-types)
  '("exe" "com" "bat" "btm"))

(define (os/default-shell-args)
  '())

(define (os/default-shell-prompt-pattern)
  "^[^]$]*[]>] *")

(define (os/comint-filename-region start point end)
  (let ((chars "]\\\\A-Za-z0-9!#$%&'()+,.:;=@[^_`{}~---"))
    (let ((start (skip-chars-backward chars point start)))
      (make-region start (skip-chars-forward chars start end)))))

(define (os/shell-command-separators)
  "&|")

(define (os/shell-command-regexp)
  (string-append "[^" (os/shell-command-separators) "\n]+"))

;;;; File-Encoding Methods

(define (os/read-file-methods)
  `((,read/write-compressed-file?
     . ,(lambda (pathname mark visit?)
	  visit?
	  (read-compressed-file "gzip -d" pathname mark)))
    (,read/write-encrypted-file?
     . ,(lambda (pathname mark visit?)
	  visit?
	  (read-encrypted-file pathname mark)))))

(define (os/write-file-methods)
  `((,read/write-compressed-file?
     . ,(lambda (region pathname visit?)
	  visit?
	  (write-compressed-file "gzip" region pathname)))
    (,read/write-encrypted-file?
     . ,(lambda (region pathname visit?)
	  visit?
	  (write-encrypted-file region pathname)))))

(define (os/alternate-pathnames group pathname)
  (if (dos/fs-long-filenames? pathname)
      (append (if (and (ref-variable enable-compressed-files group)
		       (not (equal? "gz" (pathname-type pathname))))
		  (list (string-append (->namestring pathname) ".gz"))
		  '())
	      (if (and (ref-variable enable-encrypted-files group)
		       (not (equal? "bf" (pathname-type pathname))))
		  (list (string-append (->namestring pathname) ".bf"))
		  '())
	      (if (and (ref-variable enable-encrypted-files group)
		       (not (equal? "ky" (pathname-type pathname))))
		  (list (string-append (->namestring pathname) ".ky"))
		  '()))
      '()))

;;;; Compressed Files

(define-variable enable-compressed-files
  "If true, compressed files are automatically uncompressed when read,
and recompressed when written.  A compressed file is identified by the
filename suffix \".gz\"."
  #t
  boolean?)

(define (read/write-compressed-file? group pathname)
  (and (ref-variable enable-compressed-files group)
       (equal? "gz" (pathname-type pathname))))

(define (read-compressed-file program pathname mark)
  (message "Uncompressing file " (->namestring pathname) "...")
  (let ((value
	 (call-with-temporary-file-pathname
	  (lambda (temporary)
	    (if (not (equal? '(EXITED . 0)
			     (shell-command #f #f
					    (directory-pathname pathname)
					    #f
					    (string-append
					     program
					     " < "
					     (file-namestring pathname)
					     " > "
					     (->namestring temporary)))))
		(error:file-operation pathname
				      program
				      "file"
				      "[unknown]"
				      read-compressed-file
				      (list pathname mark)))
	    (group-insert-file! (mark-group mark)
				(mark-index mark)
				temporary
				(pathname-newline-translation pathname))))))
    (append-message "done")
    value))

(define (write-compressed-file program region pathname)
  (message "Compressing file " (->namestring pathname) "...")
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
			    (list region pathname)))
  (append-message "done"))

;;;; Encrypted files

(define-variable enable-encrypted-files
  "If true, encrypted files are automatically decrypted when read,
and recrypted when written.  An encrypted file is identified by the
filename suffixes \".bf\" and \".ky\"."
  #t
  boolean?)

(define (read/write-encrypted-file? group pathname)
  (and (ref-variable enable-encrypted-files group)
       (or (and (equal? "bf" (pathname-type pathname))
		(blowfish-available?))
	   (equal? "ky" (pathname-type pathname)))))

(define (read-encrypted-file pathname mark)
  (let ((password (prompt-for-password "Password: "))
	(type (pathname-type pathname)))
    (message "Decrypting file " (->namestring pathname) "...")
    (cond ((equal? "bf" type)
	   (call-with-binary-input-file pathname
	     (lambda (input)
	       (read-blowfish-file-header input)
	       (call-with-output-mark mark
		 (lambda (output)
		   (blowfish-encrypt-port input output password #f))))))
	  ((or (equal? "ky" type) (equal? "KY" type))
	   (insert-string (let ((the-encrypted-file
				 (call-with-binary-input-file pathname
				   (lambda (port)
				     (read-string (char-set) port)))))
			    (decrypt the-encrypted-file password
				     (lambda () 
				       (kill-buffer (mark-buffer mark))
				       (editor-error "krypt: Password error!"))
				     (lambda (x) 
				       (editor-beep)
				       (message "krypt: Checksum error!")
				       x)))
			  mark)))
    ;; Disable auto-save here since we don't want to
    ;; auto-save the unencrypted contents of the 
    ;; encrypted file.
    (define-variable-local-value! (mark-buffer mark)
	(ref-variable-object auto-save-default)
      #f)
    (append-message "done")))

(define (write-encrypted-file region pathname)
  (let ((password (prompt-for-confirmed-password))
	(type (pathname-type pathname)))
    (message "Encrypting file " (->namestring pathname) "...")
    (cond ((equal? "bf" type)
	   (let ((input
		  (make-buffer-input-port (region-start region)
					  (region-end region))))
	     (call-with-binary-output-file pathname
	       (lambda (output)
		 (write-blowfish-file-header output)
		 (blowfish-encrypt-port input output password #t)))))
	  ((or (equal? "ky" type) (equal? "KY" type))
	   (let ((the-encrypted-file
		  (encrypt (extract-string (region-start region)
					   (region-end region))
			   password)))
	     (call-with-binary-output-file pathname
	       (lambda (port)
		 (write-string the-encrypted-file port))))))
    (append-message "done")))
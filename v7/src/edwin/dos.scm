;;; -*-Scheme-*-
;;;
;;;	$Id: dos.scm,v 1.15 1994/10/07 19:59:53 adams Exp $
;;;
;;;	Copyright (c) 1992-1994 Massachusetts Institute of Technology
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

;;;; DOS Customizations for Edwin

(declare (usual-integrations))

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
  true
  (lambda (thing)
    (or (eq? thing 'NEVER) (boolean? thing))))

(define-variable kept-old-versions
  "Number of oldest versions to keep when a new numbered backup is made."
  2
  exact-nonnegative-integer?)

(define-variable kept-new-versions
  "Number of newest versions to keep when a new numbered backup is made.
Includes the new backup.  Must be > 0."
  2
  (lambda (n) (and (exact-integer? n) (> n 0))))

(define os/directory-char-set (char-set #\\ #\/))
(define os/expand-char-set (char-set #\$ #\~))

(define (os/trim-pathname-string string)
  ;; Trim a filename with false starts to a unique name
  (define (trim-for-duplicate-top-level-directory string)
    (let ((end (string-length string)))
      (let loop ((index end))
	(let ((slash
	       (substring-find-previous-char-in-set string 0 index
						    os/directory-char-set)))
	  (cond ((not slash) string)
		((and (fix:< (fix:1+ slash) end)
		      (char-set-member? os/expand-char-set
					(string-ref string (fix:1+ slash))))
		 (string-tail string (fix:1+ slash)))
		((zero? slash)
		 string)
		((char-set-member? os/directory-char-set
				   (string-ref string (fix:-1+ slash)))
		 (string-tail string slash))
		(else
		 (loop (fix:-1+ slash))))))))

  (define (trim-for-duplicate-device string)
    (let ((end (string-length string))
	  (sep (char-set-union (char-set #\:)
			       (char-set-union os/expand-char-set
					       os/directory-char-set))))
      (let ((colon  (substring-find-previous-char string 0 end #\:)))
	(cond ((or (not colon) (zero? colon))
	       string)
	      ((and (fix:< (fix:1+ colon) end)
		    (char-set-member? os/expand-char-set
				      (string-ref string (fix:1+ colon))))
	       (string-tail string (fix:1+ colon)))
	      ((substring-find-previous-char-in-set string 0 colon sep)
	       =>
	       (lambda (before)
		 (string-tail string 
			      (if (char-set-member? os/expand-char-set
						    (string-ref string before))
				  before
				  (fix:+ before 1)))))
	      (else
	       string)))))

  (trim-for-duplicate-device (trim-for-duplicate-top-level-directory string)))

(define (os/pathname->display-string pathname)
  (os/filename->display-string (->namestring pathname)))

(define (os/filename->display-string filename)
  (let ((name (string-copy filename)))
    (let ((end (string-length name)))
      (let loop ((index 0))
	(let ((slash (substring-find-next-char name index end #\/)))
	  (if slash
	      (begin
		(string-set! name slash #\\)
		(loop (1+ slash)))))))
    name))

(define version-fill-char #\~ )
(define version-radix 10)

(define (file-name->version name version)
  (let ((version-string
	 (and (fix:fixnum? version)
	      (number->string version version-radix))))
    (if (not version-string)
	(error "Illegal version" version)
	(let* ((digits (string-length version-string))
	       (version-string
		(string-append (string version-fill-char) version-string)))
	  (if (string? name)
	      (let ((cut-point (min (string-length name) (- 8 digits 1))))
		(string-append (substring name 0 cut-point)
			       version-string))
	      version-string)))))

(define (filename->version-number-index name) ; string->#F or integer
  (and (string? name)
       (let loop ((i (- (string-length name) 1))
		  (first-digit #F))
	 (cond ((< i 0)  #F)
	       ((char->digit (string-ref name i) version-radix)
		(loop (- i 1) i))
	       ((and first-digit (char=? (string-ref name i) version-fill-char))
		first-digit)
	       (else #F)))))

(define (filename+index->version-number name first-digit)
  (substring->number name first-digit (string-length name)))

(define (filename->version-number filename)
  (let ((name (pathname-name filename)))
    (let ((first-digit (filename->version-number-index name)))
      (and first-digit
	   (filename+index->version-number name first-digit)))))

(define (plausible-backup? name possible-backup) ; both filename strings
  ;; "foolish" "foolis76" -> #F
  ;; "foolish" "fooli~76" -> #T
  ;; "f"       "f~76"     -> #T
  ;; "f"       "foo~76"   -> #F
  (let ((index  (filename->version-number-index possible-backup)))
    (and index
	 (let ((end  (min (string-length name) (- index 1))))
	   (and (substring-ci=? name 0 end possible-backup 0 end)
		(or (= (string-length possible-backup) 8) ; truncated
		    (= end (string-length name))))))))    ; or exact match

(define (os/auto-save-pathname pathname buffer)
  buffer
  (pathname-new-name pathname
		     (file-name->version (pathname-name pathname) 0)))

(define (os/precious-backup-pathname pathname)
  ;; Use the autosave name for the precious backup
  (pathname-new-name pathname
		     (file-name->version (pathname-name pathname) 0)))

(define (os/backup-buffer? truename)
  (let ((attrs (file-attributes truename)))
    (and attrs
	 (memv (string-ref (file-attributes/mode-string attrs) 0)
	       '(#\- #\l))
	 (not (let ((directory (pathname-directory truename)))
		(and (pair? directory)
		     (eq? 'ABSOLUTE (car directory))
		     (pair? (cdr directory))
		     (eqv? "tmp" (cadr directory))))))))

(define (os/default-backup-filename)
  "c:/tmp/edwin.bak")

(define (os/truncate-filename-for-modeline filename width)
  (let ((length (string-length filename)))
    (if (< 0 width length)
	(let ((result
	       (substring
		filename
		(let ((index (- length width)))
		  (or (and (not
			    (char-set-member? os/directory-char-set
					      (string-ref filename index)))
			   (substring-find-next-char-in-set
			    filename index length os/directory-char-set))
		      (1+ index)))
		length)))
	  (string-set! result 0 #\$)
	  result)
	filename)))

(define (os/backup-by-copying? truename buffer) 
  truename buffer
  false)
	
(define (os/buffer-backup-pathname truename)
  (let ((directory (directory-namestring truename))
	(type      (pathname-type truename))
	(filename  (pathname-name truename)))

    (define (no-versions)
      (values (version->pathname 0) '()))
    (define (version->pathname version)
      (pathname-new-name truename (file-name->version filename version)))

    (define (find-plausible-backups)
      ;; all existing files of the form XXXX~NN.YYY where XXXX and YYY match
      ;; truename
      (let* ((plen     (min (string-length filename)
			    (- 8 5))) ; max version is 99999
	     (pattern  (string-append directory
				      (string-head filename plen)
				      "*." type))
	     (pathnames (directory-read pattern)))
	(let loop ((pathnames pathnames)
		   (found '()))
	  ;; pathnames all have the form XXX*.YYY
	  (if (null? pathnames)
	      found
	      (let* ((pathname  (car pathnames)))
		(if (plausible-backup? filename (pathname-name pathname))
		    (loop (cdr pathnames)
			  (cons (file-namestring pathname) found))
		    (loop (cdr pathnames) found)))))))
    
    (define (files->versions files accum)
      (if (or (not files) (null? files))
	  accum
	  (let ((number (filename->version-number (car files))))
	    (if number
		(files->versions (cdr files) (cons number accum))
		(files->versions (cdr files) accum)))))

    (if (eq? 'NEVER (ref-variable version-control))
	(no-versions)
	(let ((filenames (find-plausible-backups)))
	  (let ((versions (sort (files->versions filenames '() ) <)))
	    (let ((high-water-mark (reduce max 0 versions)))
	      (if (or (ref-variable version-control)
		      (positive? high-water-mark))
		  (values
		   (version->pathname (+ high-water-mark 1))
		   (let ((start (ref-variable kept-old-versions))
			 (end (- (length versions)
				 (1+ (ref-variable kept-new-versions)))))
		     (if (fix:< start end)
			 (map version->pathname
			      (sublist versions start end))
			 '())))
		  (no-versions))))))))

(define (os/directory-list-completions directory prefix)
  (define (->directory-namestring s)
    (->namestring (pathname-as-directory (->pathname s))))

  (map file-namestring
       (directory-read
	(string-append (->directory-namestring directory) ; "d:\\xxx\\yy\\"
		       prefix
		       (if (string-find-next-char prefix #\.) "*" "*.*")))))

(define (os/directory-list directory)
  (os/directory-list-completions directory ""))

(define-integrable os/file-directory?
  (ucode-primitive file-directory?))

(define-integrable (os/make-filename directory filename)
  (string-append directory filename))

(define-integrable (os/filename-as-directory filename)
  (string-append filename "\\"))

(define (os/filename-directory filename)
  (let ((end (string-length filename)))
    (let ((index (substring-find-previous-char-in-set
    		  filename 0 end os/directory-char-set)))
      (and index
	   (substring filename 0 (+ index 1))))))

(define (os/filename-non-directory filename)
  (let ((end (string-length filename)))
    (let ((index (substring-find-previous-char-in-set
		  filename 0 end os/directory-char-set)))
      (if index
	  (substring filename (+ index 1) end)
	  filename))))

(define dos/encoding-pathname-types '())

(define dos/backup-suffixes '())

(define (os/backup-filename? filename)
  (let ((version (filename->version-number filename)))
    (and (fix:fixnum? version)
	 (fix:> version 0))))

(define (os/numeric-backup-filename? filename)
  (let ((name (pathname-name filename)))
    (and (string? name)
	 (let ((index (filename->version-number-index name)))
	   (and index
		(cons (->namestring
		       (pathname-new-name filename
					  (string-head name (- index 1))))
		      (filename+index->version-number name index)))))))


(define (os/auto-save-filename? filename)
  (let ((version (filename->version-number filename)))
    (and (fix:fixnum? version)
	 (fix:= version 0))))  

(define (os/pathname-type-for-mode pathname)
  (let ((type (pathname-type pathname)))
    (if (member type dos/encoding-pathname-types)
	(pathname-type (->namestring (pathname-new-type pathname false)))
	type)))

(define (os/completion-ignore-filename? filename)
  (or (os/backup-filename? filename)
      (os/auto-save-filename? filename)
      (and (not (os/file-directory? filename))
	   (there-exists? (ref-variable completion-ignored-extensions)
   	     (lambda (extension)
	       (string-suffix? extension filename))))))

(define (os/completion-ignored-extensions)
  (append '(".bin" ".com" ".ext"
	    ".inf" ".bif" ".bsm" ".bci" ".bcs"
  	    ".psb" ".moc" ".fni"
  	    ".bco" ".bld" ".bad" ".glo" ".fre"
	    ".obj" ".exe" ".pif" ".grp"
	    ".dvi" ".toc" ".log" ".aux")
	  (list-copy dos/backup-suffixes)))

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
  (let ((user-init-file
	 (merge-pathnames "edwin.ini"
			  (pathname-as-directory
			   (dos/current-home-directory)))))
    (if (file-exists? user-init-file)
	(->namestring user-init-file)
	"/scheme/lib/edwin.ini")))

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

;;;; Dired customization

(define-variable dired-listing-switches
  "Dired listing format -- Ignored under DOS."
  #f
  false?)

(define-variable list-directory-brief-switches
  "list-directory brief listing format -- Ignored under DOS."
  #f
  false?)

(define-variable list-directory-verbose-switches
  "list-directory verbose listing format -- Ignored under DOS."
  #f
  false?)

(define (read-directory pathname switches mark)
  switches				; ignored
  (if (file-directory? pathname)
      (generate-dired-listing!
       (string-append (->namestring (pathname-as-directory pathname))
		      "*.*")
       mark)
      (generate-dired-listing! pathname mark)))

(define (insert-dired-entry! pathname directory lstart)
  directory				; ignored
  (let ((start (mark-left-inserting lstart)))
    (insert-string "  " start)
    (generate-dired-entry! pathname start)))

;;;; Scheme version of ls

(define (generate-dired-listing! pathname point)
  (let ((files (directory-read (->namestring (merge-pathnames pathname)))))
    (for-each (lambda (file) (generate-dired-entry! file point))
	      files)))

(define (generate-dired-entry! file point)
  (define (file-attributes/ls-time-string attr)
    ;; Swap year around to the start
    (let ((time-string ((ucode-primitive file-time->string 1)
			(file-attributes/modification-time attr))))
      (if (string? time-string)
	  (or (let ((len (string-length time-string)))
		(and (fix:> len 5) ;; Grap the space char as well
		     (string-append (substring time-string (fix:- len 5) len)
				    " "
				    (substring time-string 0 (fix:- len 5)))))
	      ""))))

  (let ((name (file-namestring file))
	(attr (or (file-attributes file)
		  (dummy-file-attributes))))
    (let ((entry (string-append
		  (string-pad-right	; Mode string
		   (file-attributes/mode-string attr) 12 #\Space)
		  (string-pad-left    ; Length
		   (number->string (file-attributes/length attr)) 10 #\Space)
		  (string-pad-right   ; Mod time
		   (file-attributes/ls-time-string attr) 26 #\Space)
		  name)))
      (insert-string entry point)
      (insert-newline point))))

(define-integrable (dummy-file-attributes)
  '#(#f 0 0 0 0 0 0 0 "----------" 0))

(define (os/scheme-can-quit?)
  true)

(define (os/quit dir)
  (without-interrupts
    (lambda ()
      (with-real-working-directory-pathname dir %quit))))

(define (with-real-working-directory-pathname dir thunk)
  (let ((inside dir)
	(outside false))
    (without-interrupts
      (lambda ()
	(dynamic-wind
	 (lambda ()
	   (set! outside (working-directory-pathname))
	   (set-working-directory-pathname! inside)
	   ((ucode-primitive set-working-directory-pathname! 1) inside))
	 thunk
	 (lambda ()
	   (set! inside (working-directory-pathname))
	   ((ucode-primitive set-working-directory-pathname! 1) outside)
	   (set-working-directory-pathname! outside)))))))
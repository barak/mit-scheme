;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/dos.scm,v 1.1 1992/05/12 15:29:45 mhwu Exp $
;;;
;;;	Copyright (c) 1992 Massachusetts Institute of Technology
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

(define (os/trim-pathname-string string)
  ;; Trim a filename with false starts to a unique name
  (define (trim-for-duplicate-top-level-directory string)
    (let ((end (string-length string)))
      (let loop ((index end))
	(let ((slash
	       (substring-find-previous-char-in-set string 0 index
						    os/directory-char-set)))
	  (cond ((not slash) string)
		((and (fix:< (1+ slash) end)
		      (char=? (string-ref string (1+ slash)) #\$))
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
	  (sep (char-set-union (char-set #\: #\$) os/directory-char-set)))
      (let ((colon
	     (substring-find-previous-char string 0 end #\:)))
	(cond ((or (not colon) (zero? colon))
	       string)
	      ((and (fix:< (fix:1+ colon) end)
		    (char=? (string-ref string (fix:1+ colon)) #\$))
	       (string-tail string (fix:1+ colon)))
	      ((substring-find-previous-char-in-set string 0 colon sep)
	       =>
	       (lambda (before)
		 (string-tail string 
			      (if (char=? (string-ref string before) #\$)
				  before
				  (fix:1+ before)))))
	      (else
	       string)))))
  (trim-for-duplicate-device (trim-for-duplicate-top-level-directory string)))
						      

(define (os/pathname->display-string pathname)
  (os/filename->display-string (->namestring pathname)))

(define (os/filename->display-string filename)
  (let ((name (string-copy filename)))
    (slash->backslash! name)
    name))

(define (slash->backslash! name)
  (let ((end (string-length name)))
    (let loop ((index 0))
      (let ((slash (substring-find-next-char name index end #\/)))
        (if (not slash)
            '()
            (begin
              (string-set! name slash #\\)
	      (loop (1+ slash))))))))

(define (file-type->version type version)
  (let ((version-string
	 (and (fix:fixnum? version)
	      (number->string (fix:remainder version 1000)))))
    (if (not version-string)
	(error "Illegal version" version)
	(let ((version-string
	       (string-pad-left version-string 3 #\0)))
	  (if (string? type)
	      (if (fix:> (string-length type) 0)
		  (string-append (substring type 0 1)
				 (substring version-string 1 3))
		  version-string)
	      version-string)))))

(define (filename->version-number filename)
  (let ((type (pathname-type filename)))
    (and (string? type)
	 (fix:= (string-length type) 3)
	 (or (string->number type)
	     (string->number (substring type 1 3))))))

(define (os/auto-save-pathname pathname buffer)
  buffer
  (pathname-new-type pathname
		     (file-type->version (pathname-type pathname) 0)))

(define (os/precious-backup-pathname pathname)
  ;; Use the autosave name for the precious backup
  (pathname-new-type pathname
		     (file-type->version (pathname-type pathname) 0)))

(define (os/backup-buffer? truename)
  (and (memv (string-ref 
	      (file-attributes/mode-string (file-attributes truename)) 0)
	     '(#\- #\l))
       (not
	(let ((directory (pathname-directory truename)))
	  (and (pair? directory)
	       (eq? 'ABSOLUTE (car directory))
	       (pair? (cdr directory))
	       (eqv? "tmp" (cadr directory)))))))

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

(define (os/backup-by-copying? truename) 
  truename
  false)
	
(define (os/buffer-backup-pathname truename)
  (let ((directory (directory-namestring truename))
	(type (pathname-type truename))
	(filename (pathname-name truename)))

    (define (no-versions)
      (values (pathname-new-type truename (file-type->version type 0)) '()))
    (define (version->pathname version)
      (pathname-new-type truename (file-type->version type version)))
    (define (files->versions files)
      (if (or (not files) (null? files))
	  '()
	  (let ((type-number (filename->version-number (car files))))
	    (if type-number
		(cons type-number (files->versions (cdr files)))
		(files->versions (cdr files))))))
	  
    (if (eq? 'NEVER (ref-variable version-control))
	(no-versions)
	(let ((search-name (string-append filename ".")))
	  (let ((filenames
		 (os/directory-list-completions directory search-name)))
	    (let ((versions (sort (files->versions filenames) <)))
	      (let ((high-water-mark (apply max (cons 0 versions))))
		(if (or (ref-variable version-control)
			(positive? high-water-mark))
		    (values
		     (version->pathname (+ high-water-mark 1))
		     (let ((start (ref-variable kept-old-versions))
			   (end (fix:- (length versions)
				       (fix:-1+
					(ref-variable kept-new-versions)))))
		       (if (fix:< start end)
			   (map version->pathname
				(sublist versions start end))
			   '())))
		    (no-versions)))))))))


(define (os/directory-list-completions directory prefix)
  (define (->directory-namestring s)
    (->namestring (pathname-as-directory (->pathname s))))
  (define (->directory-wildcard s)
    (string-append (->directory-namestring s) "*.*"))
  (let ((plen (string-length prefix)))
    (let loop ((pathnames (directory-read (->directory-wildcard directory))))
      (if (null? pathnames)
	  '()
	  (let ((filename (file-namestring (car pathnames))))
	    (if (and (fix:>= (string-length filename) plen)
		     (string-ci=? prefix (substring filename 0 plen)))
		(cons filename (loop (cdr pathnames)))
		(loop (cdr pathnames))))))))

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
  (append '(".bin" ".com" ".ext" ".inf"
		   ".psb" ".moc" ".fni"
		   ".bco" ".bld" ".bad" ".glo" ".fre"
		   ".obj" ".exe" ".pif"
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
   `(("article" . text)
     ("asm" . midas)
     ("bat" . text)
     ("bib" . text)
     ("c" . c)
     ("cc" . c)
     ("h" . c)
     ("m4" . midas)
     ("pas" . pascal)
     ("s" . scheme)
     ("scm" . scheme)
     ("text" . text)
     ("txi" . texinfo)
     ("txt" . text)
     ("y" . c))))


(define (os/init-file-name)
  (let* ((home (dos/current-home-directory))
	 (user-init-file (merge-pathnames "edwin.ini" home)))
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


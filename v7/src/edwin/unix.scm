;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/unix.scm,v 1.19 1991/11/04 20:52:15 cph Exp $
;;;
;;;	Copyright (c) 1989-91 Massachusetts Institute of Technology
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

(define (os/filename->display-string filename)
  (let ((home (unix/current-home-directory)))
    (cond ((not (string-prefix? home filename))
	   filename)
	  ((string=? home filename)
	   "~")
	  ((char=? #\/ (string-ref filename (string-length home)))
	   (string-append "~" (string-tail filename (string-length home))))
	  (else
	   filename))))

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
  (->pathname (string-append (->namestring pathname) "#")))

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

(define (os/backup-by-copying? truename)
  (let ((attributes (file-attributes truename)))
    (and (ref-variable backup-by-copying-when-linked)
	 (> (file-attributes/n-links attributes) 1))
    (and (ref-variable backup-by-copying-when-mismatch)
	 (not (and (= (file-attributes/uid attributes) (unix/current-uid))
		   (= (file-attributes/gid attributes) (unix/current-gid)))))))

(define (os/buffer-backup-pathname truename)
  (let ((no-versions
	 (lambda ()
	   (values
	    (->pathname (string-append (->namestring truename) "~"))
	    '()))))
    (if (eq? 'NEVER (ref-variable version-control))
	(no-versions)
	(let ((prefix (string-append (file-namestring truename) ".~")))
	  (let ((filenames
		 (os/directory-list-completions
		  (directory-namestring truename)
		  prefix))
		(prefix-length (string-length prefix)))
	    (let ((possibilities
		   (list-transform-positive filenames
		     (let ((non-numeric (char-set-invert char-set:numeric)))
		       (lambda (filename)
			 (let ((end (string-length filename)))
			   (let ((last (-1+ end)))
			     (and (char=? #\~ (string-ref filename last))
				  (eqv? last
					(substring-find-next-char-in-set
					 filename
					 prefix-length
					 end
					 non-numeric))))))))))
	      (let ((versions
		     (sort (map (lambda (filename)
				  (string->number
				   (substring filename
					      prefix-length
					      (-1+ (string-length filename)))))
				possibilities)
			   <)))
		(let ((high-water-mark (apply max (cons 0 versions))))
		  (if (or (ref-variable version-control)
			  (positive? high-water-mark))
		      (let ((version->pathname
			     (let ((directory (directory-pathname truename)))
			       (lambda (version)
				 (merge-pathnames
				  (string-append prefix
						 (number->string version)
						 "~")
				  directory)))))
			(values
			 (version->pathname (1+ high-water-mark))
			 (let ((start (ref-variable kept-old-versions))
			       (end
				(- (length versions)
				   (-1+ (ref-variable kept-new-versions)))))
			   (if (< start end)
			       (map version->pathname
				    (sublist versions start end))
			       '()))))
		      (no-versions))))))))))

(define (os/directory-list directory)
  ((ucode-primitive directory-close 0))
  ((ucode-primitive directory-open-noread 1) directory)
  (let loop ((result '()))
    (let ((name ((ucode-primitive directory-read 0))))
      (if name
	  (loop (cons name result))
	  (begin
	    ((ucode-primitive directory-close 0))
	    result)))))

(define (os/directory-list-completions directory prefix)
  ((ucode-primitive directory-close 0))
  ((ucode-primitive directory-open-noread 1) directory)
  (let loop ((result '()))
    (let ((name ((ucode-primitive directory-read-matching 1) prefix)))
      (if name
	  (loop (cons name result))
	  (begin
	    ((ucode-primitive directory-close 0))
	    result)))))

(define-integrable os/file-directory?
  (ucode-primitive file-directory?))

(define-integrable (os/make-filename directory filename)
  (string-append directory filename))

(define-integrable (os/filename-as-directory filename)
  (string-append filename "/"))

(define (os/filename-directory filename)
  (let ((end (string-length filename)))
    (let ((index (substring-find-previous-char filename 0 end #\/)))
      (and index
	   (substring filename 0 (+ index 1))))))

(define (os/filename-non-directory filename)
  (let ((end (string-length filename)))
    (let ((index (substring-find-previous-char filename 0 end #\/)))
      (if index
	  (substring filename (+ index 1) end)
	  filename))))

(define (os/completion-ignored-extensions)
  (list-copy
   '(".o" ".elc" "~" ".bin" ".lbin" ".fasl"
     ".dvi" ".toc" ".log" ".aux"
     ".lof" ".blg" ".bbl" ".glo" ".idx" ".lot")))

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
#| -*-Scheme-*-

$Id: edextra.scm,v 1.35 2002/12/27 03:48:38 cph Exp $

Copyright (c) 1992-2002 Massachusetts Institute of Technology

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

;;;; 6.001: Edwin Extensions

(declare (usual-integrations))

(define student-root-directory)
(define student-work-directory)
(define pset-directory)
(define pset-list-file)
(define command-line-student-directory #f)

(set-command-line-parser! "student"
  (lambda (command-line)
    (let ((name (cadr command-line)))
      (if (file-directory? name)
	  (set! command-line-student-directory (->pathname name)))
      (values (cddr command-line) #F))))

(set! standard-editor-initialization
      (let ((usual standard-editor-initialization))
	(lambda ()
	  (usual)
	  (standard-login-initialization))))

(define (standard-login-initialization)
  (set! student-root-directory
	(if (and command-line-student-directory
		 (file-directory? command-line-student-directory))
	    (pathname-as-directory command-line-student-directory)
	    (let ((6001-dir
		   (get-environment-variable "MITSCHEME_6001_DIRECTORY")))
	      (if (and 6001-dir (file-directory? 6001-dir))
		  (pathname-as-directory 6001-dir)
		  "~u6001/"))))
  (set! student-work-directory
	(merge-pathnames "work/" student-root-directory))
  (if (not (file-directory? student-root-directory))
      (set! student-root-directory (user-homedir-pathname)))
  (if (not (file-directory? student-work-directory))
      (set! student-work-directory (user-homedir-pathname)))
  (set! pset-directory (merge-pathnames "psets/" student-root-directory))
  (set! pset-list-file (merge-pathnames "probsets.scm" pset-directory))
  (set-default-directory student-work-directory)
  (set-working-directory-pathname! student-work-directory)
  (let ((hairy-floppy-stuff?
	 (and (eq? 'UNIX microcode-id/operating-system)
	      (string-ci=? "HP-UX" microcode-id/operating-system-variant))))
    (if hairy-floppy-stuff?
	(run-floppy-login-loop))
    (let ((pathname (merge-pathnames "motd" student-root-directory)))
      (if (file-exists? pathname)
	  (let ((buffer (temporary-buffer "*motd*")))
	    (call-with-current-continuation
	     (lambda (k)
	       (bind-condition-handler (list condition-type:file-error)
		   (lambda (condition)
		     condition
		     (kill-buffer buffer)
		     (k unspecific))
		 (lambda ()
		   (%insert-file (buffer-start buffer) pathname #f)))
	       (set-buffer-point! buffer (buffer-start buffer))
	       (select-buffer buffer))))))
    (if hairy-floppy-stuff?
	(message "Login completed."))))

(define-command logout
  "Logout from the 6.001 Scheme system."
  ()
  (lambda ()
    (fluid-let ((paranoid-exit? #f))
      ((ref-command save-buffers-kill-scheme) #f))))

(define (restore-focus-to-editor)
  (let ((name (graphics-type-name (graphics-type #f))))
    (case name
      ((X)
       (let ((screen (selected-screen)))
	 (if (xterm-screen/grab-focus! screen)
	     (xterm-screen/flush! screen))))
      ((WIN32)
       ((access set-focus (->environment '(win32)))
	((access get-handle (->environment '(win32))) 1)))
      ((OS/2)
       (os2-screen/activate! (selected-screen)))
      (else
       (error "Unsupported graphics type:" name)))))

(link-variables '(student pictures) 'restore-focus-to-editor
		'(edwin) 'restore-focus-to-editor)

(if (eq? 'UNIX microcode-id/operating-system)
    (load-edwin-library 'PRINT))

(define-command print-graphics
  "Print out window with graphics."
  '()
  (lambda ()
    (let ((window (prompt-for-expression-value "Window to print" 'mouse)))
      (if (eq? window 'mouse)
	  (print-pointed-x-window)
	  (if (graphics-device? window)
	      (print-given-x-window (x-graphics/window-id window))
	      (editor-error "Not a window object!"))))))

(define (print-given-x-window x-window-id)
  ((message-wrapper #f "Spooling")
   (lambda ()
     (shell-command
      #f #f #f #f
      (string-append (->namestring
		      (merge-pathnames "bin/print-given-x-window"
				       student-root-directory))
		     " 0x"
		     (number->string x-window-id 16)
		     " "
		     (print/assemble-switches "Scheme Picture" '()))))))

(define (print-pointed-x-window)
  ((message-wrapper #f "Click desired window")
   (lambda ()
     (shell-command
      #f #f #f #f
      (string-append (->namestring
		      (merge-pathnames "bin/print-pointed-x-window"
				       student-root-directory))
		     " "
		     (print/assemble-switches "Scheme Picture" '()))))))
#|
;;; If using pointer (mouse).

xwd | /usr/local/pbmbin/xwdtopnm | /usr/local/pbmbin/ppmtopgm | /usr/local/pbmbin/pnmscale 4 | /usr/local/pbmbin/pgmtopbm -cluster4 | /usr/local/pbmbin/pbmtolj -resolution 300 | lpr -h

;;; If using *** = x-graphics/window-id

xwd -id *** | /usr/local/pbmbin/xwdtopnm | /usr/local/pbmbin/ppmtopgm | /usr/local/pbmbin/pnmscale 4 | /usr/local/pbmbin/pgmtopbm -cluster4 | /usr/local/pbmbin/pbmtolj -resolution 300 | lpr -h

Now, there is formatting stuff to be considered here, in print-pgm.sh.
|#

;;;; EDWIN Command "Load Problem Set"

;;; Wired-in pathnames

;;; The structure "problem-sets" must be loaded from pset-list-file whenever
;;; the set of available problem sets changes, or when the default
;;; problem set changes.  Files should appear with name and extension, but
;;; without device, directory, or version; these will be supplied
;;; automatically.
;;;
;;; Example problem-sets variable:

;(define problem-sets
;  `(1 (1  (load&reference "ps1-c-curve.scm" "ps1-debug.scm"))
;      (2  (copy "ps2-ans.scm") (load&reference "ps2-primes.scm"))
;      (3  (copy "ps3-ans.scm")
;	  (load&reference "ps3-squares.scm" "ps3-tri.scm"))
;      (4  (copy "ps4-ans.scm") (load&reference "ps4-doctor.scm")
;	  (select "ps4-ans.scm"))
;      (5  (copy "ps5-ans.scm")
;	  (load&reference "ps5-graph.scm" "ps5-imp.scm" "ps5-res.scm"))
;      (6  (copy "ps6-mods.scm") (load&reference "ps6-adv.scm"))
;      (7  (copy "ps7-ans.scm")
;	  (load&reference "ps7-ps.scm" "ps7-psutil.scm" "ps7-ratnum.scm"))
;      (8  (copy "ps8-mods.scm") (load&reference "ps8-mceval.scm"))))

;;; Data abstraction for the "problem-sets" object:

(define problem-sets/default-ps car)
(define problem-sets/psets cdr)
(define psets/first-pset car)
(define psets/rest-psets cdr)
(define psets/empty? null?)
(define pset/ps car)
(define pset/groups cdr)
(define (groups/files-to-copy groups)
  (let ((any (assq 'copy groups)))
    (if any (cdr any) '())))
(define (groups/files-to-load groups)
  (let ((any (assq 'load groups)))
    (if any (cdr any) '())))
(define (groups/files-to-reference groups)
  (let ((any (assq 'reference groups)))
    (if any (cdr any) '())))
(define (groups/files-to-load&reference groups)
  (let ((any (assq 'load&reference groups)))
    (if any (cdr any) '())))
(define (groups/buffer-to-select groups)
  (let ((any (assq 'select groups)))
    (if any (cadr any) '())))
(define (groups/all-files groups)
  (merge-lists (groups/files-to-copy groups)
	       (groups/files-to-load groups)
	       (groups/files-to-reference groups)
	       (groups/files-to-load&reference groups)))

;;; Procedure to get the "files" object corresponding to a particular
;;; problem set.  Runs error-handler (which should never return) if
;;; the problem set number is not listed in the "problem-sets" object.

(define (ps-groups ps error-handler)
  (let loop ((remaining-psets (problem-sets/psets problem-sets)))
    (if (psets/empty? remaining-psets)
	(error-handler)
	(let ((first-ps (psets/first-pset remaining-psets)))
	  (if (string=? ps (->string (pset/ps first-ps)))
	      (pset/groups first-ps)
	      (loop (psets/rest-psets remaining-psets)))))))

;;; Horribly inefficient procedure to merge lists, ensuring that no member
;;; is repeated in the resulting list.
(define (merge-lists . lists)
  (let ((one-list (apply append lists)))
    (let loop ((remaining one-list)
	       (accumulated '()))
      (if (null? remaining)
	  accumulated
	  (let ((first (car remaining))
		(rest (cdr remaining)))
	    (if (memq first rest)
		(loop rest accumulated)
		(loop rest (cons first accumulated))))))))

;;; Returns #t iff FILES all exist in DIRECTORY.
(define (files-all-exist? files directory)
  (for-all? files
    (lambda (file)
      (file-exists? (merge-pathnames directory file)))))

(define-command load-problem-set
  "Load a 6.001 problem set."
  ()
  (lambda ()
    (load-quietly pset-list-file '(EDWIN))
    (let* ((ps
	    (prompt-for-string "Load Problem Set"
			       (->string
				(problem-sets/default-ps problem-sets))))
	   (error-handler
	    (lambda ()
	      (editor-error "There doesn't appear to be a problem set "
			    ps
			    " installed; ask a TA for help.")))
	   (groups (ps-groups ps error-handler))
	   (pset-path
	    (merge-pathnames (string-append "ps" ps "/") pset-directory)))
      (if (not (files-all-exist? (groups/all-files groups) pset-path))
	  (error-handler))
      (for-each (lambda (file)
		  (find-file-noselect (merge-pathnames file pset-path) #t))
		(groups/files-to-reference groups))
      (for-each (lambda (file)
		  (let ((filename (merge-pathnames file pset-path)))
		    ((message-wrapper #f
				      "Evaluating file "
				      (->namestring filename))
		     (lambda ()
		       (load-quietly filename '(STUDENT))))))
		(groups/files-to-load groups))
      (for-each (lambda (file)
		  (let ((filename (merge-pathnames file pset-path)))
		    ((message-wrapper #f
				      "Evaluating file "
				      (->namestring filename))
		     (lambda ()
		       (load-quietly filename '(STUDENT))))
		    (find-file-noselect filename #t)))
		(groups/files-to-load&reference groups))
      (for-each (lambda (file)
		  (load-ps-copy-file file pset-path student-work-directory))
		(groups/files-to-copy groups)))))

(define (load-quietly pathname environment)
  (fluid-let ((load/suppress-loading-message? #t))
    (load pathname environment)))

(define (->string object)
  (if (string? object)
      object
      (with-output-to-string (lambda () (display object)))))

(define (load-ps-copy-file file source-dir dest-dir)
  (let ((source-file (merge-pathnames file source-dir))
	(dest-file (merge-pathnames file dest-dir))
	(filename (->namestring file)))
    (if (file-exists? dest-file)
	(let* ((backup-pathname (pathname-new-type file "bak"))
	       (backup-filename (->namestring backup-pathname)))
	  (with-saved-configuration
	   (lambda ()
	     (delete-other-windows (current-window))
	     (let ((buffer (temporary-buffer " *load-problem-set-dialog*")))
	       (select-buffer buffer)
	       (append-string
		"This problem set contains a file named ")
	       (append-string
		(write-to-string filename))
	       (append-string ",
but your working directory already contains a file of that name.

Answer \"yes\" to replace your file with the file from the problem set.
If you choose this option your file will be renamed \"")
	       (append-string backup-filename)
	       (append-string "\".

Otherwise answer \"no\" to leave your file unchanged; if you choose this
option the file from the problem set will not be installed.
"))
	     (if (prompt-for-yes-or-no? "Install problem set file")
		 (begin
		   (append-string
		    (string-append "\nRenaming \""
				   filename
				   "\" to \""
				   backup-filename
				   "\"..."))
		   (rename-ps-file dest-file backup-pathname)
		   (append-string
		    (string-append "done\n\nCopying file \""
				   filename
				   "\" to working area..."))
		   (copy-ps-file source-file dest-file)
		   (append-string "done"))
		 (begin
		   (append-string "\nOK, not using problem set file.")
		   (find-file-noselect dest-file #t))))))
	(let ((msg
	       (string-append "Copying file \""
			      filename
			      "\" to working area...")))
	  (message msg)
	  (copy-ps-file source-file dest-file)
	  (message msg "done")))))

(define (rename-ps-file from-file to-file)
  (call-with-current-continuation
   (lambda (k)
     (bind-condition-handler (list condition-type:file-error
				   condition-type:port-error)
	 (lambda (condition)
	   condition
	   (k unspecific))
       (lambda ()
	 (delete-file to-file)))))
  (bind-condition-handler (list condition-type:file-error
				condition-type:port-error)
      (lambda (condition)
	(editor-error "Rename failed: "
		      (condition/report-string condition)))
    (lambda ()
      (rename-file from-file to-file))))

(define (copy-ps-file from-file to-file)
  (let ((buffer (find-file-noselect from-file #t)))
    (set-buffer-writeable! buffer)
    (set-visited-pathname buffer to-file)
    (write-buffer buffer)))

;;;; Customization

(set! default-homedir-pathname (lambda () student-work-directory))

(set! editor-can-exit? #f)
(set! scheme-can-quit? #f)
(set! paranoid-exit? #t)

(set-variable! enable-transcript-buffer #t)
(set-variable! evaluate-in-inferior-repl #t)
(set-variable! repl-error-decision #t)
(set-variable! version-control #t)
(set-variable! trim-versions-without-asking #t)

#|
;; No longer needed.
(if (eq? 'UNIX microcode-id/operating-system)
    (set-variable!
     mail-header-function
     (let ((default-reply-to #f))
       (lambda (point)
	 (let ((reply-to
		(prompt-for-string "Please enter an email address for replies"
				   default-reply-to
				   'DEFAULT-TYPE 'INSERTED-DEFAULT)))
	   (if (not (string-null? reply-to))
	       (begin
		 (set! default-reply-to reply-to)
		 (insert-string "From: " point)
		 (insert-string reply-to point)
		 (insert-newline point)
		 (insert-string "Reply-to: " point)
		 (insert-string reply-to point)
		 (insert-newline point))))))))
|#

(set-variable! select-buffer-not-found-hooks
	       (cons (lambda (name)
		       (find-file-noselect
			(merge-pathnames name student-work-directory)
			#t))
		     (ref-variable select-buffer-not-found-hooks)))
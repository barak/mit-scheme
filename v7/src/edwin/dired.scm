;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/dired.scm,v 1.105 1991/03/15 23:38:39 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-91 Massachusetts Institute of Technology
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

;;;; Directory Editor

(declare (usual-integrations))

(define-major-mode dired fundamental "Dired"
  "Major mode for editing a list of files.
Each line describes a file in the directory.
F -- visit the file on the current line.
D -- mark that file to be killed.
U -- remove all marks from the current line.
Rubout -- back up a line and remove marks.
Space -- move down one line.
X -- kill marked files.
Q -- quit, killing marked files.
  This is like \\[dired-do-deletions] followed by \\[kill-buffer].
C-] -- abort Dired; this is like \\[kill-buffer] on this buffer."
  (local-set-variable! case-fold-search true))

(define-key 'dired #\f 'dired-find-file)
(define-key 'dired #\o 'dired-find-file-other-window)
(define-key 'dired #\g 'dired-revert)
(define-key 'dired #\d 'dired-flag-file-deleted)
(define-key 'dired #\c-d 'dired-flag-file-deleted)
(define-key 'dired #\u 'dired-unflag)
(define-key 'dired #\rubout 'dired-backup-unflag)
(define-key 'dired #\space 'dired-next-line)
(define-key 'dired #\c-n 'dired-next-line)
(define-key 'dired #\c-p 'dired-previous-line)
(define-key 'dired #\x 'dired-do-deletions)
(define-key 'dired #\q 'dired-quit)
(define-key 'dired #\c-\] 'dired-abort)
(define-key 'dired #\? 'dired-summary)

(define-command dired
  "\"Edit\" directory DIRNAME--delete, rename, print, etc. some files in it.
Dired displays a list of files in DIRNAME.
You can move around in it with the usual commands.
You can flag files for deletion with C-d
and then delete them by typing `x'.
Type `h' after entering dired for more info."
  "DDired (directory)"
  (lambda (directory)
    (select-buffer (make-dired-buffer directory))))

(define-command dired-other-window
  "\"Edit\" directory DIRNAME.  Like \\[dired] but selects in another window."
  "DDired in other window (directory)"
  (lambda (directory)
    (select-buffer-other-window (make-dired-buffer directory))))

(define (make-dired-buffer directory)
  (let ((directory (->pathname directory)))
    (let ((buffer (get-dired-buffer directory)))
      (set-buffer-major-mode! buffer (ref-mode-object dired))
      (set-buffer-default-directory! buffer
				     (pathname-directory-path directory))
      (buffer-put! buffer 'REVERT-BUFFER-METHOD revert-dired-buffer)
      (buffer-put! buffer 'DIRED-DIRECTORY directory)
      (fill-dired-buffer! buffer directory)
      buffer)))

(define (get-dired-buffer directory)
  (or (list-search-positive (buffer-list)
	(lambda (buffer)
	  (let ((directory* (buffer-get buffer 'DIRED-DIRECTORY)))
	    (and directory*
		 (pathname=? directory* directory)))))
      (new-buffer (pathname->buffer-name directory))))

(define (dired-buffer-directory buffer)
  (or (buffer-get buffer 'DIRED-DIRECTORY)
      (let ((directory (buffer-default-directory buffer)))
	(buffer-put! buffer 'DIRED-DIRECTORY directory)
	directory)))

(define (revert-dired-buffer buffer dont-use-auto-save? dont-confirm?)
  dont-use-auto-save? dont-confirm?	;ignore
  (fill-dired-buffer! buffer (dired-buffer-directory buffer)))

(define (fill-dired-buffer! buffer pathname)
  (set-buffer-writeable! buffer)
  (region-delete! (buffer-region buffer))
  (temporary-message
   (string-append "Reading directory "
		  (pathname->string pathname)
		  "..."))
  (let ((pathnames (read&sort-directory pathname)))
    (let ((lines (map os/make-dired-line pathnames))
	  (point (buffer-point buffer)))
      (append-message "done")
      (for-each (lambda (line pathname)
		  (if (not line)
		      (begin
			(insert-string "can't find file: " point)
			(insert-string (pathname-name-string pathname) point)
			(insert-newline point))))
		lines
		pathnames)
      (insert-string "Directory " point)
      (insert-string (pathname->string pathname) point)
      (insert-newlines 2 point)
      (buffer-put! buffer 'DIRED-HEADER-END (mark-right-inserting point))
      (for-each (lambda (line)
		  (if line
		      (begin
			(insert-string line point)
			(insert-newline point))))
		lines)))
  (buffer-not-modified! buffer)
  (set-buffer-read-only! buffer)
  (add-buffer-initialization! buffer
    (lambda ()
      (set-dired-point! (buffer-get (current-buffer) 'DIRED-HEADER-END)))))

(define-command dired-find-file
  "Read the current file into a buffer."
  ()
  (lambda ()
    (find-file (dired-current-pathname))))

(define-command dired-find-file-other-window
  "Read the current file into a buffer in another window."
  ()
  (lambda ()
    (find-file-other-window (dired-current-pathname))))

(define-command dired-revert
  "Read the current buffer."
  ()
  (lambda ()
    (revert-buffer (current-buffer) true true)))

(define-command dired-flag-file-deleted
  "Mark the current file to be killed."
  "p"
  (lambda (argument)
    (dired-mark #\D argument)))

(define-command dired-unflag
  "Cancel the kill requested for the current file."
  "p"
  (lambda (argument)
    (dired-mark #\Space argument)))

(define-command dired-backup-unflag
  "Cancel the kill requested for the file on the previous line."
  "p"
  (lambda (argument)
    (set-dired-point! (line-start (current-point) -1 'ERROR))
    (dired-mark #\Space argument)
    (set-dired-point! (line-start (current-point) -1 'ERROR))))

(define-command dired-next-line
  "Move down to the next line."
  "p"
  (lambda (argument)
    (set-dired-point! (line-start (current-point) argument 'BEEP))))

(define-command dired-previous-line
  "Move up to the previous line."
  "p"
  (lambda (argument)
    (set-dired-point! (line-start (current-point) (- argument) 'BEEP))))

(define-command dired-do-deletions
  "Kill all marked files."
  ()
  (lambda ()
    (dired-kill-files)))

(define-command dired-quit
  "Exit Dired, offering to kill any files first."
  ()
  (lambda ()
    (dired-kill-files)
    (kill-buffer-interactive (current-buffer))))

(define-command dired-abort
  "Exit Dired."
  ()
  (lambda ()
    (kill-buffer-interactive (current-buffer))))

(define-command dired-summary
  "Summarize the Dired commands in the typein window."
  ()
  (lambda ()
    (message "d-elete, u-ndelete, x-ecute, q-uit, f-ind, o-ther window")))

(define (set-dired-point! mark)
  (set-current-point!
   (let ((lstart (line-start mark 0)))
     (if (dired-filename-line? lstart)
	 (region-start (os/dired-filename-region lstart))
	 lstart))))

(define (dired-current-pathname)
  (let ((lstart (line-start (current-point) 0)))
    (guarantee-dired-filename-line lstart)
    (dired-pathname lstart)))

(define (guarantee-dired-filename-line lstart)
  (if (not (dired-filename-line? lstart))
      (editor-error "No file on this line")))

(define (dired-filename-line? lstart)
  (and (mark>= lstart (buffer-get (current-buffer) 'DIRED-HEADER-END))
       (not (group-end? lstart))))

(define (dired-pathname lstart)
  (merge-pathnames
   (pathname-directory-path (dired-buffer-directory (current-buffer)))
   (string->pathname (region->string (os/dired-filename-region lstart)))))

(define (dired-mark char n)
  (with-read-only-defeated (current-point)
    (lambda ()
      (dotimes n
	(lambda (i)
	  i				;ignore
	  (let ((lstart (line-start (current-point) 0)))
	    (guarantee-dired-filename-line lstart)
	    (delete-right-char lstart)
	    (insert-chars char 1 lstart)
	    (set-dired-point! (line-start lstart 1))))))))

(define (dired-kill-files)
  (let ((filenames (dired-killable-filenames)))
    (if (not (null? filenames))
	(let ((buffer (temporary-buffer " *Deletions*")))
	  (with-output-to-mark (buffer-point buffer)
	    (lambda ()
	      (write-strings-densely
	       (map (lambda (filename)
		      (pathname-name-string (car filename)))
		    filenames))))
	  (set-buffer-point! buffer (buffer-start buffer))
	  (buffer-not-modified! buffer)
	  (set-buffer-read-only! buffer)
	  (if (with-selected-buffer buffer
		(lambda ()
		  (prompt-for-yes-or-no? "Delete these files")))
	      (for-each dired-kill-file! filenames))
	  (kill-buffer buffer)))))

(define (dired-killable-filenames)
  (define (loop start)
    (let ((next (line-start start 1)))
      (if next
	  (let ((rest (loop next)))
	    (if (char=? #\D (mark-right-char start))
		(cons (cons (dired-pathname start) (mark-permanent! start))
		      rest)
		rest))
	  '())))
  (loop (line-start (buffer-start (current-buffer)) 1)))

(define (dired-kill-file! filename)
  (if (file-exists? (car filename))
      (delete-file (car filename)))
  (with-read-only-defeated (cdr filename)
    (lambda ()
      (delete-string (cdr filename) (mark1+ (line-end (cdr filename) 0))))))

;;;; List Directory

(define-variable list-directory-unpacked
  "If not false, \\[list-directory] puts one file on each line.
Normally it packs many onto a line.
This has no effect if \\[list-directory] is invoked with an argument."
  false)

(define-command list-directory
  "Generate a directory listing."
  "DList directory\nP"
  (lambda (directory argument)
    (temporary-message
     (string-append "Reading directory "
		    directory
		    "..."))
    (let ((pathnames (read&sort-directory directory)))
      (append-message "done")
      (with-output-to-temporary-buffer "*Directory*"
	(lambda ()
	  (write-string "Directory ")
	  (write-string directory)
	  (newline)
	  (newline)
	  (cond (argument
		 (for-each (lambda (pathname)
			     (write-string (os/make-dired-line pathname))
			     (newline))
			   pathnames))
		((ref-variable list-directory-unpacked)
		 (for-each (lambda (pathname)
			     (write-string (pathname-name-string pathname))
			     (newline))
			   pathnames))
		(else
		 (write-strings-densely
		  (map pathname-name-string pathnames)))))))))

(define (read&sort-directory pathname)
  (os/dired-sort-pathnames (directory-read pathname false)))
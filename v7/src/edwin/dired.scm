;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/dired.scm,v 1.97 1989/03/14 08:00:23 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989 Massachusetts Institute of Technology
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

;;;; Directory Editor

(declare (usual-integrations))

(define-command ("Dired")
  "Edit a directory.  You type the directory name."
  (select-buffer (make-dired-buffer "Dired")))

(define-command ("Dired Other Window")
  "Edit a directory in another window.  You type the directory name."
  (select-buffer-other-window (make-dired-buffer "Dired Other Window")))

(define (make-dired-buffer prompt)
  (let ((pathname
	 (prompt-for-pathname prompt
			      (pathname-directory-path
			       (or (buffer-pathname (current-buffer))
				   (working-directory-pathname))))))
    (let ((buffer (get-dired-buffer pathname)))
      (set-buffer-major-mode! buffer dired-mode)
      (set-buffer-truename! buffer pathname)
      (buffer-put! buffer 'REVERT-BUFFER-METHOD revert-dired-buffer)
      (fill-dired-buffer! buffer)
      buffer)))

(define (get-dired-buffer pathname)
  (or (list-search-positive (buffer-list)
	(lambda (buffer)
	  (and (eq? dired-mode (buffer-major-mode buffer))
	       (pathname=? pathname (buffer-truename buffer)))))
      (new-buffer (pathname-name-string pathname))))

(define (revert-dired-buffer argument)
  argument				;ignore
  (fill-dired-buffer! (current-buffer)))

(define (fill-dired-buffer! buffer)
  (set-buffer-writeable! buffer)
  (region-delete! (buffer-region buffer))
  (let ((pathname (buffer-truename buffer)))
    (with-output-to-mark (buffer-point buffer)
      (lambda ()
	(write-string "Directory ")
	(write-string (pathname->string pathname))
	(newline)
	(newline)
	(for-each (lambda (pathname)
		    (write-string (os/make-dired-line pathname))
		    (newline))
		  (directory-read pathname)))))
  (buffer-not-modified! buffer)
  (set-buffer-read-only! buffer)
  (add-buffer-initialization! buffer
    (lambda ()
      (set-current-point! (line-start (buffer-start (current-buffer)) 2)))))

(define-major-mode "Dired" "Fundamental"
  "Major mode for editing a list of files.
Each line describes a file in the directory.
F -- visit the file on the current line.
D -- mark that file to be killed.
U -- remove all marks from the current line.
Rubout -- back up a line and remove marks.
Space -- move down one line.
X -- kill marked files.
Q -- quit, killing marked files.
  This is like \\[^R Dired Execute] followed by \\[Kill Buffer].
C-] -- abort Dired; this is like \\[Kill Buffer] on this buffer."
  (local-set-variable! "Case Fold Search" true)
  (local-set-variable! "Cursor Centering Threshold" 0)
  (local-set-variable! "Cursor Centering Point" 10))

(define-key "Dired" #\F "^R Dired Find File")
(define-key "Dired" #\O "^R Dired Find File Other Window")
(define-key "Dired" #\D "^R Dired Kill")
(define-key "Dired" #\K "^R Dired Kill")
(define-key "Dired" #\C-D "^R Dired Kill")
(define-key "Dired" #\C-K "^R Dired Kill")
(define-key "Dired" #\U "^R Dired Unmark")
(define-key "Dired" #\Rubout "^R Dired Backup Unmark")
(define-key "Dired" #\Space "^R Dired Next")
(define-key "Dired" #\X "^R Dired Execute")
(define-key "Dired" #\Q "^R Dired Quit")
(define-key "Dired" #\C-\] "^R Dired Abort")
(define-key "Dired" #\? "^R Dired Summary")

(define-command ("^R Dired Find File")
  "Read the current file into a buffer."
  (find-file (dired-current-pathname)))

(define-command ("^R Dired Find File Other Window")
  "Read the current file into a buffer in another window."
  (find-file-other-window (dired-current-pathname)))

(define-command ("^R Dired Kill" (argument 1))
  "Mark the current file to be killed."
  (dired-mark #\D argument))

(define-command ("^R Dired Unmark" (argument 1))
  "Cancel the kill requested for the current file."
  (dired-mark #\Space argument))

(define-command ("^R Dired Backup Unmark" (argument 1))
  "Cancel the kill requested for the file on the previous line."
  (set-current-point! (line-start (current-point) -1 'ERROR))
  (dired-mark #\Space argument)
  (set-current-point! (line-start (current-point) -1 'ERROR)))

(define-command ("^R Dired Next" (argument 1))
  "Move down to the next line."
  (set-current-point! (line-start (current-point) argument 'BEEP)))

(define-command ("^R Dired Execute")
  "Kill all marked files."
  (dired-kill-files))

(define-command ("^R Dired Quit")
  "Exit Dired, offering to kill any files first."
  (dired-kill-files)
  (kill-buffer-interactive (current-buffer)))

(define-command ("^R Dired Abort")
  "Exit Dired."
  (kill-buffer-interactive (current-buffer)))

(define-command ("^R Dired Summary")
  "Summarize the Dired commands in the typein window."
  (message "d-elete, u-ndelete, x-ecute, q-uit, f-ind, o-ther window"))

(define (dired-current-pathname)
  (let ((lstart (line-start (current-point) 0)))
    (guarantee-dired-filename-line lstart)
    (dired-pathname lstart)))

(define (guarantee-dired-filename-line lstart)
  (if (not (dired-filename-line? lstart))
      (editor-error "No file on this line")))

(define (dired-filename-line? lstart)
  (let ((lend (line-end lstart 0)))
    (and (not (mark= lstart lend))
	 (not (match-forward "Directory" lstart)))))

(define (dired-pathname lstart)
  (merge-pathnames (pathname-directory-path (buffer-truename (current-buffer)))
		   (string->pathname (dired-filename lstart))))

(define (dired-filename lstart)
  (let ((start (mark+ lstart 2)))
    (char-search-forward #\Space start (line-end start 0))
    (extract-string start (re-match-start 0))))

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
	    (set-current-point! (line-start lstart 1))))))))

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

(define-command ("List Directory" argument)
  "Generate a directory listing."
  (let ((pathname
	 (prompt-for-pathname "List Directory"
			      (pathname-directory-path
			       (or (buffer-pathname (current-buffer))
				   (working-directory-pathname))))))
    (let ((pathnames (directory-read pathname))
	  (directory (pathname->string pathname)))
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
		((ref-variable "List Directory Unpacked")
		 (for-each (lambda (pathname)
			     (write-string (pathname-name-string pathname))
			     (newline))
			   pathnames))
		(else
		 (write-strings-densely
		  (map pathname-name-string pathnames)))))))))
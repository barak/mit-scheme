;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/autosv.scm,v 1.21 1989/04/28 22:47:00 cph Rel $
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
;;; NOTE: Parts of this program (Edwin) were created by translation
;;; from corresponding parts of GNU Emacs.  Users should be aware that
;;; the GNU GENERAL PUBLIC LICENSE may apply to these parts.  A copy
;;; of that license should have been included along with this file.
;;;

;;;; Auto Save

(declare (usual-integrations))

(define-variable auto-save-visited-file-name
  "*True says auto-save a buffer in the file it is visiting, when practical.
Normally auto-save files are written under other names."
  false)

(define-variable auto-save-default
  "*True says by default do auto-saving of every file-visiting buffer."
  true)

(define-variable auto-save-interval
  "*Number of keyboard input characters between auto-saves.
Zero means disable autosaving."
  300)

(define-variable delete-auto-save-files
  "*True means delete a buffer's auto-save file
when the buffer is saved for real."
  true)

(define-command auto-save-mode
  "Toggle auto-saving of contents of current buffer.
With arg, turn auto-saving on if arg is positive, else off."
  "P"
  (lambda (argument)
    (let ((buffer (current-buffer)))
      (if (if argument
	      (positive? argument)
	      (not (buffer-auto-save-pathname buffer)))
	  (begin
	    (enable-buffer-auto-save! buffer)
	    (temporary-message "Auto Save enabled"))
	  (begin
	    (disable-buffer-auto-save! buffer)
	    (temporary-message "Auto Save disabled"))))))

(define (setup-buffer-auto-save! buffer)
  (if (ref-variable auto-save-default)
      (enable-buffer-auto-save! buffer)
      (disable-buffer-auto-save! buffer)))

(define (enable-buffer-auto-save! buffer)
  (set-buffer-auto-save-pathname!
   buffer
   (let ((pathname (buffer-pathname buffer)))
     (if (and pathname
	      (ref-variable auto-save-visited-file-name))
	 pathname
	 (os/auto-save-pathname pathname (buffer-name buffer))))))

(define (disable-buffer-auto-save! buffer)
  (set-buffer-auto-save-pathname! buffer false))

(define *auto-save-keystroke-count*)

(define (do-auto-save)
  (let ((buffers
	 (list-transform-positive (buffer-list)
	   (lambda (buffer)
	     (and (buffer-auto-save-pathname buffer)
		  (buffer-auto-save-modified? buffer)
		  (<= (* 10 (buffer-save-length buffer))
		      (* 13 (buffer-length buffer))))))))
    (if (not (null? buffers))
	(begin (temporary-message "Auto saving...")
	       (for-each auto-save-buffer buffers)
	       (clear-message))))
  (set! *auto-save-keystroke-count* 0))

(define (auto-save-buffer buffer)
  (region->file (buffer-unclipped-region buffer)
		(buffer-auto-save-pathname buffer))
  (set-buffer-save-length! buffer)
  (set-buffer-auto-saved! buffer))

(define (delete-auto-save-file! buffer)
  (if (and (ref-variable delete-auto-save-files)
	   (buffer-auto-save-pathname buffer)
	   (file-exists? (buffer-auto-save-pathname buffer)))
      (delete-file (buffer-auto-save-pathname buffer))))
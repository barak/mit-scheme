;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
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
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Buffer Menu

(declare (usual-integrations))
(using-syntax edwin-syntax-table

(define-variable "Buffer Menu Kill on Quit"
  "If not false, kill the *Buffer-List* buffer when leaving it."
  #!FALSE)

(define buffer-menu-package
  (make-environment

(define-command ("List Buffers" argument)
  "Display a list of names of existing buffers."
  (pop-up-buffer (update-buffer-list) #!FALSE))

(define-command ("Buffer Menu" argument)
  "Display a list of names of existing buffers."
  (pop-up-buffer (update-buffer-list) #!TRUE)
  (message "Commands: d, s, x; 1, 2, m, u, q; rubout; ? for help."))

(define (update-buffer-list)
  (let ((buffer (temporary-buffer "*Buffer-List*")))
    (set-buffer-major-mode! buffer buffer-menu-mode)
    (buffer-put! buffer 'REVERT-BUFFER-METHOD revert-buffer-menu)
    (fill-buffer-menu! buffer)
    buffer))

(define (revert-buffer-menu argument)
  (let ((buffer (current-buffer)))
    (set-buffer-writeable! buffer)
    (region-delete! (buffer-region buffer))
    (fill-buffer-menu! buffer)))

(define (fill-buffer-menu! buffer)
  (with-output-to-mark (buffer-point buffer)
    (lambda ()
      (write-string list-buffers-header)
      (let ((current (current-buffer)))
	(for-each (lambda (buffer)
		    (if (not (minibuffer? buffer))			(begin
			 (write-string
			  (list-buffers-format
			   (if (eq? buffer current) "." " ")
			   (if (buffer-modified? buffer) "*" " ")
			   (if (buffer-writeable? buffer) " " "%")
			   (buffer-name buffer)
			   (write-to-string
			    (group-length (buffer-group buffer)))
			   (mode-name (buffer-major-mode buffer))
			   (let ((truename (buffer-truename buffer)))
			     (if truename (pathname->string truename) ""))))
			 (newline))))
		  (buffer-list)))))
  (set-buffer-point! buffer (line-start (buffer-start buffer) 2))
  (set-buffer-read-only! buffer))

(define-major-mode "Buffer-Menu" "Fundamental"
  "Major mode for editing a list of buffers.
Each line describes a buffer in the editor.
M -- mark buffer to be displayed.
Q -- select buffer of line point is in.
1 -- select that buffer in full-screen window.
2 -- select that buffer in one window,
  together with buffer selected before this one in another window.
F -- select buffer of line point is in,
  leaving *Buffer-List* as the previous buffer.
O -- like F, but select buffer in another window.
~ -- clear modified-flag of that buffer.
S -- mark that buffer to be saved.
D or K or C-D or C-K -- mark that buffer to be killed.
X -- kill or save marked buffers.
U -- remove all kinds of marks from the current line.
Rubout -- move up a line and remove marks.
Space -- move down a line.
C-] -- abort Buffer-Menu edit, killing *Buffer-List*."
  ((mode-initialization fundamental-mode)))

(define-key "Buffer-Menu" #\M "^R Buffer Menu Mark")
(define-key "Buffer-Menu" #\Q "^R Buffer Menu Quit")
(define-key "Buffer-Menu" #\1 "^R Buffer Menu 1 Window")
(define-key "Buffer-Menu" #\2 "^R Buffer Menu 2 Window")
(define-key "Buffer-Menu" #\F "^R Buffer Menu Find")
(define-key "Buffer-Menu" #\O "^R Buffer Menu Find Other Window")
(define-key "Buffer-Menu" #\~ "^R Buffer Menu Not Modified")
(define-key "Buffer-Menu" #\S "^R Buffer Menu Save")
(define-key "Buffer-Menu" #\D "^R Buffer Menu Kill")
(define-key "Buffer-Menu" #\K "^R Buffer Menu Kill")
(define-key "Buffer-Menu" #\C-D "^R Buffer Menu Kill")
(define-key "Buffer-Menu" #\C-K "^R Buffer Menu Kill")
(define-key "Buffer-Menu" #\X "^R Buffer Menu Execute")
(define-key "Buffer-Menu" #\U "^R Buffer Menu Unmark")
(define-key "Buffer-Menu" #\Rubout "^R Buffer Menu Backup Unmark")
(define-key "Buffer-Menu" #\Space "^R Buffer Menu Next")
(define-key "Buffer-Menu" #\C-\] "^R Buffer Menu Abort")
(define-key "Buffer-Menu" #\? "Describe Mode")

(define-command ("^R Buffer Menu Mark" (argument 1))
  "Mark buffer on this line for being displayed by \\[^R Buffer Menu Quit] command."
  (set-multiple-marks! 0 #\> argument))

(define-command ("^R Buffer Menu Quit" argument)
  "Select this line's buffer; also display buffers marked with >.
You can mark buffers with the \\[^R Buffer Menu Mark] command."
  (let ((lstart (current-lstart))
	(window (current-window)))
    (let ((menu (window-buffer window))
	  (buffer (buffer-menu-buffer lstart))
	  (others (map buffer-menu-buffer (find-buffers-marked 0 #\>))))
      (if (and (ref-variable "Preserve Window Arrangement")
	       (null? others))
	  (buffer-menu-select menu buffer #!FALSE)
	  (begin
	   (delete-other-windows window)
	   (buffer-menu-select menu buffer (memq menu others))
	   (let ((height (max (quotient (1+ (window-y-size window))
					(1+ (length others)))
			      (1+ (ref-variable "Window Minimum Height")))))
	     (define (loop window buffers)
	       (let ((new (window-split-vertically! window height)))
		 (if new
		     (begin (set-window-buffer! new (car buffers))
			    (loop new (cdr buffers))))))
	     (loop window others))))))
  (clear-message))

(define-command ("^R Buffer Menu 1 Window" argument)
  "Select this line's buffer, alone, in full screen."
  (let ((window (current-window)))
    (delete-other-windows window)
    (buffer-menu-select (window-buffer window)
			(buffer-menu-buffer (current-lstart))
			#!FALSE))
  (clear-message))

(define-command ("^R Buffer Menu 2 Window" argument)
  "Select this line's buffer, with previous buffer in second window."
  (buffer-menu-select (window-buffer (current-window))
		      (buffer-menu-buffer (current-lstart))
		      #!FALSE)
  (fluid-let (((ref-variable "Pop Up Windows") #!TRUE))
    (pop-up-buffer (previous-buffer)))
  (clear-message))

(define-command ("^R Buffer Menu Find" argument)
  "Select this line's buffer."
  (buffer-menu-find select-buffer))

(define-command ("^R Buffer Menu Find Other Window" argument)
  "Select this line's buffer in another window."
  (buffer-menu-find select-buffer-other-window))

(define (buffer-menu-find select-buffer)
  (let ((buffer (buffer-menu-buffer (current-lstart))))
    (if (not (eq? (current-buffer) buffer))
	(select-buffer buffer)))
  (clear-message))

(define-command ("^R Buffer Menu Not Modified" argument)
  "Mark buffer on this line as unmodified (no changes to save)."
  (buffer-not-modified! (buffer-menu-buffer (current-lstart)))
  (let ((lstart (current-lstart)))
    (if (char=? #\* (buffer-menu-mark lstart 1))
	(set-buffer-menu-mark! lstart 1 #\Space))))

(define-command ("^R Buffer Menu Save" (argument 1))
  "Mark buffer on this line to be saved by X command."
  (set-multiple-marks! 1 #\S argument))

(define-command ("^R Buffer Menu Kill" (argument 1))
  "Mark buffer on this line to be killed by X command."
  (set-multiple-marks! 0 #\K argument))

(define-command ("^R Buffer Menu Execute" argument)
  "Save and/or Kill buffers marked with \\[^R Buffer Menu Save] or \\[^R Buffer Menu Kill]."
  (buffer-menu-save-and-kill!))

(define-command ("^R Buffer Menu Unmark" argument)
  "Remove all marks from this line."
  (let ((lstart (mark-right-inserting (current-lstart))))
    (let ((buffer (buffer-menu-buffer lstart)))
      (set-buffer-menu-mark! lstart 0 #\Space)
      (set-buffer-menu-mark! lstart 1
			     (if (buffer-modified? buffer) #\* #\Space))))
  (set-current-point! (next-lstart)))

(define-command ("^R Buffer Menu Backup Unmark" argument)
  "Remove all marks from the previous line."
  (set-current-point! (previous-lstart))
  (^r-buffer-menu-unmark-command)
  (set-current-point! (previous-lstart)))

(define-command ("^R Buffer Menu Next" (argument 1))
  "Move down to the next line."
  (set-current-point! (line-start (current-point) argument 'BEEP)))

(define-command ("^R Buffer Menu Abort" argument)
  "Abort buffer menu edit."
  (kill-buffer-interactive (current-buffer))
  (clear-message))

(define (buffer-menu-select menu buffer needed?)
  (select-buffer buffer)
  (if (not (or (eq? menu buffer) needed?))
      (if (ref-variable "Buffer Menu Kill on Quit")
	  (kill-buffer-interactive menu)
	  (bury-buffer menu))))

(define (buffer-menu-save-and-kill!)
  (for-each buffer-menu-save! (find-buffers-marked 1 #\S))
  (for-each buffer-menu-kill! (find-buffers-marked 0 #\K)))

(define (buffer-menu-save! lstart)
  (save-file (buffer-menu-buffer lstart))
  (set-buffer-menu-mark! lstart 1 #\Space))

(define (buffer-menu-kill! lstart)
  (define (erase-line)
    (with-read-only-defeated lstart
      (lambda ()
	(delete-string lstart (line-start lstart 1)))))
  (let ((buffer (find-buffer (buffer-menu-buffer-name lstart))))
    (cond ((not buffer) (erase-line))
	  ((not (eq? buffer (current-buffer)))
	   (kill-buffer-interactive buffer)
	   (erase-line)))))

(define (buffer-menu-buffer lstart)
  (let ((name (buffer-menu-buffer-name lstart)))
    (or (find-buffer name)
	(editor-error "No buffer named '" name "'"))))

(define (buffer-menu-buffer-name lstart)
  (guarantee-buffer-line lstart)
  (buffer-line-name lstart))

(define (current-lstart)
  (line-start (current-point) 0))

(define (next-lstart)
  (line-start (current-point) 1))

(define (previous-lstart)
  (line-start (current-point) -1))

(define (set-multiple-marks! column char n)
  (dotimes n
    (lambda (i)
      (set-buffer-menu-mark! (current-lstart) column char)
      (set-current-point! (next-lstart)))))

(define (guarantee-buffer-line lstart)
  (if (not (buffer-line? lstart))
      (editor-error "No buffer on this line")))

(define (buffer-line? lstart)
  (and (mark>= lstart (line-start (group-start lstart) 2))
       (not (mark= lstart (line-end lstart 0)))))

(define (buffer-line-name lstart)
  (let ((start (mark+ lstart 4)))
    (char-search-forward #\Space start (line-end start 0))
    (extract-string start (re-match-start 0))))

(define (buffer-menu-mark lstart column)
  (guarantee-buffer-line lstart)
  (mark-right-char (mark+ lstart column)))

(define (set-buffer-menu-mark! lstart column char)
  (guarantee-buffer-line lstart)
  (let ((m (mark+ lstart column)))
    (with-read-only-defeated m
      (lambda ()
	(delete-right-char m)
	(region-insert-char! m char)))))

(define (list-buffers-format k m r buffer size mode file)
  (let ((buffer (pad-on-right-to buffer 12)))
    (let ((size (pad-on-right-to size
				 (- 5 (max 0 (- (string-length buffer) 12))))))
      (let ((mode (pad-on-right-to mode
				   (- 12 (max 0 (- (+ (string-length buffer)
						      (string-length size))
						   17))))))
	(string-append k m r " " buffer " " size " " mode " " file)))))

(define list-buffers-header
  (string-append
   (list-buffers-format " " "M" "R" "Buffer" "Size" "Mode" "File") "
"
   (list-buffers-format " " "-" "-" "------" "----" "----" "----") "
"))

(define (find-buffers-marked column char)
  (define (loop lstart)
    (let ((next (line-start lstart 1)))
      (cond ((not next) '())
	    ((char=? (mark-right-char (mark+ lstart column)) char)
	     (cons (mark-permanent! lstart) (loop next)))
	    (else (loop next)))))
  (loop (line-start (buffer-start (current-buffer)) 2)))

;;; end BUFFER-MENU-PACKAGE
)))

;;; Edwin Variables:
;;; Scheme Environment: (access buffer-menu-package edwin-package)
;;; Scheme Syntax Table: edwin-syntax-table
;;; Tags Table Pathname: (access edwin-tags-pathname edwin-package)
;;; End:

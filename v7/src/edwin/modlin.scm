;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/modlin.scm,v 1.9 1991/05/10 22:21:18 cph Exp $
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

;;;; Modeline Format
;;; package: (edwin mode-line-format)

(declare (usual-integrations))

(define-variable-per-buffer mode-line-format
  "Template for displaying mode line for current buffer.
Each buffer has its own value of this variable.
Value may be a string, a symbol, or a (possibly improper) list.
For a symbol, its value is used (but it is ignored if #t or #f).
 A string appearing directly as the value of a symbol is processed verbatim
 in that the %-constructs below are not recognized.
For a list whose car is a symbol, the symbol's value is taken,
 and if that is true, the cadr of the list is processed recursively.
 Otherwise, the caddr of the list (if there is one) is processed.
For a list whose car is a string or list, each element is processed
 recursively and the results are effectively concatenated.
For a list whose car is an integer, the cdr of the list is processed
  and padded (if the number is positive) or truncated (if negative)
  to the width specified by that number.
A string is printed verbatim in the mode line except for %-constructs:
  (%-constructs are allowed when the string is the entire mode-line-format
   or when it is found in a cons-cell or a list)
  %b -- print buffer name.      %f -- print visited file name.
  %* -- print *, % or hyphen.   %M -- print major mode name.
  %s -- print process status.   %m -- print minor mode names.
  %p -- print percent of buffer above top of window, or top, bot or all.
  %n -- print Narrow if appropriate.
  %[ -- print one [ for each recursive editing level.  %] similar.
  %% -- print %.   %- -- print infinitely many dashes.
Decimal digits after the % specify field width to which to pad."
  '("" mode-line-modified
       mode-line-buffer-identification
       "   "
       global-mode-string
       "   %[(%M%m%n"
       mode-line-process
       ")%]----"
       (-3 . "%p")
       "-%-"))

(define-variable-per-buffer mode-line-modified
  "Mode-line control for displaying whether current buffer is modified."
  '("--%1*%1*-"))

(define-variable-per-buffer mode-line-buffer-identification
  "Mode-line control for identifying the buffer being displayed.
Its default value is \"Edwin: %17b\".  Major modes that edit things
other than ordinary files may change this (e.g. Info, Dired,...)"
  '("Edwin: %17b"))

(define-variable global-mode-string
  "Extra stuff appearing after buffer-name in standard mode-line-format."
  false)

(define-variable-per-buffer mode-line-process
  "Mode-line control for displaying info on process status.
Normally false in most modes, since there is no process to display."
  false)

(define-variable-per-buffer mode-line-procedure
  "Procedure used to generate the mode-line.
Must accept four arguments: WINDOW STRING START END.
Must generate a modeline string for WINDOW in the given substring.
If #F, the normal method is used."
  false)

(define (modeline-string! window line start end)
  (let ((procedure
	 (variable-local-value (window-buffer window)
			       (ref-variable-object mode-line-procedure))))
    (if procedure
	(procedure window line start end)
	(let ((last
	       (display-mode-element
		(variable-local-value (window-buffer window)
				      (ref-variable-object mode-line-format))
		window line start end end)))
	  (if (fix:< last end)
	      (do ((x last (fix:+ x 1)))
		  ((fix:= x end))
		(string-set! line x #\space)))))))

(define (format-modeline-string window format size)
  (let ((line (string-allocate size)))
    (display-mode-element format window line 0 size size)
    line))

(define (display-mode-element element window line column min-end max-end)
  (cond ((pair? element)
	 (display-mode-pair element window line column min-end max-end))
	((string? element)
	 (display-mode-string element window line column min-end max-end))
	((symbol? element)
	 (let ((value (window-symbol-value window element)))
	   (cond ((string? value)
		  (display-string value line column min-end max-end))
		 ((boolean? value)
		  (display-pad line column min-end))
		 (else
		  (display-mode-element
		   value window line column min-end max-end)))))
	((procedure? element)
	 (display-mode-element (element window)
			       window line column min-end max-end))
	(else
	 (display-string "*invalid*" line column min-end max-end))))

(define (display-mode-pair element window line column min-end max-end)
  (let ((invalid
	 (lambda () (display-string "*invalid*" line column min-end max-end)))
	(finish (lambda (column) (display-pad line column min-end)))
	(key (car element))
	(rest (cdr element)))
    (cond ((symbol? key)
	   (cond ((not (pair? rest))
		  (invalid))
		 ((window-symbol-value window key)
		  (display-mode-element (car rest)
					window line column min-end max-end))
		 ((null? (cdr rest))
		  (finish column))
		 ((pair? (cdr rest))
		  (display-mode-element (cadr rest)
					window line column min-end max-end))
		 (else
		  (invalid))))
	  ((integer? key)
	   (let ((values
		  (lambda (min-end max-end)
		    (display-mode-element rest window line column
					  min-end
					  max-end))))
	     (cond ((negative? key)
		    (values min-end (min max-end (- column key))))
		   ((positive? key)
		    (values (max min-end (min max-end (+ column key)))
			    max-end))
		   (else
		    (values min-end max-end)))))
	  ((or (string? key) (pair? key))
	   (let loop ((element element) (column column))
	     (if (and (pair? element)
		      (< column max-end))
		 (loop (cdr element)
		       (display-mode-element
			(car element)
			window line column column max-end))
		 (finish column))))
	  (else
	   (finish column)))))

(define (display-mode-string element window line column min-end max-end)
  (let ((end (string-length element)))
    (let loop ((start 0) (column column))
      (if (and (< start end)
	       (< column max-end))
	  (let ((percent (substring-find-next-char element start end #\%)))
	    (if (not percent)
		(display-substring element start end
				   line column min-end max-end)
		(let* ((column
			(if (< start percent)
			    (display-substring
			     element start percent line column min-end max-end)
			    column))
		       (values
			(lambda (index width)
			  (if (< index end)
			      (loop (1+ index)
				    (display-string
				     (decode-mode-spec
				      window
				      (string-ref element index)
				      (- max-end column))
				     line column
				     (min max-end (+ width column))
				     max-end))
			      (loop index column)))))
		  (let loop ((index (1+ percent)) (width 0))
		    (if (< index end)
			(let* ((char (string-ref element index))
			       (digit (char->digit char)))
			  (if digit
			      (loop (1+ index) (+ (* 10 width) digit))
			      (values index width)))
			(values index width))))))
	  (display-pad line column min-end)))))

(define (decode-mode-spec window char max-width)
  (let ((buffer (window-buffer window)))
    (case char
      ((#\b)
       (let ((name (buffer-name buffer)))
	 (if (< 2 max-width (string-length name))
	     (let ((result (substring name 0 max-width)))
	       (string-set! result (-1+ max-width) #\\)
	       result)
	     name)))
      ((#\f)
       (let ((pathname (buffer-pathname buffer)))
	 (cond ((not pathname)
		"[none]")
	       ((pathname? pathname)
		(os/truncate-filename-for-modeline (pathname->string pathname)
						   max-width))
	       (else
		""))))
      ((#\M)
       (mode-display-name (buffer-major-mode buffer)))
      ((#\m)
       (let loop ((modes (buffer-minor-modes buffer)))
	 (if (null? modes)
	     (if *defining-keyboard-macro?* " Def" "")
	     (string-append " "
			    (mode-display-name (car modes))
			    (loop (cdr modes))))))
      ((#\n)
       (if (group-clipped? (buffer-group buffer))
	   " Narrow"
	   ""))
      ((#\*)
       (cond ((not (buffer-writeable? buffer)) "%")
	     ((buffer-modified? buffer) "*")
	     (else "-")))
      ((#\s)
       (let ((process (get-buffer-process buffer)))
	 (if process
	     (symbol->string (process-status process))
	     "no process")))
      ((#\p)
       (if (let ((end (buffer-end buffer)))
	     (or (window-mark-visible? window end)
		 (and (line-start? end)
		      (not (group-start? end))
		      (window-mark-visible? window (mark-1+ end)))))
	   (if (window-mark-visible? window (buffer-start buffer))
	       "All"
	       "Bottom")
	   (if (window-mark-visible? window (buffer-start buffer))
	       "Top"
	       (string-append
		(string-pad-left
		 (number->string
		  (min
		   (let ((start (mark-index (buffer-start buffer))))
		     (integer-round
		      (* 100 (- (mark-index (window-start-mark window)) start))
		      (- (mark-index (buffer-end buffer)) start)))
		   99))
		 2)
		"%"))))
      ((#\[ #\])
       (cond ((<= recursive-edit-level 10)
	      (make-string recursive-edit-level char))
	     ((char=? #\[ char)
	      "[[[... ")
	     (else
	      "]]]... ")))
      ((#\%)
       "%")
      ((#\-)
       (make-string max-width #\-))
      (else
       ""))))

(define (display-string string line column min-end max-end)
  (display-substring string 0 (string-length string)
		     line column min-end max-end))

(define (display-substring string start end line column min-end max-end)
  (let ((results substring-image-results))
    (substring-image! string start end
		      line column max-end
		      false 0 results)
    (if (fix:< (vector-ref results 1) min-end)
	(begin
	  (do ((x (vector-ref results 1) (fix:+ x 1)))
	      ((fix:= x min-end))
	    (string-set! line x #\space))
	  min-end)
	(vector-ref results 1))))

(define (display-pad line column min-end)
  (if (< column min-end)
      (begin
	(substring-fill! line column min-end #\space)
	min-end)
      column))

(define (window-symbol-value window symbol)
  (variable-local-value (window-buffer window) (name->variable symbol)))
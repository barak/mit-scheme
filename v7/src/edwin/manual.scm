;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/manual.scm,v 1.4 1991/10/21 12:49:45 cph Exp $
;;;
;;;	Copyright (c) 1991 Massachusetts Institute of Technology
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

;;;; Display Manual Pages

(declare (usual-integrations))

(define-variable manual-entry-reuse-buffer?
  "If true, MANUAL-ENTRY uses buffer *Manual-Entry* for all entries.
Otherwise, a new buffer is created for each topic."
  false
  boolean?)

(define-command manual-entry
  "Display the Unix manual entry for TOPIC.
TOPIC is either the title of the entry, or has the form TITLE(SECTION)
where SECTION is the desired section of the manual, as in `tty(4)'."
  "sManual entry (topic): "
  (lambda (topic #!optional section)
    (if (and (default-object? section)
	     (re-match-string-forward
	      (re-compile-pattern
	       "\\`[ \t]*\\([^( \t]+\\)[ \t]*(\\(.+\\))[ \t]*\\'"
	       false)
	      true
	      false
	      topic))
	(begin
	  (set! section
		(substring topic
			   (re-match-start-index 2)
			   (re-match-end-index 2)))
	  (set! topic
		(substring topic
			   (re-match-start-index 1)
			   (re-match-end-index 1))))
	(set! section false))
    (let ((buffer-name
	   (if (ref-variable manual-entry-reuse-buffer?)
	       "*Manual-Entry*"
	       (string-append
		"*"
		topic
		(if section (string-append "(" section ")") "")
		"-Manual-Entry*"))))
      (let ((buffer (temporary-buffer buffer-name)))
	(message "Invoking man "
		 (if section (string-append section " ") "")
		 topic
		 "...")
	(let ((manual-program
	       (if (file-exists? "/usr/bin/man")
		   "/usr/bin/man"
		   "/usr/ucb/man")))
	  (if section
	      (shell-command
	       (string-append manual-program " " section " " topic)
	       (buffer-point buffer))
	      (shell-command
	       (string-append manual-program " " topic)
	       (buffer-point buffer))))
	(message "Cleaning manual entry for " topic "...")
	(nuke-nroff-bs buffer)
	(buffer-not-modified! buffer)
	(set-buffer-read-only! buffer)
	(set-buffer-point! buffer (buffer-start buffer))
	(pop-up-buffer buffer false)
	(message "Manual page ready")))))

(define manual-vendor-pattern
  (string-append
   "^\\("
   "\\(Printed\\|Sun Release\\) [0-9].*[0-9]"
   "\\|"
   " *Page [0-9]*.*(printed [0-9/]*)"
   "\\|"
   "[ \t]*Hewlett-Packard\\( Company\\|\\)[ \t]*- [0-9]* -.*"
   "\\)$"))

(define (nuke-nroff-bs buffer)
  (let* ((group (buffer-group buffer))
	 (syntax-table (group-syntax-table group))
	 (nuke-regexp
	  (lambda (regexp case-fold-search replacement)
	    (let ((pattern (re-compile-pattern regexp case-fold-search)))
	      (let loop ((index (group-start-index group)))
		(if (re-search-buffer-forward pattern
					      case-fold-search
					      syntax-table
					      group
					      index
					      (group-end-index group))
		    (begin
		      (replace-match replacement false true)
		      (loop (re-match-start-index 0)))))))))
    ;; Nuke underlining and overstriking
    (nuke-regexp "\\(_\b\\)+" false "")
    (nuke-regexp "\\(\b.\\)+" false "")
    ;; Nuke headers: "MORE(1) UNIX Programmer's Manual MORE(1)"
    (nuke-regexp "^ *\\([A-Za-z][-_A-Za-z0-9]*([0-9A-Z]+)\\).*\\1$" false "")
    ;; Nuke footers: "Printed 12/3/85	27 April 1981	1"
    (nuke-regexp manual-vendor-pattern true "")
    (nuke-regexp "^\\([A-Za-z0-9_]+\\|\\)[ \t]*[0-9]+$" false "")
    ;; Crunch blank lines
    (nuke-regexp "\n\n\n\n*" false "\n\n"))
  ;; Nuke blanks lines at start.
  (let ((start (buffer-start buffer)))
    (let ((delete-blanks
	   (lambda ()
	     (if (line-blank? start)
		 (delete-string start
				(let loop ((mark start))
				  (let ((m (line-start mark 1 false)))
				    (cond ((not m) (line-end mark 0))
					  ((not (line-blank? m)) m)
					  (else (loop m))))))))))
      (delete-blanks)
      ;; Also get "Reformatting page" message if any.
      (if (re-match-forward "^Reformatting page"
			    start
			    (buffer-end buffer)
			    false)
	  (begin
	    (delete-string start (line-end start 0))
	    (delete-blanks)))))
  ;; Nuke blanks lines at end.
  (let ((end (buffer-end buffer)))
    (if (line-blank? (line-start end 0))
	(delete-string (let loop ((mark (line-start end 0)))
			 (let ((m (line-start mark -1 false)))
			   (cond ((not m) mark)
				 ((not (line-blank? m)) mark)
				 (else (loop m)))))
		       end))))
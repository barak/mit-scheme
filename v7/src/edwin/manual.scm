;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/manual.scm,v 1.8 1992/04/02 08:14:42 cph Exp $
;;;
;;;	Copyright (c) 1991-92 Massachusetts Institute of Technology
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
	(shell-command false (buffer-point buffer) false false
		       (string-append (if (file-exists? "/usr/bin/man")
					  "/usr/bin/man"
					  "/usr/ucb/man")
				      (if section
					  (string-append " " section)
					  "")
				      " "
				      topic))
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
	 (syntax-table (group-syntax-table group)))
    (let ((nuke-regexp
	   (lambda (regexp case-fold-search)
	     (let ((pattern (re-compile-pattern regexp case-fold-search)))
	       (let loop ((index (group-start-index group)))
		 (if (re-search-buffer-forward pattern
					       case-fold-search
					       syntax-table
					       group
					       index
					       (group-end-index group))
		     (let ((start (re-match-start-index 0)))
		       (group-delete! group start (re-match-end-index 0))
		       (loop start))))))))
      ;; Nuke underlining
      (nuke-regexp "\\(_\b\\)+" false)
      ;; Nuke overstriking
      (nuke-regexp "\\(\b.\\)+" false)
      ;; Nuke headers: "MORE(1) UNIX Programmer's Manual MORE(1)"
      (nuke-regexp "^ *\\([A-Za-z][-_A-Za-z0-9]*([-0-9A-Z]+)\\).*\\1$" false)
      ;; Nuke vendor-specific footers
      (nuke-regexp manual-vendor-pattern true)
      ;; Nuke generic footers
      (nuke-regexp "^[A-Za-z0-9_]*[ \t]*[0-9]+$" false))
    ;; Crunch blank lines
    (let ((pattern (re-compile-pattern "\n\n\n+" false)))
      (let loop ((index (group-start-index group)))
	(if (re-search-buffer-forward pattern
				      false
				      syntax-table
				      group
				      index
				      (group-end-index group))
	    (let ((start (re-match-start-index 0)))
	      (group-delete! group (fix:+ start 2) (re-match-end-index 0))
	      (loop start))))))
  ;; Nuke blanks lines at start.
  (if (re-match-forward "\\([ \t]*\n\\)+"
			(buffer-start buffer)
			(buffer-end buffer)
			false)
      (delete-match))
  ;; Nuke "Reformatting page" message, plus trailing blank lines.
  (if (re-match-forward "Reformatting page.*\n\\([ \t]*\n\\)*"
			(buffer-start buffer)
			(buffer-end buffer)
			false)
      (delete-match))
  ;; Nuke blanks lines at end.
  (let ((end (buffer-end buffer)))
    (if (line-blank? (line-start end 0))
	(delete-string (let loop ((mark (line-start end 0)))
			 (let ((m (line-start mark -1 false)))
			   (cond ((not m) mark)
				 ((not (line-blank? m)) mark)
				 (else (loop m)))))
		       end))))
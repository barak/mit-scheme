;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/manual.scm,v 1.2 1991/09/18 15:59:26 arthur Exp $
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
		(substring topic (match-beginning 2) (match-end 2)))
	  (set! topic
		(substring topic (match-beginning 1) (match-end 1))))
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

(define (nuke-nroff-bs buffer)

  (let ((start (buffer-start buffer))
	(end (buffer-end buffer)))

    ;; Nuke underlining and overstriking (only by the same letter)
    (let ((pattern "\\(_\b\\|\b.\\)"))
      (let loop ((point (re-search-forward pattern start end false)))
	(if point
	    (begin
	      (replace-match "" false true)
	      (loop (re-search-forward
		     pattern (re-match-start 0) end false))))))

    ;; Nuke headers: "MORE(1) UNIX Programmer's Manual MORE(1)"
    (let ((pattern "^ *\\([A-Za-z][-_A-Za-z0-9]*([0-9A-Z]+)\\).*\\1$"))
      (let loop ((point (re-search-forward pattern start end false)))
	(if point
	    (begin
	      (replace-match "" false true)
	      (loop (re-search-forward
		     pattern (re-match-start 0) end false))))))

    ;; Crunch blank lines
    (let ((pattern "\n\n\n\n*"))
      (let loop ((point (re-search-forward pattern start end false)))
	(if point
	    (begin (replace-match "\n\n" false true)
		   (loop (re-search-forward
			  pattern (re-match-start 0) end false))))))

    ;; Nuke blanks lines at start.
    (delete-string start (skip-chars-forward "\n" start end 'limit))))
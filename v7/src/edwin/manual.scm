;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/manual.scm,v 1.10 1992/08/25 19:27:22 cph Exp $
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
	(disable-group-undo! (buffer-group buffer))
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

(define (nuke-nroff-bs buffer)
  (nuke-underlining buffer)
  (nuke-overstriking buffer)
  ;; Nuke headers: "MORE(1) UNIX Programmer's Manual MORE(1)"
  (nuke-regexp buffer
	       "^ *\\([A-Za-z][-_A-Za-z0-9]*([-0-9A-Z]+)\\).*\\1$"
	       false)
  ;; Nuke vendor-specific footers
  (nuke-regexp buffer manual-vendor-pattern true)
  ;; Nuke generic footers
  (nuke-regexp buffer "^[A-Za-z0-9_]*[ \t]*[0-9]+$" false)
  (crunch-blank-lines buffer)
  ;; Nuke blanks lines at start.
  (if (re-match-forward "\\([ \t]*\n\\)+"
			(buffer-start buffer)
			(buffer-end buffer)
			false)
      (delete-match))
  ;; Nuke "Reformatting page" message, plus trailing blank lines.
  (if (re-match-forward "Reformatting \\(page\\|entry\\).*\n\\([ \t]*\n\\)*"
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

(define manual-vendor-pattern
  (string-append
   "^\\("
   "\\(Printed\\|Sun Release\\) [0-9].*[0-9]"
   "\\|"
   " *Page [0-9]*.*(printed [0-9/]*)"
   "\\|"
   "[ \t]*Hewlett-Packard\\( Company\\|\\)[ \t]*- [0-9]* -.*"
   "\\)$"))

(define (nuke-underlining buffer)
  (let ((group (buffer-group buffer)))
    (let loop
	((index
	  (let ((start (group-start-index group)))
	    (if (and (fix:< start (group-end-index group))
		     (char=? #\backspace (group-right-char group start)))
		(fix:+ start 1)
		start))))
      (let ((bs
	     (group-find-next-char group
				   index
				   (group-end-index group)
				   #\backspace)))
	(if bs
	    (if (char=? #\_ (group-left-char group bs))
		(begin
		  (group-delete! group (fix:- bs 1) (fix:+ bs 1))
		  (loop (fix:- bs 1)))
		(loop (fix:+ bs 1))))))))

(define (nuke-overstriking buffer)
  (let ((group (buffer-group buffer)))
    (let loop ((start (group-start-index group)))
      (let ((end (group-end-index group)))
	(let ((bs (group-find-next-char group start end #\backspace)))
	  (if bs
	      (if (fix:< (fix:+ bs 2) end)
		  (let find-end ((index (fix:+ bs 2)))
		    (if (and (fix:< (fix:+ index 2) end)
			     (char=? #\backspace
				     (group-right-char group index)))
			(find-end (fix:+ index 2))
			(begin
			  (group-delete! group bs index)
			  (loop bs)))))))))))

(define (nuke-regexp buffer regexp case-fold-search)
  (let ((group (buffer-group buffer))
	(pattern (re-compile-pattern regexp case-fold-search)))
    (let ((syntax-table (group-syntax-table group)))
      (let loop ((index (group-start-index group)))
	(if (re-search-buffer-forward pattern
				      case-fold-search
				      syntax-table
				      group
				      index
				      (group-end-index group))
	    (let ((start (re-match-start-index 0)))
	      (group-delete! group start (re-match-end-index 0))
	      (loop start)))))))

(define (crunch-blank-lines buffer)
  (let ((group (buffer-group buffer)))
    (let loop ((start (group-start-index group)))
      (let ((end (group-end-index group)))
	(let ((nl (group-find-next-char group start end #\newline)))
	  (if nl
	      (let ((nl+2 (fix:+ nl 2)))
		(if (fix:< nl+2 end)
		    (begin
		      (if (and (char=? #\newline
				       (group-right-char group (fix:+ nl 1)))
			       (char=? #\newline
				       (group-right-char group nl+2)))
			  (let find-end ((index (fix:+ nl 3)))
			    (if (and (fix:< index end)
				     (char=? #\newline
					     (group-right-char group index)))
				(find-end (fix:+ index 1))
				(group-delete! group nl+2 index))))
		      (loop nl+2))))))))))
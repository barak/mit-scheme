;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/tagutl.scm,v 1.41 1991/05/15 01:12:06 cph Exp $
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

;;;; Tags Facility
;;;  From GNU Emacs (thank you RMS)

(declare (usual-integrations))

(define-variable tags-table-pathname
  "Pathname of current tags table."
  false)

(define-command visit-tags-table
  "Tell tags commands to use tag table file FILE.
FILE should be the name of a file created with the `etags' program.
A directory name is ok too; it means file TAGS in that directory."
  "FVisit tags table (default TAGS)"
  (lambda (filename)
    (let ((pathname (->pathname filename)))
      (set-variable!
       tags-table-pathname
       (if (file-directory? pathname)
	   (pathname-new-name (pathname-as-directory pathname) "TAGS")
	   pathname)))))

(define-command find-tag
  "Find tag (in current tag table) whose name contains TAGNAME.
 Selects the buffer that the tag is contained in
and puts point at its definition.
 If TAGNAME is a null string, the expression in the buffer
around or before point is used as the tag name.
 If second arg NEXT is non-false (interactively, with prefix arg),
searches for the next tag in the tag table
that matches the tagname used in the previous find-tag.

See documentation of variable tags-file-name."
  (lambda () (find-tag-arguments "Find tag"))
  (lambda (string previous-tag?)
    (&find-tag-command string previous-tag? find-file)))

(define-command find-tag-other-window
  "Find tag (in current tag table) whose name contains TAGNAME.
 Selects the buffer that the tag is contained in in another window
and puts point at its definition.
 If TAGNAME is a null string, the expression in the buffer
around or before point is used as the tag name.
 If second arg NEXT is non-false (interactively, with prefix arg),
searches for the next tag in the tag table
that matches the tagname used in the previous find-tag.

See documentation of variable tags-file-name."
  (lambda () (find-tag-arguments "Find tag in other window"))
  (lambda (string previous-tag?)
    (&find-tag-command string previous-tag? find-file-other-window)))

;;;; Find Tag

(define previous-find-tag-string
  false)

(define (find-tag-arguments prompt)
  (let ((previous-tag? (command-argument)))
    (if previous-tag?
	(list false true)
	(let ((string (prompt-for-string prompt (find-tag-default))))
	  (set! previous-find-tag-string string)
	  (list string false)))))

(define (&find-tag-command string previous-tag? find-file)
  (let ((buffer (tags-table-buffer)))
    (if previous-tag?
	(find-tag previous-find-tag-string
		  buffer
		  (buffer-point buffer)
		  find-file)
	(find-tag string buffer (buffer-start buffer) find-file)))
  (set! tags-loop-continuation
	(lambda () ((ref-command find-tag) false true)))
  unspecific)

(define (find-tag-default)
  (let ((point (current-point)))
    (let ((end (group-end point)))
      (let ((mark
	     (re-search-backward
	      "\\sw\\|\\s_"
	      (or (re-match-forward "\\(\\sw\\|\\s_\\)*" point end)
		  point)
	      (group-start point))))
	(and mark
	     (let ((mark (mark1+ mark)))
	       (let ((mark*
		      (re-search-forward "\\(\\s'\\)*"
					 (backward-sexp mark 1 'LIMIT)
					 mark)))
		 (and mark*
		      (extract-string mark* mark)))))))))

(define (find-tag string buffer start find-file)
  (let ((end (group-end start)))
    (let ((tag
	   (let loop ((mark start))
	     (let ((mark (search-forward string mark end)))
	       (and mark
		    (or (re-match-forward find-tag-match-regexp mark)
			(loop mark)))))))
      (if (not tag)
	  (editor-failure "No "
			  (if (group-start? start) "" "more ")
			  "entries containing "
			  string)
	  (let ((pathname
		 (merge-pathnames
		  (tag->pathname tag)
		  (pathname-directory-path (buffer-pathname buffer))))
		(regexp
		 (string-append
		  "^"
		  (re-quote-string (extract-string (mark-1+ tag)
						   (line-start tag 0)))))
		(start
		 (-1+
		  (string->number
		   (let ((mark (search-forward "," tag end)))
		     (extract-string mark (line-end mark 0)))))))
	    (set-buffer-point! buffer (line-end tag 0))
	    (find-file pathname)
	    (let* ((buffer (current-buffer))
		   (group (buffer-group buffer)))
	      (buffer-widen! buffer)
	      (push-current-mark! (current-point))
	      (let ((mark
		     (let loop ((offset 1000))
		       (let ((index (- start offset)))
			 (if (positive? index)
			     (or (re-search-forward
				  regexp
				  (make-mark group index)
				  (make-mark group
					     (min (+ start offset)
						  (group-end-index group))))
				 (loop (* 3 offset)))
			     (re-search-forward regexp
						(make-mark group 0)
						(group-end-mark group)))))))
		(if (not mark)
		    (editor-failure regexp
				    " not found in "
				    (pathname-name-string pathname))
		    (set-current-point! (line-start mark 0))))))))))

(define find-tag-match-regexp
  "[^\n\177]*\177")

;;;; Tags Search

(define-command tags-search
  "Search through all files listed in tag table for match for REGEXP.
Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue].

See documentation of variable tags-file-name."
  (re-search-prompt "Tags search")
  (lambda (regexp)
    (set! tags-loop-continuation
	  (lambda ()
	    (let ((mark
		   (let ((point (current-point)))
		     (re-search-forward regexp point (group-end point)))))
	      (if mark
		  (begin
		    (set-current-point! mark)
		    (clear-message))
		  (tags-loop-start)))))
    (set! tags-loop-pathnames (tags-table-pathnames))
    (tags-loop-start)))

(define-command tags-query-replace
  "Query-replace-regexp FROM with TO through all files listed in tag table.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (C-G or ESC), you can resume the query-replace
with the command \\[tags-loop-continue].

See documentation of variable tags-file-name."
  (lambda ()
    (let ((source (prompt-for-string "Tags query replace (regexp)" false)))
      (list source
	    (prompt-for-string
	     (string-append "Tags query replace " source " with")
	     false
	     'NULL-DEFAULT)
	    (command-argument))))
  (lambda (source target delimited)
    (set! tags-loop-continuation
	  (lambda ()
	    (if (not (replace-string source target delimited true true))
		(tags-loop-start))))
    (set! tags-loop-pathnames (tags-table-pathnames))
    (tags-loop-start)))

(define tags-loop-continuation false)
(define tags-loop-pathnames)

(define (tags-loop-start)
  (let ((pathnames tags-loop-pathnames))
    (if (null? pathnames)
	(editor-error "All files processed.")
	(begin
	  (set! tags-loop-pathnames (cdr pathnames))
	  (find-file (car pathnames))
	  (message "Scanning file "
		   (pathname->string (buffer-truename (current-buffer)))
		   "...")
	  (set-current-point! (buffer-start (current-buffer)))
	  (tags-loop-continuation)))))

(define-command tags-loop-continue
  "Continue last \\[tags-search] or \\[tags-query-replace] command."
  ()
  (lambda ()
    (if tags-loop-continuation
	(tags-loop-continuation)
	(editor-error "No tags loop in progress"))))

(define (tags-table-buffer)
  (if (not (ref-variable tags-table-pathname))
      (dispatch-on-command (ref-command-object visit-tags-table)))
  (let ((pathname (ref-variable tags-table-pathname)))
    (let ((buffer (find-file-noselect pathname false)))
      (if (and (not (verify-visited-file-modification-time? buffer))
	       (prompt-for-yes-or-no?
		"Tags file has changed, read new contents"))
	  (revert-buffer buffer true true))
      (if (not (eqv? (extract-right-char (buffer-start buffer)) #\Page))
	  (editor-error "File "
			(pathname->string pathname)
			" not a valid tag table"))
      buffer)))

(define (tag->pathname tag)
  (define (loop mark)
    (let ((file-mark (skip-chars-backward "^,\n" (line-end mark 1))))
      (let ((mark (mark+ (line-start file-mark 1)
			 (with-input-from-mark file-mark read))))
	(if (mark> mark tag)
	    (string->pathname (extract-string (line-start file-mark 0)
					      (mark-1+ file-mark)))
	    (loop mark)))))
  (loop (group-start tag)))

(define (tags-table-pathnames)
  (let ((buffer (tags-table-buffer)))
    (or (buffer-get buffer tags-table-pathnames)
	(let ((pathnames
	       (let ((directory
		      (pathname-directory-path (buffer-truename buffer))))
		 (let loop ((mark (buffer-start buffer)))
		   (let ((file-mark
			  (skip-chars-backward "^,\n" (line-end mark 1))))
		     (let ((mark
			    (mark+ (line-start file-mark 1)
				   (with-input-from-mark file-mark read))))
		       (cons (merge-pathnames
			      (string->pathname
			       (extract-string (line-start file-mark 0)
					       (mark-1+ file-mark)))
			      directory)
			     (if (group-end? mark)
				 '()
				 (loop mark)))))))))
	  (buffer-put! buffer tags-table-pathnames pathnames)
	  pathnames))))
;;; -*-Scheme-*-
;;;
;;;	$Id: tagutl.scm,v 1.55 1996/04/23 23:07:32 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-96 Massachusetts Institute of Technology
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

(define-variable tags-table-pathnames
  "List of pathnames of all of the active tags tables.

See documentation for visit-tags-table and visit-additional-tags-table."
  false)

(define-command visit-tags-table
  "Tell tags commands to use only the tag table file FILE.
FILE should be the name of a file created with the `etags' program.
A directory name is ok too; it means file TAGS in that directory.
To use more than one tag table file at a time,
see \\[visit-additional-tags-table]."
  "FVisit tags table (default TAGS)"
  (lambda (filename)
    (let ((pathname (->pathname filename)))
      (set-variable! tags-table-pathnames (list (expand-pathname pathname))))))

(define-command visit-additional-tags-table
  "Adds another tags table file to the current list of active tags tables."
  "FVisit additional tags table (default TAGS)"
  (lambda (filename)
    (let ((pathname (->pathname filename)))
      (set-variable! tags-table-pathnames
		     (append (ref-variable tags-table-pathnames)
			     (list (expand-pathname pathname)))))))

(define (expand-pathname pathname)
  (if (or (not (pathname-name pathname))
	  (file-directory? pathname))
      (pathname-new-name (pathname-as-directory pathname) "TAGS")
      pathname))

(define-command find-tag
  "Find tag (in current list of tag tables) whose name contains TAGNAME.
 Selects the buffer that the tag is contained in
and puts point at its definition.
 If TAGNAME is a null string, the expression in the buffer
around or before point is used as the tag name.
 If second arg NEXT is non-false (interactively, with prefix arg),
searches for the next tag in the tag table
that matches the tagname used in the previous find-tag.

See documentation of variable tags-table-pathnames."
  (lambda () (find-tag-arguments "Find tag"))
  (lambda (string previous-tag?)
    (&find-tag-command string previous-tag? find-file)))

(define-command find-tag-other-window
  "Like \\[find-tag], but select buffer in another window."
  (lambda () (find-tag-arguments "Find tag in other window"))
  (lambda (string previous-tag?)
    (&find-tag-command string previous-tag? find-file-other-window)))

(define-command find-tag-other-frame
  "Like \\[find-tag], but select buffer in another frame."
  (lambda () (find-tag-arguments "Find tag in other frame"))
  (lambda (string previous-tag?)
    (&find-tag-command string previous-tag? find-file-other-screen)))

;;;; Find Tag

(define find-tag-pathnames-list
  false)

(define (handle-includes! included-pathnames)
  (if included-pathnames
      (set! find-tag-pathnames-list
	    (append (list (car find-tag-pathnames-list))
		    (if included-pathnames
			included-pathnames
			'())			
		    (cdr find-tag-pathnames-list)))))

(define (first-tags-table-buffer)
  (if (not (ref-variable tags-table-pathnames))
      (dispatch-on-command (ref-command-object visit-tags-table)))
  (set! find-tag-pathnames-list (ref-variable tags-table-pathnames))
  (let* ((pathname (car find-tag-pathnames-list))
	 (buffer (verify-tags-table (find-file-noselect pathname false)
				    pathname))
	 (included-pathnames (get-included-pathnames buffer)))
    (handle-includes! included-pathnames)
    buffer))

(define (current-tags-table-buffer)
  (if find-tag-pathnames-list
      (find-file-noselect (car find-tag-pathnames-list) false)
      #f))
  
(define (next-tags-table-buffer)
  (if (and find-tag-pathnames-list
	   (not (null? (cdr find-tag-pathnames-list))))
      (let ((pathname (second find-tag-pathnames-list)))
	(set! find-tag-pathnames-list
	      (cdr find-tag-pathnames-list))
	(let* ((buffer (verify-tags-table (find-file-noselect pathname false)
					 pathname))
	       (included-pathnames (get-included-pathnames buffer)))
	  (handle-includes! included-pathnames)
	  buffer))
      #f))

(define (find-tag-arguments prompt)
  (let ((previous-tag? (command-argument)))
    (list (and (not previous-tag?)
	       (prompt-for-string prompt (find-tag-default)))
	  previous-tag?)))

(define (&find-tag-command string previous-tag? find-file)
  (if previous-tag?
      (let ((buffer (current-tags-table-buffer)))
	(find-tag previous-find-tag-string
		  buffer
		  (buffer-point buffer)
		  find-file))
	(begin
	  (set! previous-find-tag-string string)
	  (let ((buffer (first-tags-table-buffer)))
	    (find-tag string buffer (buffer-start buffer) find-file))))
  (set! tags-loop-continuation
	(lambda ()
	  (&find-tag-command false true find-file)))
  unspecific)

(define previous-find-tag-string
  false)

(define (find-tag-default)
  (let ((end
	 (let ((point (current-point)))
	   (or (re-match-forward "\\(\\sw\\|\\s_\\)+"
				 point
				 (group-end point)
				 false)
	       (let ((mark
		      (re-search-backward "\\sw\\|\\s_"
					  point
					  (group-start point)
					  false)))
		 (and mark
		      (mark1+ mark)))))))
    (and end
	 (extract-string (forward-prefix-chars (backward-sexp end 1 'LIMIT)
					       end)
			 end))))

(define (find-tag string buffer start find-file)
  (let ((end (group-end start)))
    (let ((tag
	   (let loop ((mark start))
	     (let ((mark (search-forward string mark end)))
	       (and mark
		    (or (re-match-forward find-tag-match-regexp mark)
			(loop mark)))))))
      (if (not tag)
	  (let ((next-buffer (next-tags-table-buffer)))
	    (if (not next-buffer)
		(editor-failure "No "
				(if (group-start? start) "" "more ")
				"entries containing "
				string)
		(find-tag string next-buffer (buffer-start next-buffer)
			  find-file)))
	  (let ((pathname
		 (merge-pathnames
		  (tag->pathname tag)
		  (directory-pathname (buffer-pathname buffer))))
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
				    (file-namestring pathname))
		    (set-current-point! (line-start mark 0))))))))))

(define find-tag-match-regexp
  "[^\n\177]*\177")

;;;; Tags Search

(define-command tags-search
  "For every tag table in the current list, search through all files
specified in it for match for REGEXP.  Stops when a match is found.
To continue searching for next match, use command
\\[tags-loop-continue].

See documentation of variable tags-table-pathnames."
  (re-search-prompt "Tags search")
  (lambda (arg regexp)
    ;; arg controls case-sensitivity, just ignore for right now
    arg
    (set! tags-loop-continuation
	  (lambda ()
	    (let ((mark
		   (let ((point (current-point)))
		     (re-search-forward regexp point (group-end point)))))
	      (if mark
		  (begin
		    (set-current-point! mark)
		    (clear-message))
		  (begin
		    (smart-buffer-kill)
		    (tags-loop-start))))))
    (set! tags-loop-pathnames
	  (get-all-pathnames (initial-tags-table-buffers)))
    (tags-loop-start)))

(define-command tags-query-replace
  "Query-replace-regexp FROM with TO through all files listed in all of
the tag tables.  Third arg DELIMITED (prefix arg) means replace only
word-delimited matches.  If you exit (C-g or ESC), you can resume the
query-replace with the command \\[tags-loop-continue].

See documentation of variable tags-file-pathnames."
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
		(begin
		  (smart-buffer-kill)
		  (tags-loop-start)))))
    (set! tags-loop-pathnames
	  (get-all-pathnames (initial-tags-table-buffers)))
    (tags-loop-start)))

(define-command tags-loop-continue
  "Continue last \\[find-tag], \\[tags-search] or \\[tags-query-replace]
command."
  ()
  (lambda ()
    (if (not tags-loop-continuation)
	(editor-error "No tags loop in progress"))
    (tags-loop-continuation)))

(define tags-loop-continuation false)
(define tags-loop-pathnames)
(define tags-loop-current-buffer false)

(define (tags-loop-start)
  (let ((pathnames tags-loop-pathnames))
    (if (null? pathnames)
	(begin
	  (set! tags-loop-continuation false)
	  (editor-error "All files processed.")))
    (set! tags-loop-pathnames (cdr pathnames))
    (let ((buffer
	   (let ((buffer (pathname->buffer (car pathnames))))
	     (if buffer
		 (begin
		   (select-buffer buffer)
		   (buffer-remove! buffer 'TAGS-LOOP-MODIFIED-TICK)
		   buffer)
		 (let ((buffer (find-file-noselect (car pathnames) #t)))
		   (buffer-put! buffer
				'TAGS-LOOP-MODIFIED-TICK
				(buffer-modified-tick buffer))
		   buffer)))))
      (message "Scanning file " (->namestring (buffer-pathname buffer)) "...")
      (select-buffer buffer)
      (set-current-point! (buffer-start buffer))
      (set! tags-loop-current-buffer buffer))
    (tags-loop-continuation)))

(define (smart-buffer-kill)
  (let ((buffer tags-loop-current-buffer))
    (if buffer
	(begin
	  (if (and (ref-variable new-tags-behavior? buffer)
		   (let ((tick (buffer-get buffer 'TAGS-LOOP-MODIFIED-TICK)))
		     (and tick
			  (fix:= tick (buffer-modified-tick buffer)))))
	      (kill-buffer buffer)
	      (buffer-remove! buffer 'TAGS-LOOP-MODIFIED-TICK))
	  (set! tags-loop-current-buffer #f)
	  unspecific))))

(define (buffer-modified-tick buffer)
  (group-modified-tick (buffer-group buffer)))

(define-variable new-tags-behavior?
  "This variable controls the behavior of tags-search and
tags-query-replace.  The new behavior cause any new buffers to be
killed if they are not modified."
  true
  boolean?)

;;;; Tags Tables

(define (tag->pathname tag)
  (define (loop mark)
    (let ((file-mark (skip-chars-backward "^,\n" (line-end mark 1))))
      (let ((mark (mark+ (line-start file-mark 1)
			 (with-input-from-mark file-mark read))))
	(if (mark> mark tag)
	    (->pathname (extract-string (line-start file-mark 0)
					(mark-1+ file-mark)))
	    (loop mark)))))
  (loop (group-start tag)))

(define (verify-tags-table buffer pathname)
  (if (and (not (verify-visited-file-modification-time? buffer))
	   (prompt-for-yes-or-no?
	    "Tags file has changed; read new contents"))
      (revert-buffer buffer true true))
  (if (not (eqv? (extract-right-char (buffer-start buffer)) #\Page))
      (editor-error "File "
		    (->namestring pathname)
		    " not a valid tag table"))
  buffer)

(define (pathnames->tags-table-buffers pathnames)
  (map (lambda (pathname)
	 (verify-tags-table (find-file-noselect pathname false)
			    pathname))
       pathnames))       

(define (initial-tags-table-buffers)
  ;; first make sure there is at least one tags table
  (if (not (ref-variable tags-table-pathnames))
      (dispatch-on-command (ref-command-object visit-tags-table)))
  (pathnames->tags-table-buffers (ref-variable tags-table-pathnames)))

(define (tags-table-pathnames buffers)
  (append-map 
   (lambda (buffer)
     (or (buffer-get buffer 'TAGS-TABLE-PATHNAMES)
	 (let ((directory
		(directory-pathname (buffer-truename buffer)))
	       (finish (lambda (pathnames included-pathnames)
			 (buffer-put! buffer 'TAGS-TABLE-PATHNAMES pathnames)
			 (buffer-put! buffer
				      'TAGS-TABLE-INCLUDED-PATHNAMES
				      included-pathnames)
			 pathnames)))
	   (let loop ((mark (buffer-start buffer))
		      (pathnames '())
		      (included-tables '()))
	     (let ((file-mark
		    (skip-chars-backward "^,\n" (line-end mark 1))))
	       (let ((word (with-input-from-mark file-mark read))
		     (name
		      (merge-pathnames
		       (extract-string (line-start file-mark 0)
				       (mark-1+ file-mark))
		       directory)))
		 (if (number? word)
		     (let ((mark
			    (mark+ (line-start file-mark 1) word)))
		       (if (group-end? mark)
			   (finish (reverse (cons name pathnames))
				   (reverse included-tables))
			   (loop mark
				 (cons name pathnames)
				 included-tables)))
		     ;; if it is not a number than it must be an include
		     (if (group-end? (line-end file-mark 1))
			 (finish (reverse pathnames)
				 (reverse (cons name included-tables)))
			 (loop (line-start mark 2)
			       pathnames
			       (cons name included-tables))))))))))
   buffers))

(define (get-included-pathnames buffer)
  (tags-table-pathnames (list buffer))
  (buffer-get buffer 'TAGS-TABLE-INCLUDED-PATHNAMES))

(define (get-all-pathnames buffers)
  (let ((pathnames (tags-table-pathnames buffers))
	(includes (append-map get-included-pathnames buffers)))
    (if (null? includes)
	pathnames
	(append pathnames
		(get-all-pathnames
		 (pathnames->tags-table-buffers includes))))))
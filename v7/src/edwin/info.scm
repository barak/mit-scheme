;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/info.scm,v 1.96 1991/02/15 18:13:44 cph Exp $
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

;;;; Info Mode
;;; Shamelessly copied from GNU Emacs.

(declare (usual-integrations))

(define-command info
  "Create a buffer for Info, the documentation browser program."
  ()
  (lambda ()
    (let ((buffer (find-buffer info-buffer-name)))
      (if buffer
	  (select-buffer buffer)
	  ((ref-command info-directory))))))

(define info-buffer-name "*info*")

(define-variable info-history
  "List of info nodes user has visited.
Each element of list is a vector #(FILENAME NODENAME BUFFERPOS)."
  '())

(define-variable info-current-file
  "Info file that Info is now looking at, or #F."
  false)

(define-variable info-current-subfile
  "Info subfile that is actually in the *info* buffer now,
or #F if current info file is not split into subfiles."
  false)

(define-variable info-current-node
  "Name of node that Info is now looking at, or #F."
  false)

(define-variable info-tag-table-start
  "Mark pointing at beginning of current Info file's tag table,
or #F if file has no tag table.")

(define-variable info-tag-table-end
  "Mark pointing at end of current Info file's tag table,
or #F if file has no tag table.")

(define-major-mode info fundamental "Info"
  "Info mode provides commands for browsing through the Info documentation tree.
Documentation in Info is divided into \"nodes\", each of which
discusses one topic and contains references to other nodes
which discuss related topics.  Info has commands to follow
the references and show you other nodes.

h	Invoke the Info tutorial.

Selecting other nodes:
n	Move to the \"next\" node of this node.
p	Move to the \"previous\" node of this node.
u	Move \"up\" from this node.
m	Pick menu item specified by name (or abbreviation).
	Picking a menu item causes another node to be selected.
f	Follow a cross reference.  Reads name of reference.
l	Move to the last node you were at.

Moving within a node:
Space	scroll forward a page.
Rubout	scroll backward.
b	Go to beginning of node.

Advanced commands:
q	Quit Info: reselect previously selected buffer.
e	Edit contents of selected node.
1	Pick first item in node's menu.
2, 3, 4, 5   Pick second ... fifth item in node's menu.
g	Move to node specified by name.
	You may include a filename as well, as (FILENAME)NODENAME.
s	Search through this Info file for specified regexp,
	and select the node in which the next occurrence is found."
  (local-set-variable! syntax-table text-mode:syntax-table)
  (local-set-variable! case-fold-search true)
  (local-set-variable! info-history (ref-variable info-history))
  (local-set-variable! info-current-file false)
  (local-set-variable! info-current-subfile false)
  (local-set-variable! info-current-node false)
  (local-set-variable! info-tag-table-start false)
  (local-set-variable! info-tag-table-end false)
  (info-set-mode-line!))

(define-key 'info #\space 'scroll-up)
(define-key 'info #\. 'beginning-of-buffer)
(define-key 'info #\1 'info-first-menu-item)
(define-key 'info #\2 'info-second-menu-item)
(define-key 'info #\3 'info-third-menu-item)
(define-key 'info #\4 'info-fourth-menu-item)
(define-key 'info #\5 'info-fifth-menu-item)
(define-key 'info #\? 'info-summary)
(define-key 'info #\b 'beginning-of-buffer)
(define-key 'info #\d 'info-directory)
(define-key 'info #\e 'info-edit)
(define-key 'info #\f 'info-follow-reference)
(define-key 'info #\g 'info-goto-node)
(define-key 'info #\h 'info-help)
(define-key 'info #\l 'info-last)
(define-key 'info #\m 'info-menu)
(define-key 'info #\n 'info-next)
(define-key 'info #\p 'info-previous)
(define-key 'info #\q 'info-exit)
(define-key 'info #\s 'info-search)
(define-key 'info #\u 'info-up)
(define-key 'info #\rubout 'scroll-down)

(define (info-set-mode-line!)
  (local-set-variable! mode-line-buffer-identification
		       (string-append
			"Info:  ("
			(let ((pathname (ref-variable info-current-file)))
			  (if pathname
			      (pathname-name-string pathname)
			      ""))
			")"
			(or (ref-variable info-current-node) ""))))

;;;; Motion

(define-command info-directory
  "Go to the Info directory node."
  ()
  (lambda ()
    (find-node "dir" "Top")))

(define-command info-help
  "Enter the Info tutorial."
  ()
  (lambda ()
    (find-node "info"
	       (if (< (window-y-size (current-window)) 23)
		   "Help-Small-Screen"
		   "Help"))))

(define-command info-next
  "Go to the next node of this node."
  ()
  (lambda ()
    (follow-pointer extract-node-next "Next")))

(define-command info-previous
  "Go to the previous node of this node."
  ()
  (lambda ()
    (follow-pointer extract-node-previous "Previous")))

(define-command info-up
  "Go to the superior node of this node."
  ()
  (lambda ()
    (follow-pointer extract-node-up "Up")))

(define (follow-pointer extractor name)
  (goto-node
   (or (extractor (buffer-start (current-buffer)))
       (editor-error "Node has no " name))))

(define-command info-goto-node
  "Go to Info node of given name.  Give just NODENAME or (FILENAME)NODENAME."
  "sGoto node"
  (lambda (name)
    (goto-node name)))

(define-command info-last
  "Go back to the last node visited."
  ()
  (lambda ()
    (if (null? (ref-variable info-history))
	(editor-error "This is the first Info node you have looked at"))
    (let ((entry (car (ref-variable info-history))))
      (set-variable! info-history (cdr (ref-variable info-history)))
      (find-node (vector-ref entry 0) (vector-ref entry 1))
      (set-variable! info-history (cdr (ref-variable info-history)))
      (set-current-point!
       (mark+ (region-start (buffer-unclipped-region (current-buffer)))
	      (vector-ref entry 2))))))

;;;; Miscellaneous

(define-command info-exit
  "Exit Info by selecting some other buffer."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (select-buffer (previous-buffer))
      (bury-buffer buffer))))

(define-command info-summary
  "Display a brief summary of all Info commands."
  ()
  (lambda ()
    (let ((buffer (temporary-buffer "*Help*")))
      (with-output-to-mark (buffer-point buffer)
	(lambda ()
	  (write-description (mode-description (current-major-mode)))))
      (set-buffer-point! buffer (buffer-start buffer))
      (buffer-not-modified! buffer)
      (with-selected-buffer buffer
	(lambda ()
	  (let loop ()
	    (update-selected-screen! false)
	    (let ((end-visible?
		   (window-mark-visible? (current-window)
					 (buffer-end buffer))))
	      (message (if end-visible?
			   "Type Space to return to Info"
			   "Type Space to see more"))
	      (let ((char (keyboard-peek-char)))
		(if (char=? char #\Space)
		    (begin
		      (keyboard-read-char)
		      (if (not end-visible?)
			  (begin
			    ((ref-command scroll-up) false)
			    (loop))))))))
	  (clear-message))))))

(define-command info-edit
  "Edit the contents of this Info node.
Allowed only if the variable Info Enable Edit is not false."
  ()
  (lambda ()
    (if (not (ref-variable info-enable-edit))
	(editor-error "Editing Info nodes is not enabled"))
    (set-buffer-writeable! (current-buffer))
    (set-current-major-mode! (ref-mode-object info-edit))
    (message "Editing: Type C-c C-c to return to Info")))

(define-major-mode info-edit text "Info-Edit"
  "Major mode for editing the contents of an Info node.
The editing commands are the same as in Text mode,
except for \\[info-cease-edit] to return to Info."
  (local-set-variable! page-delimiter
		       (string-append "^\f\\|"
				      (ref-variable page-delimiter))))

(define-prefix-key 'info-edit #\c-c 'prefix-char)
(define-key 'info-edit '(#\c-c #\c-c) 'info-cease-edit)

(define-command info-cease-edit
  "Finish editing Info node; switch back to Info proper."
  ()
  (lambda ()
    (save-buffer-changes (current-buffer))
    (set-current-major-mode! (ref-mode-object info))
    (set-buffer-read-only! (current-buffer))
    (clear-message)))

;;;; Search

(define-command info-search
  "Search for regexp, starting from point, and select node it's found in."
  (lambda ()
    (let ((regexp
	   (prompt-for-string "Search (regexp)"
			      (ref-variable info-previous-search))))
      (set-variable! info-previous-search regexp)
      (list regexp)))
  (lambda (regexp)
    (let ((regexp
	   (if (string-null? regexp)
	       (ref-variable info-previous-search)
	       (begin
		 (set-variable! info-previous-search regexp)
		 regexp)))
	  (buffer (current-buffer)))
      (let ((original-file (ref-variable info-current-file))
	    (original-subfile (ref-variable info-current-subfile))
	    (original-node (ref-variable info-current-node))
	    (original-point (mark-index (current-point)))
	    (perform-search
	     (lambda (start)
	       (without-group-clipped! (buffer-group buffer)
		 (lambda ()
		   (re-search-forward regexp start)))))
	    (win
	     (lambda (mark)
	       (buffer-widen! buffer)
	       (select-buffer buffer)
	       (select-node mark))))
	(let ((mark (perform-search (current-point))))
	  (if mark
	      (win mark)
	      (if (not original-subfile)
		  (editor-error)
		  (let loop
		      ((subfiles
			(let ((subfile (ref-variable info-current-subfile)))
			  (let loop ((subfiles (subfile-list)))
			    (if (pathname=? (subfile-pathname (car subfiles))
					    subfile)
				(cdr subfiles)
				(loop (cdr subfiles)))))))
		    (if (null? subfiles)
			(begin
			  (clear-message)
			  (set-current-subfile! original-subfile)
			  (select-node
			   (mark+ (buffer-start buffer) original-point))
			  (editor-error))
			(begin
			  (let ((pathname (subfile-pathname (car subfiles))))
			    (message "Searching subfile "
				     (pathname-name-string pathname)
				     "...")
			    (set-current-subfile! pathname))
			  (let ((mark (perform-search (buffer-start buffer))))
			    (if mark
				(begin
				  (clear-message)
				  (win mark))
				(loop (cdr subfiles))))))))))
	(if (and original-file
		 (not (and (pathname=? original-file
				       (ref-variable info-current-file))
			   (string-ci=? original-node
					(ref-variable info-current-node)))))
	    (record-node original-file original-node original-point))))))

;;;; Menus

(define-command info-menu
  "Go to node for menu item of given name."
  ()
  (lambda ()
    (let ((menu (find-menu)))
      (if (not menu)
	  (editor-error "No menu in this node")
	  (goto-node (prompt-for-alist-value "Menu item"
					     (collect-menu-items menu)))))))

(define (nth-menu-item n)
  (lambda ()
    (let loop
	((mark
	  (next-menu-item
	   (or (find-menu) (editor-error "No menu in this node"))))
	 (n n))
      (cond ((not mark) (editor-error "Too few items in menu"))
	    ((zero? n) (goto-node (menu-item-name mark)))
	    (else (loop (next-menu-item mark) (-1+ n)))))))

(define-command info-first-menu-item
  "Go to the node of the first menu item."
  ()
  (nth-menu-item 0))

(define-command info-second-menu-item
  "Go to the node of the second menu item."
  ()
  (nth-menu-item 1))

(define-command info-third-menu-item
  "Go to the node of the third menu item."
  ()
  (nth-menu-item 2))

(define-command info-fourth-menu-item
  "Go to the node of the fourth menu item."
  ()
  (nth-menu-item 3))

(define-command info-fifth-menu-item
  "Go to the node of the fifth menu item."
  ()
  (nth-menu-item 4))

(define (find-menu)
  (search-forward "\n* menu:" (buffer-start (current-buffer))))

(define (collect-menu-items mark)
  (let ((item (next-menu-item mark)))
    (if (not item)
	'()
	(cons (cons (menu-item-keyword item)
		    (menu-item-name item))
	      (collect-menu-items item)))))

(define (next-menu-item mark)
  (re-search-forward "\n\\*[ \t]+" (line-end mark 0)))

(define (menu-item-keyword item)
  (let ((end (char-search-forward #\: item (line-end item 0))))
    (if end
	(extract-string item (re-match-start 0))
	(error "Menu item missing colon"))))

(define (menu-item-name item)
  (let ((colon (char-search-forward #\: item (line-end item 0))))
    (cond ((not colon) (error "Menu item missing colon"))
	  ((match-forward "::" (re-match-start 0))
	   (extract-string item (re-match-start 0)))
	  (else
	   (%menu-item-name (horizontal-space-end colon))))))

(define (%menu-item-name start)
  (if (line-end? start)
      (error "Menu item missing node name")
      (extract-string start
		      (let ((end (line-end start 0)))
			(if (re-search-forward "[.,\t]" start end)
			    (re-match-start 0)
			    end)))))

;;;; Cross References

(define-command info-follow-reference
  "Follow cross reference, given name, to the node it refers to.
The name may be an abbreviation of the reference name."
  ()
  (lambda ()
    (let ((items (collect-cref-items (buffer-start (current-buffer)))))
      (if (null? items)
	  (editor-error "No cross references in this node")
	  (goto-node
	   (prompt-for-alist-value "Follow reference named" items))))))

(define (collect-cref-items mark)
  (let ((item (next-cref-item mark)))
    (if (not item)
	'()
	(cons (cons (cref-item-keyword item)
		    (cref-item-name item))
	      (collect-cref-items item)))))

(define (next-cref-item start)
  (re-search-forward "\\*Note[ \t\n]*" start))

(define (cref-item-keyword item)
  (let ((colon (char-search-forward #\: item)))
    (if colon
	(%cref-item-keyword item (re-match-start 0))
	(error "Cross reference missing colon"))))

(define (%cref-item-keyword item colon)
  (let ((string (extract-string item colon)))
    (string-replace! string #\newline #\Space)
    (string-trim string)))

(define (cref-item-name item)
  (let ((colon (char-search-forward #\: item)))
    (cond ((not colon) (error "Cross reference missing colon"))
	  ((match-forward "::" (re-match-start 0))
	   (%cref-item-keyword item (re-match-start 0)))
	  (else
	   (%menu-item-name (cref-item-space-end colon))))))

(define (cref-item-space-end mark)
  (skip-chars-forward " \t\n" mark))

;;;; Validation

(define-command info-validate
  "Check that every node pointer points to an existing node."
  ()
  (lambda ()
    (let ((nodes (current-nodes-list))
	  (losers '()))
      (define (validate this-name type node-name)
	(and node-name
	     (parse-node-name node-name
	       (lambda (filename nodename)
		 (and (not filename)
		      (let ((entry (node-assoc nodename nodes)))
			(if (not entry)
			    (set! losers
				  (cons (vector this-name type node-name)
					losers)))
			entry))))))
      (for-each (lambda (entry)
		  (let ((name (car entry))
			(node (region-start (cadr entry))))
		    (define (validate-extract type extractor)
		      (validate name type (extractor node)))

		    (define ((validate-item prefix) item)
		      (validate name
				(string-append prefix " " (car item))
				(cdr item)))

		    (with-region-clipped! (cadr entry)
		      (lambda ()
			(let ((entry* (validate-extract "Next"
							extract-node-next)))
			  (if (and entry*
				   (or (not (caddr entry*))
				       (not
					(string-ci=? (caddr entry*) name))))
			      (set! losers
				    (cons (vector name
						  "Previous-pointer in Next"
						  (car entry*))
					  losers))))
			(validate-extract "Previous" extract-node-previous)
			(validate-extract "Up" extract-node-up)
			(let ((menu (find-menu)))
			  (if menu
			      (for-each (validate-item "Menu item")
					(collect-menu-items menu))))
			(for-each (validate-item "Reference")
				  (collect-cref-items node))))))
		nodes)
      (report-losers losers))))

(define (report-losers losers)
  (if (null? losers)
      (message "File appears valid")
      (with-output-to-temporary-buffer " *problems in info file*"
	(lambda ()
	  (for-each (lambda (loser)
		      (write-string
		       (string-append "In node " (vector-ref loser 0)
				      ", invalid " (vector-ref loser 1)
				      ": " (vector-ref loser 2)))
		      (newline))
		    losers)))))

(define (current-nodes-list)
  (let ((buffer (current-buffer)))
    (without-group-clipped! (buffer-group buffer)
      (lambda ()
	(collect-nodes (buffer-start buffer))))))

(define (collect-nodes mark)
  (let ((node (next-node mark (group-end mark))))
    (if (not node)
	'()
	(let ((name (extract-node-name node)))
	  (if name
	      (cons (list name (node-region node) (extract-node-previous node))
		    (collect-nodes node))
	      (collect-nodes node))))))

(define node-assoc
  (association-procedure string-ci=? car))

;;;; Node Parsing

(define (goto-node name)
  (parse-node-name name find-node))

(define (find-node filename nodename)
  (let ((pathname
	 (and filename
	      (let ((pathname
		     (let ((pathname (->pathname filename)))
		       (merge-pathnames
			pathname
			;; Use Info's default directory,
			;; unless filename is explicitly self-relative.
			(if (let ((directory (pathname-directory pathname)))
			      (and (pair? directory)
				   (eq? (car directory) 'SELF)))
			    (pathname-directory-path
			     (current-default-pathname))
			    (let ((info-directory
				   (ref-variable info-directory)))
			      (if info-directory
				  (->pathname info-directory)
				  (edwin-info-directory))))))))
		(if (file-exists? pathname)
		    pathname
		    (let ((pathname*
			   (pathname-new-name
			    pathname
			    (string-downcase (pathname-name pathname)))))
		      (if (file-exists? pathname*)
			  pathname*
			  (editor-error "Info file does not exist: "
					pathname))))))))
    (let ((buffer (find-or-create-buffer info-buffer-name)))
      (select-buffer buffer)
      (if (ref-variable info-current-file)
	  (record-node (ref-variable info-current-file)
		       (ref-variable info-current-node)
		       (mark-index (current-point))))
      ;; Switch files if necessary.
      (if (and pathname
	       (let ((pathname* (ref-variable info-current-file)))
		 (not (and pathname* (pathname=? pathname pathname*)))))
	  (begin
	    (read-buffer buffer pathname)
	    (if (not (eq? (buffer-major-mode buffer) (ref-mode-object info)))
		(set-buffer-major-mode! buffer (ref-mode-object info)))
	    (find-tag-table buffer)
	    (set-variable! info-current-file pathname)
	    (set-variable! info-current-subfile false))
	  (begin
	    (if (not (eq? (buffer-major-mode buffer) (ref-mode-object info)))
		(set-buffer-major-mode! buffer (ref-mode-object info)))
	    (group-un-clip! (buffer-group buffer))))
      (set-buffer-read-only! buffer)
      (if (string=? nodename "*")
	  (begin
	    (set-variable! info-current-subfile false)
	    (set-variable! info-current-node nodename)
	    (info-set-mode-line!))
	  (select-node
	   (let ((end (buffer-end buffer)))
	     (let loop ((start (node-search-start nodename)))
	       (let ((node (next-node start end)))
		 (if (not node)
		     (editor-error "No such node: " nodename))
		 (if (let ((name (extract-node-name node)))
		       (and name
			    (string-ci=? nodename name)))
		     node
		     (loop node))))))))))

(define (parse-node-name name receiver)
  (let ((name (string-trim name)))
    (if (char=? (string-ref name 0) #\()
	(let ((index (string-find-next-char name #\))))
	  (if index
	      (let ((filename (string-trim (substring name 1 index)))
		    (nodename (string-trim (substring name (1+ index)
						      (string-length name)))))
		(receiver filename
			  (if (string-null? nodename) "Top" nodename)))
	      (error "PARSE-NODE-NAME: Missing close paren" name)))
	(receiver false (if (string-null? name) "Top" name)))))

(define (select-node point)
  (let ((node (node-start point (group-start point))))
    (set-variable! info-current-node (extract-node-name node))
    (info-set-mode-line!)
    ;; **** need to add active node hacking here ****
    (region-clip! (node-region node))
    (set-current-point! point)))

(define (record-node file node point)
  (set-variable! info-history
		 (cons (vector file node point)
		       (ref-variable info-history))))

(define (node-start start end)
  (line-start (search-backward "\n" start end 'ERROR) 2 'ERROR))

(define (node-region node)
  (make-region node (node-end node)))

(define (node-end node)
  (let ((end (group-end node)))
    (let loop ((start node))
      (let ((mark (re-search-forward "[\f]" start)))
	(cond ((not mark) end)
	      ((char=? (extract-left-char (re-match-start 0)) #\newline)
	       (mark-1+ (re-match-start 0)))
	      (else (loop mark)))))))

(define (next-node start end)
  (let ((mark (search-forward "\n" start end)))
      (and mark
	   (line-start mark 1))))

(define ((field-value-extractor field) node)
  (let ((end (line-end node 0)))
    (let ((mark (re-search-forward field node end)))
      (and mark
	   (string-trim
	    (extract-string mark
			    (skip-chars-forward "^,\t" mark end)))))))

(define extract-node-name
  (field-value-extractor "Node:"))

(define extract-node-up
  (field-value-extractor "Up:"))

(define extract-node-previous
  (field-value-extractor "Prev\\(ious\\|\\):"))

(define extract-node-next
  (field-value-extractor "Next:"))

;;;; Tag Tables

(define-command info-tagify
  "Create or update tag table of current info file."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (without-group-clipped! (buffer-group buffer)
	(lambda ()
	  (with-read-only-defeated (buffer-end buffer)
	    (lambda ()
	      ;; Flush old tag table if present.
	      (find-tag-table buffer)
	      (if (ref-variable info-tag-table-start)
		  (delete-string (mark- (ref-variable info-tag-table-start)
					(string-length tag-table-start-string))
				 (mark+ (ref-variable info-tag-table-end)
					(string-length tag-table-end-string))))
	      ;; Then write new table.
	      (let ((entries (collect-tag-entries (buffer-start buffer))))
		(with-output-to-mark (buffer-end buffer)
		  (lambda ()
		    (write-string tag-table-start-string)
		    (for-each (lambda (entry)
				(write-string (cdr entry))
				(write-char #\Rubout)
				(write (mark-index (car entry)))
				(newline))
			      entries)
		    (write-string tag-table-end-string))))))
	  ;; Finally, reset the tag table marks.
	  (find-tag-table buffer))))))

(define (collect-tag-entries mark)
  (let ((node (next-node mark (group-end mark))))
    (if (not node)
	'()
	(let ((entry (extract-tag-entry node)))
	  (if entry
	      (cons (cons node entry)
		    (collect-tag-entries node))
	      (collect-tag-entries node))))))

(define (extract-tag-entry node)
  (let ((end (line-end node 0)))
    (let ((mark (search-forward "Node:" node end)))
      (and mark
	   (string-trim
	    (extract-string node
			    (skip-chars-forward "^,\t" mark end)))))))

(define tag-table-start-string
  "\f\nTag table:\n")

(define tag-table-end-string
  "\nEnd tag table\n")

(define (find-tag-table buffer)
  (let* ((end (buffer-end buffer))
	 (mark (line-start end -8))
	 (tag-table-end
	  (and mark
	       (search-forward tag-table-end-string mark)
	       (re-match-start 0)))
	 (tag-table-start
	  (and tag-table-end
	       (search-backward tag-table-start-string
				tag-table-end)
	       (re-match-end 0))))
    (if (and tag-table-end (not tag-table-start))
	(begin
	  (message "Ill-formed tag table, ignoring")
	  (editor-beep)))
    ;; If this is an indirect file, read the top node into another
    ;; buffer and set the marks to point at it.
    (if (and tag-table-start
	     (match-forward "(Indirect)\n" tag-table-start))
	(let* ((buffer* (find-or-create-buffer " *info tag table*"))
	       (group (buffer-group buffer*)))
	  (insert-string (extract-string (buffer-start buffer)
					 (buffer-end buffer))
			 (buffer-start buffer*))
	  (set-variable! info-tag-table-start
			 (make-mark group (mark-index tag-table-start)))
	  (set-variable! info-tag-table-end
			 (make-mark group (mark-index tag-table-end))))
	(begin
	  (set-variable! info-tag-table-start tag-table-start)
	  (set-variable! info-tag-table-end
			 (and tag-table-start tag-table-end))))))

(define (node-search-start nodename)
  (let ((buffer (current-buffer)))
    (if (ref-variable info-tag-table-start)
	(let ((mark
	       (or (search-forward (string-append "Node: " nodename "\177")
				   (ref-variable info-tag-table-start)
				   (ref-variable info-tag-table-end))
		   (editor-error "No such node: " nodename))))
	  ;; Force order of events, since read-subfile has side-effect.
	  (let ((index
		 (let ((start (read-index-from-mark mark)))
		   (if (mark~ (ref-variable info-tag-table-start)
			      (buffer-start buffer))
		       start
		       (read-subfile start)))))
	    (mark+ (buffer-start buffer) (max 0 (- index 1000)))))
	(buffer-start buffer))))

(define (read-subfile index)
  (let loop
      ((subfiles
	(let ((subfiles (subfile-list)))
	  (if (> (cdar subfiles) index)
	      (editor-error "Illegal indirect index" index))
	  subfiles)))
    (if (or (null? (cdr subfiles))
	    (> (cdadr subfiles) index))
	(begin
	  (set-current-subfile! (subfile-pathname (car subfiles)))
	  (+ (- index (subfile-index (car subfiles)))
	     (mark-index
	      (let ((buffer (current-buffer)))
		(search-forward "\n"
				(buffer-start buffer)
				(buffer-end buffer)
				'ERROR)))))
	(loop (cdr subfiles)))))

(define (set-current-subfile! pathname)
  (let ((subfile (ref-variable info-current-subfile)))
    (if (or (not subfile)
	    (not (pathname=? subfile pathname)))
	(begin
	  (read-buffer (current-buffer) pathname)
	  (set-variable! info-current-subfile pathname)))))

(define-integrable subfile-filename car)
(define-integrable subfile-index cdr)

(define (subfile-pathname subfile)
  (merge-pathnames (->pathname (subfile-filename subfile))
		   (ref-variable info-current-file)))

(define (subfile-list)
  (let ((result
	 (let loop ((start
		     (let ((start (ref-variable info-tag-table-start)))
		       (search-forward "\n\nIndirect:\n"
				       (group-start start)
				       start
				       'ERROR))))
	   (if (match-forward "" start)
	       '()
	       (begin
		 (search-forward ": " start (group-end start) 'ERROR)
		 (let* ((colon (re-match-start 0))
			(index (read-index-from-mark (re-match-end 0))))
		   (cons (cons (extract-string start colon) index)
			 (loop (line-start start 1 'ERROR)))))))))
    (if (null? result)
	(editor-error "Empty indirect file list"))
    result))

(define (read-index-from-mark mark)
  (let ((lose
	 (lambda ()
	   (editor-error "Malformed index in Info file"))))
    (bind-condition-handler (list condition-type:error)
	(lambda (condition)
	  condition
	  (lose))
      (lambda ()
	(let ((index (with-input-from-mark mark read)))
	  (if (and (integer? index)
		   (positive? index))
	      (-1+ index)
	      (lose)))))))
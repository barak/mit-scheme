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

;;;; Info Mode
;;; Shamelessly copied from GNU Emacs.

(declare (usual-integrations))
(using-syntax edwin-syntax-table

(define history '())
(define current-file #!FALSE)
(define current-node #!FALSE)

(define-major-mode "Info" "Fundamental"
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
  (local-set-variable! "Syntax Table" text-mode:syntax-table)
  (local-set-variable! "Case Fold Search" #!TRUE)
  (local-set-variable! "Info Tag Table Start" #!FALSE)
  (local-set-variable! "Info Tag Table End" #!FALSE)
  (buffer-put! (current-buffer) 'MODELINE-STRING info-modeline-string))

(define (info-modeline-string window)
  (string-append "--"
		 (modeline-modified-string window)
		 "-Info: ("
		 (let ((pathname (buffer-pathname (window-buffer window))))
		   (if pathname
		       (pathname-name pathname)
		       ""))
		 ")"
		 (or current-node "")
		 "      "
		 (modeline-mode-string window)
		 "--"
		 (modeline-percentage-string window)))

(define-key "Info" #\Space "^R Next Screen")
(define-key "Info" #\. "^R Goto Beginning")
(define-key "Info" #\1 "^R Info First Menu Item")
(define-key "Info" #\2 "^R Info Second Menu Item")
(define-key "Info" #\3 "^R Info Third Menu Item")
(define-key "Info" #\4 "^R Info Fourth Menu Item")
(define-key "Info" #\5 "^R Info Fifth Menu Item")
(define-key "Info" #\? "^R Info Summary")
(define-key "Info" #\B "^R Goto Beginning")
(define-key "Info" #\D "^R Info Directory")
(define-key "Info" #\E "^R Info Edit")
(define-key "Info" #\F "^R Info Follow Reference")
(define-key "Info" #\G "^R Info Goto Node")
(define-key "Info" #\H "^R Info Help")
(define-key "Info" #\L "^R Info Last")
(define-key "Info" #\M "^R Info Menu")
(define-key "Info" #\N "^R Info Next")
(define-key "Info" #\P "^R Info Previous")
(define-key "Info" #\Q "^R Info Exit")
(define-key "Info" #\S "^R Info Search")
(define-key "Info" #\U "^R Info Up")
(define-key "Info" #\Rubout "^R Previous Screen")

(define-major-mode "Info-Edit" "Text"
  "Major mode for editing the contents of an Info node.
The editing commands are the same as in Text mode,
except for \\[^R Info Cease Edit] to return to Info."
  (local-set-variable! "Page Delimiter"
		       (string-append "^\f\\|"
				      (ref-variable "Page Delimiter")))
  ((mode-initialization text-mode)))

(define-prefix-key "Info-Edit" #\C-C "^R Prefix Character")
(define-key "Info-Edit" '(#\C-C #\C-C) "^R Info Cease Edit")

(define-command ("^R Info Edit" argument)
  "Edit the contents of this Info node.
Allowed only if the variable Info Enable Edit is not false."
  (if (not (ref-variable "Info Enable Edit"))
      (editor-error "Editing Info nodes is not enabled"))
  (set-buffer-writeable! (current-buffer))
  (set-current-major-mode! info-edit-mode)
  (message "Editing: Type C-C C-C to return to Info"))

(define-command ("^R Info Cease Edit" argument)
  "Finish editing Info node; switch back to Info proper."
  (save-buffer-changes (current-buffer))
  (set-current-major-mode! info-mode)
  (set-buffer-read-only! (current-buffer))
  (clear-message))

(define-command ("Info" argument)
  "Create a buffer for Info, the documentation browser program."
  (let ((buffer (find-buffer "*Info*")))
    (if buffer
	(select-buffer buffer)
	(begin (set! current-file #!FALSE)
	       (set! current-node #!FALSE)
	       (set! history '())
	       (^r-info-directory-command)))))

(define-command ("^R Info Directory" argument)
  "Go to the Info directory node."
  (find-node "dir" "Top"))

(define-command ("^R Info Help" argument)
  "Enter the Info tutorial."
  (find-node "info"
	     (if (< (window-y-size (current-window)) 23)
		 "Help-Small-Screen"
		 "Help")))

(define-command ("^R Info Next" argument)
  "Go to the next node of this node."
  (follow-pointer extract-node-next "Next"))

(define-command ("^R Info Previous" argument)
  "Go to the previous node of this node."
  (follow-pointer extract-node-previous "Previous"))

(define-command ("^R Info Up" argument)
  "Go to the superior node of this node."
  (follow-pointer extract-node-up "Up"))

(define (follow-pointer extractor name)
  (goto-node (or (extractor (buffer-start (current-buffer)))
		 (editor-error "Node has no " name))))

(define-command ("^R Info Last" argument)
  "Go back to the last node visited."
  (if (null? history)
      (editor-error "This is the first Info node you have looked at"))
  (let ((entry (car history)))
    (set! history (cdr history))
    (find-node (vector-ref entry 0) (vector-ref entry 1))
    (set! history (cdr history))
    (set-current-point!
     (mark+ (region-start (buffer-unclipped-region (current-buffer)))
	    (vector-ref entry 2)))))

(define-command ("^R Info Exit" argument)
  "Exit Info by selecting some other buffer."
  (let ((buffer (current-buffer)))
    (select-buffer (previous-buffer))
    (bury-buffer buffer)))

(define-command ("^R Info Goto Node" argument)
  "Go to Info node of given name.  Give just NODENAME or (FILENAME)NODENAME."
  (goto-node (prompt-for-string "Goto node" #!FALSE)))

(define-command ("^R Info Search" argument)
  "Search for regexp, starting from point, and select node it's found in."
  (let ((regexp (prompt-for-string "Search (regexp)"
				   (ref-variable "Info Previous Search")))
	(buffer (current-buffer)))
    (set-variable! "Info Previous Search" regexp)
    (let ((mark
	   (without-group-clipped! (buffer-group buffer)
	     (lambda ()
	       (re-search-forward regexp)))))
      (if mark
	  (begin (if (group-end? mark)	;then not in current node
		     (record-current-node))
		 (buffer-widen! buffer)
		 (select-node buffer mark))
	  (editor-failure)))))

(define-command ("^R Info Summary" argument)
  "Display a brief summary of all Info commands."
  (let ((buffer (temporary-buffer "*Help*")))
    (with-output-to-mark (buffer-point buffer)
      (lambda ()
	(write-description (mode-description (current-major-mode)))))
    (set-buffer-point! buffer (buffer-start buffer))
    (buffer-not-modified! buffer)
    (with-selected-buffer buffer
      (lambda ()
	(define (loop)
	  (update-alpha-window! #!FALSE)
	  (let ((end-visible? (window-mark-visible? (current-window)
						    (buffer-end buffer))))
	    (message (if end-visible?
			 "Type Space to return to Info"
			 "Type Space to see more"))
	    (let ((char (%keyboard-peek-char)))
	      (if (char=? char #\Space)
		  (begin (keyboard-read-char)
			 (if (not end-visible?)
			     (begin (^r-next-screen-command)
				    (loop))))))))
	(loop)
	(clear-message)))))

;;;; Menus

(define-command ("^R Info Menu" argument)
  "Go to node for menu item of given name."
  (let ((menu (find-menu)))
    (if (not menu)
	(editor-error "No menu in this node")
	(goto-node (prompt-for-alist-value "Menu item"
					   (collect-menu-items menu))))))

(define-command ("^R Info First Menu Item" argument)
  "Go to the node of the first menu item."
  (nth-menu-item 0))

(define-command ("^R Info Second Menu Item" argument)
  "Go to the node of the second menu item."
  (nth-menu-item 1))

(define-command ("^R Info Third Menu Item" argument)
  "Go to the node of the third menu item."
  (nth-menu-item 2))

(define-command ("^R Info Fourth Menu Item" argument)
  "Go to the node of the fourth menu item."
  (nth-menu-item 3))

(define-command ("^R Info Fifth Menu Item" argument)
  "Go to the node of the fifth menu item."
  (nth-menu-item 4))

(define (nth-menu-item n)
  (define (loop mark n)
    (cond ((not mark) (editor-error "Too few items in menu"))
	  ((zero? n) (goto-node (menu-item-name mark)))
	  (else (loop (next-menu-item mark) (-1+ n)))))
  (loop (next-menu-item (or (find-menu) (editor-error "No menu in this node")))
	n))

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

(define-command ("^R Info Follow Reference" argument)
  "Follow cross reference, given name, to the node it refers to.
The name may be an abbreviation of the reference name."
  (let ((items (collect-cref-items (buffer-start (current-buffer)))))
    (if (null? items)
	(editor-error "No cross references in this node")
	(goto-node (prompt-for-alist-value "Follow reference named" items)))))

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
    (string-replace! string char:newline #\Space)
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

(define-command ("Info Validate" argument)
  "Check that every node pointer points to an existing node."
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
				     (not (string-ci=? (caddr entry*) name))))
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
    (report-losers losers)))

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
	      (merge-pathnames (->pathname filename)
			       (->pathname (ref-variable "Info Directory"))))))
    (if (and pathname (not (file-exists? pathname)))
	(error "Info file does not exist" pathname))
    (record-current-node)
    (let ((buffer (find-or-create-buffer "*Info*")))
      ;; Switch files if necessary.
      (if (and pathname
	       (not (and (buffer-pathname buffer)
			 (pathname=? pathname (buffer-pathname buffer)))))
	  (begin (buffer-reset! buffer)
		 (read-buffer buffer pathname)
		 (set-buffer-major-mode! buffer info-mode)
		 (find-tag-table buffer))
	  (group-un-clip! (buffer-group buffer)))
      (set-buffer-read-only! buffer)
      (if (string=? nodename "*")
	  (begin (set! current-file pathname)
		 (set! current-node nodename)
		 (select-buffer buffer))
	  (select-node buffer
		       (let ((end (buffer-end buffer)))
			 (define (loop start)
			   (let ((node (next-node start end)))
			     (if node
				 (if (let ((name (extract-node-name node)))
				       (and name
					    (string-ci=? nodename name)))
				     node
				     (loop node))
				 (error "FIND-NODE: No such node" nodename))))
			 (loop (node-search-start buffer nodename))))))))

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
	(receiver #!FALSE (if (string-null? name) "Top" name)))))

(define (record-current-node)
  (if current-file
      (set! history
	    (cons (vector current-file
			  current-node
			  (mark-index (current-point)))
		  history))))

(define (select-node buffer point)
  (let ((node (node-start point (group-start point))))
    (set! current-file (buffer-pathname buffer))
    (set! current-node (extract-node-name node))
    ;; **** need to add active node hacking here ****
    (region-clip! (node-region node))
    (select-buffer buffer)
    (set-current-point! point)))

(define (node-start start end)
  (let ((mark (search-backward "\n" start end)))
    (and mark
	 (line-start mark 2))))

(define (node-region node)
  (make-region node (node-end node)))

(define (node-end node)
  (let ((end (group-end node)))
    (define (loop start)
      (let ((mark (re-search-forward "[\f]" start)))
	(cond ((not mark) end)
	      ((char=? (extract-left-char (re-match-start 0)) char:newline)
	       (mark-1+ (re-match-start 0)))
	      (else (loop mark)))))
    (loop node)))

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

(define-command ("Info Tagify" argument)
  "Create or update tag table of current info file."
  (let ((buffer (current-buffer)))
    (without-group-clipped! (buffer-group buffer)
      (lambda ()
	(with-read-only-defeated (buffer-end buffer)
	  (lambda ()
	    ;; Flush old tag table if present.
	    (find-tag-table buffer)
	    (if (ref-variable "Info Tag Table Start")
		(delete-string (mark- (ref-variable "Info Tag Table Start")
				      (string-length tag-table-start-string))
			       (mark+ (ref-variable "Info Tag Table End")
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
	(find-tag-table buffer)))))

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
  (let ((end (buffer-end buffer)))
    (let ((mark (line-start end -8)))
      (if mark
	  (let ((tag-table-end
		 (and (search-forward tag-table-end-string mark)
		      (re-match-start 0))))
	    (set-variable! "Info Tag Table Start"
			   (and tag-table-end
				(search-backward tag-table-start-string
						 tag-table-end)
				(re-match-end 0)))
	    (set-variable! "Info Tag Table End" tag-table-end))
	  (begin (set-variable! "Info Tag Table Start" #!FALSE)
		 (set-variable! "Info Tag Table End" #!FALSE))))))

(define (node-search-start buffer nodename)
  (if (not (ref-variable "Info Tag Table Start"))
      (buffer-start buffer)
      (let ((string (string-append "Node: " nodename "¢")))
	(let ((mark (search-forward string
				    (ref-variable "Info Tag Table Start")
				    (ref-variable "Info Tag Table End"))))
	  (or (and mark
		   (mark+ (buffer-start buffer)
			  (max 0 (- (with-input-from-mark mark read) 1000))))
	      (buffer-start buffer))))))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: (access info-package edwin-package)
;;; Scheme Syntax Table: edwin-syntax-table
;;; End:

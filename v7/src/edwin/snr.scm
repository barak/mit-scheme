;;; -*-Scheme-*-
;;;
;;;	$Id: snr.scm,v 1.2 1995/05/06 02:21:51 cph Exp $
;;;
;;;	Copyright (c) 1995 Massachusetts Institute of Technology
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

;;;; Scheme News Reader

(declare (usual-integrations))

(load-option 'ORDERED-VECTOR)

(define-variable news-server
  "Host name of the default news server.
This is the name used by \\[rnews].  If it is an empty string,
\\[rnews] will prompt for a host name and save it back into
news-server."
  ""
  string?)

(define-variable news-server-show-domain
  "Switch controlling appearance of server name in news buffers.
The buffers used by the News reader contain the server name.  If this
switch is false (the default), the server's domain is suppressed
before forming the buffer name.  Otherwise, the full server name is
used."
  #f
  boolean?)

(define-variable show-unsubscribed-news-groups
  "Switch controlling whether unsubscribed news groups appear in news buffers.
If false (the default), only currently subscribed groups are shown.
If true, previously subscribed buffers are also shown."
  #f
  boolean?)

(define-variable news-article-context-lines
  "The number of lines to show in a News group context window."
  5
  (lambda (object) (and (exact-integer? object) (> object 0))))

(define-variable news-full-name
  "Your full name.
Appears in the From: field of posted messages, following the email address.
If set to the null string, From: field contains only the email address."
  ""
  string?)

(define-variable news-organization
  "The name of your organization.
Appears in the Organization: field of posted messages.
If set to the null string, no Organization: field is generated."
  ""
  string?)

(define-command rnews
  "Start a News reader.
Normally uses the server specified by the variable news-server,
but with a prefix arg prompts for the server name.
Only one News reader may be open per server; if a previous News reader
is open the that server, its buffer is selected."
  "P"
  (lambda (prompt?)
    (select-buffer
     (let ((server (get-news-server-name prompt?)))
       (or (find-news-server-buffer server)
	   (make-news-server-buffer server))))))

(define (get-news-server-name prompt?)
  (let ((server (ref-variable news-server #f)))
    (if (or prompt? (string-null? server))
	(prompt-for-news-server "News server")
	server)))

(define (prompt-for-news-server prompt)
  (let ((default (ref-variable news-server #f)))
    (let ((server
	   (prompt-for-string prompt
			      (and (not (string-null? default))
				   default))))
      (if (string-null? default)
	  (set-variable! news-server server #f))
      server)))

(define-command news-kill-current-buffer
  "Kill the current buffer."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (let ((parent (buffer-tree:parent buffer #f)))
	(if parent
	    (select-buffer parent)))
      (kill-buffer buffer))))

(define-command news-next-line
  "Move down one or more lines and put point at start of line."
  "P"
  (lambda (argument)
    (set-current-point! (line-start (current-point) 0))
    ((ref-command next-line) argument)))

(define-command news-previous-line
  "Move up one or more lines and put point at start of line."
  "p"
  (lambda (argument)
    (set-current-point! (line-start (current-point) 0))
    ((ref-command previous-line) argument)))

;;;; News-Server Buffer

(define (find-news-server-buffer server)
  (let ((buffer
	 (list-search-positive (buffer-list)
	   (lambda (buffer)
	     (and (news-server-buffer? buffer)
		  (string-ci=? (news-server-buffer:server buffer) server))))))
    (and buffer
	 (begin
	   (news-server-buffer:guarantee-connection buffer)
	   buffer))))

(define (make-news-server-buffer server)
  (let ((buffer (new-buffer (news-server-buffer-name server))))
    (set-buffer-major-mode! buffer (ref-mode-object news-server))
    (disable-group-undo! (buffer-group buffer))
    (add-kill-buffer-hook buffer news-server-buffer:kill)
    (news-server-buffer:open-connection buffer server)
    (let ((groups
	   (sort (get-ini-file-groups (news-server-buffer:connection buffer))
		 news-group:<)))
      (for-each news-group:update-ranges! groups)
      (initialize-buffer-news-groups buffer groups))
    (set-buffer-point! buffer (buffer-start buffer))
    (buffer-not-modified! buffer)
    (set-buffer-read-only! buffer)
    buffer))

(define (news-server-buffer-name server)
  (string-append (if (ref-variable news-server-show-domain #f)
		     server
		     (let ((dot (string-find-next-char server #\.)))
		       (if dot
			   (string-head server dot)
			   server)))
		 ":news"))

(define (news-server-buffer:kill buffer)
  (news-server-buffer:save-groups buffer)
  (news-server-buffer:close-connection buffer)
  (for-each kill-buffer (buffer-tree:children buffer)))

(define (news-server-buffer:save-groups buffer)
  (put-ini-file-groups (news-server-buffer:connection buffer)
		       (buffer-news-groups buffer)))

(define (news-server-buffer? buffer)
  (nntp-connection? (buffer-get buffer 'NNTP-CONNECTION #f)))

(define (news-server-buffer:connection buffer)
  (let ((connection (buffer-get buffer 'NNTP-CONNECTION #f)))
    (if (not (nntp-connection? connection))
	(error "Buffer isn't a News server buffer:" (buffer-name buffer)))
    connection))

(define (news-server-buffer:server buffer)
  (nntp-connection:server (news-server-buffer:connection buffer)))

(define (news-server-buffer:open-connection buffer server)
  (let ((msg (string-append "Opening connection to " server "... ")))
    (message msg)
    (buffer-put! buffer 'NNTP-CONNECTION (open-nntp-connection server))
    (message msg "done")))

(define (news-server-buffer:guarantee-connection buffer)
  (let ((connection (news-server-buffer:connection buffer)))
    (if (nntp-connection:closed? connection)
	(let ((msg
	       (string-append "Reopening connection to "
			      (nntp-connection:server connection)
			      "... ")))
	  (message msg)
	  (nntp-connection:reopen connection)
	  (message msg "done")))
    connection))

(define (news-server-buffer:close-connection buffer)
  (let ((connection (buffer-get buffer 'NNTP-CONNECTION #f)))
    (if connection
	(let ((msg
	       (string-append "Closing connection to "
			      (nntp-connection:server connection)
			      "... ")))
	  (message msg)
	  (nntp-connection:close connection)
	  (message msg "done")))))

(define (initialize-buffer-news-groups buffer groups)
  (let ((mark (mark-left-inserting-copy (buffer-start buffer))))
    (for-each (lambda (group)
		(if (show-buffer-news-group? buffer group)
		    (begin
		      (insert-news-group-line group mark)
		      (insert-newline mark))))
	      groups)
    (mark-temporary! mark))
  (let loop ((start (buffer-start buffer)) (groups groups))
    (if (not (null? groups))
	(if (show-buffer-news-group? buffer (car groups))
	    (let ((end (line-start start 1 'ERROR))
		  (group (car groups)))
	      (region-put! start end 'NEWS-GROUP group)
	      (loop end (cdr groups)))
	    (loop start (cdr groups)))))
  (buffer-put! buffer 'NEWS-GROUPS groups))

(define (buffer-news-groups buffer)
  (buffer-get buffer 'NEWS-GROUPS '()))

(define (buffer-news-group-mark buffer group error?)
  (or (find-buffer-line buffer group 'NEWS-GROUP news-group:<)
      (and error?
	   (error "Buffer has no line for this group:" group buffer))))

(define (add-buffer-news-group buffer group)
  (let loop ((groups (buffer-news-groups buffer)) (prev #f))
    (cond ((or (null? groups)
	       (news-group:< group (car groups)))
	   (let ((groups (cons group groups)))
	     (if prev
		 (set-cdr! prev groups)
		 (buffer-put! buffer 'NEWS-GROUPS groups))))
	  ((not (eq? group (car groups)))
	   (loop (cdr groups) groups)))))

(define (maybe-update-buffer-news-group buffer group)
  (if (memq group (buffer-news-groups buffer))
      (update-buffer-news-group buffer group)))

(define (update-buffer-news-group buffer group)
  (with-buffer-open buffer
    (lambda ()
      (let ((mark (buffer-news-group-mark buffer group #f)))
	(if mark
	    (if (show-buffer-news-group? buffer group)
		(let ((start (mark-right-inserting-copy mark))
		      (end (mark-left-inserting-copy mark))
		      (point (buffer-point buffer)))
		  (let ((le (line-end end 0)))
		    (let ((column
			   (and (mark<= end point)
				(mark<= point le)
				(mark-column point))))
		      (delete-string end le)
		      (insert-news-group-line group end)
		      (if column
			  (set-buffer-point! buffer
					     (move-to-column start column)))))
		  (region-put! start (mark1+ end) 'NEWS-GROUP group)
		  (mark-temporary! end)
		  (mark-temporary! start))
		(delete-string mark (line-start mark 1 'ERROR)))
	    (if (show-buffer-news-group? buffer group)
		(let ((mark
		       (let ((tail (memq group (buffer-news-groups buffer))))
			 (if (null? (cdr tail))
			     (buffer-end buffer)
			     (buffer-news-group-mark buffer (cadr tail) #t)))))
		  (let ((start (mark-right-inserting-copy mark))
			(end (mark-left-inserting-copy mark)))
		    (insert-news-group-line group end)
		    (insert-newline end)
		    (region-put! start end 'NEWS-GROUP group)
		    (mark-temporary! end)
		    (mark-temporary! start)))))))))

(define (show-buffer-news-group? buffer group)
  (and (news-group:active? group)
       (or (news-group:subscribed? group)
	   (ref-variable show-unsubscribed-news-groups buffer))))

(define (insert-news-group-line group mark)
  (insert-string (if (news-group:subscribed? group) "  " "U ") mark)
  (insert-string-pad-left
   (number->string (news-group:number-of-articles group))
   5 #\space mark)
  (insert-string ": " mark)
  (insert-string (news-group:name group) mark))

;;;; News-Server Mode

(define-major-mode news-server read-only "News Server"
  "Major mode for browsing a News server.
Each line shows one of the News groups on the server.  The number near
the left of the line is an estimate of the number of unread messages
available in that group.  A `U' character appearing in the left column
indicates that the group is Unsubscribed.

This mode's commands include:

\\[news-all-groups]	select a buffer showing all of the server's News groups
\\[news-select-group]	browse articles in the News group indicated by point
\\[news-subscribe-group]	subscribe to the News group indicated by point
\\[news-unsubscribe-group]	unsubscribe from the News group indicated by point")

(define-key 'news-server #\space 'news-select-group)
(define-key 'news-server #\a 'news-compose)
(define-key 'news-server #\g 'news-all-groups)
(define-key 'news-server #\q 'news-kill-current-buffer)
(define-key 'news-server #\r 'news-refresh-group)
(define-key 'news-server #\R 'news-refresh-groups)
(define-key 'news-server #\s 'news-subscribe-group)
(define-key 'news-server #\S 'news-subscribe-group-by-name)
(define-key 'news-server #\u 'news-unsubscribe-group)
(define-key 'news-server #\c-n 'news-next-line)
(define-key 'news-server #\c-p 'news-previous-line)
(define-key 'news-server '(#\c-x #\c-s) 'news-save-server-data)

(define-command news-select-group
  "Browse the News group indicated by point.
Selects a buffer showing the subject lines of the articles in the News group."
  ()
  (lambda ()
    (let ((buffer
	   (let ((server-buffer (current-news-server-buffer #t))
		 (group (current-news-group)))
	     (or (find-news-group-buffer server-buffer group)
		 (make-news-group-buffer server-buffer group)))))
      (select-buffer buffer)
      (news-group-buffer:update-server-buffer buffer))))

(define-command news-refresh-groups
  "Update the unread-message estimates for all of the News groups shown.
This will take a long time if done in the all-groups buffer."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (for-each
       (lambda (group)
	 (if (show-buffer-news-group? buffer group)
	     (begin
	       (news-group:update-ranges! group)
	       (update-buffer-news-group buffer group))))
       (buffer-news-groups buffer)))))

(define-command news-refresh-group
  "Update the unread-message estimate for the News group indicated by point.
With prefix argument, updates the next several News groups."
  "P"
  (lambda (argument)
    (news-group-command argument
      (let ((buffer (current-buffer)))
	(lambda (group)
	  (news-group:update-ranges! group)
	  (update-buffer-news-group buffer group))))))

(define-command news-subscribe-group
  "Subscribe to the News group indicated by point.
Normally useful only in the all-groups buffer, since the server buffer
doesn't show unsubscribed groups.
With prefix argument, subscribes to the next several News groups."
  "P"
  (lambda (argument)
    (news-group-command argument
			(make-news-group-subscriber (current-buffer)))))

(define-command news-subscribe-group-by-name
  "Subscribe to a News group by name.
Prompts for the News-group name, with completion."
  ()
  (lambda ()
    ((make-news-group-subscriber (current-buffer))
     (prompt-for-active-news-group "Subscribe to news group"
				   #f
				   (current-news-server-buffer #t)))))

(define (make-news-group-subscriber buffer)
  (let ((server-buffer (news-server-buffer buffer #t)))
    (let ((all-groups (find-all-news-groups-buffer server-buffer)))
      (lambda (group)
	(set-news-group:subscribed?! group #t)
	(add-buffer-news-group server-buffer group)
	(update-buffer-news-group buffer group)
	(cond ((not (eq? buffer server-buffer))
	       (update-buffer-news-group server-buffer group))
	      ((and all-groups (not (eq? buffer all-groups)))
	       (update-buffer-news-group all-groups group)))))))

(define-command news-unsubscribe-group
  "Unsubscribe from the News group indicated by point.
With prefix argument, unsubscribes from the next several News groups."
  "P"
  (lambda (argument)
    (news-group-command argument
      (let ((buffer (current-buffer)))
	(let ((server-buffer (news-server-buffer buffer #t)))
	  (let ((all-groups (find-all-news-groups-buffer server-buffer)))
	    (lambda (group)
	      (set-news-group:subscribed?! group #f)
	      (update-buffer-news-group buffer group)
	      (cond ((not (eq? buffer server-buffer))
		     (maybe-update-buffer-news-group server-buffer group))
		    ((and all-groups (not (eq? buffer all-groups)))
		     (update-buffer-news-group all-groups group))))))))))

(define-command news-all-groups
  "Select a buffer showing all of the News groups on this server.
This buffer shows subscribed and unsubscribed groups, and is useful
for choosing new groups to subscribe to.

Making this buffer for the first time can be slow."
  ()
  (lambda ()
    (select-buffer
     (let ((server-buffer (current-news-server-buffer #t)))
       (or (find-all-news-groups-buffer server-buffer)
	   (make-all-news-groups-buffer server-buffer))))))

(define-command news-save-server-data
  "Update the \"snr.ini\" file with current data."
  ()
  (lambda ()
    (news-server-buffer:save-groups (current-news-server-buffer #t))))

(define (current-news-server-buffer error?)
  (news-server-buffer (current-buffer) error?))

(define (news-server-buffer buffer error?)
  (if (news-server-buffer? buffer)
      buffer
      (let ((buffer (buffer-tree:parent buffer error?)))
	(and buffer
	     (news-server-buffer buffer error?)))))

(define (current-news-server error?)
  (let ((buffer (current-news-server-buffer error?)))
    (and buffer
	 (news-server-buffer:server buffer))))

(define (current-news-group)
  (current-property-item 'NEWS-GROUP "news-group"))

(define (news-group-command argument procedure)
  (repeating-command argument procedure 'NEWS-GROUP "news-group"))

(define (prompt-for-active-news-group prompt default server-buffer)
  (let ((connection (news-server-buffer:connection server-buffer)))
    (let ((groups (lambda () (nntp-connection:active-groups connection)))
	  (string->group
	   (lambda (string) (find-active-news-group connection string))))
      (string->group
       (let ((convert
	      (lambda (vector) (map news-group:name (vector->list vector)))))
	 (prompt-for-completed-string prompt default 'VISIBLE-DEFAULT
	   (lambda (string if-unique if-not-unique if-not-found)
	     (ordered-vector-minimum-match (groups) string news-group:name
					   string-order (prefix-matcher string)
	       (lambda (group)
		 (if-unique (news-group:name group)))
	       (lambda (group gcm all-matches)
		 (if-not-unique (string-head (news-group:name group) gcm)
				(lambda () (convert (all-matches)))))
	       if-not-found))
	   (lambda (string)
	     (convert
	      (ordered-vector-matches (groups) string news-group:name
				      string-order (prefix-matcher string))))
	   string->group
	   #t))))))

(define (string-order x y)
  (string-compare x y
		  (lambda () 'EQUAL)
		  (lambda () 'LESS)
		  (lambda () 'GREATER)))

(define (prefix-matcher prefix)
  (let ((plen (string-length prefix)))
    (lambda (x y)
      (let ((n (string-match-forward x y)))
	(and (fix:>= n plen)
	     n)))))

;;;; All-Groups Buffer

(define (find-all-news-groups-buffer server-buffer)
  (buffer-tree:child server-buffer 'ALL-NEWS-GROUPS #f))

(define (make-all-news-groups-buffer server-buffer)
  (let ((buffer
	 (new-buffer
	  (string-append "all-groups:"
			 (news-server-buffer-name
			  (news-server-buffer:server server-buffer))))))
    (set-buffer-major-mode! buffer (ref-mode-object news-server))
    (local-set-variable! show-unsubscribed-news-groups #t buffer)
    (disable-group-undo! (buffer-group buffer))
    (let ((groups
	   (nntp-connection:active-groups
	    (news-server-buffer:connection server-buffer))))
      (let ((msg "Building all-groups buffer... "))
	(message msg)
	(initialize-buffer-news-groups buffer (vector->list groups))
	(message msg "done")))
    (buffer-tree:attach-child! server-buffer 'ALL-NEWS-GROUPS buffer)
    (set-buffer-point! buffer (buffer-start buffer))
    (buffer-not-modified! buffer)
    (set-buffer-read-only! buffer)
    buffer))

;;;; News-Group Buffer

(define (find-news-group-buffer server-buffer group)
  (buffer-tree:child server-buffer group #f))

(define (make-news-group-buffer server-buffer group)
  (let ((buffer (new-buffer (news-group-buffer-name group))))
    (set-buffer-major-mode! buffer (ref-mode-object news-group))
    (disable-group-undo! (buffer-group buffer))
    (buffer-put! buffer 'NEWS-GROUP group)
    (buffer-tree:attach-child! server-buffer group buffer)
    (add-kill-buffer-hook buffer news-group-buffer:kill)
    (add-select-buffer-hook buffer news-group-buffer:select)
    (initialize-news-group-buffer buffer #f)
    (set-buffer-read-only! buffer)
    buffer))

(define (news-group-buffer-name group)
  (string-append (news-group:name group)
		 ":"
		 (news-server-buffer-name (news-group:server group))))

(define (news-group-buffer? buffer)
  (news-group? (buffer-get buffer 'NEWS-GROUP #f)))

(define (news-group-buffer:group buffer)
  (let ((group (buffer-get buffer 'NEWS-GROUP #f)))
    (if (not (news-group? group))
	(error "Buffer isn't a News group buffer:" (buffer-name buffer)))
    group))

(define (news-group-buffer buffer error?)
  (if (news-group-buffer? buffer)
      buffer
      (let ((buffer (buffer-tree:parent buffer error?)))
	(and buffer
	     (news-group-buffer buffer error?)))))

(define (news-group-buffer:kill buffer)
  (news-group-buffer:update-server-buffer buffer)
  (let ((group (news-group-buffer:group buffer)))
    (for-each
     (lambda (header)
       (if (not (news-header:article-unseen? header))
	   (news-group:discard-cached-header! group
					      (news-header:number header))))
     (news-group:cached-headers group))))

(define (news-group-buffer:select group-buffer window)
  (news-group-buffer:delete-context-window group-buffer window))

(define (initialize-news-group-buffer buffer all?)
  (fill-news-group-buffer
   buffer
   (let ((group (news-group-buffer:group buffer)))
     (news-group:headers
      group
      (ranges->list
       (complement-ranges (if all? '() (news-group:ranges-seen group))
			  (news-group:first-article group)
			  (news-group:last-article group))))))
  (set-buffer-point! buffer (buffer-start buffer))
  (buffer-not-modified! buffer))

(define (fill-news-group-buffer buffer headers)
  (let ((mark (mark-left-inserting-copy (buffer-end buffer)))
	(subject #f)
	(line 0)
	(lines '()))
    (for-each
     (lambda (thread)
       (let insert-headers
	   ((headers (news-thread:root-headers thread))
	    (indentation 0))
	 (for-each
	  (lambda (header)
	    (if (news-header:dummy? header)
		(insert-headers (news-header:followups header) indentation)
		(let* ((subject*
			(canonicalize-subject
			 (news-header:field-value header "subject"))))
		  (let ((comparison
			 (and subject (compare-subjects subject subject*))))
		    (insert-news-header-line header
					     indentation
					     (not comparison)
					     mark)
		    (if (or (not comparison)
			    (eq? 'RIGHT-PREFIX comparison))
			(set! subject subject*)))
		  (set-news-header:line-number! header line)
		  (set! line (fix:+ line 1))
		  (set! lines (cons header lines))
		  (insert-headers (news-header:followups header)
				  (+ indentation 4)))))
	  headers)))
     (organize-headers-into-threads headers))
    (mark-temporary! mark)
    (buffer-put! buffer 'NEWS-HEADERS (list->vector (reverse! lines)))))

(define (insert-news-header-line header indentation subject? mark)
  (let ((start (mark-right-inserting-copy mark)))
    (insert-char (news-header:status header) mark)
    (insert-char #\space mark)
    (insert-string-pad-left (news-header:field-value header "lines")
			    4 #\space mark)
    (insert-char #\: mark)
    (insert-chars #\space indentation mark)
    (if subject?
	(begin
	  (insert-char #\space mark)
	  (insert-string (news-header:field-value header "subject") mark)))
    (insert-string " (" mark)
    (insert-string (let ((from (news-header:field-value header "from")))
		     (or (rfc822-first-address from)
			 from))
		   mark)
    (insert-char #\) mark)
    (insert-newline mark)
    (region-put! start mark 'NEWS-HEADER header)
    (mark-temporary! start)))

(define (buffer-news-headers buffer)
  (buffer-get buffer 'NEWS-HEADERS '#()))

(define (buffer-news-header-mark buffer header error?)
  (or (find-buffer-line buffer header 'NEWS-HEADER
			(lambda (x y)
			  (fix:< (news-header:line-number x)
				 (news-header:line-number y))))
      (and error?
	   (error "Buffer has no line for this header:" header buffer))))

(define (update-buffer-news-header-status buffer header)
  (with-buffer-open buffer
    (lambda ()
      (let ((mark
	     (mark-right-inserting-copy
	      (buffer-news-header-mark buffer header #t))))
	(let ((preserve-point? (mark= (buffer-point buffer) mark)))
	  (delete-right-char mark)
	  (insert-char (news-header:status header) mark)
	  ;; Grumble: must rewrite 'NEWS-HEADER property because
	  ;; inserted characters have no properties.
	  (region-put! mark (mark1+ mark) 'NEWS-HEADER header)
	  (if preserve-point? (set-buffer-point! buffer mark))
	  (mark-temporary! mark))))))

(define (news-group-buffer:update-server-buffer buffer)
  (let ((group (news-group-buffer:group buffer)))
    (news-group:update-ranges! group)
    (let ((server-buffer (buffer-tree:parent buffer #f)))
      (if server-buffer
	  (update-buffer-news-group server-buffer group)))))

(define (show-news-article-context article-window context-lines)
  (let ((context-window
	 (window-split-vertically! article-window
				   (- (window-y-size article-window)
				      context-lines)))
	(group-buffer (buffer-tree:parent (window-buffer article-window) #t)))
    (buffer-put! group-buffer 'CONTEXT-WINDOW (weak-cons context-window #f))
    (select-buffer-in-window group-buffer context-window #t)
    (center-news-article-context context-window)))

(define (news-group-buffer:delete-context-window group-buffer window)
  (let ((context-window (news-group-buffer:context-window group-buffer #t)))
    (if (and context-window (not (eq? window context-window)))
	(begin
	  (window-delete! context-window window)
	  (buffer-remove! group-buffer 'CONTEXT-WINDOW)))))

(define (news-group-buffer:set-point! group-buffer mark)
  (set-buffer-point! group-buffer mark)
  (let ((context-window (news-group-buffer:context-window group-buffer #t)))
    (if context-window
	(begin
	  (set-window-point! context-window mark)
	  (center-news-article-context context-window)))))

(define (center-news-article-context context-window)
  (window-scroll-y-absolute! context-window
			     (integer-floor (window-y-size context-window) 2)))

(define (news-group-buffer:context-window buffer require-buffer?)
  (let ((pair (buffer-get buffer 'CONTEXT-WINDOW #f)))
    (and pair
	 (let ((window
		(let ((window (weak-car pair)))
		  (and window
		       (window-visible? window)
		       (or (not require-buffer?)
			   (eq? buffer (window-buffer window)))
		       window))))
	   (if (not window)
	       (buffer-remove! buffer 'CONTEXT-WINDOW))
	   window))))

;;;; News-Group Mode

(define-major-mode news-group read-only "News Group"
  "Major mode for browsing subjects of articles in a News group.
Each line shows one of the articles in the group.  The number near the
left is an estimate of the number of lines in the article.  A `D' in
the left column indicates that the article has either been read or
marked as such.  The right-hand side of the line shows the subject
line from the article, followed by the author's name in parenthesis.

Articles are grouped into conversational `threads' where possible.  In
such threads, the subjects of followup articles are suppressed, and
the parenthesized author's name appears indented.  The indentation
shows structure of the conversation, with followups being indented a
bit more than the articles they follow-up to.

This mode's commands include:

\\[news-select-article]	select a buffer containing the article indicated by point
\\[news-compose]	post a new article to this group
\\[news-delete-article]	mark the article indicated by point as read
\\[news-delete-thread]	mark the whole thread as read
\\[news-undelete-article]	unmark the article indicated by point
\\[news-undelete-thread]	unmark the whole thread
\\[news-expunge-group]	remove from the buffer all marked lines"
  (lambda (buffer)
    (local-set-variable! truncate-lines #t buffer)))

(define-key 'news-group #\space 'news-select-article)
(define-key 'news-group #\a 'news-compose)
(define-key 'news-group #\c 'news-catch-up-group)
(define-key 'news-group #\d 'news-delete-article)
(define-key 'news-group #\D 'news-delete-thread)
(define-key 'news-group #\g 'news-revert-group)
(define-key 'news-group #\q 'news-kill-current-buffer)
(define-key 'news-group #\u 'news-undelete-article)
(define-key 'news-group #\U 'news-undelete-thread)
(define-key 'news-group #\x 'news-expunge-group)
(define-key 'news-group #\c-n 'news-next-line)
(define-key 'news-group #\c-p 'news-previous-line)
(define-key 'news-server '(#\c-x #\c-s) 'news-save-server-data)

(define-command news-select-article
  "Select a buffer containing the News article indicated by point."
  ()
  (lambda ()
     (select-news-article (current-buffer) (current-news-header))))

(define (select-news-article group-buffer header)
  (select-buffer
   (or (find-news-article-buffer group-buffer header)
       (make-news-article-buffer group-buffer header)
       (editor-error "Article no longer available from server."))))

(define-command news-delete-article
  "Mark as `read' the News article indicated by point.
With prefix argument, marks the next several articles."
  "P"
  (lambda (argument)
    (news-header-command argument (header-deletion-procedure))))

(define-command news-delete-thread
  "Mark as `read' the conversation thread indicated by point.
This marks the article indicated by point and any other articles in
the same thread as that article."
  ()
  (lambda ()
    (let ((root (news-header:thread (current-news-header))))
      (news-thread:for-each-header root (header-deletion-procedure))
      (set-current-point!
       (find-next-header-line (line-start (current-point) 0)
	 (lambda (header)
	   (not (eq? root (news-header:thread header)))))))))

(define (header-deletion-procedure)
  (let ((buffer (current-buffer)))
    (lambda (header)
      (if (not (news-header:dummy? header))
	  (begin
	    (news-header:article-seen! header)
	    (update-buffer-news-header-status buffer header))))))

(define-command news-undelete-article
  "Unmark the News article indicated by point.
With prefix argument, unmarks the next several articles."
  "P"
  (lambda (argument)
    (news-header-command argument (header-undeletion-procedure))))

(define-command news-undelete-thread
  "Unmark the conversation thread indicated by point.
This unmarks the article indicated by point and any other articles in
the same thread as that article."
  ()
  (lambda ()
    (let ((root (news-header:thread (current-news-header))))
      (news-thread:for-each-header root (header-undeletion-procedure))
      (set-current-point!
       (find-next-header-line (line-start (current-point) 0)
	 (lambda (header)
	   (not (eq? root (news-header:thread header)))))))))

(define (header-undeletion-procedure)
  (let ((buffer (current-buffer)))
    (lambda (header)
      (if (not (news-header:dummy? header))
	  (begin
	    (news-header:article-unseen! header)
	    (update-buffer-news-header-status buffer header))))))

(define-command news-expunge-group
  "Remove all marked lines from the current buffer."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (with-buffer-open buffer
	(lambda ()
	  (let loop ((mark (buffer-absolute-start buffer)))
	    (if (not (group-end? mark))
		(if (news-header:article-unseen? (get-news-header mark))
		    (loop (line-start mark 1 'ERROR))
		    (let ((mark (mark-right-inserting-copy mark)))
		      (delete-string mark (line-start mark 1 'ERROR))
		      (mark-temporary! mark)
		      (loop mark))))))))))

(define-command news-catch-up-group
  "Mark all of the articles as read, and return to the News server buffer.
This kills the current buffer."
  ()
  (lambda ()
    (if (prompt-for-confirmation? "Delete all articles not marked as read")
	(begin
	  (let ((buffer (current-buffer)))
	    (for-each-vector-element (buffer-news-headers buffer)
				     news-header:article-seen!))
	  ((ref-command news-kill-current-buffer))))))

(define-command news-revert-group
  "Refresh the article list from the News server.
This gets any new article headers from the News server, adding their
lines to the current buffer.  With a prefix argument, this shows all
of the articles in the News group, including those that were
previously marked as `read'."
  "P"
  (lambda (argument)
    (let ((buffer (current-buffer)))
      (with-buffer-open buffer
	(lambda ()
	  (region-delete! (buffer-region buffer))
	  (initialize-news-group-buffer buffer argument))))))

(define (find-next-header-line ls predicate)
  (if (group-end? ls)
      ls
      (let loop ((ls (line-start ls 1 'ERROR)))
	(if (or (let ((header (region-get ls 'NEWS-HEADER #f)))
		  (or (not header)
		      (predicate header)))
		(group-end? ls))
	    ls
	    (loop (line-start ls 1 'ERROR))))))

(define (find-previous-header-line ls predicate)
  (if (group-start? ls)
      ls
      (let loop ((ls (line-start ls -1 'ERROR)))
	(if (or (let ((header (region-get ls 'NEWS-HEADER #f)))
		  (or (not header)
		      (predicate header)))
		(group-start? ls))
	    ls
	    (loop (line-start ls -1 'ERROR))))))

(define (next-unseen-header-line ls)
  (find-next-header-line ls news-header:article-unseen?))

(define (previous-unseen-header-line ls)
  (find-previous-header-line ls news-header:article-unseen?))

(define (current-news-header)
  (current-property-item 'NEWS-HEADER "article header"))

(define (news-header-command argument procedure)
  (repeating-command argument procedure 'NEWS-HEADER "article header"))

(define (get-news-header mark)
  (property-item mark 'NEWS-HEADER "news header"))

;;;; News-Article Buffer

(define (find-news-article-buffer group-buffer header)
  (buffer-tree:child group-buffer header #f))

(define (make-news-article-buffer group-buffer header)
  (let ((buffer (new-buffer (news-article-buffer-name header))))
    (set-buffer-major-mode! buffer (ref-mode-object news-article))
    (disable-group-undo! (buffer-group group-buffer))
    (if (let ((msg "Reading article... "))
	  (message msg)
	  (let ((value
		 (call-with-output-mark (buffer-end buffer)
		   (lambda (port)
		     (news-header:read-body header port)))))
	    (message msg "done")
	    value))
	(begin
	  (insert-news-header header buffer #t)
	  (enable-group-undo! (buffer-group group-buffer))
	  (buffer-put! buffer 'NEWS-HEADER header)
	  (buffer-tree:attach-child! group-buffer header buffer)
	  (set-buffer-point! buffer (buffer-start buffer))
	  (buffer-not-modified! buffer)
	  (set-buffer-read-only! buffer)
	  (news-header:article-seen! header)
	  (update-buffer-news-header-status group-buffer header)
	  buffer)
	(begin
	  (kill-buffer buffer)
	  (news-header:article-seen! header)
	  (update-buffer-news-header-status group-buffer header)
	  #f))))

(define (news-article-buffer-name header)
  (string-append (number->string (news-header:number header))
		 ":"
		 (news-group-buffer-name (news-header:group header))))

(define (news-article-buffer? buffer)
  (news-header? (buffer-get buffer 'NEWS-HEADER #f)))

(define (news-article-buffer:header buffer)
  (let ((header (buffer-get buffer 'NEWS-HEADER #f)))
    (if (not (news-header? header))
	(error "Buffer isn't a News article buffer:" (buffer-name buffer)))
    header))

(define (insert-news-header header buffer truncate?)
  (with-buffer-open buffer
    (lambda ()
      (let ((hend (mark-left-inserting-copy (buffer-start buffer))))
	(insert-string (news-header:text header) hend)
	(insert-newline hend)
	(if truncate? (delete-ignored-headers (buffer-start buffer) hend))
	(mark-temporary! hend))))
  (buffer-put! buffer 'NEWS-ARTICLE-HEADER-TRUNCATED? truncate?))

(define (delete-ignored-headers hstart hend)
  (let ((regexp (ref-variable rmail-ignored-headers hstart)))
    (if regexp
	(let ((point (mark-right-inserting-copy hstart))
	      (group (mark-group hstart))
	      (p1 (re-compile-pattern regexp #t))
	      (p2 (re-compile-pattern "\n[^ \t]" #f)))
	  (do ()
	      ((not (re-search-buffer-forward p1 #t #f
					      group
					      (mark-index point)
					      (mark-index hend))))
	    (move-mark-to! point (line-start (re-match-start 0) 0))
	    (delete-string
	     point
	     (make-mark group
			(fix:- (re-search-buffer-forward p2 #f #f
							 group
							 (mark-index point)
							 (mark-index hend))
			       1))))
	  (mark-temporary! point)))))

(define (delete-news-header buffer)
  (with-buffer-open buffer
    (lambda ()
      (let ((start (buffer-start buffer)))
	(delete-string start (mark1+ (mail-header-end start)))))))

;;;; News-Article Mode

(define-major-mode news-article read-only "News Article"
  "Major mode for reading a News article.
This mode's commands include:

\\[news-next-article]	read the next unread article
\\[news-previous-article]	read the previous unread article
\\[news-toggle-article-header]	show/don't show all of the articles header lines
\\[news-toggle-article-context]	show/don't show window of the News group buffer
\\[news-compose]	post a new article to this group
\\[news-compose-followup]	post a reply to this article
\\[news-reply-to-article]	reply by email to this article
\\[news-output-article]	output this article to a mail file
\\[news-output-article-to-rmail-file]	output this article to an RMAIL file")

(define-key 'news-article #\space '(news-article . #\c-v))
(define-key 'news-article #\rubout '(news-article . #\m-v))
(define-key 'news-article #\a 'news-compose)
(define-key 'news-article #\c 'news-toggle-article-context)
(define-key 'news-article #\f 'news-compose-followup)
(define-key 'news-article #\n 'news-next-article)
(define-key 'news-article #\o 'news-output-article-to-rmail-file)
(define-key 'news-article #\p 'news-previous-article)
(define-key 'news-article #\q 'news-kill-current-buffer)
(define-key 'news-article #\r 'news-reply-to-article)
(define-key 'news-article #\t 'news-toggle-article-header)
(define-key 'news-article #\c-o 'news-output-article)

(define-command news-next-article
  "Select a buffer containing the next unread article in the News group.
If there is no such article, returns to the News group buffer.
Kills the current buffer in either case."
  ()
  (lambda ()
    (news-article-motion-command
     (lambda (group-buffer header)
       (let ((headers (buffer-news-headers group-buffer)))
	 (let ((index (vector-find-next-element headers header))
	       (length (vector-length headers)))
	   (if (not index)
	       (error "News header missing from headers list:" header))
	   (let loop ((index (fix:+ index 1)))
	     (if (fix:= index length)
		 "No next message in group."
		 (let ((header (vector-ref headers index)))
		   (if (buffer-news-header-mark group-buffer header #f)
		       header
		       (loop (fix:+ index 1))))))))))))

(define-command news-previous-article
  "Select a buffer containing the previous unread article in the News group.
If there is no such article, returns to the News group buffer.
Kills the current buffer in either case."
  ()
  (lambda ()
    (news-article-motion-command
     (lambda (group-buffer header)
       (let ((headers (buffer-news-headers group-buffer)))
	 (let ((index (vector-find-next-element headers header)))
	   (if (not index)
	       (error "News header missing from headers list:" header))
	   (let loop ((index (fix:- index 1)))
	     (if (fix:< index 0)
		 "No previous message in group."
		 (let ((header (vector-ref headers index)))
		   (if (buffer-news-header-mark group-buffer header #f)
		       header
		       (loop (fix:- index 1))))))))))))

(define (news-article-motion-command procedure)
  (let ((buffer (current-buffer)))
    (let ((group-buffer (buffer-tree:parent buffer #t)))
      (let ((header
	     (procedure group-buffer (news-article-buffer:header buffer))))
	(if (news-header? header)
	    (begin
	      (select-news-article group-buffer header)
	      (news-group-buffer:set-point!
	       group-buffer
	       (buffer-news-header-mark group-buffer header #t)))
	    (begin
	      (select-buffer group-buffer)
	      (message header)))))
    (kill-buffer buffer)))

(define-command news-toggle-article-header
  "Show original article header if pruned header currently shown, or vice versa.
Normally, the header lines specified in the variable rmail-ignored-headers
are not shown; this command shows them, or hides them if they are shown."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (let ((header (news-article-buffer:header buffer)))
	(delete-news-header buffer)
	(insert-news-header
	 header
	 buffer
	 (not (buffer-get buffer 'NEWS-ARTICLE-HEADER-TRUNCATED? #f))))
      (set-current-point! (buffer-start buffer)))))

(define-command news-toggle-article-context
  "Show context window into News group buffer, or hide it if currently shown.
This is a small window showing a few lines around the subject line of the
current article.  The number of lines is specified by the variable
news-article-context-lines, but a prefix argument overrides this."
  "P"
  (lambda (argument)
    (let ((article-window (current-window))
	  (context-lines
	   (if argument
	       (min 1 (command-argument-value argument))
	       (ref-variable news-article-context-lines))))
      (let ((article-buffer (window-buffer article-window)))
	(let ((group-buffer (buffer-tree:parent article-buffer #t)))
	  (let ((context-window
		 (news-group-buffer:context-window group-buffer #f)))
	    (let ((set-height
		   (lambda ()
		     (let ((delta
			    (- context-lines (window-y-size context-window))))
		       (if (not (= delta 0))
			   (window-grow-vertically! context-window delta)))
		     (center-news-article-context context-window))))
	      (cond ((not context-window)
		     (show-news-article-context article-window context-lines))
		    ((not (eq? group-buffer (window-buffer context-window)))
		     (select-buffer-in-window group-buffer context-window #f)
		     (set-height))
		    (argument
		     (set-height))
		    (else
		     (window-delete! context-window article-window)
		     (buffer-remove! group-buffer 'CONTEXT-WINDOW))))))))))

(define-command news-output-article-to-rmail-file
  "Append the current article to an Rmail file named FILE-NAME.
If the file does not exist, ask if it should be created.
If file is being visited, the article is appended to the
buffer visiting that file."
  (lambda ()
    (list (prompt-for-rmail-output-filename
	   "Output article to Rmail file"
	   (ref-variable rmail-last-rmail-file))))
  (lambda (pathname)
    (set-variable! rmail-last-rmail-file (->namestring pathname))
    (let ((buffer (get-article-output-buffer (current-buffer))))
      (rfc822-region->babyl (buffer-region buffer))
      (rmail-output-to-rmail-file (buffer-region buffer) pathname)
      (kill-buffer buffer))))

(define-command news-output-article
  "Append this article to Unix mail file named FILE-NAME."
  (lambda ()
    (list (prompt-for-rmail-output-filename "Output article to Unix mail file"
					    (ref-variable rmail-last-file))))
  (lambda (pathname)
    (set-variable! rmail-last-file (->namestring pathname))
    (let ((buffer (get-article-output-buffer (current-buffer))))
      (rmail-output-to-unix-mail-file (buffer-region buffer) pathname)
      (kill-buffer buffer))))

(define (get-article-output-buffer buffer)
  (let ((buffer* (temporary-buffer " news conversion")))
    (insert-region (buffer-absolute-start buffer)
		   (buffer-absolute-end buffer)
		   (buffer-start buffer*))
    (delete-news-header buffer*)
    (insert-news-header (news-article-buffer:header buffer) buffer* #f)
    buffer*))

(define-command news-reply-to-article
  "Mail a reply to the author of the current News article.
While composing the reply, use \\[mail-yank-original] to yank the
original message into it."
  ()
  (lambda ()
    (let ((reply-buffer (current-buffer)))
      (make-mail-buffer
       (let ((buffer (temporary-buffer " news conversion")))
	 (insert-news-header (news-article-buffer:header reply-buffer)
			     buffer #f)
	 (let ((headers
		(rfc822-region-reply-headers (buffer-region buffer) #t)))
	   (kill-buffer buffer)
	   headers))
       reply-buffer
       select-buffer-other-window))))

;;;; Posting

(define-command news-compose
  "Begin editing a News article to be posted.
Argument means resume editing previous article (don't erase).
Type \\[describe-mode] once editing the article to get a list of commands."
  "P"
  (lambda (no-erase?)
    (compose-news no-erase? select-buffer)))

(define-command news-compose-other-window
  "Like \\[news-compose], but display article buffer in other window."
  "P"
  (lambda (no-erase?)
    (compose-news no-erase? select-buffer-other-window)))

(define (compose-news no-erase? selector)
  (let ((server (current-news-server #f))
	(newsgroups
	 (let ((buffer (news-group-buffer (current-buffer) #f)))
	   (and buffer
		(news-group:name (news-group-buffer:group buffer))))))
    (let ((buffer
	   (make-mail-buffer `(("Newsgroups" ,(or newsgroups ""))
			       ("Subject" ""))
			     #f
			     selector
			     (if no-erase?
				 'KEEP-PREVIOUS-MAIL
				 'QUERY-DISCARD-PREVIOUS-MAIL)
			     "*news*"
			     (ref-mode-object compose-news))))
      (if buffer
	  (begin
	    (if server (buffer-put! buffer 'NEWS-SERVER server))
	    (if (not newsgroups)
		(set-buffer-point! buffer
				   (mail-position-on-field buffer
							   "Newsgroups"))))))))

(define-command news-compose-followup
  "Begin editing a follow-up to the current News article.
While composing the follow-up, use \\[mail-yank-original] to yank the
original message into it."
  ()
  (lambda ()
    (let ((article-buffer (current-buffer)))
      (let ((header (news-article-buffer:header article-buffer)))
	(let ((followup-to (news-header:field-value header "followup-to")))
	  (if (string-ci=? followup-to "poster")
	      ((ref-command news-reply-to-article))
	      (let ((buffer
		     (make-mail-buffer (news-header-followup-fields header)
				       article-buffer
				       select-buffer-other-window
				       'QUERY-DISCARD-PREVIOUS-MAIL
				       "*news*"
				       (ref-mode-object compose-news))))
		(if buffer
		    (buffer-put! buffer 'NEWS-SERVER
				 (nntp-connection:server
				  (news-group:connection
				   (news-header:group header))))))))))))

(define (news-header-followup-fields header)
  `(("Newsgroups" ,(news-header:field-value header "Newsgroups"))
    ("Subject" ,(let ((subject (news-header:field-value header "Subject")))
		  (if (and (not (string-null? subject))
			   (not (string-prefix-ci? "re:" subject)))
		      (string-append "Re: " subject)
		      subject)))
    ("References" ,(let ((refs (news-header:field-value header "References"))
			 (id (news-header:message-id header)))
		     (if (string-null? refs)
			 id
			 (string-append refs " " id)))
		  #T)
    ("In-reply-to"
     ,(make-in-reply-to-field (news-header:field-value header "From")
			      (news-header:field-value header "Date")
			      (news-header:message-id header)))
    ("Distribution"
     ,(let ((distribution (news-header:field-value header "Distribution")))
	(and (not (string-null? distribution))
	     distribution)))))

(define-major-mode compose-news mail "News"
  "Major mode for editing news to be posted on USENET.
Like Text mode but with these additional commands:

C-c C-s  mail-send (post the message)           C-c C-c  mail-send-and-exit
C-c C-f	 move to a header field (and create it if there isn't):
	 C-c C-f C-n  move to Newsgroups:	C-c C-f C-s  move to Subject:
	 C-c C-f C-f  move to Followup-To:      C-c C-f C-k  move to Keywords:
	 C-c C-f C-d  move to Distribution:	C-c C-f C-a  move to Summary:
C-c C-w  mail-signature (insert ~/.signature at end).
C-c C-y  mail-yank-original (insert current message, in News reader).
C-c C-q  mail-fill-yanked-message (fill what was yanked)."
  (lambda (buffer)
    (local-set-variable! send-mail-procedure
			 (lambda () (news-post-it))
			 buffer)))

(define-key 'compose-news '(#\c-c #\c-f #\c-a) 'news-move-to-summary)
(define-key 'compose-news '(#\c-c #\c-f #\c-d) 'news-move-to-distribution)
(define-key 'compose-news '(#\c-c #\c-f #\c-f) 'news-move-to-followup-to)
(define-key 'compose-news '(#\c-c #\c-f #\c-k) 'news-move-to-keywords)
(define-key 'compose-news '(#\c-c #\c-f #\c-n) 'news-move-to-newsgroups)

(define ((field-mover field))
  (set-current-point! (mail-position-on-field (current-buffer) field)))

(define-command news-move-to-newsgroups
  "Move point to end of Newsgroups: field."
  ()
  (field-mover "Newsgroups"))

(define-command news-move-to-followup-to
  "Move point to end of Followup-to: field."
  ()
  (field-mover "Followup-to"))

(define-command news-move-to-distribution
  "Move point to end of Distribution: field."
  ()
  (field-mover "Distribution"))

(define-command news-move-to-keywords
  "Move point to end of Keywords: field."
  ()
  (field-mover "Keywords"))

(define-command news-move-to-summary
  "Move point to end of Summary: field."
  ()
  (field-mover "Summary"))

(define (news-post-it)
  (let ((article-buffer (current-buffer)))
    (let ((temp-buffer
	   (prepare-mail-buffer-for-sending article-buffer
					    news-post-process-headers)))
      (if (let* ((start (buffer-start temp-buffer))
		 (end (mail-header-end start)))
	    (or (mail-field-start start end "To")
		(mail-field-start start end "CC")
		(mail-field-start start end "BCC")))
	  (let ((errors (send-mail-buffer temp-buffer article-buffer)))
	    (if errors
		(begin
		  (kill-buffer temp-buffer)
		  (editor-error errors)))))
      (let ((errors (post-news-buffer temp-buffer article-buffer)))
	(kill-buffer temp-buffer)
	(if errors (editor-error errors))))))

(define (post-news-buffer article-buffer lookup-buffer)
  (let ((do-it
	 (lambda (connection)
	   (let ((msg "Posting..."))
	     (message msg)
	     (let ((error
		    (nntp-connection:post-article
		     connection
		     (make-buffer-input-port (buffer-start article-buffer)
					     (buffer-end article-buffer)))))
	       (if error
		   (string-append msg "failed: " error)
		   (begin
		     (message msg "done")
		     #f)))))))
    (let ((server
	   (or (buffer-get lookup-buffer 'NEWS-SERVER #f)
	       (get-news-server-name #f))))
      (let ((server-buffer (find-news-server-buffer server)))
	(if server-buffer
	    (do-it (news-server-buffer:guarantee-connection server-buffer))
	    (let ((connection (open-nntp-connection server)))
	      (let ((result (do-it connection)))
		(nntp-connection:close connection)
		result)))))))

(define (news-post-process-headers start end)
  (let ((start (mark-left-inserting-copy start)))
    (if (not (mail-field-end start end "From"))
	(insert-string (news-post-default-from)
		       (mail-insert-field start "From")))
    (if (not (mail-field-end start end "Organization"))
	(let ((organization (news-post-default-organization)))
	  (if organization
	      (insert-string organization
			     (mail-insert-field start "Organization")))))
    (if (not (mail-field-end start end "Date"))
	(insert-string (news-post-default-date)
		       (mail-insert-field start "Date")))
    (if (not (mail-field-end start end "Subject"))
	(mail-insert-field start "Subject"))
    (if (not (mail-field-end start end "Lines"))
	(insert-string (number->string
			(count-lines (line-start end 1 'ERROR)
				     (group-end end)))
		       (mail-insert-field start "Lines")))
    (let ((region (mail-field-region start end "Newsgroups")))
      (if region
	  (news-post-canonicalize-newsgroups region)
	  (mail-insert-field start "Newsgroups")))
    (if (not (mail-field-end start end "Message-id"))
	(insert-string (news-post-default-message-id)
		       (mail-insert-field end "Message-id")))
    (if (not (mail-field-end start end "Path"))
	(insert-string (news-post-default-path)
		       (mail-insert-field end "Path")))
    (mark-temporary! start)))

(define (news-post-canonicalize-newsgroups region)
  (let ((start (mark-right-inserting-copy (region-start region)))
	(end (mark-left-inserting-copy (region-end region))))
    (let ((replace-regexp
	   (lambda (from to)
	     (let loop ((start start))
	       (let ((mark (re-search-forward from start end #f)))
		 (if mark
		     (loop (replace-match to))))))))
      (replace-regexp "\n[ \t]+" " ")
      (replace-regexp "[ \t]*,[ \t]*" ",")
      (replace-regexp "[ \t]+" ","))
    (mark-temporary! end)
    (mark-temporary! start)))

(define (news-post-default-path)
  (string-append (get-news-server-name #f) "!" (current-user-name)))

(define (news-post-default-from)
  (string-append (current-user-name)
		 "@"
		 (os/hostname)
		 (let ((full-name (ref-variable news-full-name #f)))
		   (if (string-null? full-name)
		       ""
		       (string-append " (" full-name ")")))))

(define (news-post-default-date)
  (file-time->string (current-file-time)))

(define (news-post-default-message-id)
  (string-append "<"
		 (current-user-name)
		 "."
		 (number->string (get-universal-time))
		 "@"
		 (os/hostname)
		 ">"))

(define (news-post-default-organization)
  (let ((organization (ref-variable news-organization #f)))
    (and (not (string-null? organization))
	 organization)))

;;;; INI File

(define (get-ini-file-groups connection)
  (let ((buffer (ini-file-buffer))
	(server (nntp-connection:server connection)))
    (let ((mark (ini-file-buffer:server-groups-entry buffer server)))
      (if mark
	  (map (lambda (entry)
		 (make-news-group-1 connection
				    (car entry)
				    (cadr entry)
				    (cddr entry)))
	       (ini-file-buffer:read-server-groups-entry mark server))
	  '()))))

(define (put-ini-file-groups connection groups)
  (let ((buffer (ini-file-buffer))
	(server (nntp-connection:server connection)))
    (let ((mark (ini-file-buffer:server-groups-entry buffer server))
	  (insert-groups
	   (lambda (mark)
	     (call-with-output-mark mark
	       (lambda (port)
		 (write-string "(server-groups " port)
		 (write server port)
		 (for-each (lambda (group)
			     (write-string "\n\t" port)
			     (write (cons* (news-group:name group)
					   (news-group:subscribed? group)
					   (news-group:ranges-seen group))
				    port))
			   groups)
		 (write-string "\n\t)" port))))))
      (cond (mark
	     (let ((mark (mark-right-inserting-copy mark)))
	       (set-buffer-major-mode! buffer (ref-mode-object scheme))
	       (delete-string mark (forward-sexp mark 1 'ERROR))
	       (if (not (null? groups))
		   (insert-groups mark))
	       (mark-temporary! mark))
	     (save-buffer buffer #f))
	    ((not (null? groups))
	     (let ((mark (mark-left-inserting-copy (buffer-end buffer))))
	       (if (mark= (buffer-start buffer) mark)
		   (insert-string ";;; -*-Scheme-*- News Reader INI file"
				  mark))
	       (guarantee-newlines 2 mark)
	       (insert-groups mark)
	       (mark-temporary! mark))
	     (save-buffer buffer #f))))))

(define (ini-file-buffer)
  (find-file-noselect (merge-pathnames "snr.ini" (current-home-directory)) #t))

(define (ini-file-buffer:server-groups-entry buffer server)
  (let ((end (buffer-end buffer)))
    (let loop ((start (buffer-start buffer)))
      (and (re-search-forward "^(server-groups[ \t]+\"\\([^\"]+\\)\""
			      start end #t)
	   (if (string-ci=? (extract-string (re-match-start 1)
					    (re-match-end 1))
			    server)
	       (re-match-start 0)
	       (loop (re-match-end 0)))))))

(define (ini-file-buffer:read-server-groups-entry mark server)
  (bind-condition-handler (list condition-type:error)
      (lambda (condition)
	condition
	(editor-error "Entry for "
		      server
		      " in "
		      (->namestring (buffer-pathname (mark-buffer mark)))
		      " is damaged."))
    (lambda ()
      (let ((entry (with-input-from-mark mark read)))
	(if (not (ini-file-buffer:valid-server-groups-entry? entry))
	    (error "Invalid server entry:" entry))
	(cddr entry)))))

(define (ini-file-buffer:valid-server-groups-entry? entry)
  (and (list? entry)
       (>= (length entry) 2)
       (eq? 'SERVER-GROUPS (car entry))
       (string? (cadr entry))
       (for-all? (cddr entry)
	 (lambda (entry)
	   (and (list? entry)
		(>= (length entry) 2)
		(string? (car entry))
		(boolean? (cadr entry))
		(for-all? (cddr entry) range?))))))

;;;; .newsrc File

(define (get-newsrc-file-groups connection)
  (parse-newsrc-buffer connection (newsrc-file-buffer connection)))

(define (parse-newsrc-buffer connection buffer)
  (let loop ((start (buffer-start buffer)) (groups '()))
    (let ((end (line-end start 0)))
      (let ((groups
	     (let ((mark (re-match-forward "^[^:! \t\n]+[:!]" start end)))
	       (if mark
		   (cons (make-news-group-1
			  connection
			  (extract-string start (mark-1+ mark))
			  (char=? #\: (extract-left-char mark))
			  (parse-newsrc-group-ranges mark end))
			 groups)
		   groups))))
	(if (group-end? end)
	    (reverse! groups)
	    (loop (mark1+ end) groups))))))

(define (parse-newsrc-group-ranges mark end)
  (let loop ((mark mark) (ranges '()))
    (if (re-match-forward "[, \t]*\\([0-9-]+\\)" mark end)
	(let ((s (re-match-start 1))
	      (e (re-match-end 1)))
	  (loop e
		(cond ((re-match-forward "[0-9]+" s e)
		       (cons (let ((n (extract-nonnegative-integer s e)))
			       (make-range n n))
			     ranges))
		      ((re-match-forward "\\([0-9]+\\)-\\([0-9]+\\)" s e)
		       (let ((n
			      (extract-nonnegative-integer (re-match-start 1)
							   (re-match-end 1)))
			     (m
			      (extract-nonnegative-integer (re-match-start 2)
							   (re-match-end 2))))
			 (if (< n m)
			     (cons (make-range n m) ranges)
			     ranges)))
		      (else
		       ranges))))
	(reverse! ranges))))

(define (extract-nonnegative-integer start end)
  (let loop ((mark start) (n 0))
    (if (mark= mark end)
	n
	(loop (mark1+ mark)
	      (+ (* n 10)
		 (fix:- (char->integer (extract-right-char mark))
			(char->integer #\0)))))))

(define (update-newsrc-group group)
  (let ((buffer (newsrc-file-buffer (news-group:connection group))))
    (let ((mark
	   (re-search-forward
	    (string-append "^"
			   (re-quote-string (news-group:name group))
			   "[:!]")
	    (buffer-start buffer)))
	  (finish
	   (lambda (mark)
	     (insert-char (if (news-group:subscribed? group) #\: #\!) mark)
	     (insert-char #\space mark)
	     (for-each
	      (lambda (range)
		(let ((f (range-first range))
		      (l (range-last range)))
		  (if (= f l)
		      (insert-string (number->string f) mark)
		      (begin
			(insert-string (number->string f) mark)
			(insert-char #\- mark)
			(insert-string (number->string l) mark)))))
	      (news-group:ranges-seen group))
	     (mark-temporary! mark))))
      (if mark
	  (let ((mark (mark-left-inserting-copy (mark-1+ mark))))
	    (delete-string mark (line-end mark 0))
	    (finish mark))
	  (let ((mark (mark-left-inserting-copy (buffer-end buffer))))
	    (guarantee-newline mark)
	    (insert-string (news-group:name group) mark)
	    (finish mark))))))

(define (newsrc-file-buffer connection)
  (find-file-noselect (os/newsrc-file-name (nntp-connection:server connection))
		      #f))

(define (os/newsrc-file-name server)
  (let ((homedir (user-homedir-pathname)))
    (if (os2/fs-long-filenames? homedir)
	(let ((specific
	       (merge-pathnames (string-append ".newsrc-" server)
				homedir)))
	  (if (file-exists? specific)
	      specific
	      (merge-pathnames ".newsrc" homedir)))
	(merge-pathnames "newsrc.ini" homedir))))

;;;; Miscellaneous

(define (repeating-command argument procedure key adjective)
  (let ((procedure
	 (lambda (group)
	   (procedure group)
	   (set-current-point! (line-start (current-point) 1 'ERROR)))))
    (if argument
	(for-each procedure
		  (next-n-property-items (command-argument-value argument)
					 key))
	(procedure (current-property-item key adjective)))))

(define (current-property-item key adjective)
  (let ((item (region-get (current-point) key #f)))
    (if (not item)
	(editor-error "Point isn't on a"
		      (if (memv (string-ref adjective 0)
				'(#\a #\e #\i #\o #\u))
			  "n"
			  "")
		      " "
		      adjective
		      " line."))
    item))

(define (next-n-property-items n key)
  (let loop ((start (line-start (current-point) 0)) (n n))
    (if (<= n 0)
	'()
	(cons (let ((item (region-get start key #f)))
		(if (not item)
		    (error "Missing property item:" key))
		item)
	      (let ((next (line-start start 1 #f)))
		(if next
		    (loop next (- n 1))
		    '()))))))

(define (property-item mark key adjective)
  (let ((item (region-get mark key #f)))
    (if (not item)
	(editor-error "Missing " adjective " property."))
    item))

(define (find-buffer-line buffer item key <)
  (let ((group (mark-group (buffer-absolute-start buffer))))
    (let loop
	((low (mark-index (buffer-absolute-start buffer)))
	 (high (mark-index (buffer-absolute-end buffer))))
      (and (fix:< low high)
	   (let ((index (fix:quotient (fix:+ low high) 2)))
	     (let ((item* (get-text-property group index key #f)))
	       (if (not item*)
		   (error "Missing text property:" key (buffer-name buffer)))
	       (cond ((eq? item item*)
		      (line-start (make-mark group index) 0))
		     ((< item item*)
		      (loop low index))
		     (else
		      (loop (fix:+ index 1) high)))))))))

;;;; Buffer Trees

(define (buffer-tree:parent buffer error?)
  (or (let ((node (buffer-tree:node buffer #f)))
	(and node
	     (car node)))
      (and error?
	   (error "Missing parent buffer:" (buffer-name buffer)))))

(define (buffer-tree:child buffer key error?)
  (or (let ((node (buffer-tree:node buffer #f)))
	(and node
	     (let ((entry (assq key (cdr node))))
	       (and entry
		    (cdr entry)))))
      (and error?
	   (error "Missing child buffer:" key (buffer-name buffer)))))

(define (buffer-tree:children buffer)
  (let ((node (buffer-tree:node buffer #f)))
    (if node
	(map cdr (cdr node))
	'())))

(define (buffer-tree:attach-child! parent key child)
  (let ((node (buffer-tree:node parent #t)))
    (let ((entry (assq key (cdr node))))
      (if entry
	  (set-cdr! entry child)
	  (set-cdr! node (cons (cons key child) (cdr node))))))
  (set-car! (buffer-tree:node child #t) parent))

(define (buffer-tree:node buffer intern?)
  (or (buffer-get buffer 'BUFFER-TREE #f)
      (and intern?
	   (let ((node (cons #f '())))
	     (buffer-put! buffer 'BUFFER-TREE node)
	     (add-kill-buffer-hook buffer buffer-tree:kill)
	     node))))

(define (buffer-tree:kill buffer)
  (let ((node (buffer-tree:node buffer #f)))
    (if node
	(begin
	  (let ((parent (car node)))
	    (if parent
		(let ((node (buffer-tree:node parent #f)))
		  (and node
		       (set-cdr! node
				 ((list-deletor!
				   (lambda (entry)
				     (eq? buffer (cdr entry))))
				  (cdr node)))))))
	  (for-each (lambda (child)
		      (let ((node (buffer-tree:node child #f)))
			(if node
			    (set-car! node #f))))
		    (map cdr (cdr node)))))))

;;;; News-Group Extensions

(define-structure (news-group-extra
		   (conc-name news-group-extra:)
		   (constructor make-news-group-extra ()))
  (subscribed? #f)
  (ranges-seen '()))

(define (get-news-group-extra group)
  (or (news-group:reader-hook group)
      (let ((extra (make-news-group-extra)))
	(set-news-group:reader-hook! group extra)
	extra)))

(define-integrable (news-group:subscribed? group)
  (news-group-extra:subscribed? (get-news-group-extra group)))

(define-integrable (set-news-group:subscribed?! group value)
  (set-news-group-extra:subscribed?! (get-news-group-extra group) value))

(define-integrable (news-group:ranges-seen group)
  (news-group-extra:ranges-seen (get-news-group-extra group)))

(define-integrable (set-news-group:ranges-seen! group value)
  (set-news-group-extra:ranges-seen! (get-news-group-extra group) value))

(define (make-news-group-1 connection name subscribed? ranges-seen)
  (let ((group (make-news-group connection name)))
    (set-news-group:subscribed?! group subscribed?)
    (set-news-group:ranges-seen! group (canonicalize-ranges ranges-seen))
    group))

(define (news-group:update-ranges! group)
  (news-group:update-probe! group)
  (if (news-group:active? group)
      (set-news-group:ranges-seen!
       group
       (clip-ranges! (news-group:ranges-seen group)
		     (news-group:first-article group)
		     (news-group:last-article group)))))

(define (news-group:number-of-articles group)
  (let ((n-seen (count-ranges (news-group:ranges-seen group))))
    (if (= n-seen 0)
	(news-group:estimated-n-articles group)
	(- (- (+ (news-group:last-article group) 1)
	      (news-group:first-article group))
	   n-seen))))

(define (news-group:article-seen! group header)
  (let ((do-it
	 (lambda (group number)
	   (set-news-group:ranges-seen!
	    group
	    (add-to-ranges! (news-group:ranges-seen group) number)))))
    (do-it group (news-header:number header))
    (for-each (let ((connection (news-group:connection group)))
		(lambda (xref)
		  (let ((group (find-news-group connection (car xref))))
		    (if (and group (news-group:subscribed? group))
			(do-it group (token->number (cdr xref)))))))
	      (news-header:xref header))))

(define (news-group:article-unseen! group header)
  (set-news-group:ranges-seen!
   group
   (remove-from-ranges! (news-group:ranges-seen group)
			(news-header:number header))))

;;;; Article Ranges

(define (range? object)
  (or (article-number? object)
      (and (pair? object)
	   (article-number? (car object))
	   (article-number? (cdr object))
	   (<= (car object) (cdr object)))))

(define (article-number? object)
  (and (exact-integer? object)
       (> object 0)))

(define (make-range f l) (if (= f l) f (cons f l)))
(define (range-first r)  (if (pair? r) (car r) r))
(define (range-last r)   (if (pair? r) (cdr r) r))
(define (range-length r) (if (pair? r) (+ (- (cdr r) (car r)) 1) 1))

(define (count-ranges ranges)
  (let loop ((ranges ranges) (count 0))
    (if (null? ranges)
	count
	(loop (cdr ranges) (+ count (range-length (car ranges)))))))

(define (canonicalize-ranges ranges)
  (if (null? ranges)
      ranges
      (let ((ranges
	     (sort ranges (lambda (x y) (< (range-first x) (range-first y))))))
	(let loop ((ranges ranges))
	  (if (not (null? (cdr ranges)))
	      (let ((x (car ranges))
		    (y (cadr ranges)))
		(if (<= (range-first y) (+ (range-last x) 1))
		    (begin
		      (set-car! ranges
				(make-range (range-first x)
					    (max (range-last x)
						 (range-last y))))
		      (set-cdr! ranges (cddr ranges))
		      (loop ranges))
		    (loop (cdr ranges))))))
	ranges)))

(define (clip-ranges! ranges first last)
  (let ((holder
	 (cons 'HOLDER
	       (let clip-first ((ranges ranges))
		 (cond ((or (null? ranges)
			    (<= first (range-first (car ranges))))
			ranges)
		       ((< (range-last (car ranges)) first)
			(clip-first (cdr ranges)))
		       (else
			(set-car! ranges
				  (make-range first (range-last (car ranges))))
			ranges))))))
    (let clip-last ((ranges (cdr holder)) (prev holder))
      (cond ((null? ranges)
	     unspecific)
	    ((< (range-last (car ranges)) last)
	     (clip-last (cdr ranges) ranges))
	    ((> (range-first (car ranges)) last)
	     (set-cdr! prev '()))
	    (else
	     (if (> (range-last (car ranges)) last)
		 (set-car! ranges
			   (make-range (range-first (car ranges))
				       last)))
	     (set-cdr! ranges '()))))
    (cdr holder)))

(define (complement-ranges ranges first last)
  (if (null? ranges)
      (list (make-range first last))
      (let loop
	  ((e (range-last (car ranges)))
	   (ranges (cdr ranges))
	   (result
	    (let ((s (range-first (car ranges))))
	      (if (< first s)
		  (list (make-range first (- s 1)))
		  '()))))
	(if (null? ranges)
	    (reverse! (if (< e last)
			  (cons (make-range (+ e 1) last) result)
			  result))
	    (loop (range-last (car ranges))
		  (cdr ranges)
		  (cons (make-range (+ e 1) (- (range-first (car ranges)) 1))
			result))))))

(define (add-to-ranges! ranges number)
  (let ((holder (cons 'HOLDER ranges)))
    (let loop ((ranges ranges) (prev holder))
      (if (null? ranges)
	  (set-cdr! prev (list (make-range number number)))
	  (let ((f (range-first (car ranges)))
		(l (range-last (car ranges))))
	    (cond ((> number (+ l 1))
		   (loop (cdr ranges) ranges))
		  ((< number (- f 1))
		   (set-cdr! prev (cons (make-range number number) ranges)))
		  (else
		   (let ((f (min f number))
			 (l (max l number)))
		     (if (and (not (null? (cdr ranges)))
			      (= (+ l 1) (range-first (cadr ranges))))
			 (begin
			   (set-car! ranges
				     (make-range f (range-last (cadr ranges))))
			   (set-cdr! ranges (cddr ranges)))
			 (set-car! ranges (make-range f l)))))))))
    (cdr holder)))

(define (remove-from-ranges! ranges number)
  (let ((holder (cons 'HOLDER ranges)))
    (let loop ((ranges ranges) (prev holder))
      (if (not (null? ranges))
	  (let ((f (range-first (car ranges)))
		(l (range-last (car ranges))))
	    (cond ((> number l)
		   (loop (cdr ranges) ranges))
		  ((>= number f)
		   (if (= number f)
		       (if (= number l)
			   (set-cdr! prev (cdr ranges))
			   (set-car! ranges (make-range (+ f 1) l)))
		       (if (= number l)
			   (set-car! ranges (make-range f (- l 1)))
			   (begin
			     (set-car! ranges (make-range (+ number 1) l))
			     (set-cdr! prev
				       (cons (make-range f (- number 1))
					     ranges))))))))))
    (cdr holder)))

(define (member-of-ranges? ranges number)
  (let loop ((ranges ranges))
    (and (not (null? ranges))
	 (or (<= (range-first (car ranges)) number (range-last (car ranges)))
	     (loop (cdr ranges))))))

(define (ranges->list ranges)
  (let loop ((ranges ranges) (result '()))
    (if (null? ranges)
	(reverse! result)
	(loop (cdr ranges)
	      (let ((e (range-last (car ranges))))
		(let loop ((n (range-first (car ranges))) (result result))
		  (let ((result (cons n result)))
		    (if (= n e)
			result
			(loop (+ n 1) result)))))))))

(define (for-each-range-element procedure ranges)
  (for-each (lambda (range)
	      (let ((e (+ (range-last range) 1)))
		(do ((n (range-first range) (+ n 1)))
		    ((= n e) unspecific)
		  (procedure n))))
	    ranges))

;;;; News-Header Extensions

(define-structure (news-header-extra
		   (conc-name news-header-extra:)
		   (constructor make-news-header-extra ()))
  (status #\space)
  (line-number '()))

(define (get-news-header-extra header)
  (or (news-header:reader-hook header)
      (let ((extra (make-news-header-extra)))
	(set-news-header:reader-hook! header extra)
	extra)))

(define news-header-extra-table
  (make-eq-hash-table))

(define-integrable (news-header:status header)
  (news-header-extra:status (get-news-header-extra header)))

(define-integrable (set-news-header:status! header value)
  (set-news-header-extra:status! (get-news-header-extra header) value))

(define-integrable (news-header:line-number header)
  (news-header-extra:line-number (get-news-header-extra header)))

(define-integrable (set-news-header:line-number! header value)
  (set-news-header-extra:line-number! (get-news-header-extra header) value))

(define (news-header:article-seen! header)
  (set-news-header:status! header #\D)
  (news-group:article-seen! (news-header:group header) header))

(define (news-header:article-unseen! header)
  (set-news-header:status! header #\space)
  (news-group:article-unseen! (news-header:group header) header))

(define (news-header:article-unseen? header)
  (char=? #\space (news-header:status header)))
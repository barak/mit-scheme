#| -*-Scheme-*-

$Id: imail-browser.scm,v 1.14 2007/01/05 21:19:25 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; IMAIL mail reader: folder browser

(declare (usual-integrations))

(define (imail-browse-container url)
  (select-buffer (get-imail-browser-buffer url)))

(define (get-imail-browser-buffer url)
  (or (list-search-positive (buffer-list)
	(lambda (buffer)
	  (eq? (selected-container-url #f buffer) url)))
      (let ((container (open-resource url))
	    (buffer
	     (new-buffer
	      (string-append (url-presentation-name url) "-browser"))))
	(set-buffer-imail-container! buffer container)
	(add-kill-buffer-hook buffer close-browser-container)
	(set-buffer-imail-url-selector! buffer browser-selected-url)
	(receive-modification-events container notice-container-events)
	(rebuild-imail-browser-buffer buffer)
	buffer)))

(define (close-browser-container buffer)
  (let ((container (selected-container #f buffer)))
    (if container
	(close-resource container #t))))

(define (browser-selected-url mark)
  (let ((info (browser-line-info #f mark)))
    (and info
	 (browser-line-info-url info))))

(define (rebuild-imail-browser-buffer buffer)
  (buffer-widen! buffer)
  (let ((container (selected-container #t buffer))
	(url (selected-url #f (buffer-point buffer)))
	(marks (all-marked-urls buffer))
	(expanded (all-expanded-containers buffer)))
    (with-read-only-defeated (buffer-start buffer)
      (lambda ()
	(region-delete! (buffer-region buffer))
	(let ((container-url (resource-locator container))
	      (mark (mark-left-inserting-copy (buffer-start buffer))))
	  (let ((title (url->string container-url)))
	    (insert-string title mark)
	    (insert-newline mark)
	    (insert-chars #\- (string-length title) mark)
	    (insert-newline mark))
	  (let ((point (mark-right-inserting-copy mark)))
	    (insert-browser-lines container-url container-url mark)
	    (set-buffer-point! buffer point)
	    (mark-temporary! point)))))
    (set-buffer-major-mode! buffer (ref-mode-object imail-browser))
    (buffer-not-modified! buffer)
    (set-buffer-read-only! buffer)
    (set-all-expanded-containers! buffer expanded)
    (set-all-marked-urls! buffer marks)
    (if url
	(call-with-values (lambda () (find-browser-line-for url buffer))
	  (lambda (mark match?)
	    match?
	    (set-buffer-point! buffer mark))))))

(define (insert-browser-lines container-1 container-2 mark)
  (for-each (lambda (subfolder-url)
	      (insert-browser-line subfolder-url container-2 mark))
	    (sort (container-url-contents container-1) browser-url<?)))

(define (insert-browser-line url container-url mark)
  (let ((info (make-browser-line-info url)))
    (with-region-marked mark
      (lambda (start end)
	(region-put! start end 'IMAIL-BROWSER-LINE-INFO info))
      (lambda ()
	(insert-string "  " mark)
	(insert-chars #\space (* 4 (browser-url-depth url container-url)) mark)
	(with-region-marked mark
	  (lambda (start end)
	    (set-region-local-comtabs!
	     (make-region start end)
	     (let ((comtab (make-comtab)))
	       (define-key comtab button1-down
		 (ref-command-object imail-browser-mouse-toggle-container))
	       (list comtab))))
	  (lambda ()
	    (insert-char (if (browser-line-info-container-url info)
			     #\+
			     #\space)
			 mark)))
	(insert-char #\space mark)
	(insert-string (url-content-name url) mark)
	(insert-newline mark)))))

(define (update-container-line-marker mark char)
  (replace-right-char
   (mark+ (line-start mark 0)
	  (+ 2
	     (* 4
		(browser-url-depth
		 (selected-url #t mark)
		 (resource-locator
		  (selected-container #t (mark-buffer mark)))))))
   char))

(define (browser-url-depth url container)
  (let loop ((url* url))
    (let ((container* (container-url url*)))
      (if (eq? container* container)
	  0
	  (begin
	    (if (eq? container* url*)
		(error "URL not in container:" url container))
	    (+ 1 (loop container*)))))))

(define (make-browser-line-info url)
  (vector url (url-corresponding-container url) #f))

(define (browser-line-info-url info)
  (vector-ref info 0))

(define (browser-line-info-container-url info)
  (vector-ref info 1))

(define (browser-line-info-container-expanded? info)
  (vector-ref info 2))

(define (browser-line-info-container-expanded! info)
  (vector-set! info 2 #t))

(define (browser-line-info-container-collapsed! info)
  (vector-set! info 2 #f))

(define (browser-line-info #!optional error? mark)
  (or (region-get (if (or (default-object? mark) (not mark))
		      (current-point)
		      mark)
		  'IMAIL-BROWSER-LINE-INFO
		  #f)
      (and (if (default-object? error?) #t error?)
	   (editor-error "Point not on browser line."))))

(define (notice-container-events container type arguments)
  (for-each
   (lambda (buffer)
     (case type
       ((CREATE-RESOURCE)
	(let ((url (car arguments)))
	  (with-buffer-open buffer
	    (lambda ()
	      (call-with-values
		  (lambda () (find-browser-line-for url buffer))
		(lambda (mark match?)
		  (let ((mark (mark-left-inserting-copy mark)))
		    (if match?
			(delete-string mark (line-start mark 1 'LIMIT)))
		    (insert-browser-line url
					 (resource-locator
					  (selected-container #t buffer))
					 mark)
		    (mark-temporary! mark))))))))
       ((DELETE-RESOURCE)
	(let ((url (car arguments)))
	  (with-buffer-open buffer
	    (lambda ()
	      (call-with-values
		  (lambda () (find-browser-line-for url buffer))
		(lambda (mark match?)
		  (if match?
		      (delete-string mark
				     (line-start mark 1 'LIMIT)))))))))))
   (find-browsers-for container)))

(define (find-browsers-for container)
  (list-transform-positive (buffer-list)
    (lambda (buffer)
      (or (eq? (selected-container #f buffer) container)
	  (memq container (browser-expanded-containers buffer))))))

(define (browser-expanded-containers buffer)
  (buffer-get buffer 'IMAIL-BROWSER-EXPANDED-CONTAINERS '()))

(define (add-browser-expanded-container! buffer url)
  (let ((container (open-resource url)))
    (receive-modification-events container notice-container-events)
    (buffer-put! buffer
		 'IMAIL-BROWSER-EXPANDED-CONTAINERS
		 (let ((containers (browser-expanded-containers buffer)))
		   (if (memq container containers)
		       containers
		       (cons container containers))))))

(define (remove-browser-expanded-container! buffer url)
  (let ((container (get-memoized-resource url #f)))
    (if container
	(begin
	  (close-resource container #f)
	  (buffer-put! buffer
		       'IMAIL-BROWSER-EXPANDED-CONTAINERS
		       (delq! container
			      (browser-expanded-containers buffer)))))))

(define (find-browser-line-for url buffer)
  (let loop ((mark (buffer-start buffer)))
    (if (group-end? mark)
	(values mark #f)
	(let ((url* (selected-url #f mark)))
	  (cond ((not url*) (loop (line-start mark 1 'LIMIT)))
		((eq? url* url) (values mark #t))
		((browser-url<? url url*) (values mark #f))
		(else (loop (line-start mark 1 'LIMIT))))))))

(define (browser-url<? url1 url2)
  (string<? (url->string url1) (url->string url2)))

(define (url-contained? url1 url2)
  (let loop ((url url1))
    (or (eq? url url2)
	(let ((url* (container-url url)))
	  (and (not (eq? url* url))
	       (loop url*))))))

(define (with-buffer-open buffer thunk)
  (without-text-clipped buffer
    (lambda ()
      (with-read-only-defeated buffer
	(lambda ()
	  (let ((value (thunk)))
	    (buffer-not-modified! buffer)
	    value))))))

(define (with-region-marked mark marker thunk)
  (let ((start (mark-right-inserting-copy mark)))
    (let ((value (thunk)))
      (marker start mark)
      (mark-temporary! start)
      value)))

(define (mouse-command-mark)
  (let ((button-event (current-button-event)))
    (let ((window (button-event/window button-event)))
      (or (window-coordinates->mark window
				    (button-event/x button-event)
				    (button-event/y button-event))
	  (buffer-end (window-buffer window))))))

(define (replace-right-char mark char)
  (group-replace-char! (mark-group mark)
		       (mark-index mark)
		       char))

(define-major-mode imail-browser read-only "IMAIL Browser"
  "Major mode in effect in IMAIL folder browser.
Each line summarizes a single mail folder (or container).
You can move using the usual cursor motion commands.

Type \\[imail-browser-flag-folder-deletion] to flag a folder for Deletion.
Type \\[imail-browser-mark] to Mark a folder for later commands.
  Most commands operate on the marked folders and use the current folder
  if no folders are marked.  Use a numeric prefix argument to operate on
  the next ARG (or previous -ARG if ARG<0) folders, or just `1'
  to operate on the current folder only.  Prefix arguments override marks.
Type \\[imail-browser-unmark] to Unmark a folder.
Type \\[imail-browser-unmark-backward] to back up one line and unmark.
Type \\[imail-browser-do-flagged-delete] to eXecute the deletions requested.
Type \\[imail-browser-view-selected-folder] to Find the current line's folder
  (or browse it in another buffer, if it is a container).
Type \\[imail-browser-view-selected-container] to browse the current line's container in another buffer.
Type \\[imail-browser-view-container] to browse this container's container.
Type \\[imail-browser-toggle-container] to show the contents of this line's container in the buffer,
  or hide them if they are already shown.
Type \\[imail-browser-do-rename] to rename a folder or move the marked folders to another container.
Type \\[imail-browser-do-copy] to copy folders.
Type \\[imail-browser-revert] to read the container again.  This discards all deletion-flags.

\\{imail-browser}"
  (lambda (buffer)
    (local-set-variable! truncate-lines #t buffer)
    (buffer-put! buffer 'REVERT-BUFFER-METHOD imail-browser-revert-buffer)
    (event-distributor/invoke! (ref-variable imail-browser-mode-hook buffer)
			       buffer)))

(define-variable imail-browser-mode-hook
  "An event distributor that is invoked when entering IMAIL Browser mode."
  (make-event-distributor))

(define (imail-browser-revert-buffer buffer dont-use-auto-save? dont-confirm?)
  dont-use-auto-save?
  (if (or dont-confirm? (prompt-for-yes-or-no? "Revert IMAIL browser buffer"))
      (rebuild-imail-browser-buffer buffer)))

(define-key 'imail-browser #\+ 'imail-create-folder)
(define-key 'imail-browser #\C 'imail-browser-do-copy)
(define-key 'imail-browser #\D 'imail-browser-do-delete)
(define-key 'imail-browser #\R 'imail-browser-do-rename)

(define-key 'imail-browser #\? 'describe-mode)
(define-key 'imail-browser #\c 'imail-browser-view-selected-container)
(define-key 'imail-browser #\d 'imail-browser-flag-folder-deletion)
(define-key 'imail-browser #\f 'imail-browser-view-selected-folder)
(define-key 'imail-browser #\g 'imail-browser-revert)
(define-key 'imail-browser #\h 'describe-mode)
(define-key 'imail-browser #\m 'imail-browser-mark)
(define-key 'imail-browser #\q 'imail-browser-quit)
(define-key 'imail-browser #\t 'imail-browser-toggle-container)
(define-key 'imail-browser #\u 'imail-browser-unmark)
(define-key 'imail-browser #\x 'imail-browser-do-flagged-delete)
(define-key 'imail-browser #\^ 'imail-browser-view-container)

(define-key 'imail-browser #\return 'imail-browser-view-selected-folder)
(define-key 'imail-browser #\rubout 'imail-browser-unmark-backward)
(define-key 'imail-browser #\M-rubout 'imail-browser-unmark-all-folders)

(define-command imail-browser-view-selected-folder
  "Visit the folder or container named on this line.
If this line names a resource that is both a folder and a container,
this command visits it as a folder."
  ()
  (lambda ()
    (let ((url (selected-url)))
      (if (folder-url? url)
	  ((ref-command imail) (url->string url))
	  (editor-error "Not a selectable folder.")))))

(define-command imail-browser-view-selected-container
  "Browse the container named on this line."
  ()
  (lambda ()
    (let ((info (browser-line-info)))
      (let ((container (browser-line-info-container-url info)))
	(if container
	    (imail-browse-container container)
	    (editor-error "Not a selectable container."))))))

(define-command imail-browser-view-container
  "Browse the container of the resource being viewed in this buffer.
With prefix arg, prompt for the container to browse."
  (lambda ()
    (list
     (and (command-argument)
	  (prompt-for-container "Browse IMAIL container" #f
				'HISTORY 'IMAIL-BROWSER-VIEW-CONTAINER
				'REQUIRE-MATCH? #t))))
  (lambda (url-string)
    (imail-browse-container
     (or (and url-string (imail-parse-partial-url url-string))
	 (let ((resource
		(or (selected-container #f)
		    (selected-folder #f))))
	   (if resource
	       (container-url-for-prompt resource)
	       (editor-error "This is not an IMAIL buffer.")))))))

(define-command imail-browser-mouse-toggle-container
  "Show the contents of the container pointed at.
Like \\[imail-browser-toggle-container] except that the container is
selected by the position of the mouse rather than point."
  ()
  (lambda ()
    ((ref-command imail-browser-toggle-container) (mouse-command-mark))))

(define-command imail-browser-toggle-container
  "Show the contents of the container named by this line.
The contents are inserted immediately after this line,
indented slightly to indicate where they are contained.
If the containers contents are currently shown, then hide them instead."
  "d"
  (lambda (mark)
    (let ((info (browser-line-info #t mark)))
      (if (not (browser-line-info-container-url info))
	  (editor-error "Not on a container line."))
      (if (browser-line-info-container-expanded? info)
	  (browser-collapse-container info mark)
	  (browser-expand-container info mark)))))

(define (browser-expand-container info mark)
  (let ((container (browser-line-info-container-url info))
	(buffer (mark-buffer mark)))
    (with-buffer-open buffer
      (lambda ()
	(let ((mark (mark-left-inserting-copy (line-start mark 1 'LIMIT))))
	  (insert-browser-lines container
				(selected-container-url #t buffer)
				mark)
	  (mark-temporary! mark))
	(update-container-line-marker mark #\-)
	(add-browser-expanded-container! buffer container)
	(browser-line-info-container-expanded! info)))))

(define (browser-collapse-container info mark)
  (let ((container (browser-line-info-container-url info))
	(buffer (mark-buffer mark)))
    (with-buffer-open buffer
      (lambda ()
	(let ((start (line-start mark 1 'LIMIT)))
	  (let loop ((end start))
	    (if (and (not (group-end? end))
		     (let ((url (selected-url #f end)))
		       (and url
			    (url-contained? url container))))
		(loop (line-start end 1 'LIMIT))
		(delete-string start end))))
	(update-container-line-marker mark #\+)
	(remove-browser-expanded-container! buffer container)
	(browser-line-info-container-collapsed! info)))))

(define-command imail-browser-revert
  "Re-read the contents of the buffer."
  ()
  (lambda () (revert-buffer (selected-buffer) #t #t)))

(define-command imail-browser-quit
  "Kill the selected buffer.
Discards any pending changes."
  ()
  (lambda () (kill-buffer-interactive (selected-buffer))))

(define-command imail-browser-flag-folder-deletion
  "Mark the folder under point to be deleted.
With prefix argument, mark the next N folders for deletion."
  "p"
  (lambda (n) (imail-browser-mark-lines n #\D)))

(define-command imail-browser-mark
  "Mark the current (or next ARG) folder.
Use \\[imail-browser-unmark-all-folders] to remove all marks."
  "p"
  (lambda (n) (imail-browser-mark-lines n #\*)))

(define-command imail-browser-unmark
  "Unmark the current (or next ARG) folders."
  "p"
  (lambda (n) (imail-browser-mark-lines n #\space)))

(define-command imail-browser-unmark-backward
  "Move up lines and remove marks there.
Optional prefix ARG says how many lines to unmark; default is one line."
  "p"
  (lambda (n) ((ref-command imail-browser-unmark) (- n))))

(define-command imail-browser-unmark-all-folders
  "Remove a specific mark (or any mark) from every folder.
After this command, type the mark character to remove, 
or type RET to remove all marks."
  "cRemove marks (RET means all)"
  (lambda (mark-char)
    (let ((buffer (selected-buffer)))
      (with-buffer-open buffer
	(lambda ()
	  (let loop ((mark (line-start (buffer-start buffer) 0)))
	    (if (not (group-end? mark))
		(begin
		  (if (and (or (char=? mark-char #\return)
			       (char=? (extract-right-char mark) mark-char))
			   (selected-url #f mark))
		      (replace-right-char mark #\space))
		  (let ((mark (line-start mark 1 #f)))
		    (if mark
			(loop mark)))))))))))

(define (imail-browser-mark-lines n mark-char)
  (with-buffer-open (selected-buffer)
    (lambda ()
      (cond ((> n 0)
	     (let loop ((n n) (mark (line-start (current-point) 0)))
	       (if (selected-url #f mark)
		   (begin
		     (replace-right-char mark mark-char)
		     (let ((mark (line-start mark 1 'ERROR)))
		       (set-current-point! mark)
		       (if (> n 1)
			   (loop (- n 1) mark))))
		   (editor-failure))))
	    ((< n 0)
	     (let loop ((n n) (mark (line-start (current-point) -1 'ERROR)))
	       (set-current-point! mark)
	       (if (selected-url #f mark)
		   (begin
		     (replace-right-char mark mark-char)
		     (if (< n -1)
			 (loop (+ n 1) (line-start mark -1 'ERROR))))
		   (editor-failure))))))))

(define-command imail-browser-do-copy
  "Copy all marked (or next ARG) folders, or copy the current folder.
When operating on just the current folder, you specify the new name.
When operating on multiple or marked folders, you specify a container,
and new copies of these folders are made in that container
with the same names that the folders currently have."
  "P"
  (lambda (argument)
    (browser-transfer-resources "copy" "copied" argument copy-folder)))

(define-command imail-browser-do-rename
  "Rename current folder or all marked (or next ARG) folders.
When renaming just the current folder, you specify the new name.
When renaming multiple or marked folders, you specify a container."
  "P"
  (lambda (argument)
    (browser-transfer-resources "rename" "renamed" argument rename-resource)))

(define (browser-transfer-resources present-tense past-tense argument
				    operation)
  (call-with-values (lambda () (browser-url-list argument (current-point)))
    (lambda (mark urls)
      (cond ((not (pair? urls))
	     (message "No folders to " present-tense "."))
	    ((pair? (cdr urls))
	     (let ((container
		    (imail-parse-partial-url
		     (prompt-for-container (string-append
					    (string-capitalize present-tense)
					    " folders into")
					   #f
					   'HISTORY 'IMAIL-BROWSER-TRANSFER-N
					   'HISTORY-INDEX 0))))
	       (for-each
		(lambda (url)
		  (operation url
			     (make-content-url container
					       (url-content-name url))))
		urls)
	       (message "Folders " past-tense " into "
			(url->string container))))
	    (else
	     (let* ((url (car urls))
		    (new-url
		     (imail-parse-partial-url
		      (prompt-for-url (string-append
				       (string-capitalize present-tense)
				       " folder to")
				      #f
				      'HISTORY 'IMAIL-BROWSER-TRANSFER-1
				      'HISTORY-INDEX 0)))
		    (new-url
		     (if (container-url? new-url)
			 (make-content-url new-url (url-content-name url))
			 new-url)))
	       (operation url new-url)
	       (message "Folder " past-tense " to " (url->string new-url)))))
      (set-current-point! mark)
      (mark-temporary! mark))))

(define-command imail-browser-do-flagged-delete
  "Delete the folders that are flagged for deletion."
  ()
  (lambda ()
    (let ((buffer (selected-buffer)))
      (with-buffer-open buffer
	(lambda ()
	  (browser-internal-do-delete (browser-marked-urls buffer #\D)))))))

(define-command imail-browser-do-delete
  "Delete all marked (or next ARG) folders."
  "P"
  (lambda (argument)
    (with-buffer-open (selected-buffer)
      (lambda ()
	(call-with-values
	    (lambda () (browser-url-list argument (current-point)))
	  (lambda (mark urls)
	    (browser-internal-do-delete urls)
	    (set-current-point! mark)
	    (mark-temporary! mark)))))))

(define (browser-internal-do-delete urls)
  (if (pair? urls)
      (if (if (pair? (cdr urls))
	      (cleanup-pop-up-buffers
	       (lambda ()
		 (browser-pop-up-urls-window urls)
		 (prompt-for-yes-or-no? "Delete these folders")))
	      (prompt-for-yes-or-no?
	       (string-append "Delete folder " (url->string (car urls)))))
	  (for-each delete-resource urls))))

(define (browser-pop-up-urls-window urls)
  (pop-up-temporary-buffer " *imail-browser-folders*"
			   '(READ-ONLY SHRINK-WINDOW)
    (lambda (buffer window)
      (local-set-variable! truncate-partial-width-windows #f buffer)
      (write-strings-densely
       (map url->string urls)
       (mark->output-port (buffer-point buffer))
       (window-x-size (or window (car (buffer-windows buffer))))))))

(define (browser-url-list argument mark)
  (if argument
      (browser-next-n-urls (command-argument-numeric-value argument) mark)
      (values (mark-left-inserting-copy (line-start mark 0))
	      (let ((urls (browser-marked-urls (mark-buffer mark) #\*)))
		(if (pair? urls)
		    urls
		    (list (selected-url #t mark)))))))

(define (browser-marked-urls buffer mark-char)
  (let loop ((mark (buffer-start buffer)) (result '()))
    (let ((char (extract-right-char mark)))
      (if char
	  (loop (line-start mark 1 'ERROR)
		(let ((url
		       (and (eq? char mark-char)
			    (selected-url #f mark))))
		  (if url
		      (cons url result)
		      result)))
	  (reverse! result)))))

(define (browser-next-n-urls n mark)
  (cond ((> n 0)
	 (let loop ((n n) (mark (line-start mark 0)) (urls '()))
	   (let ((n (- n 1))
		 (mark (line-start mark 1 'ERROR))
		 (urls (cons (selected-url #t mark) urls)))
	     (if (> n 0)
		 (loop n mark urls)
		 (values (mark-left-inserting-copy mark)
			 (reverse! urls))))))
	((< n 0)
	 (let loop ((n n) (mark (line-start mark -1 'ERROR)) (urls '()))
	   (let ((n (+ n 1))
		 (urls (cons (selected-url #t mark) urls)))
	     (if (< n 0)
		 (loop n (line-start mark -1 'ERROR) urls)
		 (values (mark-right-inserting-copy mark)
			 urls)))))
	(else
	 (values (mark-left-inserting-copy (line-start mark 0))
		 '()))))

(define (all-marked-urls buffer)
  (let loop ((mark (buffer-start buffer)) (result '()))
    (let ((char (extract-right-char mark)))
      (if char
	  (loop (line-start mark 1 'ERROR)
		(let ((url (selected-url #f mark)))
		  (if url
		      (cons (cons char url) result)
		      result)))
	  (reverse! result)))))

(define (set-all-marked-urls! buffer alist)
  (with-buffer-open buffer
    (lambda ()
      (for-each (lambda (c.u)
		  (call-with-values
		      (lambda () (find-browser-line-for (cdr c.u) buffer))
		    (lambda (mark match?)
		      (if match?
			  (replace-right-char mark (car c.u))))))
		alist))))

(define (all-expanded-containers buffer)
  (let loop ((mark (buffer-start buffer)) (result '()))
    (let ((result
	   (let ((info (browser-line-info #f mark)))
	     (if (and info (browser-line-info-container-expanded? info))
		 (cons (browser-line-info-container-url info) result)
		 result)))
	  (mark (line-start mark 1 #f)))
      (if mark
	  (loop mark result)
	  (sort result browser-url<?)))))

(define (set-all-expanded-containers! buffer urls)
  ;; URLS is sorted so that all containers appear before their contents.
  (for-each
   (lambda (url)
     (call-with-values (lambda () (find-browser-line-for url buffer))
       (lambda (mark match?)
	 (if match?
	     (browser-expand-container (browser-line-info #t mark) mark)))))
   urls))
;;; -*-Scheme-*-
;;;
;;; $Id: imail-browser.scm,v 1.1 2001/05/26 03:00:53 cph Exp $
;;;
;;; Copyright (c) 2001 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

;;;; IMAIL mail reader: folder browser

#|

To do:

* Change revert command to preserve the position of point as well as
  possible.

* Change revert command to preserve which folders are expanded and
  collapsed.

* Change commands to operate on marked folders if any are marked:

	imail-create-folder
	imail-copy-folder
	imail-rename-folder

|#

(declare (usual-integrations))

(define-command imail-browse-container
  "Visit a buffer showing the contents of an IMAIL container."
  (lambda ()
    (list (prompt-for-container "Browse container" #f
				'HISTORY 'IMAIL-BROWSE-CONTAINER
				'HISTORY-INDEX 0
				'REQUIRE-MATCH? #t)))
  (lambda (url-string)
    (let* ((url (imail-parse-partial-url url-string))
	   (container (open-resource url))
	   (buffer
	    (new-buffer
	     (string-append (url-presentation-name url)
			    "-browser"))))
      (set-buffer-imail-container! buffer container)
      (set-buffer-imail-url-selector! buffer browser-selected-url)
      (receive-modification-events container notice-container-events)
      (rebuild-imail-browser-buffer buffer)
      (select-buffer buffer))))

(define (browser-selected-url mark)
  (let ((info (browser-line-info #f mark)))
    (and info
	 (browser-line-info-url info))))

(define (rebuild-imail-browser-buffer buffer)
  (let ((container (selected-container #t buffer)))
    (buffer-widen! buffer)
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
	  (insert-browser-lines container-url container-url mark))))
    (set-buffer-major-mode! buffer (ref-mode-object imail-browser))
    (buffer-not-modified! buffer)
    (set-buffer-read-only! buffer)
    (set-buffer-point! buffer (buffer-start buffer))))

(define (insert-browser-lines container-1 container-2 mark)
  (for-each (lambda (subfolder-url)
	      (insert-browser-line subfolder-url container-2 mark))
	    (sort (container-url-contents container-1) browser-url<?)))

(define (insert-browser-line url container-url mark)
  (let ((start (mark-right-inserting-copy mark))
	(info (make-browser-line-info url)))
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
  (vector url (url-is-container? url) #f))

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

(define (add-browser-expanded-container! buffer container)
  (buffer-put! buffer
	       'IMAIL-BROWSER-EXPANDED-CONTAINERS
	       (let ((containers (browser-expanded-containers buffer)))
		 (if (memq container containers)
		     containers
		     (cons container containers)))))

(define (remove-browser-expanded-container! buffer container)
  (buffer-put! buffer
	       'IMAIL-BROWSER-EXPANDED-CONTAINERS
	       (delq! container (browser-expanded-containers buffer))))

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
Each line summarizes a single mail folder.

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
(define-key 'imail-browser #\C 'imail-copy-folder)
(define-key 'imail-browser #\R 'imail-rename-folder)

(define-key 'imail-browser #\? 'describe-mode)
(define-key 'imail-browser #\c 'imail-browser-view-container)
(define-key 'imail-browser #\d 'imail-browser-flag-folder-deletion)
(define-key 'imail-browser #\f 'imail-browser-view-folder)
(define-key 'imail-browser #\g 'imail-browser-revert)
(define-key 'imail-browser #\h 'describe-mode)
(define-key 'imail-browser #\m 'imail-browser-mark-folder)
(define-key 'imail-browser #\q 'imail-browser-quit)
(define-key 'imail-browser #\t 'imail-browser-toggle-container)
(define-key 'imail-browser #\u 'imail-browser-unmark)
(define-key 'imail-browser #\x 'imail-browser-do-deletions)

(define-key 'imail-browser #\rubout 'imail-browser-backup-unmark)
(define-key 'imail-browser #\M-rubout 'imail-browser-unmark-all-folders)

(define-command imail-browser-view-folder
  ""
  ()
  (lambda ()
    (let ((url (selected-url)))
      (if (folder-url? url)
	  ((ref-command imail) (url->string url))
	  (editor-error "Not a selectable folder.")))))

(define-command imail-browser-view-container
  ""
  ()
  (lambda ()
    (let ((url (selected-url)))
      (let ((container (url-is-container? url)))
	(if container
	    ((ref-command imail-browse-container) (url->string container))
	    (editor-error "Not a selectable container."))))))

(define-command imail-browser-mouse-toggle-container
  ""
  ()
  (lambda ()
    ((ref-command imail-browser-toggle-container) (mouse-command-mark))))

(define-command imail-browser-toggle-container
  ""
  "d"
  (lambda (mark)
    (let ((buffer (mark-buffer mark))
	  (info (browser-line-info #t mark)))
      (let ((container (browser-line-info-container-url info)))
	(if (not container)
	    (editor-error "Not on a container line."))
	(with-buffer-open buffer
	  (lambda ()
	    (if (browser-line-info-container-expanded? info)
		(let ((start (line-start mark 1 'LIMIT)))
		  (let loop ((end start))
		    (if (and (not (group-end? end))
			     (let ((url (selected-url #f end)))
			       (and url
				    (url-contained? url container))))
			(loop (line-start end 1 'LIMIT))
			(delete-string start end)))
		  (update-container-line-marker mark #\+)
		  (let ((container (get-memoized-resource container #f)))
		    (if container
			(remove-browser-expanded-container! buffer container)))
		  (browser-line-info-container-collapsed! info))
		(begin
		  (let ((mark
			 (mark-left-inserting-copy
			  (line-start mark 1 'LIMIT))))
		    (insert-browser-lines container
					  (selected-container-url #t buffer)
					  mark)
		    (mark-temporary! mark))
		  (update-container-line-marker mark #\-)
		  (let ((container (open-resource container)))
		    (receive-modification-events container
						 notice-container-events)
		  (add-browser-expanded-container! buffer container))
		  (browser-line-info-container-expanded! info)))))))))

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

(define-command imail-browser-mark-folder
  ""
  "p"
  (lambda (n) (imail-browser-mark-lines n #\*)))

(define-command imail-browser-unmark
  ""
  "p"
  (lambda (n) (imail-browser-mark-lines n #\space)))

(define-command imail-browser-backup-unmark
  ""
  "p"
  (lambda (n) ((ref-command imail-browser-unmark) (- n))))

(define-command imail-browser-unmark-all-folders
  ""
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

(define-command imail-browser-do-deletions
  "Delete each folder that is marked for deletion."
  ()
  (lambda ()
    (let ((buffer (selected-buffer)))
      (with-buffer-open buffer
	(lambda ()
	  (let ((urls (browser-marked-urls buffer #\D)))
	    (if (and (pair? urls)
		     (cleanup-pop-up-buffers
		      (lambda ()
			(browser-pop-up-urls-window urls)
			(prompt-for-yes-or-no? "Delete these folders"))))
		(for-each delete-resource urls))))))))

(define (browser-pop-up-urls-window urls)
  (pop-up-temporary-buffer " *imail-browser-folders*"
			   '(READ-ONLY SHRINK-WINDOW)
    (lambda (buffer window)
      (local-set-variable! truncate-partial-width-windows #f buffer)
      (write-strings-densely
       (map url->string urls)
       (mark->output-port (buffer-point buffer))
       (window-x-size (or window (car (buffer-windows buffer))))))))

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
;;; -*-Scheme-*-
;;;
;;;	$Id: buffer.scm,v 1.166 1994/11/01 22:25:42 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-94 Massachusetts Institute of Technology
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

;;;; Buffer Abstraction

(declare (usual-integrations))

(define-named-structure "Buffer"
  name
  group
  mark-ring
  modes
  comtabs
  windows
  display-start
  default-directory
  pathname
  truename
  alist
  local-bindings
  local-bindings-installed?
  auto-save-pathname
  auto-saved?
  save-length
  backed-up?
  modification-time
  )

(unparser/set-tagged-vector-method!
 %buffer-tag
 (unparser/standard-method 'BUFFER
   (lambda (state buffer)
     (unparse-object state (buffer-name buffer)))))

(define-variable buffer-creation-hook
  "An event distributor that is invoked when a new buffer is created.
The new buffer is passed as its argument.
The buffer is guaranteed to be deselected at that time."
  (make-event-distributor))

(define (make-buffer name mode directory)
  (let ((buffer (%make-buffer)))
    (let ((group (make-group buffer)))
      (vector-set! buffer buffer-index:name name)
      (vector-set! buffer buffer-index:group group)
      (add-group-clip-daemon! group (buffer-clip-daemon buffer))
      (%buffer-reset! buffer)
      (vector-set! buffer buffer-index:windows '())
      (vector-set! buffer buffer-index:display-start #f)
      (vector-set! buffer buffer-index:default-directory directory)
      (vector-set! buffer buffer-index:local-bindings '())
      (vector-set! buffer buffer-index:local-bindings-installed? #f)
      (%set-buffer-major-mode! buffer mode)
      (event-distributor/invoke!
       (variable-default-value (ref-variable-object buffer-creation-hook))
       buffer)
      buffer)))

(define (%buffer-reset! buffer)
  (let ((group (buffer-group buffer)))
    (disable-group-undo! group)
    (if (not (minibuffer? buffer))
	(enable-group-undo! group)))
  (vector-set!
   buffer
   buffer-index:mark-ring
   (make-ring
    (variable-default-value (ref-variable-object mark-ring-maximum))))
  (ring-push! (buffer-mark-ring buffer) (buffer-start buffer))
  (vector-set! buffer buffer-index:pathname #f)
  (vector-set! buffer buffer-index:truename #f)
  (vector-set! buffer buffer-index:auto-save-pathname #f)
  (vector-set! buffer buffer-index:auto-saved? #f)
  (vector-set! buffer buffer-index:save-length 0)
  (vector-set! buffer buffer-index:backed-up? #f)
  (vector-set! buffer buffer-index:modification-time #f)
  (vector-set! buffer buffer-index:alist '()))

(define (buffer-modeline-event! buffer type)
  (let loop ((windows (buffer-windows buffer)))
    (if (not (null? windows))
	(begin
	  (window-modeline-event! (car windows) type)
	  (loop (cdr windows))))))

(define (buffer-reset! buffer)
  (set-buffer-writable! buffer)
  (buffer-widen! buffer)
  (region-delete! (buffer-region buffer))
  (buffer-not-modified! buffer)
  (with-editor-interrupts-disabled
   (lambda ()
     (undo-local-bindings! buffer #t)
     (%buffer-reset! buffer)
     (%set-buffer-major-mode!
      buffer
      (variable-default-value (ref-variable-object editor-default-mode)))
     (buffer-modeline-event! buffer 'BUFFER-RESET))))

(define (set-buffer-name! buffer name)
  (vector-set! buffer buffer-index:name name)
  (buffer-modeline-event! buffer 'BUFFER-NAME))

(define (set-buffer-default-directory! buffer directory)
  (vector-set! buffer
	       buffer-index:default-directory
	       (pathname-simplify directory)))

(define (set-buffer-pathname! buffer pathname)
  (vector-set! buffer buffer-index:pathname pathname)
  (if pathname
      (set-buffer-default-directory! buffer (directory-pathname pathname)))
  (buffer-modeline-event! buffer 'BUFFER-PATHNAME))

(define (set-buffer-truename! buffer truename)
  (vector-set! buffer buffer-index:truename truename)
  (buffer-modeline-event! buffer 'BUFFER-TRUENAME))

(define-integrable (set-buffer-auto-save-pathname! buffer pathname)
  (vector-set! buffer buffer-index:auto-save-pathname pathname))

(define-integrable (set-buffer-save-length! buffer)
  (vector-set! buffer buffer-index:save-length (buffer-length buffer)))

(define-integrable (set-buffer-backed-up?! buffer flag)
  (vector-set! buffer buffer-index:backed-up? flag))

(define-integrable (set-buffer-modification-time! buffer time)
  (vector-set! buffer buffer-index:modification-time time))

(define-integrable (set-buffer-comtabs! buffer comtabs)
  (vector-set! buffer buffer-index:comtabs comtabs))

(define (buffer-point buffer)
  (if (current-buffer? buffer)
      (current-point)
      (group-point (buffer-group buffer))))

(define-integrable (%set-buffer-point! buffer mark)
  (set-group-point! (buffer-group buffer) mark))

(define-integrable (minibuffer? buffer)
  (char=? (string-ref (buffer-name buffer) 0) #\Space))

(define-integrable (buffer-region buffer)
  (group-region (buffer-group buffer)))

(define-integrable (buffer-string buffer)
  (region->string (buffer-region buffer)))

(define-integrable (buffer-unclipped-region buffer)
  (group-unclipped-region (buffer-group buffer)))

(define-integrable (buffer-widen! buffer)
  (group-widen! (buffer-group buffer)))

(define-integrable (buffer-length buffer)
  (group-length (buffer-group buffer)))

(define-integrable (buffer-start buffer)
  (group-start-mark (buffer-group buffer)))

(define-integrable (buffer-end buffer)
  (group-end-mark (buffer-group buffer)))

(define-integrable (buffer-absolute-start buffer)
  (group-absolute-start (buffer-group buffer)))

(define-integrable (buffer-absolute-end buffer)
  (group-absolute-end (buffer-group buffer)))

(define (add-buffer-window! buffer window)
  (vector-set! buffer
	       buffer-index:windows
	       (cons window (vector-ref buffer buffer-index:windows))))

(define (remove-buffer-window! buffer window)
  (vector-set! buffer
	       buffer-index:windows
	       (delq! window (vector-ref buffer buffer-index:windows))))

(define-integrable (set-buffer-display-start! buffer mark)
  (vector-set! buffer buffer-index:display-start mark))

(define (buffer-visible? buffer)
  (there-exists? (buffer-windows buffer) window-visible?))

(define (buffer-get buffer key)
  (let ((entry (assq key (buffer-alist buffer))))
    (and entry
	 (cdr entry))))

(define (buffer-put! buffer key value)
  (if value
      (let ((entry (assq key (buffer-alist buffer))))
	(if entry
	    (set-cdr! entry value)
	    (vector-set! buffer buffer-index:alist
			 (cons (cons key value) (buffer-alist buffer)))))
      (buffer-remove! buffer key)))

(define (buffer-remove! buffer key)
  (vector-set! buffer buffer-index:alist
	       (del-assq! key (buffer-alist buffer))))

(define (remove-impermanent-bindings! alist)
  ((list-deletor!
    (lambda (entry)
      (not (variable-permanent-local? (car entry)))))
   alist))

(define (->buffer object)
  (cond ((buffer? object) object)
	((and (mark? object) (mark-buffer object)))
	((and (group? object) (group-buffer object)))
	(else (error "can't coerce to buffer:" object))))

;;;; Modification Flags

(define-integrable (buffer-modified? buffer)
  (group-modified? (buffer-group buffer)))

(define (buffer-not-modified! buffer)
  (with-editor-interrupts-disabled
   (lambda ()
     (let ((group (buffer-group buffer)))
       (if (group-modified? group)
	   (begin
	     (set-group-modified?! group #f)
	     (buffer-modeline-event! buffer 'BUFFER-MODIFIED)
	     (vector-set! buffer buffer-index:auto-saved? #f)))))))

(define (buffer-modified! buffer)
  (with-editor-interrupts-disabled
   (lambda ()
     (let ((group (buffer-group buffer)))
       (if (not (group-modified? group))
	   (begin
	     (set-group-modified?! group #t)
	     (buffer-modeline-event! buffer 'BUFFER-MODIFIED)))))))

(define (set-buffer-auto-saved! buffer)
  (vector-set! buffer buffer-index:auto-saved? #t)
  (set-group-modified?! (buffer-group buffer) 'AUTO-SAVED))

(define-integrable (buffer-auto-save-modified? buffer)
  (eq? #t (group-modified? (buffer-group buffer))))

(define (buffer-clip-daemon buffer)
  (lambda (group start end)
    group start end			;ignore
    (buffer-modeline-event! buffer 'CLIPPING-CHANGED)))

(define-integrable (buffer-read-only? buffer)
  (group-read-only? (buffer-group buffer)))

(define-integrable (buffer-writable? buffer)
  (not (buffer-read-only? buffer)))

(define (set-buffer-writable! buffer)
  (set-group-writable! (buffer-group buffer))
  (buffer-modeline-event! buffer 'BUFFER-MODIFIABLE))

(define (set-buffer-read-only! buffer)
  (set-group-read-only! (buffer-group buffer))
  (buffer-modeline-event! buffer 'BUFFER-MODIFIABLE))

(define (with-read-only-defeated mark thunk)
  (let ((group (mark-group mark))
	(outside)
	(inside 'FULLY))
    (dynamic-wind (lambda ()
		    (set! outside (group-writable? group))
		    (set-group-writable?! group inside))
		  thunk
		  (lambda ()
		    (set! inside (group-writable? group))
		    (set-group-writable?! group outside)))))

;;;; Local Bindings

(define (define-variable-local-value! buffer variable value)
  (let ((value (normalize-variable-value variable value)))
    (with-editor-interrupts-disabled
     (lambda ()
       (let ((binding (search-local-bindings buffer variable)))
	 (if binding
	     (set-cdr! binding value)
	     (vector-set! buffer
			  buffer-index:local-bindings
			  (cons (cons variable value)
				(buffer-local-bindings buffer)))))
       (if (buffer-local-bindings-installed? buffer)
	   (set-variable-%value! variable value))
       (invoke-variable-assignment-daemons! buffer variable)))))

(define (undefine-variable-local-value! buffer variable)
  (with-editor-interrupts-disabled
   (lambda ()
     (let ((binding (search-local-bindings buffer variable)))
       (if binding
	   (begin
	     (vector-set! buffer
			  buffer-index:local-bindings
			  (delq! binding (buffer-local-bindings buffer)))
	     (if (buffer-local-bindings-installed? buffer)
		 (set-variable-%value! variable
				       (variable-default-value variable)))
	     (invoke-variable-assignment-daemons! buffer variable)))))))

(define (variable-local-value buffer variable)
  (let ((binding
	 (and buffer
	      (search-local-bindings (->buffer buffer) variable))))
    (if binding
	(cdr binding)
	(variable-default-value variable))))

(define (variable-local-value? buffer variable)
  (or (not buffer)
      (search-local-bindings buffer variable)))

(define (set-variable-local-value! buffer variable value)
  (cond ((not buffer)
	 (set-variable-default-value! variable value))
	((variable-buffer-local? variable)
	 (define-variable-local-value! buffer variable value))
	((search-local-bindings buffer variable)
	 =>
	 (lambda (binding)
	   (let ((value (normalize-variable-value variable value)))
	     (with-editor-interrupts-disabled
	      (lambda ()
		(set-cdr! binding value)
		(if (buffer-local-bindings-installed? buffer)
		    (set-variable-%value! variable value))
		(invoke-variable-assignment-daemons! buffer variable))))))
	(else
	 (set-variable-default-value! variable value))))

(define (set-variable-default-value! variable value)
  (let ((value (normalize-variable-value variable value)))
    (with-editor-interrupts-disabled
     (lambda ()
       (set-variable-%default-value! variable value)
       (if (not (search-local-bindings (current-buffer) variable))
	   (set-variable-%value! variable value))
       (invoke-variable-assignment-daemons! #f variable)))))

(define-integrable (search-local-bindings buffer variable)
  (let loop ((bindings (buffer-local-bindings buffer)))
    (and (not (null? bindings))
	 (if (eq? (caar bindings) variable)
	     (car bindings)
	     (loop (cdr bindings))))))

(define (undo-local-bindings! buffer all?)
  ;; Caller guarantees that interrupts are disabled.
  (let ((bindings (buffer-local-bindings buffer)))
    (if (buffer-local-bindings-installed? buffer)
	(do ((bindings bindings (cdr bindings)))
	    ((null? bindings))
	  (set-variable-%value! (caar bindings)
				(variable-default-value (caar bindings)))))
    (vector-set! buffer buffer-index:local-bindings
		 (if all? '() (remove-impermanent-bindings! bindings)))
    (do ((bindings bindings (cdr bindings)))
	((null? bindings))
      (invoke-variable-assignment-daemons! buffer (caar bindings)))))

(define (with-current-local-bindings! thunk)
  (dynamic-wind (lambda ()
		  (install-buffer-local-bindings! (current-buffer)))
		thunk
		(lambda ()
		  (uninstall-buffer-local-bindings! (current-buffer)))))

(define (change-local-bindings! old-buffer new-buffer select-buffer!)
  ;; Assumes that interrupts are disabled and that OLD-BUFFER is selected.
  (uninstall-buffer-local-bindings! old-buffer)
  (select-buffer!)
  (install-buffer-local-bindings! new-buffer))

(define (install-buffer-local-bindings! buffer)
  (do ((bindings (buffer-local-bindings buffer) (cdr bindings)))
      ((null? bindings))
    (set-variable-%value! (caar bindings) (cdar bindings)))
  (vector-set! buffer buffer-index:local-bindings-installed? #t))

(define (uninstall-buffer-local-bindings! buffer)
  (do ((bindings (buffer-local-bindings buffer) (cdr bindings)))
      ((null? bindings))
    (set-variable-%value! (caar bindings)
			  (variable-default-value (caar bindings))))
  (vector-set! buffer buffer-index:local-bindings-installed? #f))

(define (set-variable-value! variable value)
  (if within-editor?
      (set-variable-local-value! (current-buffer) variable value)
      (begin
	(let ((value (normalize-variable-value variable value)))
	  (with-editor-interrupts-disabled
	   (lambda ()
	     (set-variable-%default-value! variable value)
	     (set-variable-%value! variable value)
	     (invoke-variable-assignment-daemons! #f variable)))))))

(define (with-variable-value! variable new-value thunk)
  (let ((old-value))
    (dynamic-wind (lambda ()
		    (set! old-value (variable-value variable))
		    (set-variable-value! variable new-value)
		    (set! new-value)
		    unspecific)
		  thunk
		  (lambda ()
		    (set! new-value (variable-value variable))
		    (set-variable-value! variable old-value)
		    (set! old-value)
		    unspecific))))

;;;; Modes

(define-integrable (buffer-major-mode buffer)
  (car (buffer-modes buffer)))

(define (set-buffer-major-mode! buffer mode)
  (if (not (and (mode? mode) (mode-major? mode)))
      (error:wrong-type-argument mode "major mode" 'SET-BUFFER-MAJOR-MODE!))
  (if (buffer-get buffer 'MAJOR-MODE-LOCKED)
      (editor-error "The major mode of this buffer is locked: " buffer))
  (with-editor-interrupts-disabled
   (lambda ()
     (undo-local-bindings! buffer #f)
     (%set-buffer-major-mode! buffer mode)
     (buffer-modeline-event! buffer 'BUFFER-MODES))))

(define (%set-buffer-major-mode! buffer mode)
  (vector-set! buffer buffer-index:modes (list mode))
  (vector-set! buffer buffer-index:comtabs (mode-comtabs mode))
  (set-variable-local-value! buffer
			     (ref-variable-object mode-name)
			     (mode-display-name mode))
  ((mode-initialization mode) buffer))

(define (buffer-minor-modes buffer)
  (list-copy (cdr (buffer-modes buffer))))

(define (buffer-minor-mode? buffer mode)
  (if (not (and (mode? mode) (not (mode-major? mode))))
      (error:wrong-type-argument mode "minor mode" 'BUFFER-MINOR-MODE?))
  (memq mode (cdr (buffer-modes buffer))))

(define (enable-buffer-minor-mode! buffer mode)
  (if (not (minor-mode? mode))
      (error:wrong-type-argument mode "minor mode" 'ENABLE-BUFFER-MINOR-MODE!))
  (with-editor-interrupts-disabled
   (lambda ()
     (let ((modes (buffer-modes buffer)))
       (if (not (memq mode (cdr modes)))
	   (begin
	     (set-cdr! modes (append! (cdr modes) (list mode)))
	     (set-buffer-comtabs! buffer
				  (cons (minor-mode-comtab mode)
					(buffer-comtabs buffer)))
	     (add-minor-mode-line-entry! buffer mode)
	     ((mode-initialization mode) buffer)
	     (buffer-modeline-event! buffer 'BUFFER-MODES)))))))

(define (disable-buffer-minor-mode! buffer mode)
  (if (not (minor-mode? mode))
      (error:wrong-type-argument mode "minor mode"
				 'DISABLE-BUFFER-MINOR-MODE!))
  (with-editor-interrupts-disabled
   (lambda ()
     (let ((modes (buffer-modes buffer)))
       (if (memq mode (cdr modes))
	   (begin
	     (set-cdr! modes (delq! mode (cdr modes)))
	     (set-buffer-comtabs! buffer
				  (delq! (minor-mode-comtab mode)
					 (buffer-comtabs buffer)))
	     (remove-minor-mode-line-entry! buffer mode)
	     (buffer-modeline-event! buffer 'BUFFER-MODES)))))))
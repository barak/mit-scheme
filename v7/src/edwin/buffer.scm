;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/buffer.scm,v 1.151 1992/02/04 04:01:29 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-92 Massachusetts Institute of Technology
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
  initializations
  auto-save-pathname
  auto-save-state
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
    (let ((group (make-group (string-copy "") buffer)))
      (vector-set! buffer buffer-index:name name)
      (vector-set! buffer buffer-index:group group)
      (let ((daemon (buffer-modification-daemon buffer)))
	(add-group-insert-daemon! group daemon)
	(add-group-delete-daemon! group daemon))
      (add-group-clip-daemon! group (buffer-clip-daemon buffer))
      (if (not (minibuffer? buffer))
	  (enable-group-undo! group))
      (vector-set! buffer
		   buffer-index:mark-ring
		   (make-ring (ref-variable mark-ring-maximum)))
      (ring-push! (buffer-mark-ring buffer) (group-start-mark group))
      (vector-set! buffer buffer-index:modes (list mode))
      (vector-set! buffer buffer-index:comtabs (mode-comtabs mode))
      (vector-set! buffer buffer-index:windows '())
      (vector-set! buffer buffer-index:display-start false)
      (vector-set! buffer buffer-index:default-directory directory)
      (vector-set! buffer buffer-index:pathname false)
      (vector-set! buffer buffer-index:truename false)
      (vector-set! buffer buffer-index:alist '())
      (vector-set! buffer buffer-index:local-bindings '())
      (vector-set! buffer buffer-index:local-bindings-installed? false)
      (vector-set! buffer
		   buffer-index:initializations
		   (list (mode-initialization mode)))
      (vector-set! buffer buffer-index:auto-save-pathname false)
      (set-buffer-auto-save-state! buffer 'NO-CHANGES)
      (vector-set! buffer buffer-index:save-length 0)
      (vector-set! buffer buffer-index:backed-up? false)
      (vector-set! buffer buffer-index:modification-time false)
      (event-distributor/invoke! (ref-variable buffer-creation-hook) buffer)
      buffer)))

(define (buffer-modeline-event! buffer type)
  (let loop ((windows (buffer-windows buffer)))
    (if (not (null? windows))
	(begin
	  (window-modeline-event! (car windows) type)
	  (loop (cdr windows))))))

(define (buffer-reset! buffer)
  (set-buffer-writeable! buffer)
  (region-delete! (buffer-region buffer))
  (buffer-not-modified! buffer)
  (let ((group (buffer-group buffer)))
    (if (group-undo-data group)
	(undo-done! (group-point group))))
  (buffer-widen! buffer)
  (set-buffer-major-mode! buffer (buffer-major-mode buffer))
  (without-interrupts
   (lambda ()
     (vector-set! buffer buffer-index:pathname false)
     (vector-set! buffer buffer-index:truename false)
     (buffer-modeline-event! buffer 'BUFFER-PATHNAME)
     (vector-set! buffer buffer-index:auto-save-pathname false)
     (set-buffer-auto-save-state! buffer 'NO-CHANGES)
     (vector-set! buffer buffer-index:save-length 0))))

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

(define-integrable (set-buffer-auto-save-state! buffer state)
  (vector-set! buffer buffer-index:auto-save-state state))

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

(define-integrable (buffer-visible? buffer)
  (not (null? (buffer-windows buffer))))

(define (buffer-get buffer key)
  (let ((entry (assq key (vector-ref buffer buffer-index:alist))))
    (and entry
	 (cdr entry))))

(define (buffer-put! buffer key value)
  (let ((entry (assq key (vector-ref buffer buffer-index:alist))))
    (if entry
	(set-cdr! entry value)
	(vector-set! buffer buffer-index:alist
		     (cons (cons key value)
			   (vector-ref buffer buffer-index:alist))))))

(define (buffer-remove! buffer key)
  (vector-set! buffer
	       buffer-index:alist
	       (del-assq! key (vector-ref buffer buffer-index:alist))))

(define-integrable (reset-buffer-alist! buffer)
  (vector-set! buffer buffer-index:alist '()))

(define (add-buffer-initialization! buffer thunk)
  (without-interrupts (lambda () (%add-buffer-initialization! buffer thunk))))

(define (%add-buffer-initialization! buffer thunk)
  (if (current-buffer? buffer)
      (thunk)
      (vector-set! buffer
		   buffer-index:initializations
		   (append! (buffer-initializations buffer) (list thunk)))))

(define (perform-buffer-initializations! buffer)
  ;; Assumes that interrupts are disabled and BUFFER is selected.
  (let loop ((thunks (buffer-initializations buffer)))
    (if (not (null? thunks))
	(begin
	  ((car thunks))
	  (loop (cdr thunks)))))
  (vector-set! buffer buffer-index:initializations '()))

(define (->buffer object)
  (cond ((buffer? object) object)
	((and (mark? object) (mark-buffer object)))
	((and (group? object) (group-buffer object)))
	(else (error "can't coerce to buffer:" object))))

;;;; Modification Flags

(define-integrable (buffer-modified? buffer)
  (group-modified? (buffer-group buffer)))

(define (buffer-not-modified! buffer)
  (without-interrupts
   (lambda ()
     (let ((group (buffer-group buffer)))
       (if (group-modified? group)
	   (begin
	     (set-group-modified! group false)
	     (buffer-modeline-event! buffer 'BUFFER-MODIFIED)
	     (set-buffer-auto-save-state! buffer 'NO-CHANGES)))))))

(define (buffer-modified! buffer)
  (without-interrupts
   (lambda ()
     (%buffer-modified! buffer (buffer-group buffer)))))

(define (buffer-modification-daemon buffer)
  (lambda (group start end)
    start end				;ignore
    (%buffer-modified! buffer group)))

(define-integrable (%buffer-modified! buffer group)
  (cond ((not (group-modified? group))
	 (set-group-modified! group true)
	 (buffer-modeline-event! buffer 'BUFFER-MODIFIED)
	 (set-buffer-auto-save-state! buffer 'UNSAVED-CHANGES))
	((eq? 'AUTO-SAVED (buffer-auto-save-state buffer))
	 (set-buffer-auto-save-state! buffer 'AUTO-SAVED+CHANGES))))

(define-integrable (set-buffer-auto-saved! buffer)
  (set-buffer-auto-save-state! buffer 'AUTO-SAVED))

(define-integrable (buffer-auto-save-modified? buffer)
  (memq (buffer-auto-save-state buffer) '(UNSAVED-CHANGES AUTO-SAVED+CHANGES)))

(define-integrable (buffer-auto-saved? buffer)
  (memq (buffer-auto-save-state buffer) '(AUTO-SAVED AUTO-SAVED+CHANGES)))

(define (buffer-clip-daemon buffer)
  (lambda (group start end)
    group start end			;ignore
    (buffer-modeline-event! buffer 'CLIPPING-CHANGED)))

(define-integrable (buffer-read-only? buffer)
  (group-read-only? (buffer-group buffer)))

(define-integrable (buffer-writeable? buffer)
  (not (buffer-read-only? buffer)))

(define (set-buffer-writeable! buffer)
  (set-group-writeable! (buffer-group buffer))
  (buffer-modeline-event! buffer 'BUFFER-MODIFIABLE))

(define (set-buffer-read-only! buffer)
  (set-group-read-only! (buffer-group buffer))
  (buffer-modeline-event! buffer 'BUFFER-MODIFIABLE))

(define (with-read-only-defeated mark thunk)
  (let ((group (mark-group mark))
	(read-only?))
    (unwind-protect (lambda ()
		      (set! read-only? (group-read-only? group))
		      (set-group-writeable! group))
		    thunk
		    (lambda ()
		      (if read-only? (set-group-read-only! group))))))

;;;; Local Bindings

(define (define-variable-local-value! buffer variable value)
  (check-variable-value-validity! variable value)
  (without-interrupts
   (lambda ()
     (let ((binding (search-local-bindings buffer variable)))
       (if (buffer-local-bindings-installed? buffer)
	   (begin
	     (if (not binding)
		 (vector-set! buffer
			      buffer-index:local-bindings
			      (cons (cons variable (variable-value variable))
				    (buffer-local-bindings buffer))))
	     (%set-variable-value! variable value))
	   (if binding
	       (set-cdr! binding value)
	       (vector-set! buffer
			    buffer-index:local-bindings
			    (cons (cons variable value)
				  (buffer-local-bindings buffer)))))))))

(define (undefine-variable-local-value! buffer variable)
  (without-interrupts
   (lambda ()
     (let ((binding (search-local-bindings buffer variable)))
       (if binding
	   (begin
	     (vector-set! buffer
			  buffer-index:local-bindings
			  (delq! binding (buffer-local-bindings buffer)))
	     (if (buffer-local-bindings-installed? buffer)
		 (%set-variable-value! variable (cdr binding)))))))))

(define (variable-local-value buffer variable)
  (let ((buffer (->buffer buffer)))
    (if (buffer-local-bindings-installed? buffer)
	(variable-value variable)
	(let ((binding (search-local-bindings buffer variable)))
	  (if binding
	      (cdr binding)
	      (variable-default-value variable))))))

(define (set-variable-local-value! buffer variable value)
  (if (variable-buffer-local? variable)
      (define-variable-local-value! buffer variable value)
      (begin
	(check-variable-value-validity! variable value)
	(without-interrupts
	 (lambda ()
	   (let ((binding
		  (and (not (buffer-local-bindings-installed? buffer))
		       (search-local-bindings buffer variable))))
	     (if binding
		 (set-cdr! binding value)
		 (%set-variable-value! variable value))))))))

(define (variable-default-value variable)
  (let ((binding (search-local-bindings (current-buffer) variable)))
    (if binding
	(cdr binding)
	(variable-value variable))))

(define (set-variable-default-value! variable value)
  (check-variable-value-validity! variable value)
  (without-interrupts
   (lambda ()
     (let ((binding (search-local-bindings (current-buffer) variable)))
       (if binding
	   (set-cdr! binding value)
	   (%set-variable-value! variable value))))))

(define-integrable (search-local-bindings buffer variable)
  (let loop ((bindings (buffer-local-bindings buffer)))
    (and (not (null? bindings))
	 (if (eq? (caar bindings) variable)
	     (car bindings)
	     (loop (cdr bindings))))))

(define (undo-local-bindings!)
  ;; Caller guarantees that interrupts are disabled.
  (let ((buffer (current-buffer)))
    (let ((bindings (buffer-local-bindings buffer)))
      (do ((bindings bindings (cdr bindings)))
	  ((null? bindings))
	(%%set-variable-value! (caar bindings) (cdar bindings)))
      (vector-set! buffer buffer-index:local-bindings '())
      (do ((bindings bindings (cdr bindings)))
	  ((null? bindings))
	(invoke-variable-assignment-daemons! (caar bindings))))))

(define (with-current-local-bindings! thunk)
  (let ((wind-bindings
	 (lambda (buffer installed?)
	   (do ((bindings (buffer-local-bindings buffer) (cdr bindings)))
	       ((null? bindings))
	     (let ((old-value (variable-value (caar bindings))))
	       (%%set-variable-value! (caar bindings) (cdar bindings))
	       (set-cdr! (car bindings) old-value)))
	   (vector-set! buffer
			buffer-index:local-bindings-installed?
			installed?))))
    (unwind-protect
     (lambda ()
       (let ((buffer (current-buffer)))
	 (wind-bindings buffer true)
	 (perform-buffer-initializations! buffer)))
     thunk
     (lambda ()
       (wind-bindings (current-buffer) false)))))

(define (change-local-bindings! old-buffer new-buffer select-buffer!)
  ;; Assumes that interrupts are disabled and that OLD-BUFFER is selected.
  (let ((variables '()))
    (do ((bindings (buffer-local-bindings old-buffer) (cdr bindings)))
	((null? bindings))
      (let ((old-value (variable-value (caar bindings))))
	(%%set-variable-value! (caar bindings) (cdar bindings))
	(set-cdr! (car bindings) old-value))
      (if (not (null? (variable-assignment-daemons (caar bindings))))
	  (set! variables (cons (caar bindings) variables))))
    (vector-set! old-buffer buffer-index:local-bindings-installed? false)
    (select-buffer!)
    (do ((bindings (buffer-local-bindings new-buffer) (cdr bindings)))
	((null? bindings))
      (let ((old-value (variable-value (caar bindings))))
	(%%set-variable-value! (caar bindings) (cdar bindings))
	(set-cdr! (car bindings) old-value))
      (if (and (not (null? (variable-assignment-daemons (caar bindings))))
	       (not (let loop ((variables variables))
		      (and (not (null? variables))
			   (or (eq? (caar bindings) (car variables))
			       (loop (cdr variables)))))))
	  (set! variables (cons (caar bindings) variables))))
    (vector-set! new-buffer buffer-index:local-bindings-installed? true)
    (perform-buffer-initializations! new-buffer)
    (if (not (null? variables))
	(do ((variables variables (cdr variables)))
	    ((null? variables))
	  (invoke-variable-assignment-daemons! (car variables))))))

;;;; Modes

(define-integrable (buffer-major-mode buffer)
  (car (buffer-modes buffer)))

(define (set-buffer-major-mode! buffer mode)
  (if (not (and (mode? mode) (mode-major? mode)))
      (error:wrong-type-argument mode "major mode" 'SET-BUFFER-MAJOR-MODE!))
  (without-interrupts
   (lambda ()
     (let ((modes (buffer-modes buffer)))
       (set-car! modes mode)
       (set-cdr! modes '()))
     (set-buffer-comtabs! buffer (mode-comtabs mode))
     (vector-set! buffer buffer-index:alist '())
     (vector-set! buffer buffer-index:initializations '())
     (buffer-modeline-event! buffer 'BUFFER-MODES)
     (%add-buffer-initialization! buffer undo-local-bindings!)
     (%add-buffer-initialization! buffer (mode-initialization mode)))))

(define-integrable (buffer-minor-modes buffer)
  (cdr (buffer-modes buffer)))

(define (buffer-minor-mode? buffer mode)
  (if (not (and (mode? mode) (not (mode-major? mode))))
      (error:wrong-type-argument mode "minor mode" 'BUFFER-MINOR-MODE?))
  (memq mode (buffer-minor-modes buffer)))

(define (enable-buffer-minor-mode! buffer mode)
  (if (not (minor-mode? mode))
      (error:wrong-type-argument mode "minor mode" 'ENABLE-BUFFER-MINOR-MODE!))
  (without-interrupts
   (lambda ()
     (let ((modes (buffer-modes buffer)))
       (if (not (memq mode (cdr modes)))
	   (begin
	     (set-cdr! modes (append! (cdr modes) (list mode)))
	     (set-buffer-comtabs! buffer
				  (cons (minor-mode-comtab mode)
					(buffer-comtabs buffer)))
	     (%add-buffer-initialization! buffer (mode-initialization mode))
	     (buffer-modeline-event! buffer 'BUFFER-MODES)))))))

(define (disable-buffer-minor-mode! buffer mode)
  (if (not (minor-mode? mode))
      (error:wrong-type-argument mode "minor mode"
				 'DISABLE-BUFFER-MINOR-MODE!))
  (without-interrupts
   (lambda ()
     (let ((modes (buffer-modes buffer)))
       (if (memq mode (cdr modes))
	   (begin
	     (set-cdr! modes (delq! mode (cdr modes)))
	     (set-buffer-comtabs! buffer
				  (delq! (minor-mode-comtab mode)
					 (buffer-comtabs buffer)))
	     (buffer-modeline-event! buffer 'BUFFER-MODES)))))))
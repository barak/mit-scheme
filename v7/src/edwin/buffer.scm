;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/buffer.scm,v 1.132 1989/08/09 13:16:48 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989 Massachusetts Institute of Technology
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
  cursor-y
  pathname
  truename
  alist
  local-bindings
  initializations
  auto-save-pathname
  auto-save-modified?
  save-length
  backed-up?
  modification-time
  )
(define-variable buffer-creation-hook
  "If not false, a procedure to call when a new buffer is created.
The procedure is passed the new buffer as its argument.
The buffer is guaranteed to be deselected at that time."
  false)

(define (make-buffer name #!optional mode)
  (let ((mode (if (default-object? mode) (ref-mode-object fundamental) mode)))
    (let ((group (region-group (string->region ""))))
      (let ((buffer (%make-buffer)))
	(vector-set! buffer buffer-index:name name)
	(vector-set! buffer buffer-index:group group)
	(let ((daemon (buffer-modification-daemon buffer)))
	  (add-group-insert-daemon! group daemon)
	  (add-group-delete-daemon! group daemon))
	(if (not (minibuffer? buffer))
	    (enable-group-undo! group))
	(vector-set! buffer
		     buffer-index:mark-ring
		     (make-ring (ref-variable mark-ring-maximum)))
	(ring-push! (buffer-mark-ring buffer) (group-start-mark group))
	(vector-set! buffer buffer-index:modes (list mode))
	(vector-set! buffer buffer-index:comtabs (mode-comtabs mode))
	(vector-set! buffer buffer-index:windows '())
	(vector-set! buffer buffer-index:cursor-y false)
	(vector-set! buffer buffer-index:pathname false)
	(vector-set! buffer buffer-index:truename false)
	(vector-set! buffer buffer-index:alist '())
	(vector-set! buffer buffer-index:local-bindings '())
	(vector-set! buffer
		     buffer-index:initializations
		     (list (mode-initialization mode)))
	(vector-set! buffer buffer-index:auto-save-pathname false)
	(vector-set! buffer buffer-index:auto-save-modified? false)
	(vector-set! buffer buffer-index:save-length 0)
	(vector-set! buffer buffer-index:backed-up? false)
	(vector-set! buffer buffer-index:modification-time false)
	(let ((hook (ref-variable buffer-creation-hook)))
	  (if hook (hook buffer)))
	buffer))))

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
     (vector-set! buffer buffer-index:auto-save-modified? false)
     (vector-set! buffer buffer-index:save-length 0)
     unspecific)))

(define (set-buffer-name! buffer name)
  (vector-set! buffer buffer-index:name name)
  (buffer-modeline-event! buffer 'BUFFER-NAME))

(define (set-buffer-pathname! buffer pathname)
  (vector-set! buffer buffer-index:pathname pathname)
  (buffer-modeline-event! buffer 'BUFFER-PATHNAME))

(define (set-buffer-truename! buffer truename)
  (vector-set! buffer buffer-index:truename truename)
  (buffer-modeline-event! buffer 'BUFFER-TRUENAME))

(define-integrable (set-buffer-auto-save-pathname! buffer pathname)
  (vector-set! buffer buffer-index:auto-save-pathname pathname)
  unspecific)

(define-integrable (set-buffer-auto-saved! buffer)
  (vector-set! buffer buffer-index:auto-save-modified? false)
  unspecific)

(define-integrable (set-buffer-save-length! buffer)
  (vector-set! buffer buffer-index:save-length (buffer-length buffer))
  unspecific)

(define-integrable (set-buffer-backed-up?! buffer flag)
  (vector-set! buffer buffer-index:backed-up? flag)
  unspecific)

(define-integrable (set-buffer-modification-time! buffer flag)
  (vector-set! buffer buffer-index:modification-time flag)
  unspecific)

(define-integrable (set-buffer-comtabs! buffer comtabs)
  (vector-set! buffer buffer-index:comtabs comtabs)
  unspecific)

(define-integrable (buffer-point buffer)
  (group-point (buffer-group buffer)))

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
  (group-un-clip! (buffer-group buffer)))

(define-integrable (buffer-length buffer)
  (group-length (buffer-group buffer)))

(define-integrable (buffer-start buffer)
  (group-start-mark (buffer-group buffer)))

(define-integrable (buffer-end buffer)
  (group-end-mark (buffer-group buffer)))

(define (add-buffer-window! buffer window)
  (vector-set! buffer
	       buffer-index:windows
	       (cons window (vector-ref buffer buffer-index:windows)))
  unspecific)

(define (remove-buffer-window! buffer window)
  (vector-set! buffer
	       buffer-index:windows
	       (delq! window (vector-ref buffer buffer-index:windows)))
  unspecific)

(define-integrable (set-buffer-cursor-y! buffer cursor-y)
  (vector-set! buffer buffer-index:cursor-y cursor-y)
  unspecific)

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
			   (vector-ref buffer buffer-index:alist)))))
  unspecific)

(define (buffer-remove! buffer key)
  (vector-set! buffer
	       buffer-index:alist
	       (del-assq! key (vector-ref buffer buffer-index:alist)))
  unspecific)

(define-integrable (reset-buffer-alist! buffer)
  (vector-set! buffer buffer-index:alist '())
  unspecific)

;;;; Modification Flags

(define-integrable (buffer-modified? buffer)
  (group-modified? (buffer-group buffer)))

(define-integrable (buffer-not-modified! buffer)
  (set-buffer-modified! buffer false))

(define-integrable (buffer-modified! buffer)
  (set-buffer-modified! buffer true))

(define (set-buffer-modified! buffer sense)
  (set-group-modified! (buffer-group buffer) sense)
  (vector-set! buffer buffer-index:auto-save-modified? sense)
  (buffer-modeline-event! buffer 'BUFFER-MODIFIED))

(define (buffer-modification-daemon buffer)
  (lambda (group start end)
    ;; Open coded for speed.
    start end				;ignore
    (if (not (group-modified? group))
	(begin
	  (set-group-modified! group true)
	  (buffer-modeline-event! buffer 'BUFFER-MODIFIED)))
    (vector-set! buffer buffer-index:auto-save-modified? true)
    unspecific))
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
    (dynamic-wind (lambda ()
		    (set! read-only? (group-read-only? group))
		    (if read-only? (set-group-writeable! group)))
		  thunk
		  (lambda ()
		    (if read-only? (set-group-read-only! group))))))

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
	  (loop))))  (vector-set! buffer buffer-index:initializations '())
  unspecific)

;;;; Local Bindings

(define (make-local-binding! variable new-value)
  (without-interrupts
   (lambda ()
     (let ((buffer (current-buffer))
	   (old-value (variable-value variable)))
       (%set-variable-value! variable new-value)
       (invoke-variable-assignment-daemons! variable)
       (let ((bindings (buffer-local-bindings buffer)))
	 (let ((binding (assq variable bindings)))
	   (if (not binding)
	       (vector-set! buffer
			    buffer-index:local-bindings
			    (cons (cons variable old-value) bindings))))))
     unspecific)))

(define (unmake-local-binding! variable)
  (without-interrupts
   (lambda ()
     (let ((buffer (current-buffer)))
       (let ((bindings (buffer-local-bindings buffer)))
	 (let ((binding (assq variable bindings)))
	   (if binding
	       (begin
		 (%set-variable-value! variable (cdr binding))
		 (invoke-variable-assignment-daemons! variable)
		 (vector-set! buffer
			      buffer-index:local-bindings
			      (delq! binding bindings)))))))
     unspecific)))

(define (undo-local-bindings!)
  (let ((buffer (current-buffer)))
    (for-each (lambda (binding)
		(let ((variable (car binding)))
		  (%set-variable-value! variable (cdr binding))
		  (invoke-variable-assignment-daemons! variable)))
	      (buffer-local-bindings buffer))
    (vector-set! buffer buffer-index:local-bindings '()))
  unspecific)
(define (change-local-bindings! old-buffer new-buffer select-buffer!)
  ;; Assumes that interrupts are disabled and that OLD-BUFFER is selected.
  (let ((variables '()))
    (for-each (lambda (binding)
		(let ((variable (car binding)))
		  (let ((old-value (variable-value variable)))
		    (%set-variable-value! variable (cdr binding))
		    (set-cdr! binding old-value))
		  (if (not (null? (variable-assignment-daemons variable)))
		      (begin
			(set! variables (cons variable variables))
			unspecific))))
	      (buffer-local-bindings old-buffer))
    (select-buffer!)
    (for-each (lambda (binding)
		(let ((variable (car binding)))
		  (let ((old-value (variable-value variable)))
		    (%set-variable-value! variable (cdr binding))
		    (set-cdr! binding old-value))
		  (if (and (not (null? (variable-assignment-daemons variable)))
			   (not (memq variable variables)))
		      (begin
			(set! variables (cons variable variables))
			unspecific))))
	      (buffer-local-bindings new-buffer))
    (perform-buffer-initializations! new-buffer)
    (if (not (null? variables))
	(for-each invoke-variable-assignment-daemons! variables))))

(define (variable-local-value buffer variable)
  (let ((in-cell
	 (lambda ()
	   (variable-value variable))))
    (if (current-buffer? buffer)
	(in-cell)
	(let ((binding (assq variable (buffer-local-bindings buffer))))
	  (cond (binding
		 (cdr binding))
		((and (variable-buffer-local? variable)
		      (within-editor?))
		 (let ((binding
			(assq variable
			      (buffer-local-bindings (current-buffer)))))
		   (if binding
		       (cdr binding)
		       (in-cell))))
		(else
		 (in-cell)))))))

(define (set-variable-local-value! buffer variable value)
  (if (current-buffer? buffer)
      (set-variable-value! variable value)
      (let ((binding (assq variable (buffer-local-bindings buffer))))
	(if binding
	    (begin
	      (set-cdr! binding value)
	      unspecific)
	    (set-variable-value! variable value)))))

(define (define-variable-local-value! buffer variable value)
  (if (current-buffer? buffer)
      (make-local-binding! variable value)
      (without-interrupts
       (lambda ()
	 (let ((bindings (buffer-local-bindings buffer)))
	   (let ((binding (assq variable bindings)))
	     (if binding
		 (set-cdr! binding value)
		 (vector-set! buffer
			      buffer-index:local-bindings
			      (cons (cons variable value) bindings)))
	     unspecific))))))

(define (variable-local-value? buffer variable)
  (assq variable (buffer-local-bindings buffer)))

(define (variable-default-value variable)
  (let ((binding (assq variable (buffer-local-bindings (current-buffer)))))
    (if binding
	(cdr binding)
	(variable-value variable))))

(define (set-variable-default-value! variable value)
  (let ((binding (assq variable (buffer-local-bindings (current-buffer)))))
    (if binding
	(begin
	  (set-cdr! binding value)
	  unspecific)
	(without-interrupts
	 (lambda ()
	   (%set-variable-value! variable value)
	   (invoke-variable-assignment-daemons! variable))))))

;;;; Modes

(define-integrable (buffer-major-mode buffer)
  (car (buffer-modes buffer)))

(define (set-buffer-major-mode! buffer mode)
  (if (not (mode-major? mode)) (error "Not a major mode" mode))
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
  (if (mode-major? mode) (error "Not a minor mode" mode))
  (memq mode (buffer-minor-modes buffer)))

(define (enable-buffer-minor-mode! buffer mode)
  (if (mode-major? mode) (error "Not a minor mode" mode))
  (without-interrupts
   (lambda ()
     (let ((modes (buffer-modes buffer)))
       (if (not (memq mode (cdr modes)))
	   (begin
	     (set-cdr! modes (append! (cdr modes) (list mode)))
	     (set-buffer-comtabs! buffer
				  (cons (mode-comtab mode)
					(buffer-comtabs buffer)))
	     (%add-buffer-initialization! buffer (mode-initialization mode))
	     (buffer-modeline-event! buffer 'BUFFER-MODES)))))))

(define (disable-buffer-minor-mode! buffer mode)
  (if (mode-major? mode) (error "Not a minor mode" mode))
  (without-interrupts
   (lambda ()
     (let ((modes (buffer-modes buffer)))
       (if (memq mode (cdr modes))
	   (begin
	     (set-cdr! modes (delq! mode (cdr modes)))
	     (set-buffer-comtabs! buffer
				  (delq! (mode-comtab mode)
					 (buffer-comtabs buffer)))
	     (buffer-modeline-event! buffer 'BUFFER-MODES)))))))
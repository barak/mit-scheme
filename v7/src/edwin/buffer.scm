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
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Buffer Abstraction

(declare (usual-integrations))
(using-syntax edwin-syntax-table

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
  writeable?
  alist
  local-bindings
  initializations
  auto-save-pathname
  auto-save-modified?
  save-length)

(define-variable "Mark Ring Maximum"
  "The maximum number of entries to keep in the mark ring."
  16)

(define-variable "Buffer Creation Hook"
  "If not false, a procedure to call when a new buffer is created.
The procedure is passed the new buffer as its argument.
The buffer is guaranteed to be deselected at that time."
  #!FALSE)

(define-unparser %buffer-tag
  (lambda (buffer)
    (write-string "Buffer ")
    (write (buffer-name buffer))))

(define (make-buffer name #!optional mode)
  (if (unassigned? mode) (set! mode fundamental-mode))
  (let ((group (region-group (string->region ""))))
    (let ((buffer (%make-buffer)))
      (vector-set! buffer buffer-index:name name)
      (vector-set! buffer buffer-index:group group)
      (let ((daemon (buffer-modification-daemon buffer)))
	(add-group-insert-daemon! group daemon)
	(add-group-delete-daemon! group daemon))
      (if (not (minibuffer? buffer))
	  (enable-group-undo! group))
      (vector-set! buffer buffer-index:mark-ring
		   (make-ring (ref-variable "Mark Ring Maximum")))
      (ring-push! (buffer-mark-ring buffer) (group-start-mark group))
      (vector-set! buffer buffer-index:modes (list mode))
      (vector-set! buffer buffer-index:comtabs (mode-comtabs mode))
      (vector-set! buffer buffer-index:windows '())
      (vector-set! buffer buffer-index:cursor-y #!FALSE)
      (vector-set! buffer buffer-index:pathname #!FALSE)
      (vector-set! buffer buffer-index:truename #!FALSE)
      (vector-set! buffer buffer-index:writeable? #!TRUE)
      (vector-set! buffer buffer-index:alist '())
      (vector-set! buffer buffer-index:local-bindings '())
      (vector-set! buffer buffer-index:initializations
		   (list (mode-initialization mode)))
      (vector-set! buffer buffer-index:auto-save-pathname #!FALSE)
      (vector-set! buffer buffer-index:auto-save-modified? #!FALSE)
      (vector-set! buffer buffer-index:save-length 0)
      (if (ref-variable "Buffer Creation Hook")
	  ((ref-variable "Buffer Creation Hook") buffer))
      buffer)))

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
     (vector-set! buffer buffer-index:pathname #!FALSE)
     (vector-set! buffer buffer-index:truename #!FALSE)
     (buffer-modeline-event! buffer 'BUFFER-PATHNAME)
     (vector-set! buffer buffer-index:auto-save-pathname #!FALSE)
     (vector-set! buffer buffer-index:auto-save-modified? #!FALSE)
     (vector-set! buffer buffer-index:save-length 0))))

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
  (vector-set! buffer buffer-index:auto-save-pathname pathname))

(define-integrable (set-buffer-auto-saved! buffer)
  (vector-set! buffer buffer-index:auto-save-modified? #!FALSE))

(define-integrable (set-buffer-save-length! buffer)
  (vector-set! buffer buffer-index:save-length (buffer-length buffer)))

(define-integrable (set-buffer-comtabs! buffer comtabs)
  (vector-set! buffer buffer-index:comtabs comtabs))

(define-integrable (buffer-point buffer)
  (group-point (buffer-group buffer)))

(define-integrable (%set-buffer-point! buffer mark)
  (set-group-point! (buffer-group buffer) mark))

(define (minibuffer? buffer)
  (char=? (string-ref (buffer-name buffer) 0) #\Space))

(define-integrable (buffer-region buffer)
  (group-region (buffer-group buffer)))

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
  (vector-set! buffer buffer-index:windows
	       (cons window (vector-ref buffer buffer-index:windows))))

(define (remove-buffer-window! buffer window)
  (vector-set! buffer buffer-index:windows
	       (delq! window (vector-ref buffer buffer-index:windows))))

(define-integrable (set-buffer-cursor-y! buffer cursor-y)
  (vector-set! buffer buffer-index:cursor-y cursor-y))

(define-integrable (buffer-visible? buffer)
  (not (null? (buffer-windows buffer))))

(define (buffer-get buffer key)
  (let ((entry (assq key (vector-ref buffer buffer-index:alist))))
    (and entry (cdr entry))))

(define (buffer-put! buffer key value)
  (let ((entry (assq key (vector-ref buffer buffer-index:alist))))
    (if entry
	(set-cdr! entry value)
	(vector-set! buffer buffer-index:alist
		     (cons (cons key value)
			   (vector-ref buffer buffer-index:alist))))))

(define (buffer-remove! buffer key)
  (vector-set! buffer buffer-index:alist
	       (del-assq! key
			  (vector-ref buffer buffer-index:alist))))

(define-integrable (reset-buffer-alist! buffer)
  (vector-set! buffer buffer-index:alist '()))

;;;; Modification Flags

(define-integrable (buffer-modified? buffer)
  (group-modified? (buffer-group buffer)))

(define-integrable (buffer-not-modified! buffer)
  (set-buffer-modified! buffer #!FALSE))

(define-integrable (buffer-modified! buffer)
  (set-buffer-modified! buffer #!TRUE))

(define (set-buffer-modified! buffer sense)
  (set-group-modified! (buffer-group buffer) sense)
  (vector-set! buffer buffer-index:auto-save-modified? sense)
  (buffer-modeline-event! buffer 'BUFFER-MODIFIED))

;; Open coded for speed.
(define ((buffer-modification-daemon buffer) group start end)
  (if (not (group-modified? group))
      (begin (set-group-modified! group #!TRUE)
	     (buffer-modeline-event! buffer 'BUFFER-MODIFIED)))
  (vector-set! buffer buffer-index:auto-save-modified? #!TRUE))

(define-integrable (buffer-read-only? buffer)
  (group-read-only? (buffer-group buffer)))

(define (set-buffer-writeable! buffer)
  (set-group-writeable! (buffer-group buffer))
  (vector-set! buffer buffer-index:writeable? #!TRUE)
  (buffer-modeline-event! buffer 'BUFFER-MODIFIABLE))

(define (set-buffer-file-read-only! buffer)
  (set-group-writeable! (buffer-group buffer))
  (vector-set! buffer buffer-index:writeable? #!FALSE)
  (buffer-modeline-event! buffer 'BUFFER-MODIFIABLE))

(define (set-buffer-read-only! buffer)
  (set-group-read-only! (buffer-group buffer))
  (vector-set! buffer buffer-index:writeable? #!FALSE)
  (buffer-modeline-event! buffer 'BUFFER-MODIFIABLE))

(define (with-read-only-defeated mark thunk)
  (let ((group (mark-group mark)))
    (define read-only?)
    (dynamic-wind (lambda ()
		    (set! read-only? (group-read-only? group))
		    (if read-only?
			(set-group-writeable! group)))
		  thunk
		  (lambda ()
		    (if read-only?
			(set-group-read-only! group))))))

;;;; Modeline Interface

(define (buffer-modeline-event! buffer type)
  (define (loop windows)
    (if (not (null? windows))
	(begin (window-modeline-event! (car windows) type)
	       (loop (cdr windows)))))
  (loop (buffer-windows buffer)))

(define (buffer-display-name buffer)
  (let ((name (buffer-name buffer))
	(pathname (buffer-pathname buffer)))
    (define (display-string name*)
      (define (append-version version)
	(string-append name* " (" (write-to-string version) ")"))
      (string-append
       (if (pathname-version pathname)
	   (let ((truename (buffer-truename buffer)))
	     (if (not truename)
		 (append-version
		  (let ((version (pathname-version pathname)))
		    (if (integer? version) version 0)))
		 (let ((version (pathname-version truename)))
		   (if version (append-version version) name*))))
	   name*)
       " "
       (pathname->string (pathname-extract pathname 'DEVICE 'DIRECTORY))))
    (if (not pathname)
	name
	(let ((name* (pathname->buffer-name pathname)))
	  (if (or (string-ci=? name name*)
		  (let ((i (string-match-forward-ci name name*)))
		    (and i
			 (= i (string-length name*))
			 (char=? (string-ref name i) #\<))))
	      (display-string name)
	      (string-append name " [" (display-string name*) "]"))))))

;;;; Local Bindings

(define (make-local-binding! name #!optional new-value)
  (without-interrupts
   (lambda ()
     (let ((buffer (current-buffer))
	   (value (lexical-assignment edwin-package name (set! new-value))))
       (let ((bindings (buffer-local-bindings buffer)))
	 (let ((binding (assq name bindings)))
	   (if (not binding)
	       (vector-set! buffer buffer-index:local-bindings
			    (cons (cons name value) bindings)))))))))

(define (unmake-local-binding! name)
  (without-interrupts
   (lambda ()
     (let ((buffer (current-buffer)))
       (let ((bindings (buffer-local-bindings buffer)))
	 (let ((binding (assq name bindings)))
	   (if binding
	       (begin (lexical-assignment edwin-package name (cdr binding))
		      (vector-set! buffer buffer-index:local-bindings
				   (delq! binding bindings))))))))))

(define (undo-local-bindings!)
  (without-interrupts
   (lambda ()
     (let ((buffer (current-buffer)))
       (for-each (lambda (binding)
		   (lexical-assignment edwin-package
				       (car binding)
				       (cdr binding)))
		 (buffer-local-bindings buffer))
       (vector-set! buffer buffer-index:local-bindings '())))))

(define (%wind-local-bindings! buffer)
  ;; Assumes that interrupts are disabled and that BUFFER is selected.
  (for-each (lambda (binding)
	      (set-cdr! binding
			(lexical-assignment edwin-package
					    (car binding)
					    (cdr binding))))
	    (buffer-local-bindings buffer)))
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
     (buffer-modeline-event! buffer 'BUFFER-MODES)
     (vector-set! buffer buffer-index:initializations '())
     (add-buffer-initialization! buffer undo-local-bindings!)
     (add-buffer-initialization! buffer (mode-initialization mode)))))

(define (buffer-minor-mode? buffer mode)
  (if (mode-major? mode) (error "Not a minor mode" mode))
  (memq mode (buffer-modes buffer)))

(define (enable-buffer-minor-mode! buffer mode)
  (if (mode-major? mode) (error "Not a minor mode" mode))
  (without-interrupts
   (lambda ()
     (let ((modes (buffer-modes buffer)))
       (if (not (memq mode (cdr modes)))
	   (begin (set-cdr! modes (append! (cdr modes) (list mode)))
		  (set-buffer-comtabs! buffer
				       (cons (mode-comtab mode)
					     (buffer-comtabs buffer)))
		  (buffer-modeline-event! buffer 'BUFFER-MODES)
		  (add-buffer-initialization! buffer
					      (mode-initialization mode))))))))

(define (disable-buffer-minor-mode! buffer mode)
  (if (mode-major? mode) (error "Not a minor mode" mode))
  (without-interrupts
   (lambda ()
     (let ((modes (buffer-modes buffer)))
       (if (memq mode (cdr modes))
	   (begin (set-cdr! modes (delq! mode (cdr modes)))
		  (set-buffer-comtabs! buffer
				       (delq! (mode-comtab mode)
					      (buffer-comtabs buffer)))
		  (buffer-modeline-event! buffer 'BUFFER-MODES)))))))

(define (add-buffer-initialization! buffer thunk)
  (if (eq? buffer (current-buffer))
      (thunk)
      (vector-set! buffer buffer-index:initializations
		   (append! (buffer-initializations buffer) (list thunk)))))

(define (perform-buffer-initializations! buffer)
  ;; Assumes that BUFFER is selected.
  (define (loop)
    (let ((thunks (buffer-initializations buffer)))
      (if (not (null? thunks))
	  (begin (vector-set! buffer buffer-index:initializations
			      (cdr thunks))
		 ((car thunks))
		 (loop)))))
  (loop))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: edwin-package
;;; Scheme Syntax Table: edwin-syntax-table
;;; End:

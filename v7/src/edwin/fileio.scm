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

;;;; File <-> Buffer I/O

(declare (usual-integrations))
(using-syntax (access edwin-syntax-table edwin-package)

;;;; Input

(define (read-buffer buffer pathname)
  (let ((truename (pathname->input-truename pathname)))
    (if truename
	(begin (let ((region (file->region-interactive truename)))
		 (region-delete! (buffer-unclipped-region buffer))
		 (region-insert! (buffer-start buffer) region))
	       (set-buffer-point! buffer (buffer-start buffer)))
	(temporary-message "(New File)"))
    (set-buffer-truename! buffer truename))
  (set-buffer-pathname! buffer pathname)
  (setup-buffer-auto-save! buffer)
  (set-buffer-save-length! buffer)
  (buffer-not-modified! buffer)
  (undo-done! (buffer-point buffer))
  (initialize-buffer! buffer))

(define (initialize-buffer! buffer)
  (initialize-buffer-modes! buffer)
  (initialize-buffer-local-variables! buffer))

(define (insert-file mark pathname)
  (let ((truename (pathname->input-truename pathname)))
    (if truename
	(region-insert! mark (file->region-interactive truename))
	(editor-error "File '" (pathname->string pathname) "' not found"))))

(define (file->region-interactive truename)
  (let ((filename (pathname->string truename)))
    (temporary-message "Reading file '" filename "'")
    (let ((region (file->region truename)))
      (append-message " -- done")
      region)))

(define (file->region pathname)
  (call-with-input-file pathname port->region))

(define (port->region port)
  (group-region
   (make-group
    (if (not (lexical-unreferenceable? port ':rest->string))
	((access :rest->string port))
	((access :read-string port) char-set:null)))))

;;;; Buffer Mode Initialization

(define initialize-buffer-modes!)
(let ()

(set! initialize-buffer-modes!
(named-lambda (initialize-buffer-modes! buffer)
  (let ((mode
	 (or (let ((mode-name (parse-buffer-mode-header buffer)))
	       (and mode-name
		    (let ((mode (string-table-get editor-modes mode-name)))
		      (and mode
			   (mode-major? mode)
			   mode))))
	     (filename-default-mode buffer))))
    (set-buffer-major-mode! buffer
			    (or mode (ref-variable "Editor Default Mode"))))))

(define (filename-default-mode buffer)
  (let ((entry
	 (let ((pathname (buffer-pathname buffer)))
	   (and pathname
		(let ((type (pathname-type pathname)))
		  (and (string? type)
		       (assoc-string-ci
			type
			(ref-variable "File Type to Major Mode"))))))))
    (and entry (cdr entry))))

(define assoc-string-ci
  (association-procedure string-ci=? car))

(define (parse-buffer-mode-header buffer)
  (fluid-let (((ref-variable "Case Fold Search") true))
    (let ((start (buffer-start buffer)))
      (let ((end (line-end start 0)))
	(let ((start (re-search-forward "-\\*-[ \t]*" start end)))
	  (and start
	       (re-search-forward "[ \t]*-\\*-" start end)
	       (parse-mode-header start (re-match-start 0))))))))

(define (parse-mode-header start end)
  (if (not (char-search-forward #\: start end))
      (extract-string start end)
      (let ((mode-mark (re-search-forward "mode:[ \t]*" start end)))
	(and mode-mark
	     (extract-string mode-mark
			     (if (re-search-forward "[ \t]*;" mode-mark end)
				 (re-match-start 0)
				 end))))))

)

;;;; Local Variable Initialization

(define-variable "Local Variable Search Limit"
  "The maximum number of characters searched when looking for local variables
at the end of a file."
  3000)

(define initialize-buffer-local-variables!)
(let ()

(set! initialize-buffer-local-variables!
(named-lambda (initialize-buffer-local-variables! buffer)
  (let ((end (buffer-end buffer)))
    (let ((start
	   (with-narrowed-region!
	    (make-region (mark- end
				(ref-variable "Local Variable Search Limit")
				'LIMIT)
			 end)
	    (lambda ()
	      (backward-one-page end)))))
      (if start
	  (fluid-let (((ref-variable "Case Fold Search") true))
	    (if (re-search-forward "Edwin Variables:[ \t]*" start)
		(parse-local-variables buffer
				       (re-match-start 0)
				       (re-match-end 0)))))))))

(define ((error-hook continuation var) . args)
  (beep)
  (message "Error while processing local variable: " var)
  (continuation false))

(define (evaluate sexp)
  (scode-eval (syntax sexp system-global-syntax-table)
	      system-global-environment))

(define ((local-binding-thunk name value))
  (make-local-binding! name value))

(define (parse-local-variables buffer start end)
  (let ((prefix (extract-string (line-start start 0) start))
	(suffix (extract-string end (line-end end 0))))
    (let ((prefix-length (string-length prefix))
	  (prefix? (not (string-null? prefix)))
	  (suffix-length (string-length suffix))
	  (suffix? (not (string-null? suffix))))
      (define (loop mark)
	(let ((start (line-start mark 1)))
	  (if (not start) (editor-error "Missing local variables entry"))
	  (do-line start (line-end start 0))))

      (define (do-line start end)
	(define (check-suffix mark)
	  (if (and suffix? (not (match-forward suffix mark)))
	      (editor-error "Local variables entry is missing the suffix")))
	(let ((m1
	       (horizontal-space-end
		(if prefix?
		    (or (match-forward prefix start)
			(editor-error
			 "Local variables entry is missing the prefix"))
		    start))))
	  (let ((m2 (if (char-search-forward #\: m1 end)
			(re-match-start 0)
			(editor-error
			 "Missing colon in local variables entry"))))
	    (let ((var (extract-string m1 (horizontal-space-start m2)))
		  (m3 (horizontal-space-end (mark1+ m2))))
	      (if (not (string-ci=? var "End"))
		  (with-input-from-mark m3 read
		    (lambda (val m4)
		      (check-suffix (horizontal-space-end m4))
		      (if (string-ci=? var "Mode")
			  (let ((mode (string-table-get
				       editor-modes
				       (extract-string m3 m4))))
			    (if mode
				((if (mode-major? mode)
				     set-buffer-major-mode!
				     enable-buffer-minor-mode!)
				 buffer mode)))
			  (call-with-current-continuation
			   (lambda (continuation)
			     (fluid-let (((access *error-hook* error-system)
					  (error-hook continuation var)))
			       (if (string-ci=? var "Eval")
				   (evaluate val)
				   (add-buffer-initialization!
				    buffer
				    (local-binding-thunk
				     (variable-symbol (name->variable var))
				     (evaluate val))))))))
		      (loop m4))))))))

      (loop start))))

)

;;;; Output

(define (write-buffer-interactive buffer)
  (if (or (buffer-writeable? buffer)
	  (prompt-for-confirmation?
	   (string-append "Buffer '"
			  (buffer-name buffer)
			  "' is read only.  Save anyway")))
      (begin (require-newline buffer)
	     (write-buffer buffer))))

(define-variable "Require Final Newline"
  "True says silently put a newline at the end whenever a file is saved.
Neither false nor true says ask user whether to add a newline in each
such case.  False means don't add newlines."
  false)

(define (require-newline buffer)
  (if (ref-variable "Require Final Newline")
      (without-group-clipped! (buffer-group buffer)
        (lambda ()
	  (let ((end (buffer-end buffer)))
	    (if (and (not (eqv? char:newline (extract-left-char end)))
		     (or (eq? (ref-variable "Require Final Newline") true)
			 (prompt-for-yes-or-no?
			  (string-append
			   "Buffer " (buffer-name buffer)
			   " does not end in newline.  Add one"))))
		(insert-newline end)))))))

(define (write-buffer buffer)
  (let ((truename (write-region (buffer-unclipped-region buffer)
				(buffer-pathname buffer))))
    (if truename
	(begin (set-buffer-truename! buffer truename)
	       (delete-auto-save-file! buffer)
	       (set-buffer-save-length! buffer)
	       (buffer-not-modified! buffer)))))

(define (write-region region pathname)
  (let ((truename (pathname->output-truename pathname)))
    (let ((filename (pathname->string truename)))
      (and (or (not (file-exists? truename))
	       (prompt-for-yes-or-no?
		(string-append "File '" filename "' exists.  Write anyway")))
	   (begin (temporary-message "Writing file '" filename "'")
		  (region->file region truename)
		  (append-message " -- done")
		  truename)))))

(define (region->file region pathname)
  (call-with-output-file pathname
    (lambda (port)
      (region->port port region))))

(define (region->port port region)
  ((access :write-string port) (region->string region)))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: edwin-package
;;; Scheme Syntax Table: (access edwin-syntax-table edwin-package)
;;; Tags Table Pathname: (access edwin-tags-pathname edwin-package)
;;; End:

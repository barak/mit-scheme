;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/bufinp.scm,v 1.3 1990/11/09 08:56:14 cph Rel $
;;;
;;;	Copyright (c) 1986, 1989, 1990 Massachusetts Institute of Technology
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

;;;; Buffer Input Ports

(declare (usual-integrations))

(define (with-input-from-mark mark thunk #!optional receiver)
  (let ((port (make-buffer-input-port mark (group-end mark))))
    (let ((value (with-input-from-port port thunk)))
      (if (default-object? receiver)
	  value
	  (receiver
	   value
	   (let ((state (input-port/state port)))
	     (make-mark (buffer-input-port-state/group state)
			(buffer-input-port-state/current-index state))))))))

(define (with-input-from-region region thunk)
  (with-input-from-port (make-buffer-input-port (region-start region)
						(region-end region))
    thunk))

(define-structure (buffer-input-port-state
		   (conc-name buffer-input-port-state/))
  (group false read-only true)
  (end-index false read-only true)
  (current-index false))

(define (make-buffer-input-port mark end)
  ;; This uses indices, so it can only be used locally
  ;; where there is no buffer-modification happening.
  (input-port/copy buffer-input-port-template
		   (make-buffer-input-port-state (mark-group mark)
						 (mark-index end)
						 (mark-index mark))))

(define (operation/char-ready? port interval)
  interval				;ignore
  (let ((state (input-port/state port)))
    (< (buffer-input-port-state/current-index state)
       (buffer-input-port-state/end-index state))))

(define (operation/peek-char port)
  (let ((state (input-port/state port)))
    (let ((current-index (buffer-input-port-state/current-index state)))
      (if (< current-index (buffer-input-port-state/end-index state))
	  (group-right-char (buffer-input-port-state/group state)
			    current-index)
	  (make-eof-object port)))))

(define (operation/discard-char port)
  (let ((state (input-port/state port)))
    (set-buffer-input-port-state/current-index!
     state
     (1+ (buffer-input-port-state/current-index state)))))

(define (operation/read-char port)
  (let ((state (input-port/state port)))
    (let ((current-index (buffer-input-port-state/current-index state)))
      (if (< current-index (buffer-input-port-state/end-index state))
	  (let ((char
		 (group-right-char (buffer-input-port-state/group state)
				   current-index)))
	    (set-buffer-input-port-state/current-index! state
							(1+ current-index))
	    char)
	  (make-eof-object port)))))

(define (operation/read-string port delimiters)
  (let ((state (input-port/state port)))
    (let ((current-index (buffer-input-port-state/current-index state))
	  (end-index (buffer-input-port-state/end-index state))
	  (group (buffer-input-port-state/group state)))
      (if (>= current-index end-index)
	  (make-eof-object port)
	  (let ((new-index
		 (or (%find-next-char-in-set group current-index end-index
					     delimiters)
		     end-index)))
	    (let ((string
		   (group-extract-string group current-index new-index)))
	      (set-buffer-input-port-state/current-index! state new-index)
	      string))))))

(define (operation/discard-chars port delimiters)
  (let ((state (input-port/state port)))
    (let ((current-index (buffer-input-port-state/current-index state))
	  (end-index (buffer-input-port-state/end-index state)))
      (if (< current-index end-index)
	  (set-buffer-input-port-state/current-index!
	   state
	   (or (%find-next-char-in-set (buffer-input-port-state/group state)
				       current-index
				       end-index
				       delimiters)
	       end-index))))))

(define (operation/print-self state port)
  (unparse-string state "from buffer at ")
  (unparse-object
   state
   (let ((state (input-port/state port)))
     (make-mark (buffer-input-port-state/group state)
		(buffer-input-port-state/current-index state)))))

(define buffer-input-port-template
  (make-input-port `((CHAR-READY? ,operation/char-ready?)
		     (DISCARD-CHAR ,operation/discard-char)
		     (DISCARD-CHARS ,operation/discard-chars)
		     (PEEK-CHAR ,operation/peek-char)
		     (PRINT-SELF ,operation/print-self)
		     (READ-CHAR ,operation/read-char)
		     (READ-STRING ,operation/read-string))
		   false))
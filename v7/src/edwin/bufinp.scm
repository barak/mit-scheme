;;; -*-Scheme-*-
;;;
;;;$Id: bufinp.scm,v 1.9 2002/11/20 19:45:58 cph Exp $
;;;
;;; Copyright (c) 1986, 1989-1999 Massachusetts Institute of Technology
;;;
;;; This file is part of MIT Scheme.
;;;
;;; MIT Scheme is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation; either version 2 of the License,
;;; or (at your option) any later version.
;;;
;;; MIT Scheme is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with MIT Scheme; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

;;;; Buffer Input Ports

(declare (usual-integrations))

(define (with-input-from-mark mark thunk #!optional receiver)
  (let ((port (make-buffer-input-port mark (group-end mark))))
    (let ((value (with-input-from-port port thunk)))
      (if (default-object? receiver)
	  value
	  (receiver
	   value
	   (let ((state (port/state port)))
	     (make-mark (buffer-input-port-state/group state)
			(buffer-input-port-state/current-index state))))))))

(define (with-input-from-region region thunk)
  (with-input-from-port (make-buffer-input-port (region-start region)
						(region-end region))
    thunk))

(define-structure (buffer-input-port-state
		   (conc-name buffer-input-port-state/))
  (group #f read-only #t)
  (end-index #f read-only #t)
  (current-index #f))

(define (make-buffer-input-port mark end)
  ;; This uses indices, so it can only be used locally
  ;; where there is no buffer-modification happening.
  (make-port buffer-input-port-type
	     (make-buffer-input-port-state (mark-group mark)
					   (mark-index end)
					   (mark-index mark))))

(define (operation/char-ready? port interval)
  interval				;ignore
  (let ((state (port/state port)))
    (< (buffer-input-port-state/current-index state)
       (buffer-input-port-state/end-index state))))

(define (operation/peek-char port)
  (let ((state (port/state port)))
    (let ((current-index (buffer-input-port-state/current-index state)))
      (if (< current-index (buffer-input-port-state/end-index state))
	  (group-right-char (buffer-input-port-state/group state)
			    current-index)
	  (make-eof-object port)))))

(define (operation/discard-char port)
  (let ((state (port/state port)))
    (set-buffer-input-port-state/current-index!
     state
     (1+ (buffer-input-port-state/current-index state)))))

(define (operation/read-char port)
  (let ((state (port/state port)))
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
  (let ((state (port/state port)))
    (let ((current-index (buffer-input-port-state/current-index state))
	  (end-index (buffer-input-port-state/end-index state))
	  (group (buffer-input-port-state/group state)))
      (if (>= current-index end-index)
	  (make-eof-object port)
	  (let ((new-index
		 (or (group-find-next-char-in-set group current-index end-index
						  delimiters)
		     end-index)))
	    (let ((string
		   (group-extract-string group current-index new-index)))
	      (set-buffer-input-port-state/current-index! state new-index)
	      string))))))

(define (operation/discard-chars port delimiters)
  (let ((state (port/state port)))
    (let ((current-index (buffer-input-port-state/current-index state))
	  (end-index (buffer-input-port-state/end-index state)))
      (if (< current-index end-index)
	  (set-buffer-input-port-state/current-index!
	   state
	   (or (group-find-next-char-in-set
		(buffer-input-port-state/group state)
		current-index
		end-index
		delimiters)
	       end-index))))))

(define (operation/print-self state port)
  (unparse-string state "from buffer at ")
  (unparse-object
   state
   (let ((state (port/state port)))
     (make-mark (buffer-input-port-state/group state)
		(buffer-input-port-state/current-index state)))))

(define buffer-input-port-type
  (make-port-type `((CHAR-READY? ,operation/char-ready?)
		    (DISCARD-CHAR ,operation/discard-char)
		    (DISCARD-CHARS ,operation/discard-chars)
		    (PEEK-CHAR ,operation/peek-char)
		    (PRINT-SELF ,operation/print-self)
		    (READ-CHAR ,operation/read-char)
		    (READ-STRING ,operation/read-string))
		  #f))
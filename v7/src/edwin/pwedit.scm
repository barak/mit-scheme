;;; -*-Scheme-*-
;;;
;;; $Id: pwedit.scm,v 1.1 1999/01/14 21:30:55 cph Exp $
;;;
;;; Copyright (c) 1999 Massachusetts Institute of Technology
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
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;;; Password-Database Editor

;;; This program provides editing capabilities for a text-format
;;; password database.  The primary capability of this program is to
;;; permit editing a database of passwords without having all of the
;;; passwords visible on the screen at once.  Instead, the program
;;; displays a set of key names, and the user selectively reveals the
;;; password information hidden behind those keys.

(declare (usual-integrations))

(define-command view-password-file
  "Read in a password file and show it in password-view mode."
  "fView password file"
  (lambda (pathname)
    (let ((forms
	   (call-with-temporary-buffer " view-pw-file"
	     (lambda (buffer)
	       (read-buffer buffer pathname #f)
	       (read-pw-forms
		(make-buffer-input-port (buffer-start buffer)
					(buffer-end buffer)))))))
      (let ((buffer (new-buffer (pathname->buffer-name pathname))))
	(insert-pw-forms forms (buffer-start buffer))
	(set-buffer-major-mode! buffer (ref-mode-object password-view))
	(set-buffer-point! buffer (buffer-start buffer))
	(select-buffer buffer)))))

(define-major-mode password-view read-only "Password-View"
  "Major mode specialized for viewing password files."
  (lambda (buffer)
    (set-buffer-read-only! buffer)
    unspecific))

(define-key 'password-view #\space 'toggle-pw-form)
(define-key 'password-view button1-down 'mouse-toggle-pw-form)

(define-command toggle-pw-form
  "Toggle the body of the password form under point."
  "d"
  (lambda (point)
    (if (get-pw-form point)
	(toggle-pw-body point)
	(message "No form under point."))))

(define-command mouse-toggle-pw-form
  "Toggle the body of the password form under mouse."
  ()
  (lambda ()
    ((ref-command toggle-pw-form)
     (let ((button-event (current-button-event)))
       (let ((window (button-event/window button-event)))
	 (select-window window)
	 (or (window-coordinates->mark window
				       (button-event/x button-event)
				       (button-event/y button-event))
	     (buffer-end (window-buffer window))))))))
     

(define (insert-pw-forms pw-forms point)
  (let ((point (mark-left-inserting-copy point)))
    (for-each
     (lambda (form)
       (let ((type (car form))
	     (body (cdr form))
	     (start (mark-right-inserting-copy point)))
	 (case type
	   ((BLANK)
	    (insert-newline point))
	   ((COMMENT)
	    (for-each (lambda (line)
			(insert-string ";" point)
			(insert-string line point)
			(insert-newline point))
		      body))
	   ((SHORT LONG)
	    (insert-string (car body) point)
	    (insert-string ":" point)
	    (insert-newline point))
	   (else
	    (error "Unknown form type:" type)))
	 (region-put! start point 'PW-FORM form)
	 (mark-temporary! start)))
     pw-forms)
    (mark-temporary! point)))

(define (get-pw-form point)
  (let ((form (region-get point 'PW-FORM #f)))
    (and form
	 (memq (car form) '(SHORT LONG))
	 form)))

(define (toggle-pw-body point) (modify-pw-body point 'TOGGLE))
(define (insert-pw-body point) (modify-pw-body point 'INSERT))
(define (delete-pw-body point) (modify-pw-body point 'DELETE))

(define (modify-pw-body point operation)
  (with-buffer-open (mark-buffer point)
    (lambda ()
      (let ((form
	     (or (get-pw-form point)
		 (error:bad-range-argument point 'INSERT-PW-BODY)))
	    (le (line-end point 0)))
	(if (eq? 'SHORT (car form))
	    (let ((region (short-pw-body-region point)))
	      (if region
		  (region-delete! region))
	      (if (or (eq? 'INSERT operation)
		      (and (eq? 'TOGGLE operation)
			   (not region)))
		  (let ((end (mark-left-inserting-copy (line-end point 0))))
		    (insert-pw-body-spacer end)
		    (insert-string (cddr form) end)
		    (mark-temporary! end))))
	    (let ((region (long-pw-body-region point)))
	      (if region
		  (region-delete! region))
	      (if (or (eq? 'INSERT operation)
		      (and (eq? 'TOGGLE operation)
			   (not region)))
		  (let ((end (mark-left-inserting-copy (line-end point 0))))
		    (for-each (lambda (line)
				(insert-newline end)
				(if (pair? line)
				    (begin
				      (insert-string (car line) end)
				      (insert-string ":" end)
				      (insert-pw-body-spacer end)
				      (insert-string (cdr line) end))
				    (insert-string line end)))
			      (cddr form))
		    (mark-temporary! end)))))))))

(define (short-pw-body-region point)
  (let ((end (line-end point 0)))
    (let ((start (next-specific-property-change* point end 'PW-FORM)))
      (and start
	   (mark< start end)
	   (make-region start end)))))

(define (long-pw-body-region point)
  (let ((start (line-end point 0)))
    (let ((end
	   (let loop ((m start))
	     (let ((m* (mark1+ m)))
	       (if m*
		   (if (line-blank? m*)
		       m
		       (loop (line-end m* 0)))
		   m)))))
      (and (mark< start end)
	   (make-region start end)))))

(define (insert-pw-body-spacer point)
  (insert-string (let ((column (mark-column point)))
		   (cond ((< column 8) "\t\t")
			 ((< column 16) "\t")
			 (else " ")))
		 point))

(define (next-specific-property-change* start end key)
  (let ((index
	 (next-specific-property-change (mark-group start)
					(mark-index start)
					(mark-index end)
					key)))
    (and index
	 (make-mark (mark-group start) index))))
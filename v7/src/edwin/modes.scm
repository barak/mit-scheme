;;; -*-Scheme-*-
;;;
;;; $Id: modes.scm,v 1.28 1999/01/02 06:11:34 cph Exp $
;;;
;;; Copyright (c) 1986, 1989-1999 Massachusetts Institute of Technology
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

;;;; Modes

(declare (usual-integrations))

(define-structure (mode
		   (constructor %make-mode (name comtabs))
		   (print-procedure
		    (unparser/standard-method 'MODE
		      (lambda (state mode)
			(unparse-object state (mode-name mode))
			(if (not (mode-major? mode))
			    (unparse-string state " (minor)"))))))
  (name false read-only true)
  (comtabs false read-only true)
  display-name
  major?
  %description
  initialization
  alist)

(define (mode-description mode)
  (let ((desc (mode-%description mode)))
    (if (string? desc)
	desc
	(let ((new (->doc-string (%symbol->string (mode-name mode))
				 desc)))
	  (if new
	      (set-mode-%description! mode new))
	  new))))

(define (make-mode name major? display-name super-mode description
		   initialization)
  (let* ((sname (symbol->string name))
	 (mode
	  (or (string-table-get editor-modes sname)
	      (let ((mode (%make-mode name (list (make-comtab)))))
		(string-table-put! editor-modes sname mode)
		mode))))
    (set-mode-display-name! mode display-name)
    (set-mode-major?! mode major?)
    (set-cdr! (mode-comtabs mode)
	      (cond ((not super-mode)
		     '())
		    ((mode? super-mode)
		     (mode-comtabs super-mode))
		    (else
		     ;; Old code passes a comtabs list here, so accept
		     ;; that as a valid argument.  Later, this can be
		     ;; an error.
		     super-mode)))
    (set-mode-%description! mode (doc-string->posn sname description))
    (set-mode-initialization! mode initialization)
    (set-mode-alist! mode '())
    mode))

(define editor-modes
  (make-string-table))

(define (->mode object)
  (if (mode? object)
      object
      (let ((name (canonicalize-name object)))
	(or (string-table-get editor-modes (symbol->string name))
	    (make-mode name
		       true
		       (symbol->string name)
		       false
		       ""
		       (lambda () (error "Undefined mode" name)))))))

(define (major-mode? object)
  (and (mode? object)
       (mode-major? object)))

(define (minor-mode? object)
  (and (mode? object)
       (not (mode-major? object))))

(define-integrable (minor-mode-comtab mode)
  (car (mode-comtabs mode)))
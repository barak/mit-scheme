;;; -*-Scheme-*-
;;;
;;; $Id: modes.scm,v 1.33 2000/06/15 00:43:51 cph Exp $
;;;
;;; Copyright (c) 1986, 1989-2000 Massachusetts Institute of Technology
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
		   (constructor %make-mode
				(name major? display-name super-mode
				      %description initialization comtabs))
		   (print-procedure
		    (unparser/standard-method 'MODE
		      (lambda (state mode)
			(unparse-object state (mode-name mode))
			(if (not (mode-major? mode))
			    (unparse-string state " (minor)"))))))
  (name #f read-only #t)
  major?
  display-name
  super-mode
  %description
  initialization
  (comtabs #f read-only #t))

(define (make-mode name major? display-name super-mode description
		   initialization)
  (if (not (or (not super-mode)
	       (and major? (major-mode? super-mode))))
      (error:wrong-type-argument super-mode "major mode" 'MAKE-MODE))
  (let ((sname (symbol->string name))
	(major? (if major? #t #f))
	(super-comtabs (if super-mode (mode-comtabs super-mode) '())))
    (let ((mode (string-table-get editor-modes sname))
	  (description (doc-string->posn sname description)))
      (if mode
	  (begin
	    (set-mode-major?! mode major?)
	    (set-mode-display-name! mode display-name)
	    (set-mode-super-mode! mode super-mode)
	    (set-cdr! (mode-comtabs mode) super-comtabs)
	    (set-mode-%description! mode description)
	    (set-mode-initialization! mode initialization)
	    mode)
	  (let ((mode
		 (%make-mode name
			     major?
			     display-name
			     super-mode
			     description
			     initialization
			     (cons (make-comtab) super-comtabs))))
	    (string-table-put! editor-modes sname mode)
	    mode)))))

(define editor-modes
  (make-string-table))

(define (name->mode object #!optional if-undefined)
  (let ((name (canonicalize-name object)))
    (let ((sname (symbol->string name)))
      (or (string-table-get editor-modes sname)
	  (case (if (default-object? if-undefined) 'INTERN if-undefined)
	    ((#F) #f)
	    ((ERROR) (error "Undefined mode:" name))
	    ((INTERN)
	     (make-mode name #t sname #f ""
			(lambda () (error "Undefined mode:" name))))
	    
	  (else
	   (error:bad-range-argument if-undefined 'NAME->MODE)))))))

(define (->mode object)
  (if (mode? object)
      object
      (name->mode object)))

(define (major-mode? object)
  (and (mode? object)
       (mode-major? object)))

(define (minor-mode? object)
  (and (mode? object)
       (not (mode-major? object))))

(define (minor-mode-comtab mode)
  (car (mode-comtabs mode)))

(define (mode-description mode)
  (let ((desc (mode-%description mode)))
    (if (description? desc)
	desc
	(let ((new (->doc-string (symbol->string (mode-name mode)) desc)))
	  (if new
	      (set-mode-%description! mode new))
	  new))))

(define (sub-mode? m1 m2)
  (if (not (mode? m1))
      (error:wrong-type-argument m1 "mode" 'SUB-MODE?))
  (if (not (mode? m2))
      (error:wrong-type-argument m2 "mode" 'SUB-MODE?))
  (or (eq? m1 m2)
      (%strict-sub-mode? m1 m2)))

(define (strict-sub-mode? m1 m2)
  (if (not (mode? m1))
      (error:wrong-type-argument m1 "mode" 'STRICT-SUB-MODE?))
  (if (not (mode? m2))
      (error:wrong-type-argument m2 "mode" 'STRICT-SUB-MODE?))
  (%strict-sub-mode? m1 m2))

(define (%strict-sub-mode? m1 m2)
  (let loop ((m1 m1))
    (let ((m1 (mode-super-mode m1)))
      (and m1
	   (or (eq? m1 m2)
	       (loop m1))))))
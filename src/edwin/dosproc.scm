;;; -*-Scheme-*-
;;;
;;; $Id: dosproc.scm,v 1.9 2002/02/09 05:55:12 cph Exp $
;;;
;;; Copyright (c) 1992-2002 Massachusetts Institute of Technology
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
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

;;;; Dummy subprocess support
;; package: (edwin process)

(declare (usual-integrations))

(define subprocesses-available?
  #f)

(define (process-list)
  '())

(define (get-buffer-process buffer)
  buffer
  #f)

(define (buffer-processes buffer)
  buffer
  '())

(define (process-operation name)
  (lambda (process)
    (editor-error "Processes not implemented" name process)))

(let-syntax ((define-process-operation
	      (sc-macro-transformer
	       (lambda (form environment)
		 environment
		 `(DEFINE ,(cadr form) (PROCESS-OPERATION ',(cadr form)))))))
  (define-process-operation delete-process))

(define (process-status-changes?)
  #f)

(define (process-output-available?)
  #f)
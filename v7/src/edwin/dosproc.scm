;;; -*-Scheme-*-
;;;
;;; $Id: dosproc.scm,v 1.5 2000/10/26 02:28:04 cph Exp $
;;;
;;; Copyright (c) 1992-2000 Massachusetts Institute of Technology
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

;;;; Dummy subprocess support
;; package: (edwin process)

(declare (usual-integrations))

(define subprocesses-available? false)

(define (process-list)
  '())

(define (get-buffer-process buffer)
  buffer
  false)

(define (buffer-processes buffer)
  buffer
  '())

(define-integrable (process-operation name)
  (lambda (process)
    (editor-error "Processes not implemented" name process)))

(let-syntax ((define-process-operation
	       (macro (name)
		 `(define ,name (process-operation ',name)))))

  (define-process-operation delete-process))

(define (process-status-changes?)
  false)

(define (process-output-available?)
  false)
;;; -*-Scheme-*-
;;;
;;; $Id: midas.scm,v 1.21 2000/03/02 05:31:48 cph Exp $
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

;;;; Midas Mode

(declare (usual-integrations))

(define-command midas-mode
  "Enter Midas mode."
  ()
  (lambda () (set-current-major-mode! (ref-mode-object midas))))

(define-major-mode midas fundamental "Midas"
  "Major mode for editing assembly code."
  (lambda (buffer)
    (local-set-variable! syntax-table midas-mode:syntax-table buffer)
    (local-set-variable! comment-column 40 buffer)
    (local-set-variable! comment-locator-hook lisp-comment-locate buffer)
    (local-set-variable! comment-indent-hook midas-comment-indentation buffer)
    (local-set-variable! comment-start ";" buffer)
    (local-set-variable! comment-end "" buffer)
    (standard-alternate-paragraph-style! buffer)
    (local-set-variable! indent-line-procedure (ref-command insert-tab) buffer)
    (local-set-variable! local-abbrev-table
			 (ref-variable midas-mode-abbrev-table buffer)
			 buffer)
    (event-distributor/invoke! (ref-variable midas-mode-hook buffer) buffer)))

(define midas-mode:syntax-table (make-syntax-table))
(modify-syntax-entry! midas-mode:syntax-table #\; "<   ")
(modify-syntax-entry! midas-mode:syntax-table #\newline ">   ")
(modify-syntax-entry! midas-mode:syntax-table #\. "w   ")
(modify-syntax-entry! midas-mode:syntax-table #\' "'   ")
(modify-syntax-entry! midas-mode:syntax-table #\$ "'   ")
(modify-syntax-entry! midas-mode:syntax-table #\% "'   ")
(modify-syntax-entry! midas-mode:syntax-table #\# "'   ")

(define (midas-comment-indentation mark)
  (if (match-forward ";;;" mark)
      0
      (max (+ (mark-column (horizontal-space-start mark)) 1)
	   (ref-variable comment-column mark))))
;;; -*-Scheme-*-
;;;
;;; $Id: load.scm,v 1.6 2000/05/22 13:25:38 cph Exp $
;;;
;;; Copyright (c) 1999-2000 Massachusetts Institute of Technology
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

;;;; IMAIL mail reader: loader

(load-option 'HASH-TABLE)
(load-option 'REGULAR-EXPRESSION)
(load-option 'SOS)
(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (fluid-let ((*allow-package-redefinition?* #t))
      (package/system-loader "imail" '() 'QUERY))))

;; Kludge necessary to allow reloading of this package.
(for-each (let ((from (->environment '(EDWIN IMAIL)))
		(to (->environment '(EDWIN))))
	    (lambda (name)
	      (if (lexical-unreferenceable? to name)
		  (environment-link-name to from name)
		  (lexical-assignment to name (lexical-reference from name)))))
	  '(edwin-command$imail
	    edwin-command$imail-add-flag
	    edwin-command$imail-continue
	    edwin-command$imail-create-folder
	    edwin-command$imail-delete-backward
	    edwin-command$imail-delete-folder
	    edwin-command$imail-delete-forward
	    edwin-command$imail-delete-message
	    edwin-command$imail-expunge
	    edwin-command$imail-first-message
	    edwin-command$imail-forward
	    edwin-command$imail-get-new-mail
	    edwin-command$imail-input
	    edwin-command$imail-kill-flag
	    edwin-command$imail-last-message
	    edwin-command$imail-mail
	    edwin-command$imail-next-flagged-message
	    edwin-command$imail-next-message
	    edwin-command$imail-next-undeleted-message
	    edwin-command$imail-output
	    edwin-command$imail-previous-flagged-message
	    edwin-command$imail-previous-message
	    edwin-command$imail-previous-undeleted-message
	    edwin-command$imail-quit
	    edwin-command$imail-reply
	    edwin-command$imail-resend
	    edwin-command$imail-save-folder
	    edwin-command$imail-search
	    edwin-command$imail-select-message
	    edwin-command$imail-summary
	    edwin-command$imail-summary-by-flags
	    edwin-command$imail-summary-by-recipients
	    edwin-command$imail-summary-quit
	    edwin-command$imail-summary-select-message
	    edwin-command$imail-toggle-header
	    edwin-command$imail-undelete-backward
	    edwin-command$imail-undelete-forward
	    edwin-command$imail-undelete-previous-message
	    edwin-mode$imail
	    edwin-mode$imail-summary
	    edwin-variable$imail-default-dont-reply-to-names
	    edwin-variable$imail-default-imap-mailbox
	    edwin-variable$imail-default-imap-server
	    edwin-variable$imail-default-user-id
	    edwin-variable$imail-delete-after-output
	    edwin-variable$imail-dont-reply-to-names
	    edwin-variable$imail-ignored-headers
	    edwin-variable$imail-kept-headers
	    edwin-variable$imail-message-filter
	    edwin-variable$imail-mode-hook
	    edwin-variable$imail-primary-folder
	    edwin-variable$imail-reply-with-re
	    edwin-variable$imail-summary-highlight-message
	    edwin-variable$imail-summary-mode-hook
	    edwin-variable$imail-summary-pop-up-message
	    edwin-variable$imail-summary-show-date
	    edwin-variable$imail-summary-subject-width))
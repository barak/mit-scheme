;;; -*-Scheme-*-
;;;
;;; $Id: htmlmode.scm,v 1.5 2001/12/18 22:12:18 cph Exp $
;;;
;;; Copyright (c) 1999-2001 Massachusetts Institute of Technology
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

;;;; Major Mode for HTML

(declare (usual-integrations))

(define-major-mode html text "HTML"
  "Major mode for editing HTML.

\\{html}"
  (lambda (buffer)
    (local-set-variable! syntax-table html-syntax-table buffer)
    (local-set-variable! indent-line-procedure
			 (ref-command indent-relative)
			 buffer)
    (local-set-variable! paragraph-separate html-paragraph-separator buffer)
    (local-set-variable! paragraph-start html-paragraph-separator buffer)
    (local-set-variable! syntax-ignore-comments-backwards #f buffer)
    (local-set-variable! comment-locator-hook html-comment-locate buffer)
    (local-set-variable! comment-indent-hook html-comment-indentation buffer)
    (local-set-variable! comment-start "<!-- " buffer)
    (local-set-variable! comment-end " -->" buffer)
    (local-set-variable!
     sentence-end
     "[.?!][]\"')}]*\\(<[^>]*>\\)*\\($\\| $\\|\t\\|  \\)[ \t\n]*"
     buffer)
    (local-set-variable! local-abbrev-table
			 (ref-variable html-mode-abbrev-table buffer)
			 buffer)
    (event-distributor/invoke! (ref-variable html-mode-hook buffer) buffer)))

(define html-paragraph-separator
  "[ \t]*$\\|[ \t]*</?\\([A-Za-z]\\([-.A-Za-z0-9= \t\n]\\|\"[^\"]*\"\\|'[^']*'\\)*\\)?>$")

(define-command html-mode
  "Enter HTML mode."
  ()
  (lambda () (set-current-major-mode! (ref-mode-object html))))

(define-variable html-mode-abbrev-table
  "Mode-specific abbrev table for HTML.")
(define-abbrev-table 'html-mode-abbrev-table '())

(define-variable html-mode-hook
  "An event distributor that is invoked when entering HTML mode."
  (make-event-distributor))

(define html-syntax-table
  (let ((syntax-table (make-char-syntax-table text-mode:syntax-table)))
    (set-char-syntax! syntax-table #\< "(>1")
    (set-char-syntax! syntax-table #\! ". 2")
    (set-char-syntax! syntax-table #\- "_ 3")
    (set-char-syntax! syntax-table #\> ")<4")
    (set-char-syntax! syntax-table #\" "\"\"")
    syntax-table))

(define (html-comment-locate mark)
  (and (re-search-forward "<!--+[ \t]*" (line-start mark 0) (line-end mark 0))
       (cons (re-match-start 0) (re-match-end 0))))

(define (html-comment-indentation mark)
  mark
  0)
;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/tximod.scm,v 1.15 1991/10/02 09:16:18 cph Exp $
;;;
;;;	Copyright (c) 1987-91 Massachusetts Institute of Technology
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

;;;; Texinfo Mode

(declare (usual-integrations))

(define-command texinfo-mode
  "Make the current mode be Texinfo mode."
  ()
  (lambda ()
    (set-current-major-mode! (ref-mode-object texinfo))))

(define-major-mode texinfo text "Texinfo"
  "Major mode for editing Texinfo files.

  These are files that are used as input for TeX to make printed manuals
and also to be turned into Info files by \\[texinfo-format-buffer] or
`makeinfo'.  These files must be written in a very restricted and
modified version of TeX input format.

  Editing commands are like text-mode except that the syntax table is
set up so expression commands skip Texinfo bracket groups.

  In addition, Texinfo mode provides commands that insert various
frequently used @-sign commands into the buffer.  You can use these
commands to save keystrokes."
  (local-set-variable! syntax-table texinfo-mode:syntax-table)
  (local-set-variable! fill-column 72)
  (local-set-variable! require-final-newline true)
  (local-set-variable!
   page-delimiter
   "^@\\(chapter\\|unnumbered \\|appendix \\|majorheading\\|chapheading\\)")
  (local-set-variable! paragraph-start
		       (string-append "^\\|^@[a-zA-Z]*[ \n]\\|"
				      (ref-variable paragraph-start)))
  (local-set-variable! paragraph-separate
		       (string-append "^\\|^@[a-zA-Z]*[ \n]\\|"
				      (ref-variable paragraph-separate)))
  (event-distributor/invoke! (ref-variable texinfo-mode-hook)))

(define texinfo-mode:syntax-table (make-syntax-table))
(modify-syntax-entry! texinfo-mode:syntax-table #\" " ")
(modify-syntax-entry! texinfo-mode:syntax-table #\\ " ")
(modify-syntax-entry! texinfo-mode:syntax-table #\@ "\\")
(modify-syntax-entry! texinfo-mode:syntax-table #\DC1 "\\")
(modify-syntax-entry! texinfo-mode:syntax-table #\' "w")

(define-key 'texinfo '(#\C-c #\C-c #\c) 'texinfo-insert-@code)
(define-key 'texinfo '(#\C-c #\C-c #\d) 'texinfo-insert-@dfn)
(define-key 'texinfo '(#\C-c #\C-c #\e) 'texinfo-insert-@end)
(define-key 'texinfo '(#\C-c #\C-c #\i) 'texinfo-insert-@item)
(define-key 'texinfo '(#\C-c #\C-c #\k) 'texinfo-insert-@kbd)
(define-key 'texinfo '(#\C-c #\C-c #\o) 'texinfo-insert-@noindent)
(define-key 'texinfo '(#\C-c #\C-c #\r) 'texinfo-insert-@refill)
(define-key 'texinfo '(#\C-c #\C-c #\s) 'texinfo-insert-@samp)
(define-key 'texinfo '(#\C-c #\C-c #\v) 'texinfo-insert-@var)
(define-key 'texinfo '(#\C-c #\C-c #\x) 'texinfo-insert-@example)

(define-command texinfo-insert-@code
  "Insert the string @code in a texinfo buffer."
  ()
  (lambda ()
    (insert-string "@code{}")
    (set-current-point! (mark-1+ (current-point)))))

(define-command texinfo-insert-@dfn
  "Insert the string @dfn in a texinfo buffer."
  ()
  (lambda ()
    (insert-string "@dfn{}")
    (set-current-point! (mark-1+ (current-point)))))

(define-command texinfo-insert-@kbd
  "Insert the string @kbd in a texinfo buffer."
  ()
  (lambda ()
    (insert-string "@kbd{}")
    (set-current-point! (mark-1+ (current-point)))))

(define-command texinfo-insert-@samp
  "Insert the string @samp in a texinfo buffer."
  ()
  (lambda ()
    (insert-string "@samp{}")
    (set-current-point! (mark-1+ (current-point)))))

(define-command texinfo-insert-@var
  "Insert the string @var in a texinfo buffer."
  ()
  (lambda ()
    (insert-string "@var{}")
    (set-current-point! (mark-1+ (current-point)))))

(define-command texinfo-insert-@end
  "Insert the string `@end ' (end followed by a space) in a texinfo buffer."
  ()
  (lambda ()
    (insert-string "@end ")))

(define-command texinfo-insert-@refill
  "Insert the string @refill in a texinfo buffer."
  ()
  (lambda ()
    (insert-string "@refill")))

(define-command texinfo-insert-@example
  "Insert the string @example in a texinfo buffer."
  ()
  (lambda ()
    (insert-string "@example")
    (insert-newline)))

(define-command texinfo-insert-@item
  "Insert the string @item in a texinfo buffer."
  ()
  (lambda ()
    (insert-string "@item")
    (insert-newline)))

(define-command texinfo-insert-@noindent
  "Insert the string @noindent in a texinfo buffer."
  ()
  (lambda ()
    (insert-string "@noindent")
    (insert-newline)))
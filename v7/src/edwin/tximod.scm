;;; -*-Scheme-*-
;;;
;;;	$Id: tximod.scm,v 1.20 1996/04/23 22:33:54 cph Exp $
;;;
;;;	Copyright (c) 1987-96 Massachusetts Institute of Technology
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
  (lambda (buffer)
    (local-set-variable! syntax-table texinfo-mode:syntax-table buffer)
    (local-set-variable! fill-column 72 buffer)
    (local-set-variable! require-final-newline #t buffer)
    (local-set-variable! indent-tabs-mode #f buffer)
    (local-set-variable!
     page-delimiter
     "^@\\(chapter\\|unnumbered \\|appendix \\|majorheading\\|chapheading\\)"
     buffer)
    (local-set-variable! paragraph-start
			 (string-append "^\010\\|^@[a-zA-Z]*[ \n]\\|"
					(ref-variable paragraph-start buffer))
			 buffer)
    (local-set-variable! paragraph-separate
			 (string-append "^\010\\|^@[a-zA-Z]*[ \n]\\|"
					(ref-variable paragraph-separate
						      buffer))
			 buffer)
    (event-distributor/invoke! (ref-variable texinfo-mode-hook buffer)
			       buffer)))

(define texinfo-mode:syntax-table (make-syntax-table))
(modify-syntax-entry! texinfo-mode:syntax-table #\" " ")
(modify-syntax-entry! texinfo-mode:syntax-table #\\ " ")
(modify-syntax-entry! texinfo-mode:syntax-table #\@ "\\")
(modify-syntax-entry! texinfo-mode:syntax-table #\DC1 "\\")
(modify-syntax-entry! texinfo-mode:syntax-table #\' "w")

(define-key 'texinfo '(#\C-c #\{) 'texinfo-insert-braces)
(define-key 'texinfo '(#\C-c #\}) 'up-list)

(define-key 'texinfo '(#\C-c #\C-c #\c) 'texinfo-insert-@code)
(define-key 'texinfo '(#\C-c #\C-c #\d) 'texinfo-insert-@dfn)
(define-key 'texinfo '(#\C-c #\C-c #\e) 'texinfo-insert-@end)
(define-key 'texinfo '(#\C-c #\C-c #\f) 'texinfo-insert-@file)
(define-key 'texinfo '(#\C-c #\C-c #\i) 'texinfo-insert-@item)
(define-key 'texinfo '(#\C-c #\C-c #\k) 'texinfo-insert-@kbd)
(define-key 'texinfo '(#\C-c #\C-c #\n) 'texinfo-insert-@node)
(define-key 'texinfo '(#\C-c #\C-c #\o) 'texinfo-insert-@noindent)
(define-key 'texinfo '(#\C-c #\C-c #\s) 'texinfo-insert-@samp)
(define-key 'texinfo '(#\C-c #\C-c #\t) 'texinfo-insert-@table)
(define-key 'texinfo '(#\C-c #\C-c #\v) 'texinfo-insert-@var)
(define-key 'texinfo '(#\C-c #\C-c #\x) 'texinfo-insert-@example)

(define ((texinfo-insert-@-with-arg keyword) argument)
  (call-with-values
      (lambda ()
	(if argument
	    (let ((n (command-argument-value argument)))
	      (if (< n 0)
		  (let ((end
			 (skip-chars-backward " \t\n\r\f" (current-point))))
		    (values (forward-sexp end n 'ERROR) end))
		  (let ((start
			 (skip-chars-forward " \t\n\r\f" (current-point))))
		    (values start (forward-sexp start n 'ERROR)))))
	    (let ((start (current-point)))
	      (values start start))))
    (lambda (start end)
      (let ((end (mark-left-inserting-copy end)))
	(insert-string (string-append "@" keyword "{") start)
	(insert-string "}" end)
	(set-current-point! (if argument end (mark-1+ end)))
	(mark-temporary! end)))))

(define-command texinfo-insert-braces
  "Make a pair of braces and be poised to type inside of them.
Use \\[up-list] to move forward out of the braces."
  ()
  (lambda ()
    (insert-string "{}")
    (set-current-point! (mark-1+ (current-point)))))

(define-command texinfo-insert-@code
  "Insert a `@code{...}' command in a texinfo buffer.
A numeric argument says how many words the braces should surround.
The default is not to surround any existing words with the braces."
  "P"
  (texinfo-insert-@-with-arg "code"))

(define-command texinfo-insert-@dfn
  "Insert a `@dfn{...}' command in a texinfo buffer.
A numeric argument says how many words the braces should surround.
The default is not to surround any existing words with the braces."
  "P"
  (texinfo-insert-@-with-arg "dfn"))

(define-command texinfo-insert-@example
  "Insert the string `@example' in a texinfo buffer."
  ()
  (lambda () (insert-string "@example\n")))

(define-command texinfo-insert-@file
  "Insert a `@file{...}' command in a texinfo buffer.
A numeric argument says how many words the braces should surround.
The default is not to surround any existing words with the braces."
  "P"
  (texinfo-insert-@-with-arg "file"))

(define-command texinfo-insert-@item
  "Insert the string `@item' in a texinfo buffer."
  ()
  (lambda () (insert-string "@item\n")))

(define-command texinfo-insert-@kbd
  "Insert a `@kbd{...}' command in a texinfo buffer.
A numeric argument says how many words the braces should surround.
The default is not to surround any existing words with the braces."
  "P"
  (texinfo-insert-@-with-arg "kbd"))

(define-command texinfo-insert-@node
  "Insert the string `@node' in a texinfo buffer.
This also inserts on the following line a comment indicating
the order of arguments to @node."
  ()
  (lambda ()
    (insert-string "@node ")
    (let ((m (mark-right-inserting-copy (current-point))))
      (insert-string "\n@comment  node-name,  next,  previous,  up")
      (set-current-point! m)
      (mark-temporary! m))))

(define-command texinfo-insert-@noindent
  "Insert the string `@noindent' in a texinfo buffer."
  ()
  (lambda () (insert-string "@noindent\n")))

(define-command texinfo-insert-@samp
  "Insert a `@samp{...}' command in a texinfo buffer.
A numeric argument says how many words the braces should surround.
The default is not to surround any existing words with the braces."
  "P"
  (texinfo-insert-@-with-arg "samp"))

(define-command texinfo-insert-@table
  "Insert the string `@table' in a texinfo buffer."
  ()
  (lambda () (insert-string "@table ")))

(define-command texinfo-insert-@var
  "Insert a `@var{...}' command in a texinfo buffer.
A numeric argument says how many words the braces should surround.
The default is not to surround any existing words with the braces."
  "P"
  (texinfo-insert-@-with-arg "var"))

(define-command texinfo-insert-@end
  "Insert the matching `@end' for the last Texinfo command that needs one."
  ()
  (lambda ()
    (let ((start (buffer-start (selected-buffer))))
      (let loop ((mark (current-point)) (depth 1))
	(let ((m
	       (re-search-backward texinfo-environment-regexp mark start #f)))
	  (if m
	      (cond ((match-forward "@end" m)
		     (loop m (+ depth 1)))
		    ((> depth 1)
		     (loop m (- depth 1)))
		    (else
		     (re-match-forward texinfo-environment-regexp m)
		     (insert-string "@end ")
		     (insert-region (re-match-start 1)
				    (re-match-end 1))
		     (insert-newline)))
	      (insert-string "@end ")))))))

(define texinfo-environment-regexp
  (string-append
   "^@\\(f?table\\|enumerate\\|itemize"
   "\\|ifhtml\\|ifinfo\\|iftex\\|ifset\\|ifclear\\|format"
   "\\|example\\|quotation\\|lisp\\|smallexample\\|smalllisp\\|display"
   "\\|flushleft\\|flushright\\|ignore\\|group\\|tex\\|html\\|cartouche"
   "\\|menu\\|titlepage\\|end\\|def[a-z]*[a-wyz]\\>\\)"))
;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/modefs.scm,v 1.132 1991/08/08 19:00:04 arthur Exp $
;;;
;;;	Copyright (c) 1985, 1989-91 Massachusetts Institute of Technology
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

;;;; Fundamental Mode

(declare (usual-integrations))

(define-command fundamental-mode
  "Make the current mode be Fundamental Mode.
All normal editing modes are defined relative to this mode."
  ()
  (lambda ()
    (set-current-major-mode! (ref-mode-object fundamental))))

(define-major-mode fundamental #f "Fundamental"
  "Major mode not specialized for anything in particular.
Most other major modes are defined by comparison to this one.")

(define-variable editor-default-mode
  "The default major mode for new buffers."
  (ref-mode-object fundamental))

(define initial-buffer-name
  (ref-variable transcript-buffer-name))

(define initial-buffer-mode
  (->mode (ref-variable transcript-buffer-mode)))

(define-variable file-type-to-major-mode
  "Specifies the major mode for new buffers based on file type.
This is an alist, the cars of which are pathname types,
and the cdrs of which are major modes."
  (os/file-type-to-major-mode))

(define-default-key 'fundamental 'undefined)
(define-key 'fundamental char-set:graphic 'self-insert-command)
(define-key 'fundamental char-set:numeric 'auto-digit-argument)
(define-key 'fundamental #\- 'auto-negative-argument)
(define-key 'fundamental #\rubout 'delete-backward-char)

(define-major-mode read-only fundamental "Read-only"
  "Major mode for read-only buffers.
Like Fundamental mode, but no self-inserting characters.
Digits and - are bound to prefix argument commands.")

(define-key 'read-only char-set:graphic 'undefined)
(define-key 'read-only char-set:numeric 'digit-argument)
(define-key 'read-only #\- 'negative-argument)

(define-major-mode read-only-noarg fundamental "Read-only-noarg"
  "Major mode for read-only buffers.
Like Fundamental mode, but no self-inserting characters.")

(define-key 'read-only-noarg char-set:graphic 'undefined)

(define-key 'fundamental #\c-% 'replace-string)
(define-key 'fundamental #\c-- 'negative-argument)
(define-key 'fundamental #\c-0 'digit-argument)
(define-key 'fundamental #\c-1 'digit-argument)
(define-key 'fundamental #\c-2 'digit-argument)
(define-key 'fundamental #\c-3 'digit-argument)
(define-key 'fundamental #\c-4 'digit-argument)
(define-key 'fundamental #\c-5 'digit-argument)
(define-key 'fundamental #\c-6 'digit-argument)
(define-key 'fundamental #\c-7 'digit-argument)
(define-key 'fundamental #\c-8 'digit-argument)
(define-key 'fundamental #\c-9 'digit-argument)
(define-key 'fundamental #\c-\; 'indent-for-comment)
(define-key 'fundamental #\c-< 'mark-beginning-of-buffer)
(define-key 'fundamental #\c-= 'what-cursor-position)
(define-key 'fundamental #\c-> 'mark-end-of-buffer)
(define-key 'fundamental #\c-@ 'set-mark-command)
(define-key 'fundamental #\c-a 'beginning-of-line)
(define-key 'fundamental #\c-b 'backward-char)
(define-key 'fundamental left 'backward-char)
(define-prefix-key 'fundamental #\c-c 'prefix-key)
(define-key 'fundamental #\c-d 'delete-char)
(define-key 'fundamental deletechar 'delete-char)
(define-key 'fundamental #\c-e 'end-of-line)
(define-key 'fundamental #\c-f 'forward-char)
(define-key 'fundamental right 'forward-char)
(define-key 'fundamental #\c-g 'keyboard-quit)
(define-prefix-key 'fundamental #\c-h 'help-prefix)
(define-key 'fundamental #\c-i 'indent-for-tab-command)
(define-key 'fundamental #\c-j 'newline-and-indent)
(define-key 'fundamental #\c-k 'kill-line)
(define-key 'fundamental deleteline 'kill-line)
(define-key 'fundamental #\c-l 'recenter)
(define-key 'fundamental #\c-m 'newline)
(define-key 'fundamental #\c-n 'next-line)
(define-key 'fundamental down 'next-line)
(define-key 'fundamental #\c-o 'open-line)
(define-key 'fundamental insertline 'open-line)
(define-key 'fundamental #\c-p 'previous-line)
(define-key 'fundamental up 'previous-line)
(define-key 'fundamental #\c-q 'quoted-insert)
(define-key 'fundamental #\c-r 'isearch-backward)
(define-key 'fundamental #\c-s 'isearch-forward)
(define-key 'fundamental #\c-t 'transpose-chars)
(define-key 'fundamental #\c-u 'universal-argument)
(define-key 'fundamental #\c-v 'scroll-up)
(define-key 'fundamental next 'scroll-up)
(define-key 'fundamental #\c-w 'kill-region)
(define-prefix-key 'fundamental #\c-x 'prefix-key)
(define-key 'fundamental #\c-y 'yank)
(define-key 'fundamental #\c-z 'control-meta-prefix)
(define-key 'fundamental #\c-\[ 'meta-prefix)
(define-key 'fundamental #\c-\] 'abort-recursive-edit)
(define-key 'fundamental #\c-^ 'control-prefix)
(define-key 'fundamental #\c-_ 'undo)
(define-key 'fundamental #\c-rubout 'backward-delete-char-untabify)
;;; Jokes
(define-key 'fundamental #\h-space 'hyper-space)
(define-key 'fundamental (make-special-key 'malesymbol 4) 'super-man)
(define-key 'fundamental (make-special-key 'menu 4) 'super-menu)
(define-key 'fundamental #\t-$ 'top-dollar)
(define-key 'fundamental #\t-^ 'top-hat)

(define-key 'fundamental #\m-space 'just-one-space)
(define-key 'fundamental #\m-% 'query-replace)
(define-key 'fundamental #\m-, 'tags-loop-continue)
(define-key 'fundamental #\m-- 'auto-argument)
(define-key 'fundamental #\m-. 'find-tag)
(define-key 'fundamental #\m-0 'auto-argument)
(define-key 'fundamental #\m-1 'auto-argument)
(define-key 'fundamental #\m-2 'auto-argument)
(define-key 'fundamental #\m-3 'auto-argument)
(define-key 'fundamental #\m-4 'auto-argument)
(define-key 'fundamental #\m-5 'auto-argument)
(define-key 'fundamental #\m-6 'auto-argument)
(define-key 'fundamental #\m-7 'auto-argument)
(define-key 'fundamental #\m-8 'auto-argument)
(define-key 'fundamental #\m-9 'auto-argument)
(define-key 'fundamental #\m-\; 'indent-for-comment)
(define-key 'fundamental #\m-< 'beginning-of-buffer)
(define-key 'fundamental #\m-= 'count-lines-region)
(define-key 'fundamental #\m-> 'end-of-buffer)
(define-key 'fundamental #\m-@ 'mark-word)
(define-key 'fundamental #\m-\[ 'backward-paragraph)
(define-key 'fundamental #\m-\\ 'delete-horizontal-space)
(define-key 'fundamental #\m-\] 'forward-paragraph)
(define-key 'fundamental #\m-^ 'delete-indentation)
(define-key 'fundamental #\m-a 'backward-sentence)
(define-key 'fundamental #\m-b 'backward-word)
(define-key 'fundamental #\m-c 'capitalize-word)
(define-key 'fundamental #\m-d 'kill-word)
(define-key 'fundamental #\m-e 'forward-sentence)
(define-key 'fundamental #\m-f 'forward-word)
(define-key 'fundamental #\m-g 'fill-region)
(define-key 'fundamental #\m-h 'mark-paragraph)
(define-key 'fundamental #\m-i 'tab-to-tab-stop)
(define-key 'fundamental #\m-j 'indent-new-comment-line)
(define-key 'fundamental #\m-k 'kill-sentence)
(define-key 'fundamental #\m-l 'downcase-word)
(define-key 'fundamental #\m-m 'back-to-indentation)
(define-key 'fundamental #\m-q 'fill-paragraph)
(define-key 'fundamental home 'home-cursor)
(define-key 'fundamental #\m-r 'move-to-window-line)
(define-key 'fundamental #\m-t 'transpose-words)
(define-key 'fundamental #\m-u 'upcase-word)
(define-key 'fundamental #\m-v 'scroll-down)
(define-key 'fundamental prior 'scroll-down)
(define-key 'fundamental #\m-w 'copy-region-as-kill)
(define-key 'fundamental #\m-x 'execute-extended-command)
(define-key 'fundamental #\m-y 'yank-pop)
(define-key 'fundamental #\m-z 'zap-to-char)
(define-key 'fundamental #\m-~ 'not-modified)
(define-key 'fundamental #\m-rubout 'backward-kill-word)

(define-key 'fundamental #\c-m-space 'mark-sexp)
(define-key 'fundamental #\c-m-0 'digit-argument)
(define-key 'fundamental #\c-m-1 'digit-argument)
(define-key 'fundamental #\c-m-2 'digit-argument)
(define-key 'fundamental #\c-m-3 'digit-argument)
(define-key 'fundamental #\c-m-4 'digit-argument)
(define-key 'fundamental #\c-m-5 'digit-argument)
(define-key 'fundamental #\c-m-6 'digit-argument)
(define-key 'fundamental #\c-m-7 'digit-argument)
(define-key 'fundamental #\c-m-8 'digit-argument)
(define-key 'fundamental #\c-m-9 'digit-argument)
(define-key 'fundamental #\c-m-- 'negative-argument)
(define-key 'fundamental #\c-m-\\ 'indent-region)
(define-key 'fundamental #\c-m-^ 'delete-indentation)
(define-key 'fundamental #\c-m-\( 'backward-up-list)
(define-key 'fundamental #\c-m-\) 'up-list)
(define-key 'fundamental #\c-m-@ 'mark-sexp)
(define-key 'fundamental #\c-m-\; 'kill-comment)
(define-key 'fundamental #\c-m-\[ 'eval-expression)
(define-key 'fundamental #\c-m-a 'beginning-of-defun)
(define-key 'fundamental #\c-m-b 'backward-sexp)
(define-key 'fundamental #\c-m-c 'exit-recursive-edit)
(define-key 'fundamental #\c-m-d 'down-list)
(define-key 'fundamental #\c-m-e 'end-of-defun)
(define-key 'fundamental #\c-m-f 'forward-sexp)
(define-key 'fundamental #\c-m-h 'mark-defun)
(define-key 'fundamental #\c-m-j 'indent-new-comment-line)
(define-key 'fundamental #\c-m-k 'kill-sexp)
(define-key 'fundamental #\c-m-l 'twiddle-buffers)
(define-key 'fundamental #\c-m-n 'forward-list)
(define-key 'fundamental #\c-m-o 'split-line)
(define-key 'fundamental #\c-m-p 'backward-list)
(define-key 'fundamental #\c-m-r 'align-defun)
(define-key 'fundamental #\c-m-s 'isearch-forward-regexp)
(define-key 'fundamental #\c-m-t 'transpose-sexps)
(define-key 'fundamental #\c-m-u 'backward-up-list)
(define-key 'fundamental #\c-m-v 'scroll-other-window)
(define-key 'fundamental #\c-m-w 'append-next-kill)
(define-key 'fundamental #\c-m-rubout 'backward-kill-sexp)

(define-key 'fundamental '(#\c-c #\c-s) 'select-transcript-buffer)

(define-key 'fundamental '(#\c-h #\a) 'command-apropos)
(define-key 'fundamental '(#\c-h #\b) 'describe-bindings)
(define-key 'fundamental '(#\c-h #\c) 'describe-key-briefly)
(define-key 'fundamental '(#\c-h #\f) 'describe-function)
(define-key 'fundamental '(#\c-h #\i) 'info)
(define-key 'fundamental '(#\c-h #\k) 'describe-key)
(define-key 'fundamental '(#\c-h #\l) 'view-lossage)
(define-key 'fundamental '(#\c-h #\m) 'describe-mode)
(define-key 'fundamental '(#\c-h #\t) 'help-with-tutorial)
(define-key 'fundamental '(#\c-h #\v) 'describe-variable)
(define-key 'fundamental '(#\c-h #\w) 'where-is)

(define-key 'fundamental '(#\c-x #\c-\[) 'repeat-complex-command)
(define-key 'fundamental '(#\c-x #\c-b) 'list-buffers)
(define-key 'fundamental '(#\c-x #\c-c) 'save-buffers-kill-scheme)
(define-key 'fundamental '(#\c-x #\c-d) 'list-directory)
(define-key 'fundamental '(#\c-x #\c-e) 'eval-last-sexp)
(define-key 'fundamental '(#\c-x #\c-f) 'find-file)
(define-key 'fundamental '(#\c-x #\c-i) 'indent-rigidly)
(define-key 'fundamental '(#\c-x #\c-l) 'downcase-region)
(define-key 'fundamental '(#\c-x #\c-n) 'set-goal-column)
(define-key 'fundamental '(#\c-x #\c-o) 'delete-blank-lines)
(define-key 'fundamental '(#\c-x #\c-p) 'mark-page)
(define-key 'fundamental '(#\c-x #\c-q) 'toggle-read-only)
(define-key 'fundamental '(#\c-x #\c-s) 'save-buffer)
(define-key 'fundamental '(#\c-x #\c-t) 'transpose-lines)
(define-key 'fundamental '(#\c-x #\c-u) 'upcase-region)
(define-key 'fundamental '(#\c-x #\c-v) 'find-alternate-file)
(define-key 'fundamental '(#\c-x #\c-w) 'write-file)
(define-key 'fundamental '(#\c-x #\c-x) 'exchange-point-and-mark)
(define-key 'fundamental '(#\c-x #\c-z) 'suspend-scheme)
(define-key 'fundamental '(#\c-x #\() 'start-kbd-macro)
(define-key 'fundamental '(#\c-x #\)) 'end-kbd-macro)
(define-key 'fundamental '(#\c-x #\.) 'set-fill-prefix)
(define-key 'fundamental '(#\c-x #\/) 'point-to-register)
(define-key 'fundamental '(#\c-x #\0) 'delete-window)
(define-key 'fundamental '(#\c-x #\1) 'delete-other-windows)
(define-key 'fundamental '(#\c-x #\2) 'split-window-vertically)
(define-prefix-key 'fundamental '(#\c-x #\4) 'prefix-key)
(define-key 'fundamental '(#\c-x #\4 #\c-f) 'find-file-other-window)
(define-key 'fundamental '(#\c-x #\4 #\.) 'find-tag-other-window)
(define-key 'fundamental '(#\c-x #\4 #\b) 'switch-to-buffer-other-window)
(define-key 'fundamental '(#\c-x #\4 #\d) 'dired-other-window)
(define-key 'fundamental '(#\c-x #\4 #\f) 'find-file-other-window)
(define-key 'fundamental '(#\c-x #\4 #\m) 'mail-other-window)
(define-key 'fundamental '(#\c-x #\5) 'split-window-horizontally)
(define-key 'fundamental '(#\c-x #\;) 'set-comment-column)
(define-key 'fundamental '(#\c-x #\=) 'what-cursor-position)
(define-key 'fundamental '(#\c-x #\[) 'backward-page)
(define-key 'fundamental '(#\c-x #\]) 'forward-page)
(define-key 'fundamental '(#\c-x #\^) 'enlarge-window)
(define-key 'fundamental '(#\c-x #\b) 'switch-to-buffer)
(define-key 'fundamental '(#\c-x #\c) 'save-buffers-kill-edwin)
(define-key 'fundamental '(#\c-x #\d) 'dired)
(define-key 'fundamental '(#\c-x #\e) 'call-last-kbd-macro)
(define-key 'fundamental '(#\c-x #\f) 'set-fill-column)
(define-key 'fundamental '(#\c-x #\g) 'insert-register)
(define-key 'fundamental '(#\c-x #\h) 'mark-whole-buffer)
(define-key 'fundamental '(#\c-x #\i) 'insert-file)
(define-key 'fundamental '(#\c-x #\j) 'register-to-point)
(define-key 'fundamental '(#\c-x #\k) 'kill-buffer)
(define-key 'fundamental '(#\c-x #\l) 'count-lines-page)
(define-key 'fundamental '(#\c-x #\m) 'mail)
(define-key 'fundamental '(#\c-x #\n) 'narrow-to-region)
(define-key 'fundamental '(#\c-x #\o) 'other-window)
(define-key 'fundamental '(#\c-x #\p) 'narrow-to-page)
(define-key 'fundamental '(#\c-x #\q) 'kbd-macro-query)
(define-key 'fundamental '(#\c-x #\r) 'copy-rectangle-to-register)
(define-key 'fundamental '(#\c-x #\s) 'save-some-buffers)
;(define-key 'fundamental '(#\c-x #\t) 'transpose-regions)
(define-key 'fundamental '(#\c-x #\u) 'undo)
(define-key 'fundamental '(#\c-x #\w) 'widen)
(define-key 'fundamental '(#\c-x #\x) 'copy-to-register)
(define-key 'fundamental '(#\c-x #\z) 'suspend-edwin)
(define-key 'fundamental '(#\c-x #\{) 'shrink-window-horizontally)
(define-key 'fundamental '(#\c-x #\}) 'enlarge-window-horizontally)
(define-key 'fundamental '(#\c-x #\rubout) 'backward-kill-sentence)
;;; -*-Scheme-*-
;;;
;;;	$Id: xmodef.scm,v 1.1 1992/10/20 20:03:21 jinx Exp $
;;;
;;;	Copyright (c) 1985, 1989-92 Massachusetts Institute of Technology
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

;;;; Fundamental Mode, additional X bindings

(declare (usual-integrations))

(let-syntax ((define-function-key
               (macro (mode key command)
                 (let ((token (if (pair? key) (car key) key)))
                   `(if (not (lexical-unreferenceable? (the-environment)
                                                       ',token))
                        (define-key ,mode ,key ,command))))))

  (define-function-key 'fundamental left 'backward-char)
  (define-function-key 'fundamental deletechar 'delete-char)
  (define-function-key 'fundamental right 'forward-char)
  (define-function-key 'fundamental deleteline 'kill-line)
  (define-function-key 'fundamental down 'next-line)
  (define-function-key 'fundamental insertline 'open-line)
  (define-function-key 'fundamental up 'previous-line)
  (define-function-key 'fundamental next 'scroll-up)
  (define-function-key 'fundamental home 'home-cursor)
  (define-function-key 'fundamental prior 'scroll-down)
  (define-function-key 'fundamental (make-special-key 'next 1)
    'scroll-other-window)
  (define-function-key 'fundamental (make-special-key 'prior 1) 
    'scroll-other-window-down)

;;; Jokes

  (define-key 'fundamental #\h-space 'hyper-space)
  (define-function-key 'fundamental (make-special-key 'malesymbol 4) 
    'super-man)
  (define-function-key 'fundamental (make-special-key 'menu 4) 'super-menu)
  (define-key 'fundamental #\t-$ 'top-dollar)
  (define-key 'fundamental #\t-^ 'top-hat)

) ;; End of let-syntax
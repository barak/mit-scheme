;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/rename.scm,v 1.4 1990/11/02 03:24:41 cph Rel $
;;;
;;;	Copyright (c) 1989, 1990 Massachusetts Institute of Technology
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

;;;; Edwin Interpackage Renames

(declare (usual-integrations))

(let ((global (->environment '()))
      (edwin (->environment '(edwin)))
      (window (->environment '(edwin window))))
  (let ((g<-e
	 (lambda (g e)
	   (local-assignment global g (lexical-reference edwin e)))))
    (g<-e 'save-editor-files 'debug-save-files))
  (let ((e<-w
	 (lambda (e w)
	   (lexical-assignment edwin e (lexical-reference window w)))))
    (e<-w 'window? 'buffer-frame?)
    (e<-w 'window-x-size 'buffer-frame-x-size)
    (e<-w 'window-y-size 'buffer-frame-y-size)
    (e<-w 'window-needs-redisplay? 'buffer-frame-needs-redisplay?)
    (e<-w '%set-window-buffer! 'set-window-buffer!)))
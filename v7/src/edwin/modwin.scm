;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/modwin.scm,v 1.31 1989/08/11 11:30:36 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989 Massachusetts Institute of Technology
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

;;;; Modeline Window

(declare (usual-integrations))

(define-class modeline-window vanilla-window
  (old-buffer-modified?))

(define-method modeline-window (:initialize! window window*)
  (usual=> window :initialize! window*)
  (set! y-size 1)
  (set! old-buffer-modified? 'UNKNOWN)
  unspecific)

(define-method modeline-window (:update-display! window screen x-start y-start
						 xl xu yl yu display-style)
  display-style				;ignore
  (if (< yl yu)
      (let ((thunk
	     (lambda ()
	       (screen-write-substring!
		screen x-start y-start
		(string-pad-right (modeline-string superior) x-size #\-)
		xl xu))))
	(if (variable-local-value
	     (window-buffer superior)
	     (ref-variable-object mode-line-inverse-video))
	    (with-inverse-video! screen thunk)
	    (thunk))))  true)

(define (with-inverse-video! screen thunk)
  (let ((old-inverse? (screen-inverse-video! screen false))
	(new-inverse? true))
    (screen-inverse-video! screen old-inverse?)
    (dynamic-wind (lambda ()
		    (set! old-inverse?
			  (screen-inverse-video! screen new-inverse?)))
		  thunk
		  (lambda ()
		    (set! new-inverse?
			  (screen-inverse-video! screen old-inverse?))))))

(define-method modeline-window (:event! window type)
  (case type
    ((BUFFER-MODIFIED)
     (let ((new (buffer-modified? (window-buffer superior))))
       (if (not (eq? old-buffer-modified? new))
	   (begin
	     (setup-redisplay-flags! redisplay-flags)
	     (set! old-buffer-modified? new)))))
     ((NEW-BUFFER)
      (set! old-buffer-modified? 'UNKNOWN))
     ((CURSOR-MOVED)
      unspecific)
     (else 
      (setup-redisplay-flags! redisplay-flags)))
  unspecific)
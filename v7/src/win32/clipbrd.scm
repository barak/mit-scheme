;;; -*-Scheme-*-
;;;
;;;	$Id: clipbrd.scm,v 1.3 1998/07/09 04:29:29 cph Exp $
;;;
;;;	Copyright (c) 1995-98 Massachusetts Institute of Technology
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

;;;; Miscellaneous Win32 Facilities

(declare (usual-integrations))

(define (win32-clipboard-write-text s)
  (let* ((len (+ (string-length s) 1))
	 (mem
	  (global-alloc #x2002		;(GMEM_MOVEABLE | GMEM_DDESHARE)
			len)))
    (if (= mem 0)
	(error "Unable to allocate global memory of length" len))
    (copy-memory (global-lock mem) s len)
    (global-unlock mem)
    (if (not (open-clipboard 0))
	(error "Error opening clipboard."))
    (if (not (empty-clipboard))
	(error "Error emptying clipboard."))
    (if (not (set-clipboard-data CF_TEXT mem))
	(error "Error setting clipboard data."))
    (if (not (close-clipboard))
	(error "Error closing clipboard."))))

(define (win32-clipboard-read-text)
  (open-clipboard 0)
  (let ((mem (get-clipboard-data CF_TEXT)))
    (and (not (= mem 0))
	 (let* ((maxlen (global-size mem))
		(s (string-allocate maxlen))
		(ptr (global-lock mem)))
	   (copy-memory s ptr maxlen)
	   (global-unlock mem)
	   (close-clipboard)
	   (set-string-length! s (vector-8b-find-next-char s 0 maxlen 0))
	   s))))

(define (win32-screen-width)
  (get-system-metrics SM_CXSCREEN))

(define (win32-screen-height)
  (get-system-metrics SM_CYSCREEN))
;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/notify.scm,v 1.2 1992/02/17 22:10:28 bal Exp $
;;;
;;;	Copyright (c) 1992 Massachusetts Institute of Technology
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
;;; NOTE: Parts of this program (Edwin) were created by translation ;;; from corresponding parts of GNU Emacs.  Users should be aware that
;;; the GNU GENERAL PUBLIC LICENSE may apply to these parts.  A copy
;;; of that license should have been included along with this file. ;;;

;;;; Mode-line notifications (e.g. presence of mail, load average)

(declare (usual-integrations))

(define-variable notify-string
  "Either \" Mail\" or \"\" depending on whether mail is waiting."
  ""
  string?)

(define-variable mail-notify-directory
  "Directory in which MAIL-NOTIFY checks for mail."
  (pathname-as-directory "/usr/mail/")
  file-directory?)

(define-variable notify-interval
  "Interval at which MAIL-NOTIFY checks for mail, in milliseconds."
  60000
  exact-nonnegative-integer?)

(define (check-for-mail)
  (let ((mail-file
	 (merge-pathnames (ref-variable mail-notify-directory)
			  (unix/current-user-name))))
    (and (file-exists? mail-file)
	 (> (file-attributes/length (file-attributes mail-file)) 0))))

(define (get-load-average-string)
  (let ((temporary-buffer (temporary-buffer "*uptime*")))
    (let ((start (buffer-start temporary-buffer)))
      (shell-command false start false false "uptime")
      (re-search-forward
       "[ ]*\\([0-9]*:[0-9]*[ap]m\\).*load average: \\([0-9.]*\\),"
       start 
       (buffer-end temporary-buffer))
      (let ((result
	     (string-append 
	      (extract-string (re-match-start 1) (re-match-end 1))
	      " "
	      (extract-string (re-match-start 2) (re-match-end 2)))))
	(kill-buffer temporary-buffer)
	(or result "n/a")))))

(define mail-present-string " Mail")
(define mail-not-present-string "")

(define mail-notify-hook-installed? false)

(define (install-mail-notify-hook! load-notify?)
  (if (not mail-notify-hook-installed?)
      (begin
       (add-event-receiver!
	(ref-variable rmail-new-mail-hook)
	(lambda ()
	  (set-variable! 
	   notify-string 
	   (if load-notify?
	       (string-append (get-load-average-string) mail-not-present-string)
	       mail-not-present-string))
	  (global-window-modeline-event!)
	  (update-screens! false)))
       (set! mail-notify-hook-installed? true))))

(define (make-notifier thunk)
  (lambda ()
    (let notify-cycle ()
      (set-variable! notify-string (thunk))
      (global-window-modeline-event!)
      (update-screens! false)
      (sleep-current-thread (ref-variable notify-interval))
      (notify-cycle))))

(define current-notifier-thread false)

(define (start-notifier notifier)
  (if current-notifier-thread
      (signal-thread-event
       current-notifier-thread
       (lambda () (exit-current-thread false))))
  (call-with-current-continuation
   (lambda (continuation)
     (let ((thread (create-thread continuation notifier)))
       (set! current-notifier-thread thread)
       thread))))

(define (mail-notify)
  (install-mail-notify-hook! false)
  (start-notifier 
   (make-notifier
    (lambda ()
      (if (check-for-mail)
	  mail-present-string
	  mail-not-present-string)))))

(define (mail-and-load-notify)
  (install-mail-notify-hook! true)
  (start-notifier
   (make-notifier
    (lambda ()
      (string-append
       (get-load-average-string)
       (if (check-for-mail)
	   mail-present-string
	   mail-not-present-string))))))
;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/notify.scm,v 1.9 1992/08/28 18:44:39 jinx Exp $
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
;;; NOTE: Parts of this program (Edwin) were created by translation
;;; from corresponding parts of GNU Emacs.  Users should be aware that
;;; the GNU GENERAL PUBLIC LICENSE may apply to these parts.  A copy
;;; of that license should have been included along with this file.
;;;

;;;; Mode-line notifications (e.g. presence of mail, load average)

(declare (usual-integrations))

(define-variable notify-show-time
  "If true, the notifier displays the current time."
  true
  boolean?)

(define (notifier:time)
  (let ((time (get-decoded-time)))
    (let ((hour (decoded-time/hour time))
	  (minute (decoded-time/minute time)))
      (string-append (write-to-string
		      (cond ((zero? hour) 12)
			    ((< hour 13) hour)
			    (else (- hour 12))))
		     (if (< minute 10) ":0" ":")
		     (write-to-string minute)
		     (if (< hour 12) "am" "pm")))))

(define-variable notify-show-date
  "If true, the notifier displays the current date."
  false
  boolean?)

(define (notifier:date)
  (let ((time (get-decoded-time)))
    (string-append (vector-ref
		    '#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
		    (decoded-time/day-of-week time))
		   (vector-ref
		    '#("??" " Jan " " Feb " " Mar " " Apr " " May " " Jun "
			    " Jul " " Aug " " Sep " " Oct " " Nov " " Dec ")
		    (decoded-time/month time))
		   (write-to-string (decoded-time/day time)))))

(define-variable notify-show-load
  "If true, the notifier displays the load average."
  false
  boolean?)

(define (notifier:load-average)
  (let ((temporary-buffer (temporary-buffer "*uptime*")))
    (let ((start (buffer-start temporary-buffer)))
      (shell-command false start false false "uptime")
      (let ((result
	     (if (re-search-forward
		  ".*load average:[ ]*\\([0-9.]*\\),"
		  start 
		  (buffer-end temporary-buffer))
		 (extract-string (re-match-start 1)
				 (re-match-end 1))
		 "")))
	(kill-buffer temporary-buffer)
	result))))

(define-variable notify-show-mail
  "If true, the notifier displays your mail status."
  true
  boolean?)

(define-variable notify-mail-present
  "A string to be displayed in the modeline when mail is present.
Ignored if notify-show-mail is false."
  "Mail"
  string?)

(define-variable notify-mail-not-present
  "A string to be displayed in the modeline when mail is not present.
Ignored if notify-show-mail is false."
  ""
  string?)

(define-variable mail-notify-directory
  "Directory in which MAIL-NOTIFY checks for mail."
  (pathname-as-directory "/usr/mail/")
  file-directory?)

(define (notifier:mail-present)
  (if (let ((attributes
	     (file-attributes
	      (merge-pathnames (ref-variable mail-notify-directory)
			       (unix/current-user-name)))))
	(and attributes
	     (> (file-attributes/length attributes) 0)))
      (ref-variable notify-mail-present)
      (ref-variable notify-mail-not-present)))

(define-variable notify-interval
  "How often the notifier updates the modeline, in seconds."
  60
  exact-nonnegative-integer?)

(define notifier-elements
  (list (cons (ref-variable-object notify-show-date) notifier:date)
	(cons (ref-variable-object notify-show-time) notifier:time)
	(cons (ref-variable-object notify-show-load) notifier:load-average)))

(define-command run-notifier
  "Run the notifier.
The notifier maintains a simple display in the modeline,
which can show various things including time, load average, and mail status."
  ()
  (lambda ()
    (if (and (not mail-notify-hook-installed?)
	     (command-defined? rmail))
	(begin
	  (add-event-receiver!
	   (ref-variable rmail-new-mail-hook)
	   (lambda ()
	     (update-notify-string!
	      (if (ref-variable notify-show-mail)
		  (ref-variable notify-mail-not-present)
		  ""))))
	  (set! mail-notify-hook-installed? true)
	  unspecific))
    ((ref-command kill-notifier))
    (let ((thread
	   (create-thread
	    editor-thread-root-continuation
	    (lambda ()
	      (do () (false)
		(inferior-thread-output! notifier-thread-registration)
		(sleep-current-thread
		 (* 1000 (ref-variable notify-interval))))))))
      (detach-thread thread)
      (set! current-notifier-thread thread)
      (set! notifier-thread-registration
	    (register-inferior-thread! thread notifier)))
    unspecific))

(define (notifier)
  (set-variable! global-mode-string
		 (reduce string-append-separated
			 ""
			 (map (lambda (element)
				(if (and (car element)
					 (variable-value (car element)))
				    ((cdr element))
				    ""))
			      notifier-elements)))
  (if mail-notify-hook-installed?
      (update-notify-string!
       (if (ref-variable notify-show-mail)
	   (notifier:mail-present)
	   "")))
  true)

(define-command kill-notifier
  "Kill the current notifier, if any."
  ()
  (lambda ()
    (if (and current-notifier-thread
	     (not (thread-dead? current-notifier-thread)))
	(signal-thread-event current-notifier-thread
			     (lambda () (exit-current-thread unspecific))))
    (set-variable! global-mode-string "")
    (update-notify-string! "")))

(define (update-notify-string! string)
  (set-variable! notify-string
		 (if (or (string-null? (ref-variable global-mode-string))
			 (string-null? string))
		     string
		     (string-append " " string)))
  (global-window-modeline-event!))

(define-variable notify-string
  "This is an internal variable.  Don't change it."
  ""
  string?)

(define mail-notify-hook-installed? false)
(define current-notifier-thread false)
(define notifier-thread-registration)
;;; -*-Scheme-*-
;;;
;;;	$Id: dos.scm,v 1.46 1997/12/30 21:19:30 cph Exp $
;;;
;;;	Copyright (c) 1992-97 Massachusetts Institute of Technology
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

;;;; Win32 Customizations for Edwin

(declare (usual-integrations))

(define (dos/windows-type)
  (cond ((string-prefix? "Microsoft Windows NT"
			 microcode-id/operating-system-variant)
	 'WINNT)
	((string-prefix? "Microsoft Windows 95"
			 microcode-id/operating-system-variant)
	 'WIN95)
	((string-prefix? "Microsoft Win32s"
			 microcode-id/operating-system-variant)
	 'WIN31)
	(else #f)))

(define (dos/default-shell-file-name)
  (if (eq? 'WINNT (dos/windows-type))
      "cmd.exe"
      "command.com"))

(define (os/set-file-modes-writable! pathname)
  (set-file-modes! pathname
		   (fix:andc (file-modes pathname) nt-file-mode/read-only)))

(define (os/restore-modes-to-updated-file! pathname modes)
  (set-file-modes! pathname (fix:or modes nt-file-mode/archive)))

(define (os/scheme-can-quit?)
  #t)

(define (os/quit dir)
  (with-real-working-directory-pathname dir %quit))

(define (with-real-working-directory-pathname dir thunk)
  (let ((inside (->namestring (directory-pathname-as-file dir)))
	(outside false))
    (dynamic-wind
     (lambda ()
       (stop-thread-timer)
       (set! outside
	     (->namestring
	      (directory-pathname-as-file (working-directory-pathname))))
       (set-working-directory-pathname! inside)
       ((ucode-primitive set-working-directory-pathname! 1) inside))
     thunk
     (lambda ()
       (set! inside
	     (->namestring
	      (directory-pathname-as-file (working-directory-pathname))))
       ((ucode-primitive set-working-directory-pathname! 1) outside)
       (set-working-directory-pathname! outside)
       (start-thread-timer)))))

(define (dos/read-dired-files file all-files?)
  (map (lambda (entry) (cons (file-namestring (car entry)) (cdr entry)))
       (let ((entries (directory-read file #f #t)))
	 (if all-files?
	     entries
	     (list-transform-positive entries
	       (let ((mask
		      (fix:or nt-file-mode/hidden nt-file-mode/system)))
		 (lambda (entry)
		   (fix:= (fix:and (file-attributes/modes (cdr entry)) mask)
			  0))))))))

;;;; Win32 Clipboard Interface

(define cut-and-paste-active?
  #t)

(define (os/interprogram-cut string push?)
  push?
  (if cut-and-paste-active?
      (win32-clipboard-write-text
       (let ((string (convert-newline-to-crlf string)))
	 ;; Some programs can't handle strings over 64k.
	 (if (fix:< (string-length string) #x10000) string "")))))

(define (os/interprogram-paste)
  (if cut-and-paste-active?
      (let ((text (win32-clipboard-read-text)))
	(and text
	     (convert-crlf-to-newline text)))))

(define (convert-newline-to-crlf string)
  (let ((end (string-length string)))
    (let ((n-newlines
	   (let loop ((start 0) (n-newlines 0))
	     (let ((newline
		    (substring-find-next-char string start end #\newline)))
	       (if newline
		   (loop (fix:+ newline 1) (fix:+ n-newlines 1))
		   n-newlines)))))
      (if (fix:= n-newlines 0)
	  string
	  (let ((copy (make-string (fix:+ end n-newlines))))
	    (let loop ((start 0) (cindex 0))
	      (let ((newline
		     (substring-find-next-char string start end #\newline)))
		(if newline
		    (begin
		      (%substring-move! string start newline copy cindex)
		      (let ((cindex (fix:+ cindex (fix:- newline start))))
			(string-set! copy cindex #\return)
			(string-set! copy (fix:+ cindex 1) #\newline)
			(loop (fix:+ newline 1) (fix:+ cindex 2))))
		    (%substring-move! string start end copy cindex))))
	    copy)))))

(define (convert-crlf-to-newline string)
  (let ((end (string-length string)))
    (let ((n-crlfs
	   (let loop ((start 0) (n-crlfs 0))
	     (let ((cr
		    (substring-find-next-char string start end #\return)))
	       (if (and cr
			(not (fix:= (fix:+ cr 1) end))
			(char=? (string-ref string (fix:+ cr 1)) #\linefeed))
		   (loop (fix:+ cr 2) (fix:+ n-crlfs 1))
		   n-crlfs)))))
      (if (fix:= n-crlfs 0)
	  string
	  (let ((copy (make-string (fix:- end n-crlfs))))
	    (let loop ((start 0) (cindex 0))
	      (let ((cr
		     (substring-find-next-char string start end #\return)))
		(if (not cr)
		    (%substring-move! string start end copy cindex)
		    (let ((cr
			   (if (and (not (fix:= (fix:+ cr 1) end))
				    (char=? (string-ref string (fix:+ cr 1))
					    #\linefeed))
			       cr
			       (fix:+ cr 1))))
		      (%substring-move! string start cr copy cindex)
		      (loop (fix:+ cr 1) (fix:+ cindex (fix:- cr start)))))))
	    copy)))))

;;;; Mail Customization

(define (os/rmail-spool-directory) #f)
(define (os/rmail-primary-inbox-list system-mailboxes) system-mailboxes '())
(define (os/sendmail-program) "sendmail.exe")
(define (os/rmail-pop-procedure) #f)
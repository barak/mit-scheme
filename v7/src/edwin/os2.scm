;;; -*-Scheme-*-
;;;
;;;	$Id: os2.scm,v 1.42 1997/12/30 21:19:19 cph Exp $
;;;
;;;	Copyright (c) 1994-97 Massachusetts Institute of Technology
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

;;;; OS/2 Customizations for Edwin

(declare (usual-integrations))

(define (dos/default-shell-file-name)
  "cmd.exe")

(define (os/set-file-modes-writable! pathname)
  (set-file-modes! pathname
		   (fix:andc (file-modes pathname) os2-file-mode/read-only)))

(define (os/restore-modes-to-updated-file! pathname modes)
  (set-file-modes! pathname (fix:or modes os2-file-mode/archived)))

(define (os/scheme-can-quit?)
  #f)

(define (os/quit dir)
  dir
  (error "Can't quit."))

(define (dos/read-dired-files file all-files?)
  (let loop
      ((pathnames
	(let ((pathnames (directory-read file #f)))
	  (if all-files?
	      pathnames
	      (list-transform-positive pathnames
		(let ((mask
		       (fix:or os2-file-mode/hidden os2-file-mode/system)))
		  (lambda (pathname)
		    (fix:= (fix:and (file-modes pathname) mask) 0)))))))
       (result '()))
    (if (null? pathnames)
	result
	(loop (cdr pathnames)
	      (let ((attr (file-attributes (car pathnames))))
		(if attr
		    (cons (cons (file-namestring (car pathnames)) attr) result)
		    result))))))

;;;; OS/2 Clipboard Interface

(define (os/interprogram-cut string push?)
  push?
  (os2-clipboard-write-text
   (let ((string (convert-newline-to-crlf string)))
     ;; Some programs can't handle strings over 64k.
     (if (fix:< (string-length string) #x10000) string ""))))

(define (os/interprogram-paste)
  (let ((text (os2-clipboard-read-text)))
    (and text
	 (convert-crlf-to-newline text))))

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

(define (os/sendmail-program)
  "sendmail")

(define (os/rmail-spool-directory)
  (or (let ((etc (get-environment-variable "ETC")))
	(and etc
	     (file-directory? etc)
	     (let ((mail
		    (merge-pathnames "mail/" (pathname-as-directory etc))))
	       (and (file-directory? mail)
		    (->namestring mail)))))
      "c:\\mptn\\etc\\mail\\"))

(define (os/rmail-primary-inbox-list system-mailboxes)
  system-mailboxes)

(define (os/rmail-pop-procedure)
  (and (dos/find-program "popclient" (ref-variable exec-path) #f)
       (lambda (server user-name password directory)
	 (os2-pop-client server user-name password directory))))

(define (os2-pop-client server user-name password directory)
  (let ((target
	 (->namestring
	  (merge-pathnames (if (dos/fs-long-filenames? directory)
			       ".popmail"
			       "popmail.tmp")
			   directory))))
    (let ((buffer (temporary-buffer "*popclient*")))
      (cleanup-pop-up-buffers
       (lambda ()
	 (pop-up-buffer buffer)
	 (let ((status.reason
		(let ((args
		       (list "-u" user-name
			     "-p" (os2-pop-client-password password)
			     "-o" target
			     server)))
		  (apply run-synchronous-process
			 #f (cons (buffer-end buffer) #t) #f #f
			 "popclient"
			 "-3"
			 (if (ref-variable rmail-pop-delete)
			     args
			     (cons "-k" args))))))
	   (if (and (eq? 'EXITED (car status.reason))
		    (memv (cdr status.reason) '(0 1)))
	       (kill-pop-up-buffer buffer)
	       (begin
		 (keep-pop-up-buffer buffer)
		 (editor-error "Error getting mail from POP server.")))))))
    target))

(define (os2-pop-client-password password)
  (cond ((string? password)
	 password)
	((and (pair? password) (eq? 'FILE (car password)))
	 (call-with-input-file (cadr password)
	   (lambda (port)
	     (read-string (char-set #\newline) port))))
	(else
	 (error "Illegal password:" password))))

(define-variable rmail-pop-delete
  "If true, messages are deleted from the POP server after being retrieved.
Otherwise, messages remain on the server and will be re-fetched later."
  #t
  boolean?)
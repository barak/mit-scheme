;;; -*-Scheme-*-
;;;
;;;	$Id: os2.scm,v 1.26 1995/10/31 08:08:14 cph Exp $
;;;
;;;	Copyright (c) 1994-95 Massachusetts Institute of Technology
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

(define dos/encoding-pathname-types
  '("gz" #|"ky"|#))

(define dos/executable-pathname-types
  '("exe" "cmd"))

(define dos/default-shell-file-name
  "cmd.exe")

(define (os/form-shell-command command)
  (list "/c" command))

(define (os/directory-list directory)
  (let ((channel (directory-channel-open directory)))
    (let loop ((result '()))
      (let ((name (directory-channel-read channel)))
	(if name
	    (loop (cons (begin (string-downcase! name) name) result))
	    (begin
	      (directory-channel-close channel)
	      result))))))

(define (os/directory-list-completions directory prefix)
  (let ((channel (directory-channel-open directory)))
    (let loop ((result '()))
      (let ((name (directory-channel-read-matching channel prefix)))
	(if name
	    (loop (cons (begin (string-downcase! name) name) result))
	    (begin
	      (directory-channel-close channel)
	      result))))))

(define (os/set-file-modes-writable! pathname)
  (set-file-modes! pathname (fix:andc (file-modes pathname) #x0001)))

(define (os/scheme-can-quit?)
  #f)

(define (os/quit dir)
  dir
  (error "Can't quit."))

;;;; OS/2 Clipboard Interface

(define (os/interprogram-cut string push?)
  push?
  (os2-clipboard-write-text
   (let ((string (convert-newline-to-crlf string)))
     ;; Some programs can't handle strings over 64k.
     (if (fix:< (string-length string) #x10000) string ""))))

(define (os/interprogram-paste)
  (convert-crlf-to-newline (os2-clipboard-read-text)))

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

;;;; Dired customization

(define-variable dired-listing-switches
  "Dired listing format."
  "-l"
  string?)

(define-variable list-directory-brief-switches
  "list-directory brief listing format."
  ""
  string?)

(define-variable list-directory-verbose-switches
  "list-directory verbose listing format."
  "-l"
  string?)

(define (insert-directory! file switches mark type)
  ;; Insert directory listing for FILE at MARK.
  ;; SWITCHES are examined for the presence of "a" and "t".
  ;; TYPE can have one of three values:
  ;;   'WILDCARD means treat FILE as shell wildcard.
  ;;   'DIRECTORY means FILE is a directory and a full listing is expected.
  ;;   'FILE means FILE itself should be listed, and not its contents.
  (let ((mark (mark-left-inserting-copy mark)))
    (call-with-current-continuation
     (lambda (k)
       (bind-condition-handler (list condition-type:file-error)
	   (lambda (condition)
	     (insert-string (condition/report-string condition) mark)
	     (insert-newline mark)
	     (k unspecific))
	 (lambda ()
	   (for-each
	    (let ((now (os2/file-time->nmonths (current-file-time))))
	      (lambda (entry)
		(insert-string
		 (os2/dired-line-string (car entry) (cdr entry) now)
		 mark)
		(insert-newline mark)))
	    (if (eq? 'FILE type)
		(let ((attributes (file-attributes file)))
		  (if attributes
		      (list (cons (file-namestring file) attributes))
		      '()))
		(sort (os2/read-dired-files file
					    (string-find-next-char switches
								   #\a))
		      (if (string-find-next-char switches #\t)
			  (lambda (x y)
			    (> (file-attributes/modification-time (cdr x))
			       (file-attributes/modification-time (cdr y))))
			  (lambda (x y)
			    (string-ci<? (car x) (car y)))))))))))
    (mark-temporary! mark)))

(define (os2/dired-line-string name attr now)
  (string-append
   (file-attributes/mode-string attr)
   " "
   (string-pad-left (number->string (file-attributes/length attr)) 10 #\space)
   " "
   (os/ls-file-time-string (file-attributes/modification-time attr) now)
   " "
   name))

(define (os2/read-dired-files file all-files?)
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

;;;; Time

(define (os/ls-file-time-string time #!optional now)
  (let ((now
	 (if (or (default-object? now) (not now))
	     (os2/file-time->nmonths (current-file-time))
	     now))
	(dt (decode-file-time time))
	(ns (lambda (n m c) (string-pad-left (number->string n) m c))))
    (string-append (month/short-string (decoded-time/month dt))
		   " "
		   (ns (decoded-time/day dt) 2 #\space)
		   " "
		   (if (<= -6 (- (os2/file-time->nmonths time) now) 0)
		       (string-append (ns (decoded-time/hour dt) 2 #\0)
				      ":"
				      (ns (decoded-time/minute dt) 2 #\0))
		       (string-append " "
				      (number->string
				       (decoded-time/year dt)))))))

(define (os2/file-time->nmonths time)
  (let ((time (quotient time #x200000)))
    (+ (* (quotient time 16) 12)
       (remainder time 16))))

;;;; Compressed Files

(define (os/read-file-methods)
  `((,read/write-compressed-file?
     . ,(lambda (pathname mark visit?)
	  visit?
	  (read-compressed-file "gzip -d" pathname mark)))))

(define (os/write-file-methods)
  `((,read/write-compressed-file?
     . ,(lambda (region pathname visit?)
	  visit?
	  (write-compressed-file "gzip" region pathname)))))

(define (os/alternate-pathnames group pathname)
  (if (and (ref-variable enable-compressed-files group)
	   (dos/fs-long-filenames? pathname)
	   (not (equal? "gz" (pathname-type pathname))))
      (list (string-append (->namestring pathname) ".gz"))
      '()))

(define-variable enable-compressed-files
  "If true, compressed files are automatically uncompressed when read,
and recompressed when written.  A compressed file is identified by the
filename suffix \".gz\"."
  #t
  boolean?)

(define (read/write-compressed-file? group pathname)
  (and (ref-variable enable-compressed-files group)
       (equal? "gz" (pathname-type pathname))))

(define (read-compressed-file program pathname mark)
  (temporary-message "Uncompressing file " (->namestring pathname) "...")
  (let ((value
	 (call-with-temporary-file-pathname
	  (lambda (temporary)
	    (if (not (equal? '(EXITED . 0)
			     (shell-command #f #f
					    (directory-pathname pathname)
					    #f
					    (string-append
					     program
					     " < "
					     (file-namestring pathname)
					     " > "
					     (->namestring temporary)))))
		(error:file-operation pathname
				      program
				      "file"
				      "[unknown]"
				      read-compressed-file
				      (list pathname mark)))
	    (group-insert-file! (mark-group mark)
				(mark-index mark)
				temporary)))))
    (append-message "done")
    value))

(define (write-compressed-file program region pathname)
  (temporary-message "Compressing file " (->namestring pathname) "...")
  (if (not (equal? '(EXITED . 0)
		   (shell-command region
				  #f
				  (directory-pathname pathname)
				  #f
				  (string-append program
						 " > "
						 (file-namestring pathname)))))
      (error:file-operation pathname
			    program
			    "file"
			    "[unknown]"
			    write-compressed-file
			    (list region pathname)))
  (append-message "done"))

;;;; Mail Customization

(define (os/sendmail-program)
  "sendmail")

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
      (let ((status.reason
	     (let ((args
		    (list "-u" user-name
			  "-p" (os2-pop-client-password password)
			  "-o" target
			  server)))
	       (apply run-synchronous-process #f (buffer-end buffer) #f #f
		      "popclient"
		      "-3"
		      (if (ref-variable rmail-pop-delete)
			  args
			  (cons "-k" args))))))
	(if (and (eq? 'EXITED (car status.reason))
		 (memv (cdr status.reason) '(0 1)))
	    (kill-buffer buffer)
	    (begin
	      (pop-up-buffer buffer)
	      (editor-error "Error getting mail from POP server.")))))
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

(define (os/hostname)
  (if (not os2/cached-hostname)
      (let ((buffer (temporary-buffer "*hostname*")))
	(let ((status.reason
	       (run-synchronous-process #f (buffer-end buffer) #f #f
					"hostname")))
	  (if (not (equal? status.reason '(EXITED . 0)))
	      (begin
		(pop-up-buffer buffer)
		(error "Error running HOSTNAME program:" status.reason))))
	(set! os2/cached-hostname (string-trim (buffer-string buffer)))
	(kill-buffer buffer)))
  os2/cached-hostname)

(define os2/cached-hostname #f)
(add-event-receiver! event:after-restore
  (lambda ()
    (set! os2/cached-hostname #f)
    unspecific))
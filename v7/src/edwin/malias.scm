;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/malias.scm,v 1.1 1991/04/24 07:27:43 cph Exp $
;;;
;;;	Copyright (c) 1991 Massachusetts Institute of Technology
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

;;;; Mail Aliases

(declare (usual-integrations))

(define-command define-mail-alias
  "Define NAME as a mail-alias that translates to DEFINITION."
  (lambda ()
    (let ((alias (prompt-for-string "Define mail alias" false 'NULL-DEFAULT)))
      (list alias
	    (prompt-for-string
	     (string-append "Define " alias " as mail alias for")
	     false
	     'NULL-DEFAULT))))
  (lambda (alias definition)
    (if (string-null? alias)
	(editor-error "Mail alias must be non-null string"))
    (let ((definition (parse-mailrc-line definition 0)))
      (if (null? definition)
	  (editor-error "Mail alias definition must be non-null string"))
      (guarantee-mail-aliases)
      (define-mail-alias alias definition))))

(define (define-mail-alias alias definition)
  (let ((entry (find-mail-alias alias mail-aliases)))
    (if entry
	(set-cdr! entry definition)
	(begin
	  (set! mail-aliases (cons (cons alias definition) mail-aliases))
	  unspecific))))

(define (expand-mail-alias alias)
  (let loop ((alias alias) (disabled '()))
    (let ((entry
	   (and (not (find-mail-alias alias disabled))
		(find-mail-alias alias mail-aliases))))
      (cond (entry
	     (let ((disabled (cons entry disabled)))
	       (append-map! (lambda (definition)
			      (loop definition disabled))
			    (cdr entry))))
	    ((null? disabled)
	     false)
	    (else
	     (list alias))))))

(define (find-mail-alias alias mail-aliases)
  (let loop ((mail-aliases mail-aliases))
    (and (not (null? mail-aliases))
	 (if (string-ci=? alias (caar mail-aliases))
	     (car mail-aliases)
	     (loop (cdr mail-aliases))))))

(define (expand-mail-aliases start end)
  (guarantee-mail-aliases)
  (let loop ((start start))
    (let ((hs
	   (re-search-forward "^\\(to\\|cc\\|bcc\\):[ \t]*" start end true)))
      (if hs
	  (let ((he
		 (mark-left-inserting-copy
		  (skip-chars-backward
		   " \t\n"
		   (if (re-search-forward "^[^ \t]" hs end false)
		       (re-match-start 0)
		       end)
		   hs))))
	    (let loop ((hs hs))
	      (cond ((re-search-forward "[ \t]*[\n,][ \t]*" hs he false)
		     (let ((e (mark-left-inserting-copy (re-match-end 0))))
		       (expand-region hs (re-match-start 0))
		       (mark-temporary! e)
		       (loop e)))
		    ((mark< hs he)
		     (expand-region hs he))))
	    (mark-temporary! he)
	    (loop he))))))

(define (expand-region start end)
  (let ((strings (expand-mail-alias (extract-string start end))))
    (if strings
	(let ((point (mark-left-inserting-copy start)))
	  (delete-string point end)
	  (let loop ((strings strings))
	    (insert-string (car strings) point)
	    (if (not (null? (cdr strings)))
		(begin
		  (insert-string ", " point)
		  (loop (cdr strings)))))
	  (mark-temporary! point)))))

(define mail-aliases true)

(define (guarantee-mail-aliases)
  (if (eq? mail-aliases true)
      (begin
	(set! mail-aliases '())
	(if (file-exists? "~/.mailrc")
	    (for-each (lambda (entry)
			(define-mail-alias (car entry) (cdr entry)))
		      (parse-mailrc-file "~/.mailrc"))))))

(define (parse-mailrc-file filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ()
	(let ((line (read-mailrc-line port)))
	  (if line
	      (let ((index
		     (re-match-string-forward
		      (re-compile-pattern "^\\(a\\|alias\\|g\\|group\\)[ \t]+"
					  false)
		      false
		      false
		      line)))
		(if index
		    (let ((parsed-line (parse-mailrc-line line index)))
		      (if (null? (cdr parsed-line))
			  (loop)
			  (cons parsed-line (loop))))
		    (loop)))
	      '()))))))

(define (read-mailrc-line port)
  (let ((line (read-string char-set:newline port)))
    (and (not (eof-object? line))
	 (begin
	   (read-char port)
	   (let ((length (string-length line)))
	     (if (and (> length 0)
		      (char=? #\\ (string-ref line (- length 1))))
		 (let ((line* (read-mailrc-line port)))
		   (if line*
		       (string-append (substring line 0 (- length 1))
				      " "
				      line*)
		       (substring line 0 (- length 1))))
		 line))))))

(define (parse-mailrc-line line start)
  (let ((end (string-length line)))
    (let loop ((start start))
      (let ((nonblank
	     (substring-find-next-char-in-set line start end
					      char-set:nonblank)))
	(if nonblank
	    (let ((blank
		   (substring-find-next-char-in-set line nonblank end
						    char-set:blank)))
	      (if blank
		  (cons (substring line nonblank blank) (loop blank))
		  (list (substring line nonblank end))))
	    '())))))

(define char-set:newline (char-set #\newline))
(define char-set:blank (char-set #\space #\tab))
(define char-set:nonblank (char-set-invert char-set:blank))
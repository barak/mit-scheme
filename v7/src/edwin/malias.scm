#| -*-Scheme-*-

$Id: malias.scm,v 1.7 2003/02/14 18:25:20 cph Exp $

Copyright 1991-1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; Mail Aliases

(declare (usual-integrations))

(define-command define-mail-alias
  "Define NAME as a mail-alias that translates to DEFINITION."
  (lambda ()
    (let ((alias (prompt-for-string "Define mail alias" false)))
      (list alias
	    (prompt-for-string
	     (string-append "Define " alias " as mail alias for")
	     false))))
  (lambda (alias definition)
    (let ((definition (parse-mailrc-line definition 0)))
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
	      (let ((r
		     (re-string-match "^\\(a\\|alias\\|g\\|group\\)[ \t]+"
				      line)))
		(if r
		    (let ((parsed-line
			   (parse-mailrc-line line (re-match-end-index 0 r))))
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
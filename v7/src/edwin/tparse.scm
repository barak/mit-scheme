;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/tparse.scm,v 1.66 1991/04/23 06:47:27 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-91 Massachusetts Institute of Technology
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

;;;; Text Parsing

(declare (usual-integrations))

;;;; Pages

(define (%forward-page start end page-delimiter)
  (if (not (mark<= start end))
      (error "Marks incorrectly related:" start end))
  (and (mark< start end)
       (or (re-search-forward page-delimiter start end)
	   end)))

(define (%backward-page end start page-delimiter)
  (if (not (mark<= start end))
      (error "Marks incorrectly related:" start end))
  (and (mark< start end)
       (if (re-search-backward page-delimiter (mark-1+ end) start)
	   (re-match-end 0)
	   start)))

(define (%at-page-delimiter? mark page-delimiter)
  (re-match-forward page-delimiter (line-start mark 0) mark))

(define-variable page-delimiter
  "Regexp describing line-beginnings that separate pages."
  "^\f"
  string?)

(define (forward-one-page mark)
  (%forward-page mark
		 (group-end mark)
		 (mark-local-ref mark (ref-variable-object page-delimiter))))

(define (backward-one-page mark)
  (%backward-page mark
		  (group-start mark)
		  (mark-local-ref mark (ref-variable-object page-delimiter))))

(define (page-start mark)
  (let ((page-delimiter
	 (mark-local-ref mark (ref-variable-object page-delimiter))))
    (or (%at-page-delimiter? mark page-delimiter)
	(%backward-page mark (group-start mark) page-delimiter))))

(define forward-page)
(define backward-page)
(make-motion-pair forward-one-page backward-one-page
  (lambda (f b)
    (set! forward-page f)
    (set! backward-page b)
    unspecific))

;;;; Paragraphs

(define (%forward-paragraph mark end
			    fill-prefix paragraph-start paragraph-separate)
  (if (not (mark<= mark end))
      (error "Marks incorrectly related:" mark end))
  (and (mark< mark end)
       (let ((paragraph-separate
	      (if fill-prefix
		  (string-append paragraph-separate "\\|^"
				 (re-quote-string fill-prefix) "[ \t]*$")
		  paragraph-separate)))

	 (define (skip-separators m)
	   (cond ((mark= m end)
		  false)
		 ((re-match-forward paragraph-separate m end false)
		  (let ((m (line-end m 0)))
		    (and (mark< m end)
			 (skip-separators (mark1+ m)))))
		 (else
		  (let ((m (line-end m 0)))
		    (cond ((mark>= m end) end)
			  (fill-prefix (skip-body-prefix m))
			  (else (skip-body-no-prefix m)))))))

	 (define (skip-body-prefix m)
	   (if (mark< m end)
	       (let ((m (mark1+ m)))
		 (if (or (re-match-forward paragraph-separate m end false)
			 (not (match-forward fill-prefix m end false)))
		     m
		     (skip-body-prefix (line-end m 0))))
	       end))

	 (define (skip-body-no-prefix m)
	   (if (re-search-forward paragraph-start m end false)
	       (re-match-start 0)
	       end))

	 (skip-separators (line-start mark 0)))))

(define (%backward-paragraph mark start
			     fill-prefix paragraph-start paragraph-separate)
  (if (not (mark<= start mark))
      (error "Marks incorrectly related:" start mark))
  (and (mark< start mark)
       (let ((end (group-end mark))
	     (paragraph-separate
	      (if fill-prefix
		  (string-append paragraph-separate "\\|"
				 (re-quote-string fill-prefix) "[ \t]*$")
		  paragraph-separate)))

	 (define (skip-separators m)
	   (cond ((mark> start m)
		  false)
		 ((re-match-forward paragraph-separate m end false)
		  (and (mark< start m)
		       (skip-separators (line-start (mark-1+ m) 0))))
		 ((mark= start m)
		  start)
		 (fill-prefix
		  (skip-body-prefix m))
		 (else
		  (skip-body-no-prefix m))))

	 (define (skip-body-prefix m)
	   (if (or (re-match-forward paragraph-separate m end false)
		   (not (match-forward fill-prefix m end false)))
	       (adjust-final-position m)
	       (let ((m (line-start (mark-1+ m) 0)))
		 (if (mark< start m)
		     (skip-body-prefix m)
		     start))))

	 (define (skip-body-no-prefix m)
	   (let ((m
		  (re-search-backward paragraph-start (line-end m 0) start 
				      false)))
	     (if (not m)
		 start
		 (adjust-final-position m))))

	 (define (adjust-final-position m)
	   (let ((m
		  (if (re-match-forward paragraph-separate m end false)
		      (mark1+ (line-end m 0))
		      m)))
	     (or (and (mark< start m)
		      (let ((m (mark-1+ m)))
			(and (line-start? m)
			     m)))
		 m)))

	 (skip-separators (line-start (mark-1+ mark) 0)))))

(define-variable paragraph-start
  "Regexp for beginning of a line that starts OR separates paragraphs."
  "^[ \t\n\f]"
  string?)

(define-variable paragraph-separate
  "Regexp for beginning of a line that separates paragraphs.
If you change this, you may have to change paragraph-start also."
  "^[ \t\f]*$"
  string?)

(define-variable paragraph-ignore-fill-prefix
  "True means the paragraph commands are not affected by fill-prefix.
This is desirable in modes where blank lines are the paragraph delimiters."
  false
  boolean?)

(define (forward-one-paragraph mark #!optional end fill-prefix)
  (%forward-paragraph
   mark
   (if (default-object? end)
       (group-end mark)
       (begin
	 (if (not (mark<= mark end))
	     (error "Marks incorrectly related:" mark end))
	 end))
   (if (default-object? fill-prefix)
       (and (not (mark-local-ref
		  mark
		  (ref-variable-object paragraph-ignore-fill-prefix)))
	    (mark-local-ref mark (ref-variable-object fill-prefix)))
       fill-prefix)
   (mark-local-ref mark (ref-variable-object paragraph-start))
   (mark-local-ref mark (ref-variable-object paragraph-separate))))

(define (backward-one-paragraph mark #!optional start fill-prefix)
  (%backward-paragraph
   mark
   (if (default-object? start)
       (group-start mark)
       (begin
	 (if (not (mark<= start mark))
	     (error "Marks incorrectly related:" start mark))
	 start))
   (if (default-object? fill-prefix)
       (and (not (mark-local-ref
		  mark
		  (ref-variable-object paragraph-ignore-fill-prefix)))
	    (mark-local-ref mark (ref-variable-object fill-prefix)))
       fill-prefix)
   (mark-local-ref mark (ref-variable-object paragraph-start))
   (mark-local-ref mark (ref-variable-object paragraph-separate))))

(define forward-paragraph)
(define backward-paragraph)
(make-motion-pair forward-one-paragraph backward-one-paragraph
  (lambda (f b)
    (set! forward-paragraph f)
    (set! backward-paragraph b)
    unspecific))

(define (paragraph-text-region mark)
  (let ((end (or (paragraph-text-end mark) (group-end mark))))
    (make-region (or (paragraph-text-start end) (group-start mark)) end)))

(define (paragraph-text-start mark)
  (let ((start (backward-one-paragraph mark)))
    (and start
	 (let ((fill-prefix
		(mark-local-ref mark (ref-variable-object fill-prefix))))
	   (if fill-prefix
	       (if (match-forward fill-prefix start)
		   start
		   (line-start start 1))
	       (let ((start
		      (if (re-match-forward
			   (mark-local-ref
			    mark
			    (ref-variable-object paragraph-separate))
			   start)
			  (line-start start 1)
			  start)))
		 (or (skip-chars-forward " \t\n" start mark false)
		     (if (group-start? start)
			 start
			 (paragraph-text-start start)))))))))

(define (paragraph-text-end mark)
  (let ((end (forward-one-paragraph mark)))
    (and end
	 (let ((mark* (if (line-start? end) (mark-1+ end) end)))
	   (if (mark>= mark* mark)
	       mark*
	       (let ((mark* (mark1+ mark*)))
		 (if (group-end? mark*)
		     mark*
		     (paragraph-text-end mark*))))))))

;;;; Sentences

(define-variable sentence-end
  "Regexp describing the end of a sentence.
All paragraph boundaries also end sentences, regardless."
  "[.?!][]\"')}]*\\($\\|\t\\|  \\)[ \t\n]*"
  string?)

(define (forward-one-sentence mark)
  (let ((end (paragraph-text-end mark)))
    (and end
	 (let ((mark
		(re-search-forward
		 (mark-local-ref mark (ref-variable-object sentence-end))
		 mark
		 end)))
	   (if mark
	       (skip-chars-backward " \t\n" mark (re-match-start 0) false)
	       end)))))

(define (backward-one-sentence mark)
  (let ((start (paragraph-text-start mark)))
    (and start
	 (if (re-search-backward
	      (string-append
	       (mark-local-ref mark (ref-variable-object sentence-end))
	       "[^ \t\n]")
	      mark
	      start)
	     (mark-1+ (re-match-end 0))
	     start))))

(define forward-sentence)
(define backward-sentence)
(make-motion-pair forward-one-sentence backward-one-sentence
  (lambda (f b)
    (set! forward-sentence f)
    (set! backward-sentence b)))
;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/tparse.scm,v 1.67 1992/02/04 03:35:39 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-92 Massachusetts Institute of Technology
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

(define-integrable (mark/paragraph-start mark)
  (mark-local-ref mark (ref-variable-object paragraph-start)))

(define-integrable (mark/paragraph-separate mark)
  (mark-local-ref mark (ref-variable-object paragraph-separate)))

(define (mark/paragraph-fill-prefix mark)
  (if (mark-local-ref mark (ref-variable-object paragraph-ignore-fill-prefix))
      false
      (mark-local-ref mark (ref-variable-object fill-prefix))))

(define (forward-one-paragraph mark #!optional limit fill-prefix
			       finish-on-separator?)
  (%forward-paragraph mark
		      (if (default-object? limit)
			  (group-end mark)
			  (begin
			    (if (not (mark<= mark limit))
				(error "Marks incorrectly related:"
				       mark limit))
			    limit))
		      (if (default-object? fill-prefix)
			  (mark/paragraph-fill-prefix mark)
			  fill-prefix)
		      (mark/paragraph-start mark)
		      (mark/paragraph-separate mark)
		      (if (default-object? finish-on-separator?)
			  true
			  finish-on-separator?)))

(define (backward-one-paragraph mark #!optional limit fill-prefix
				finish-on-separator?)
  (%backward-paragraph mark
		       (if (default-object? limit)
			   (group-start mark)
			   (begin
			     (if (not (mark<= limit mark))
				 (error "Marks incorrectly related:"
					limit mark))
			     limit))
		       (if (default-object? fill-prefix)
			   (mark/paragraph-fill-prefix mark)
			   fill-prefix)
		       (mark/paragraph-start mark)
		       (mark/paragraph-separate mark)
		       (if (default-object? finish-on-separator?)
			   true
			   finish-on-separator?)))

(define forward-paragraph)
(define backward-paragraph)
(make-motion-pair forward-one-paragraph backward-one-paragraph
  (lambda (f b)
    (set! forward-paragraph f)
    (set! backward-paragraph b)
    unspecific))

(define (paragraph-text-region mark)
  (let ((end (paragraph-text-end mark)))
    (and end
	 (let ((start (paragraph-text-start end)))
	   (and start
		(make-region start end))))))

(define (paragraph-text-start mark)
  (%backward-paragraph mark
		       (group-start mark)
		       (mark/paragraph-fill-prefix mark)
		       (mark/paragraph-start mark)
		       (mark/paragraph-separate mark)
		       false))

(define (paragraph-text-end mark)
  (%forward-paragraph mark
		      (group-end mark)
		      (mark/paragraph-fill-prefix mark)
		      (mark/paragraph-start mark)
		      (mark/paragraph-separate mark)
		      false))

(define (%forward-paragraph mark limit fill-prefix para-start para-separate
			    finish-on-separator?)
  (if (not (mark<= mark limit))
      (error "Marks incorrectly related:" mark limit))
  (and (mark< mark limit)
       (let ((end (group-end mark))
	     (next-ls
	      (lambda (ls)
		(let ((le (line-end ls 0)))
		  (if (mark< le limit)
		      (mark1+ le)
		      limit)))))
	 (let ((separator?
		(if fill-prefix
		    (lambda (ls)
		      (let ((fp (match-forward fill-prefix ls end false)))
			(if fp
			    (re-match-forward "[ \t]*$" fp end false)
			    true)))
		    (lambda (ls)
		      (re-match-forward para-separate ls end false)))))
	   (letrec
	       ((skip-separators
		 (lambda (ls)
		   (cond ((mark= ls limit)
			  false)
			 ((separator? ls)
			  (skip-separators (next-ls ls)))
			 (else
			  (skip-body ls)))))
		(skip-body
		 (if fill-prefix
		     (lambda (ls)
		       (let ((ls (next-ls ls)))
			 (if (or (mark= ls limit)
				 (separator? ls))
			     (finish ls)
			     (skip-body ls))))
		     (lambda (ls)
		       (let ((le (line-end ls 0)))
			 (if (and (mark< le limit)
				  (re-search-forward para-start le limit
						     false))
			     (finish (re-match-start 0))
			     limit)))))
		(finish
		 (lambda (ls)
		   (if (or finish-on-separator? (not (line-start? ls)))
		       ls
		       (let ((le (mark-1+ ls)))
			 (if (mark< mark le)
			     le
			     (skip-separators ls)))))))
	     (if (or (line-end? mark) (separator? (line-start mark 0)))
		 (skip-separators (next-ls mark))
		 (skip-body mark)))))))

(define (%backward-paragraph mark limit fill-prefix para-start para-separate
			     finish-on-separator?)
  (if (not (mark<= limit mark))
      (error "Marks incorrectly related:" limit mark))
  (and (mark< limit mark)
       (let ((prev-ls
	      (lambda (ls)
		(let ((ls (line-start ls -1 'LIMIT)))
		  (if (mark< ls limit)
		      limit
		      ls))))
	     (end (group-end mark)))
	 (let ((separator?
		(if fill-prefix
		    (lambda (ls)
		      (let ((fp (match-forward fill-prefix ls end false)))
			(if fp
			    (re-match-forward "[ \t]*$" fp end false)
			    true)))
		    (lambda (ls)
		      (re-match-forward para-separate ls end false)))))
	   (letrec ((skip-separators
		     (lambda (ls)
		       (and (mark< limit ls)
			    (let ((ls (prev-ls ls)))
			      (cond ((separator? ls)
				     (skip-separators ls))
				    ((mark= ls limit)
				     ls)
				    (else
				     (skip-body ls)))))))
		    (skip-body
		     (if fill-prefix
			 (lambda (ls)
			   (let ((ls* (prev-ls ls)))
			     (if (separator? ls*)
				 (if finish-on-separator?
				     ls*
				     ls)
				 (skip-body ls*))))
			 (lambda (ls)
			   (let ((ps
				  (re-search-backward para-start
						      (line-end ls 0)
						      limit
						      false)))
			     (cond ((not ps)
				    limit)
				   (finish-on-separator?
				    (if (separator? ps)
					ps
					(let ((ls (prev-ls ps)))
					  (if (separator? ls)
					      ls
					      ps))))
				   (else
				    (if (separator? ps)
					(line-start ps 1)
					ps))))))))
	     (if (line-start? mark)
		 (skip-separators mark)
		 (let ((ls (line-start mark 0)))
		   (and (mark<= limit ls)
			(cond ((separator? ls)
			       (skip-separators ls))
			      ((mark= limit ls)
			       ls)
			      (else
			       (skip-body ls)))))))))))

;;;; Sentences

(define-variable sentence-end
  "Regexp describing the end of a sentence.
All paragraph boundaries also end sentences, regardless."
  "[.?!][]\"')}]*\\($\\|\t\\|  \\)[ \t\n]*"
  string?)

(define-integrable (mark/sentence-end mark)
  (mark-local-ref mark (ref-variable-object sentence-end)))

(define (forward-one-sentence mark)
  (let ((para-end
	 (let loop ((mark mark))
	   (let ((end (paragraph-text-end mark)))
	     (and end
		  (let ((end* (horizontal-space-start end)))
		    (if (mark< mark end*)
			end*
			(loop end))))))))
    (let ((mark
	   (re-search-forward (mark/sentence-end mark)
			      mark
			      (or para-end (group-end mark)))))
      (if mark
	  (skip-chars-backward " \t\n" mark (re-match-start 0) false)
	  para-end))))

(define (backward-one-sentence mark)
  (let ((para-start
	 (let loop ((mark mark))
	   (let ((start (paragraph-text-start mark)))
	     (and start
		  (let ((start* (horizontal-space-end start)))
		    (if (mark< start* mark)
			start*
			(loop start))))))))
    (if (re-search-backward (string-append (mark/sentence-end mark) "[^ \t\n]")
			    mark
			    (or para-start (group-start mark)))
	(mark-1+ (re-match-end 0))
	para-start)))

(define forward-sentence)
(define backward-sentence)
(make-motion-pair forward-one-sentence backward-one-sentence
  (lambda (f b)
    (set! forward-sentence f)
    (set! backward-sentence b)))
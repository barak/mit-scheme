;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/tparse.scm,v 1.65 1989/04/28 22:54:02 cph Rel $
;;;
;;;	Copyright (c) 1986, 1989 Massachusetts Institute of Technology
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

(define-variable page-delimiter
  "Regexp describing line-beginnings that separate pages."
  "^\f")

(define (forward-one-page mark)
  (and (not (group-end? mark))
       (or (re-search-forward (ref-variable page-delimiter) mark)
	   (group-end mark))))

(define (backward-one-page mark)
  (and (not (group-start? mark))
       (if (re-search-backward (ref-variable page-delimiter) (mark-1+ mark))
	   (re-match-end 0)
	   (group-start mark))))

(define (page-start mark)
  (let ((page-delimiter (ref-variable page-delimiter)))
    (or (re-match-forward page-delimiter (line-start mark 0))
	(if (re-search-backward page-delimiter (mark-1+ mark))
	    (re-match-end 0)
	    (group-start mark)))))

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
  "^[ \t\n]")

(define-variable paragraph-separate
  "Regexp for beginning of a line that separates paragraphs.
If you change this, you may have to change Paragraph Start also."
  "^[ \t]*$")


(define (forward-one-paragraph mark)
  (and (not (group-end? mark))
       (let ((end (group-end mark))
	     (fill-prefix (ref-variable fill-prefix))
	     (page-delimiter (ref-variable page-delimiter))
	     (forward-kernel
	      (lambda (mark separator? skip-body)
		(if (separator? (line-start mark 0))
		    (let ((para-start
			   (let skip-separators ((mark mark))
			     (let ((lstart (line-start mark 1)))
			       (and lstart
				    (if (separator? lstart)
					(skip-separators lstart)
					lstart))))))
		      (and para-start
			   (skip-body para-start)))
		    (skip-body mark)))))
	 (if (and fill-prefix
		  (not (string-null? fill-prefix)))
	     (let ((fill-prefix (re-quote-string fill-prefix)))
	       (let ((prefix
		      (string-append page-delimiter "\\|^" fill-prefix)))
		 (let ((start (string-append prefix "[ \t\n]"))
		       (separate (string-append prefix "[ \t]*$")))
		   (forward-kernel mark
		     (lambda (lstart)
		       (or (not (re-match-forward fill-prefix lstart))
			   (re-match-forward separate lstart)))
		     (letrec ((skip-body
			       (lambda (mark)
				 (let ((lstart (line-start mark 1)))
				   (cond ((not lstart) end)
					 ((or (not
					       (re-match-forward fill-prefix
								 lstart))
					      (re-match-forward start lstart))
					  lstart)
					 (else (skip-body lstart)))))))
		       skip-body)))))
	     (let ((prefix (string-append page-delimiter "\\|")))
	       (let ((start
		      (string-append prefix (ref-variable paragraph-start)))
		     (separate
		      (string-append prefix
				     (ref-variable paragraph-separate))))
		 (forward-kernel mark
		   (lambda (mark)
		     (re-match-forward separate mark))
		   (lambda (mark)
		     (if (re-search-forward start (line-end mark 0) end)
			 (re-match-start 0)
			 end)))))))))

(define (backward-one-paragraph mark)
  (and (not (group-start? mark))
       (let ((start (group-start mark))
	     (fill-prefix (ref-variable fill-prefix))
	     (page-delimiter (ref-variable page-delimiter))
	     (backward-kernel
	      (lambda (mark separator? skip-body)
		(if (separator? (line-start mark 0))
		    (let ((para-start
			   (let skip-separators ((mark mark))
			     (let ((lstart (line-start mark -1)))
			       (and lstart
				    (if (separator? lstart)
					(skip-separators lstart)
					lstart))))))
		      (and para-start
			   (skip-body para-start)))
		    (skip-body mark)))))
	 (if (and fill-prefix
		  (not (string-null? fill-prefix)))
	     (let ((fill-prefix (re-quote-string fill-prefix)))
	       (let ((prefix
		      (string-append page-delimiter "\\|^" fill-prefix)))
		 (let ((starter (string-append prefix "[ \t\n]"))
		       (separator (string-append prefix "[ \t]*$")))
		   (backward-kernel mark
		     (lambda (lstart)
		       (or (not (re-match-forward fill-prefix lstart))
			   (re-match-forward separator lstart)))
		     (letrec ((skip-body
			       (lambda (mark)
				 (let ((lstart (line-start mark -1)))
				   (cond ((not lstart) start)
					 ((or (not
					       (re-match-forward fill-prefix
								 lstart))
					      (re-match-forward starter
								lstart))
					  lstart)
					 (else (skip-body lstart)))))))
		       skip-body)))))
	     (let ((prefix (string-append page-delimiter "\\|")))
	       (let ((starter
		      (string-append prefix (ref-variable paragraph-start)))
		     (separator
		      (string-append prefix
				     (ref-variable paragraph-separate))))
		 (backward-kernel mark
		   (lambda (mark)
		     (re-match-forward separator mark))
		   (lambda (mark)
		     (if (re-search-backward starter mark start)
			 (re-match-start 0)
			 start)))))))))

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
	 (let ((fill-prefix (ref-variable fill-prefix)))
	   (if (and fill-prefix
		    (not (string-null? fill-prefix)))
	       (if (match-forward fill-prefix start)
		   start
		   (line-start start 1))
	       (let ((start
		      (if (re-match-forward (ref-variable paragraph-separate)
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
  "[.?!][]\")]*\\($\\|\t\\|  \\)[ \t\n]*")

(define (forward-one-sentence mark)
  (let ((end (paragraph-text-end mark)))
    (and end
	 (let ((mark (re-search-forward (ref-variable sentence-end)
					mark end)))
	   (if mark
	       (skip-chars-backward " \t\n" mark (re-match-start 0) false)
	       end)))))

(define (backward-one-sentence mark)
  (let ((start (paragraph-text-start mark)))
    (and start
	 (if (re-search-backward (string-append (ref-variable sentence-end)
						"[^ \t\n]")
				 mark start)
	     (mark-1+ (re-match-end 0))
	     start))))

(define forward-sentence)
(define backward-sentence)
(make-motion-pair forward-one-sentence backward-one-sentence
  (lambda (f b)
    (set! forward-sentence f)
    (set! backward-sentence b)))
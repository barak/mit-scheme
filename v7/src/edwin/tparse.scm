;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
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
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Text Parsing

(declare (usual-integrations))
(using-syntax edwin-syntax-table

;;;; Pages

(define-variable "Page Delimiter"
  "Regexp describing line-beginnings that separate pages."
  "^\f")

(define (forward-one-page mark)
  (and (not (group-end? mark))
       (or (re-search-forward (ref-variable "Page Delimiter") mark)
	   (group-end mark))))

(define (backward-one-page mark)
  (and (not (group-start? mark))
       (if (re-search-backward (ref-variable "Page Delimiter") (mark-1+ mark))
	   (re-match-end 0)
	   (group-start mark))))

(define (page-start mark)
  (or (re-match-forward (ref-variable "Page Delimiter") (line-start mark 0))
      (if (re-search-backward (ref-variable "Page Delimiter") (mark-1+ mark))
	  (re-match-end 0)
	  (group-start mark))))

(define forward-page)
(define backward-page)
(make-motion-pair forward-one-page backward-one-page
  (lambda (f b)
    (set! forward-page f)
    (set! backward-page b)))

;;;; Paragraphs

(define-variable "Paragraph Start"
  "Regexp for beginning of a line that starts OR separates paragraphs."
  "^[ \t\n]")

(define-variable "Paragraph Separate"
  "Regexp for beginning of a line that separates paragraphs.
If you change this, you may have to change Paragraph Start also."
  "^[ \t]*$")

(define forward-one-paragraph)
(let ()

(set! forward-one-paragraph
(named-lambda (forward-one-paragraph mark)
  (and (not (group-end? mark))
       ((if (and (ref-variable "Fill Prefix")
		 (not (string-null? (ref-variable "Fill Prefix"))))
	    forward-fill
	    forward-nofill)
	mark (group-end mark)))))

(define (forward-nofill mark end)
  (let ((prefix (string-append (ref-variable "Page Delimiter") "\\|")))
    (let ((start (string-append prefix (ref-variable "Paragraph Start")))
	  (separate
	   (string-append prefix (ref-variable "Paragraph Separate"))))
      (forward-kernel mark
		      (named-lambda (separator? mark)
			(re-match-forward separate mark))
		      (named-lambda (skip-body mark)
			(if (re-search-forward start (line-end mark 0) end)
			    (re-match-start 0)
			    end))))))

(define (forward-fill mark end)
  (let ((fill-prefix (re-quote-string (ref-variable "Fill Prefix"))))
    (let ((prefix (string-append (ref-variable "Page Delimiter") "\\|^"
				 fill-prefix)))
      (let ((start (string-append prefix "[ \t\n]"))
	    (separate (string-append prefix "[ \t]*$")))
	(define (skip-body mark)
	  (let ((lstart (line-start mark 1)))
	    (cond ((not lstart) end)
		  ((or (not (re-match-forward fill-prefix lstart))
		       (re-match-forward start lstart))
		   lstart)
		  (else (skip-body lstart)))))
	(forward-kernel mark
			(named-lambda (separator? lstart)
			  (or (not (re-match-forward fill-prefix lstart))
			      (re-match-forward separate lstart)))
			skip-body)))))

(define (forward-kernel mark separator? skip-body)
  (define (skip-separators mark)
    (let ((lstart (line-start mark 1)))
      (and lstart
	   (if (separator? lstart)
	       (skip-separators lstart)
	       lstart))))
  (if (separator? (line-start mark 0))
      (let ((para-start (skip-separators mark)))
	(and para-start (skip-body para-start)))
      (skip-body mark)))

)

(define backward-one-paragraph)
(let ()

(set! backward-one-paragraph
(named-lambda (backward-one-paragraph mark)
  (and (not (group-start? mark))
       ((if (and (ref-variable "Fill Prefix")
		 (not (string-null? (ref-variable "Fill Prefix"))))
	    backward-fill
	    backward-nofill)
	mark (group-start mark) (group-end mark)))))

(define (backward-nofill mark start end)
  (let ((prefix (string-append (ref-variable "Page Delimiter") "\\|")))
    (let ((starter (string-append prefix (ref-variable "Paragraph Start")))
	  (separator
	   (string-append prefix (ref-variable "Paragraph Separate"))))
      (backward-kernel mark
		       (named-lambda (separator? mark)
			 (re-match-forward separator mark))
		       (named-lambda (skip-body mark)
			 (if (re-search-backward starter mark start)
			     (re-match-start 0)
			     start))))))

(define (backward-fill mark start end)
  (let ((fill-prefix (re-quote-string (ref-variable "Fill Prefix"))))
    (let ((prefix (string-append (ref-variable "Page Delimiter") "\\|^"
				 fill-prefix)))
      (let ((starter (string-append prefix "[ \t\n]"))
	    (separator (string-append prefix "[ \t]*$")))
	(define (skip-body mark)
	  (let ((lstart (line-start mark -1)))
	    (cond ((not lstart) start)
		  ((or (not (re-match-forward fill-prefix lstart))
		       (re-match-forward starter lstart))
		   lstart)
		  (else (skip-body lstart)))))
	(backward-kernel mark
			 (named-lambda (separator? lstart)
			   (or (not (re-match-forward fill-prefix lstart))
			       (re-match-forward separator lstart)))
			 skip-body)))))

(define (backward-kernel mark separator? skip-body)
  (define (skip-separators mark)
    (let ((lstart (line-start mark -1)))
      (and lstart
	   (if (separator? lstart)
	       (skip-separators lstart)
	       lstart))))
  (if (separator? (line-start mark 0))
      (let ((para-start (skip-separators mark)))
	(and para-start (skip-body para-start)))
      (skip-body mark)))

)

(define forward-paragraph)
(define backward-paragraph)
(make-motion-pair forward-one-paragraph backward-one-paragraph
  (lambda (f b)
    (set! forward-paragraph f)
    (set! backward-paragraph b)))

(define (paragraph-text-region mark)
  (let ((end (or (paragraph-text-end mark) (group-end mark))))
    (make-region (or (paragraph-text-start end) (group-start mark))
		 end)))

(define (paragraph-text-start mark)
  (let ((start (backward-one-paragraph mark)))
    (and start
	 (if (and (ref-variable "Fill Prefix")
		  (not (string-null? (ref-variable "Fill Prefix"))))
	     (if (match-forward (ref-variable "Fill Prefix") start)
		 start
		 (line-start start 1))
	     (let ((start
		    (if (re-match-forward (ref-variable "Paragraph Separate")
					  start)
			(line-start start 1)
			start)))
	       (or (skip-chars-forward " \t\n" start mark #!FALSE)
		   (if (group-start? start)
		       start
		       (paragraph-text-start start))))))))

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

(define-variable "Sentence End"
  "Regexp describing the end of a sentence.
All paragraph boundaries also end sentences, regardless."
  "[.?!][]\")]*\\($\\|\t\\|  \\)[ \t\n]*")

(define (forward-one-sentence mark)
  (let ((end (paragraph-text-end mark)))
    (and end
	 (let ((mark (re-search-forward (ref-variable "Sentence End")
					mark end)))
	   (if mark
	       (skip-chars-backward " \t\n" mark (re-match-start 0) #!FALSE)
	       end)))))

(define (backward-one-sentence mark)
  (let ((start (paragraph-text-start mark)))
    (and start
	 (if (re-search-backward (string-append (ref-variable "Sentence End")
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

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: edwin-package
;;; Scheme Syntax Table: edwin-syntax-table
;;; End:

;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/texcom.scm,v 1.35 1992/02/04 03:37:17 cph Exp $
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

;;;; Text Commands

(declare (usual-integrations))

(define-major-mode text fundamental "Text"
  "Major mode for editing english text."
  (local-set-variable! syntax-table text-mode:syntax-table)
  (event-distributor/invoke! (ref-variable text-mode-hook)))

(define-key 'text #\m-s 'center-line)

(define text-mode:syntax-table (make-syntax-table))
(modify-syntax-entry! text-mode:syntax-table #\" "    ")
(modify-syntax-entry! text-mode:syntax-table #\\ "    ")
(modify-syntax-entry! text-mode:syntax-table #\[ "(]  ")
(modify-syntax-entry! text-mode:syntax-table #\] ")[  ")
(modify-syntax-entry! text-mode:syntax-table #\{ "(}  ")
(modify-syntax-entry! text-mode:syntax-table #\} "){  ")
(modify-syntax-entry! text-mode:syntax-table #\' "w   ")

(define-variable text-mode-hook
  "An event distributor that is invoked when entering Text mode."
  (make-event-distributor))

(define (turn-on-auto-fill)
  (enable-current-minor-mode! (ref-mode-object auto-fill)))

(define-command text-mode
  "Make the current mode be Text mode."
  ()
  (lambda ()
    (set-current-major-mode! (ref-mode-object text))))

(define-major-mode indented-text text "Indented-Text"
  "Like Text mode, but indents each line under previous non-blank line."
  (local-set-variable! indent-line-procedure (ref-command indent-relative)))

(define-command indented-text-mode
  "Make the current mode be Indented Text mode."
  ()
  (lambda ()
    (set-current-major-mode! (ref-mode-object indented-text))))

;;;; Words

(define-command forward-word
  "Move one or more words forward."
  "p"
  (lambda (argument)
    (move-thing forward-word argument 'FAILURE)))

(define-command backward-word
  "Move one or more words backward."
  "p"
  (lambda (argument)
    (move-thing backward-word argument 'FAILURE)))

(define-command mark-word
  "Set mark one or more words from point."
  "p"
  (lambda (argument)
    (mark-thing forward-word argument 'FAILURE)))

(define-command kill-word
  "Kill one or more words forward."
  "p"
  (lambda (argument)
    (kill-thing forward-word argument 'FAILURE)))

(define-command backward-kill-word
  "Kill one or more words backward."
  "p"
  (lambda (argument)
    (kill-thing backward-word argument 'FAILURE)))

(define-command transpose-words
  "Transpose the words before and after the cursor.
With a positive argument it transposes the words before and after the
 cursor, moves right, and repeats the specified number of times,
 dragging the word to the left of the cursor right.
With a negative argument, it transposes the two words to the left of
 the cursor, moves between them, and repeats the specified number of
 times, exactly undoing the positive argument form.
With a zero argument, it transposes the words at point and mark."
  "p"
  (lambda (argument)
    (transpose-things forward-word argument)))

;;;; Case Conversion

(define-command upcase-region
  "Convert region to upper case."
  "m"
  (lambda (mark)
    (upcase-area mark)))

(define-command downcase-region
  "Convert region to lower case."
  "m"
  (lambda (mark)
    (downcase-area mark)))

(define-command upcase-word
  "Uppercase one or more words.
Moves forward over the words affected.
With a negative argument, uppercases words before point
but does not move point."
  "p"
  (lambda (argument)
    (upcase-area (forward-word (current-point) argument 'ERROR))))

(define-command downcase-word
  "Lowercase one or more words.
Moves forward over the words affected.
With a negative argument, lowercases words before point
but does not move point."
  "p"
  (lambda (argument)
    (downcase-area (forward-word (current-point) argument 'ERROR))))

(define-command capitalize-word
  "Put next word in lowercase, but capitalize initial.
With an argument, capitalizes that many words."
  "p"
  (lambda (argument)
    (define (capitalize-one-word)
      (set-current-point! (forward-to-word (current-point) 'ERROR))
      (capitalize-area (forward-word (current-point) 1 'ERROR)))
    (cond ((positive? argument)
	   (dotimes argument
		    (lambda (i)
		      i			;ignore
		      (capitalize-one-word))))
	  ((negative? argument)
	   (let ((p (current-point)))
	     (set-current-point! (forward-word p argument 'ERROR))
	     (dotimes (- argument)
		      (lambda (i)
			i		;ignore
			(capitalize-one-word)))
	     (set-current-point! p))))))

;;;; Sentences

(define-command forward-sentence
  "Move forward to next sentence-end.  With argument, repeat.
With negative argument, move backward repeatedly to sentence-beginning.
Sentence ends are identified by the value of Sentence End
treated as a regular expression.  Also, every paragraph boundary
terminates sentences as well."
  "p"
  (lambda (argument)
    (move-thing forward-sentence argument 'ERROR)))

(define-command backward-sentence
  "Move backward to start of sentence.  With arg, do it arg times.
See \\[forward-sentence] for more information."
  "p"
  (lambda (argument)
    (move-thing backward-sentence argument 'ERROR)))

(define-command kill-sentence
  "Kill from point to end of sentence.
With arg, repeat, or backward if negative arg."
  "p"
  (lambda (argument)
    (kill-thing forward-sentence argument 'ERROR)))

(define-command backward-kill-sentence
  "Kill back from point to start of sentence.
With arg, repeat, or forward if negative arg."
  "p"
  (lambda (argument)
    (kill-thing backward-sentence argument 'ERROR)))

;;;; Paragraphs

(define-command forward-paragraph
  "Move forward to end of paragraph.  With arg, do it arg times.
A line which `paragraph-start' matches either separates paragraphs
\(if `paragraph-separate' matches it also) or is the first line of a paragraph.
A paragraph end is the beginning of a line which is not part of the paragraph
to which the end of the previous line belongs, or the end of the buffer."
  "p"
  (lambda (argument)
    (move-thing forward-paragraph argument 'ERROR)))

(define-command backward-paragraph
  "Move backward to start of paragraph.  With arg, do it arg times.
A paragraph start is the beginning of a line which is a first-line-of-paragraph
or which is ordinary text and follows a paragraph-separating line; except:
if the first real line of a paragraph is preceded by a blank line,
the paragraph starts at that blank line.
See forward-paragraph for more information."
  "p"
  (lambda (argument)
    (move-thing backward-paragraph argument 'ERROR)))

(define-command mark-paragraph
  "Put point at beginning of this paragraph, mark at end."
  ()
  (lambda ()
    (let ((end (forward-paragraph (current-point) 1 'ERROR)))
      (set-current-region!
       (make-region (backward-paragraph end 1 'ERROR) end)))))
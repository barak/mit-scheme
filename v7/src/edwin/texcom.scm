;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/texcom.scm,v 1.32 1989/04/28 22:53:52 cph Rel $
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

;;;; Text Commands

(declare (usual-integrations))

(define-major-mode text fundamental "Text"
  "Major mode for editing english text."
  (local-set-variable! syntax-table text-mode:syntax-table)
  (if (ref-variable text-mode-hook) ((ref-variable text-mode-hook))))

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
  "If not false, a thunk to call when entering Text mode."
  false)

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
    (move-thing forward-word argument)))

(define-command backward-word
  "Move one or more words backward."
  "p"
  (lambda (argument)
    (move-thing backward-word argument)))

(define-command mark-word
  "Set mark one or more words from point."
  "p"
  (lambda (argument)
    (mark-thing forward-word argument)))

(define-command kill-word
  "Kill one or more words forward."
  "p"
  (lambda (argument)
    (kill-thing forward-word argument)))

(define-command backward-kill-word
  "Kill one or more words backward."
  "p"
  (lambda (argument)
    (kill-thing backward-word argument)))

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
    (move-thing forward-sentence argument)))

(define-command backward-sentence
  "Move backward to start of sentence.  With arg, do it arg times.
See \\[forward-sentence] for more information."
  "p"
  (lambda (argument)
    (move-thing backward-sentence argument)))

(define-command mark-sentence
  "Put point at beginning and mark at end of sentence.
If you are between sentences, the following sentence is used
unless you are at the end of a paragraph."
  ()
  (lambda ()
    (let ((end (forward-sentence (current-point) 1 'ERROR)))
      (set-current-region!
       (make-region (backward-sentence end 1 'ERROR) end)))))

(define-command kill-sentence
  "Kill forward to end of sentence.
Accepts numeric argument of either sign."
  "p"
  (lambda (argument)
    (kill-thing forward-sentence argument)))

(define-command backward-kill-sentence
  "Kill backward to end of sentence.
Accepts numeric argument of either sign."
  "p"
  (lambda (argument)
    (kill-thing backward-sentence argument)))

;;;; Paragraphs

(define-command forward-paragraph
  "Move forward to end of paragraph.
See documentation on \\[backward-paragraph]."
  "p"
  (lambda (argument)
    (move-thing forward-paragraph argument)))

(define-command backward-paragraph
  "Move backward to start of paragraph.
Paragraphs are delimited by blank lines or by lines which
 start with a delimiter in  paragraph-delimiter  or  page-delimiter .
If there is a fill prefix, any line that doesn't start with it
 starts a paragraph.
Lines which start with the any character in text-justifier-escape-chars,
 if that character is matched by  paragraph-delimiter ,
 count as blank lines in that they separate paragraphs and
 are not part of them."
  "p"
  (lambda (argument)
    (move-thing backward-paragraph argument)))

(define-command mark-paragraph
  "Put point and mark around this paragraph.
In between paragraphs, puts it around the next one.
See \\[backward-paragraph] for paragraph definition."
  ()
  (lambda ()
    (let ((end (forward-paragraph (current-point) 1 'ERROR)))
      (set-current-region!
       (make-region (backward-paragraph end 1 'ERROR) end)))))
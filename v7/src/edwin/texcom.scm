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

;;;; Text Commands

(declare (usual-integrations))
(using-syntax edwin-syntax-table

(define-major-mode "Text" "Fundamental"
  "Major mode for editing english text."
  (local-set-variable! "Syntax Table" text-mode:syntax-table)
  (if (ref-variable "Text Mode Hook") ((ref-variable "Text Mode Hook"))))

(define-key "Text" #\M-S "^R Center Line")

(define text-mode:syntax-table (make-syntax-table))
(modify-syntax-entry! text-mode:syntax-table #\" "    ")
(modify-syntax-entry! text-mode:syntax-table #\\ "    ")
(modify-syntax-entry! text-mode:syntax-table #\[ "(]  ")
(modify-syntax-entry! text-mode:syntax-table #\] ")[  ")
(modify-syntax-entry! text-mode:syntax-table #\{ "(}  ")
(modify-syntax-entry! text-mode:syntax-table #\} "){  ")
(modify-syntax-entry! text-mode:syntax-table #\' "w   ")

(define-variable "Text Mode Hook"
  "If not false, a thunk to call when entering Text mode."
  #!FALSE)

(define (turn-on-auto-fill)
  (enable-current-minor-mode! fill-mode))

(define-command ("Text Mode" argument)
  "Make the current mode be Text mode."
  (set-current-major-mode! text-mode))

(define-major-mode "Indented-Text" "Text"
  "Like Text mode, but indents each line under previous non-blank line."
  ((mode-initialization text-mode))
  (local-set-variable! "Indent Line Procedure" ^r-indent-relative-command))

(define-command ("Indented Text Mode" argument)
  "Make the current mode be Indented Text mode."
  (set-current-major-mode! indented-text-mode))

;;;; Words

(define-command ("^R Forward Word" (argument 1))
  "Move one or more words forward."
  (move-thing forward-word argument))

(define-command ("^R Backward Word" (argument 1))
  "Move one or more words backward."
  (move-thing backward-word argument))

(define-command ("^R Mark Word" (argument 1))
  "Set mark one or more words from point."
  (mark-thing forward-word argument))

(define-command ("^R Kill Word" (argument 1))
  "Kill one or more words forward."
  (kill-thing forward-word argument))

(define-command ("^R Backward Kill Word" (argument 1))
  "Kill one or more words backward."
  (kill-thing backward-word argument))

(define-command ("^R Transpose Words" (argument 1))
  "Transpose the words before and after the cursor.
With a positive argument it transposes the words before and after the
 cursor, moves right, and repeats the specified number of times,
 dragging the word to the left of the cursor right.
With a negative argument, it transposes the two words to the left of
 the cursor, moves between them, and repeats the specified number of
 times, exactly undoing the positive argument form.
With a zero argument, it transposes the words at point and mark."
  (transpose-things forward-word argument))

;;;; Case Conversion

(define-command ("^R Uppercase Region" argument)
  "Convert region to upper case."
  (upcase-area (current-mark)))

(define-command ("^R Lowercase Region" argument)
  "Convert region to lower case."
  (downcase-area (current-mark)))

(define-command ("^R Uppercase Word" (argument 1))
  "Uppercase one or more words.
Moves forward over the words affected.
With a negative argument, uppercases words before point
but does not move point."
  (upcase-area (forward-word (current-point) argument 'ERROR)))

(define-command ("^R Lowercase Word" (argument 1))
  "Lowercase one or more words.
Moves forward over the words affected.
With a negative argument, lowercases words before point
but does not move point."
  (downcase-area (forward-word (current-point) argument 'ERROR)))

(define-command ("^R Uppercase Initial" (argument 1))
  "Put next word in lowercase, but capitalize initial.
With an argument, capitalizes that many words."
  (define (capitalize-one-word)
    (set-current-point! (forward-to-word (current-point) 'ERROR))
    (capitalize-area (forward-word (current-point) 1 'ERROR)))
  (cond ((positive? argument)
	 (dotimes argument
	   (lambda (i)
	     (capitalize-one-word))))
	((negative? argument)
	 (let ((p (current-point)))
	   (set-current-point! (forward-word p argument 'ERROR))
	   (dotimes (- argument)
	     (lambda (i)
	       (capitalize-one-word)))
	   (set-current-point! p)))))

;;;; Sentences

(define-command ("^R Forward Sentence" (argument 1))
  "Move forward to next sentence-end.  With argument, repeat.
With negative argument, move backward repeatedly to sentence-beginning.
Sentence ends are identified by the value of Sentence End
treated as a regular expression.  Also, every paragraph boundary
terminates sentences as well."
  (move-thing forward-sentence argument))

(define-command ("^R Backward Sentence" (argument 1))
  "Move backward to start of sentence.  With arg, do it arg times.
See \\[^R Forward Sentence] for more information."
  (move-thing backward-sentence argument))

(define-command ("^R Mark Sentence" (argument 1))
  "Put point at beginning and mark at end of sentence.
If you are between sentences, the following sentence is used
unless you are at the end of a paragraph."
  (let ((end (forward-sentence (current-point) 1 'ERROR)))
    (set-current-region! (make-region (backward-sentence end 1 'ERROR) end))))

(define-command ("^R Kill Sentence" (argument 1))
  "Kill forward to end of sentence.
Accepts numeric argument of either sign."
  (kill-thing forward-sentence argument))

(define-command ("^R Backward Kill Sentence" (argument 1))
  "Kill backward to end of sentence.
Accepts numeric argument of either sign."
  (kill-thing backward-sentence argument))

;;;; Paragraphs

(define-command ("^R Forward Paragraph" (argument 1))
  "Move forward to end of paragraph.
See documentation on ^R Backward Paragraph."
  (move-thing forward-paragraph argument))

(define-command ("^R Backward Paragraph" (argument 1))
  "Move backward to start of paragraph.
Paragraphs are delimited by blank lines or by lines which
start with a delimiter in Paragraph Delimiter or Page Delimiter.
If there is a fill prefix, any line that doesn't start with it
starts a paragraph.
Lines which start with the any character in Text Justifier
Escape Chars, if that character is matched by Paragraph Delimiter,
count as blank lines in that they separate paragraphs and
are not part of them."
  (move-thing backward-paragraph argument))

(define-command ("^R Mark Paragraph" argument)
  "Put point and mark around this paragraph.
In between paragraphs, puts it around the next one.
See ^R Backward Paragraph for paragraph definition."
  (let ((end (forward-paragraph (current-point) 1 'ERROR)))
    (set-current-region! (make-region (backward-paragraph end 1 'ERROR) end))))

;;; end USING-SYNTAX
)
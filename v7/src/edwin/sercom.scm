;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/sercom.scm,v 1.52 1989/03/14 08:02:48 cph Exp $
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

;;;; Search Commands

(declare (usual-integrations))

;;;; Character Search
;;; JAR Special

(define-variable "Case Fold Search"
  "If not false, search commands are insensitive to case."
  true)

(define-command ("^R Character Search" argument)
  "Search for a single character.
Special characters:
  C-A  calls \\[Search Forward].
  C-R  searches backwards for the current default.
  C-S  searches forward for the current default.
  C-Q  quotes the character to be searched for;
       this allows search for special characters."
  (character-search argument true))

(define-command ("^R Reverse Character Search" argument)
  "Like \\[^R Winning Character Search], but searches backwards."
  (character-search argument false))

(define (character-search argument forward?)
  (define (char-search char)
    (search-finish
     ((if forward? char-search-forward char-search-backward)
      char)))

  (define (string-search operator)
    (search-finish (operator (ref-variable "Previous Search String"))))

  (define (search-finish mark)
    (if mark
	(set-current-point! mark)
	(editor-failure)))

  (let ((char (prompt-for-char "Character Search")))
    (case (char-upcase char)
      ((#\C-A)
       ((if forward?
	    search-forward-command
	    search-backward-command)
	argument))
      ((#\C-S) (string-search search-forward))
      ((#\C-R) (string-search search-backward))
      ((#\C-Q)
       (char-search (prompt-for-char-without-interrupts "Quote Character")))
      (else (char-search char)))))

;;;; String Search

(define-variable "Previous Search String"
  "Last string searched for by any string search command."
  "")

(define-variable "Previous Search Regexp"
  "Last regular expression searched for by any search command."
  false)

(define-command ("Search Forward")
  "Search forward from point for a character string.
Sets point at the end of the occurrence found."
  (search-command search-prompt "Search" search-forward))

(define-command ("Search Backward")
  "Search backward from point for a character string.
Sets point at the beginning of the occurrence found."
  (search-command search-prompt "Search Backward" search-backward))

(define-command ("RE Search Forward")
  "Search forward from point for a regular expression.
Sets point at the end of the occurrence found."
  (search-command re-search-prompt "RE Search" re-search-forward))

(define-command ("RE Search Backward")
  "Search backward from point for a character string.
Sets point at the beginning of the occurrence found."
  (search-command re-search-prompt "RE Search Backward" re-search-backward))

(define (search-command prompter prompt procedure)
  (let ((mark (procedure (prompter prompt))))
    (if mark
	(begin (push-current-mark! (current-point))
	       (set-current-point! mark))
	(editor-failure))))

(define (search-prompt prompt)
  (let ((string (prompt-for-string prompt
				   (ref-variable "Previous Search String"))))
    (set-variable! "Previous Search String" string)
    string))

(define (re-search-prompt prompt)
  (let ((regexp (prompt-for-string prompt
				   (ref-variable "Previous Search Regexp"))))
    (set-variable! "Previous Search Regexp" regexp)
    regexp))
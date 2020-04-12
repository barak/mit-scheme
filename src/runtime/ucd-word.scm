#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; Unicode: words
;;; package: (runtime ucd-segmentation word)

(declare (usual-integrations))

(add-boot-deps! '(runtime ucd-glue))

(define codes
  '(carriage-return
    double-quote
    emoji-base
    emoji-base-gaz
    emoji-modifier
    extend-num-let
    extend
    format
    glue-after-zwj
    hebrew-letter
    katakana
    letter
    linefeed
    mid-num-let
    mid-letter
    mid-num
    newline
    numeric
    regional-indicator
    single-quote
    other
    zwj))

(define extra-states
  '((save xletter)
    (save hldq)
    (save xnumeric)
    ri*2))

(define transitions
  '((sot / any)
    (any / eot)

    (carriage-return _ linefeed)
    ((newline carriage-return linefeed) / any)
    (any / (newline carriage-return linefeed))

    (zwj _ (glue-after-zwj emoji-base-gaz))

    (zwj _ (extend format zwj))
    (any _ (extend format zwj) (from))

    ((letter hebrew-letter) _ (letter hebrew-letter))
    ((letter hebrew-letter) ? (mid-letter mid-num-let single-quote) xletter)
    (xletter (_ _) (letter hebrew-letter))

    ((letter hebrew-letter) ? (mid-letter mid-num-let single-quote) xletter)
    (xletter (_ _) (letter hebrew-letter))

    (hebrew-letter _ single-quote)
    (hebrew-letter ? double-quote hldq)
    (hldq (_ _) hebrew-letter)

    (numeric _ numeric)
    ((letter hebrew-letter) _ numeric)
    (numeric _ (letter hebrew-letter))

    (numeric ? (mid-num mid-num-let single-quote) xnumeric)
    (xnumeric (_ _) numeric)

    (katakana _ katakana)

    (extend-num-let _ extend-num-let)
    ((letter hebrew-letter numeric katakana) _ extend-num-let)
    (extend-num-let _ (letter hebrew-letter numeric katakana))

    ((emoji-base emoji-base-gaz) _ emoji-modifier)

    (regional-indicator _ regional-indicator ri*2)

    (any / any)))

(define evolver)
(define string-wb-fold)
(define string-wb-fold-right)
(define string-wb-stream)
(define string->wb-names)
(define show-transitions)
(add-boot-init!
 (lambda ()
   (set! evolver (make-evolver codes extra-states ucd-wb-value transitions))
   (set! string-wb-fold (folder evolver 'string-wb-fold))
   (set! string-wb-fold-right (right-folder evolver 'string-wb-fold-right))
   (set! string-wb-stream (streamer evolver 'string-wb-stream))
   (set! string->wb-names (evolver-string->code-names evolver))
   (set! show-transitions (evolver-show-transitions evolver))
   unspecific))

(define (string-word-breaks string)
  (let loop ((stream (string-wb-stream string)))
    (if (pair? stream)
	(cons (car stream) (loop (force (cdr stream))))
	'())))

(define (find-word-breaks string knil kons)
  (string-wb-fold (lambda (break prev-break acc)
		    (declare (ignore prev-break))
		    (kons break acc))
		  knil
		  string))

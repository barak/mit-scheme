#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

(add-boot-deps! '(runtime ucd-glue) '(runtime ucd-segmentation))

(define evolver)
(define string-word-breaks)
(add-boot-init!
 (lambda ()
   (set! evolver
	 (make-evolver codes abbrevs extra-states ucd-wb+ep-value rules))
   (set! string-word-breaks (evolver-interpreter evolver))
   unspecific))

(define codes
  '(carriage-return
    double-quote
    extend-num-let
    extend
    format
    hebrew-letter
    katakana
    letter-ep
    letter+ep
    linefeed
    mid-num-let
    mid-letter
    mid-num
    newline
    numeric
    ri
    single-quote
    white-seg-space
    other-ep
    other+ep
    zwj))

(define abbrevs
  '((crlf (or newline carriage-return linefeed))
    (ahletter (or letter-ep letter+ep hebrew-letter))
    (mid-num-let-q (or mid-num-let single-quote))
    (ext-pict (or letter+ep other+ep))
    (efz (or extend format zwj))
    (efz* (* _ efz))))

(define extra-states
  '(ri*2))

(define rules
  '((sot / any)
    (any / eot)
    (sot _ eot)

    (carriage-return _ linefeed)
    (crlf / any)
    (any / crlf)

    (zwj _ ext-pict)
    (white-seg-space _ white-seg-space)

    (any _ efz)

    (ahletter efz* _ ahletter)

    (ahletter efz* _ (or mid-letter mid-num-let-q) efz* _ ahletter)
    (hebrew-letter efz* _ single-quote)
    (hebrew-letter efz* _ double-quote efz* _ hebrew-letter)

    (numeric efz* _ numeric)
    (ahletter efz* _ numeric)
    (numeric efz* _ ahletter)

    (numeric efz* _ (or mid-num mid-num-let-q) efz* _ numeric)

    (katakana efz* _ katakana)

    ((or ahletter numeric katakana extend-num-let) efz* _ extend-num-let)
    (extend-num-let efz* _ (or ahletter numeric katakana))

    (ri efz* _ ri ri*2)

    (any / any)))

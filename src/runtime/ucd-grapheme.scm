#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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

;;;; Unicode: grapheme clusters
;;; package: (runtime ucd-segmentation grapheme)

(declare (usual-integrations))

(add-boot-deps! '(runtime ucd-glue) '(runtime ucd-segmentation))

(define evolver)
(define grapheme-cluster-breaks)
(add-boot-init!
 (lambda ()
   (set! evolver
	 (make-evolver codes abbrevs extra-states ucd-gcb+ep-value rules))
   (set! grapheme-cluster-breaks (evolver-interpreter evolver))
   unspecific))

(define codes
  '(control
    carriage-return
    extend
    hst=l
    linefeed
    hst=lv
    hst=lvt
    prepend
    ri
    spacing-mark
    hst=t
    hst=v
    other
    ext-pict
    zwj))

(define abbrevs
  '())

(define extra-states
  '(ri*2))

(define rules
  '((sot / any)
    (any / eot)
    (sot _ eot)

    (carriage-return _ linefeed)
    ((or control carriage-return linefeed) / any)
    (any / (or control carriage-return linefeed))

    (hst=l _ (or hst=l hst=lv hst=lvt hst=v))
    ((or hst=v hst=lv) _ (or hst=t hst=v))
    ((or hst=t hst=lvt) _ hst=t)

    (any _ (or extend zwj spacing-mark))
    (prepend _ any)

    (ext-pict (* _ extend) _ zwj _ ext-pict)

    (ri _ ri ri*2)

    (any / any)))

(define (string->grapheme-clusters string #!optional start end)
  (let ((breaks (grapheme-cluster-breaks string start end)))
    (if (pair? breaks)
	(let loop ((breaks (cdr breaks)) (prev-break (car breaks)))
	  (if (pair? breaks)
	      (cons (substring string prev-break (car breaks))
		    (loop (cdr breaks) (car breaks)))
	      '()))
	'())))

(define (grapheme-cluster-length string)
  (gclength (grapheme-cluster-breaks string)))

(define (grapheme-cluster-slice string start end)
  ;; START and END refer to the cluster breaks, they must be <= the number of
  ;; clusters in STRING.
  (let ((breaks (grapheme-cluster-breaks string)))
    (let ((end (fix:end-index end (gclength breaks) 'grapheme-cluster-slice))
	  (start (fix:start-index start end 'grapheme-cluster-slice)))
      (string-slice string
		    (list-ref breaks start)
		    (list-ref breaks end)))))

(define (gclength breaks)
  (let ((n (length breaks)))
    (if (fix:> n 0)
	(fix:- n 1)
	n)))
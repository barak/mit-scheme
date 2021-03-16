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

(define evolver)
(define string-gcb-fold)
(define string-gcb-fold-right)
(define string-gcb-stream)
(define string->gcb-names)
(define show-transitions)
(add-boot-init!
 (lambda ()
   (set! evolver
	 (make-evolver codes abbrevs extra-states ucd-gcb+ep-value rules))
   (set! string-gcb-fold (folder evolver 'string-gcb-fold))
   (set! string-gcb-fold-right (right-folder evolver 'string-gcb-fold-right))
   (set! string-gcb-stream (streamer evolver 'string-gcb-stream))
   (set! string->gcb-names (evolver-string->code-names evolver))
   (set! show-transitions (evolver-show-transitions evolver))
   unspecific))

(define (string->grapheme-clusters string #!optional start end)
  (string-gcb-fold-right (lambda (break prev-break acc)
			   (if prev-break
			       (cons (substring string prev-break break)
				     acc)
			       acc))
			 '()
			 string start end))

(define (grapheme-cluster-length string)
  (string-gcb-fold (lambda (break prev-break count)
		     (declare (ignore break))
		     (if prev-break
			 (fix:+ count 1)
			 count))
		   0
		   string))

(define (grapheme-cluster-slice string start end)
  ;; START and END refer to the cluster breaks, they must be <= the number of
  ;; clusters in STRING.
  (guarantee index-fixnum? start 'grapheme-cluster-slice)
  (guarantee index-fixnum? end 'grapheme-cluster-slice)
  (if (not (fix:<= start end))
      (error:bad-range-argument start 'grapheme-cluster-slice))
  (let ((breaks (grapheme-cluster-breaks string)))
    (string-slice string
		  (list-ref breaks start)
		  (list-ref breaks end))))

(define (grapheme-cluster-breaks string #!optional start end)
  (let loop ((stream (string-gcb-stream string start end)))
    (if (pair? stream)
	(cons (car stream) (loop (force (cdr stream))))
	'())))
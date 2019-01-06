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

;;;; Tests of string search

(declare (usual-integrations))

(define text
  (call-with-input-file
      (merge-pathnames "test-string-search-data"
		       (directory-pathname (current-load-pathname)))
    (lambda (port)
      (let ((builder (string-builder))
            (buffer (make-string #x1000)))
        (let loop ()
          (let ((n (read-string! buffer port)))
            (if (> n 0)
                (begin
                  (builder (substring buffer 0 n))
                  (loop)))))
        (builder 'immutable)))))

(define patterns
  '(
    ;; occur frequently:
    "define"
    "mutable"
    "ustring"
    "immutable"
    "lambda"
    ;; occur near start:
    "MIT/GNU Scheme"
    "Software"
    ;; occur near middle:
    "test-first-char"
    "ucd-canonical-cm-value"
    ;; occur near end:
    "decorated-string-append"
    "burst-string"))

(define (find-instances pattern)
  (let ((pend (string-length pattern))
	(tend (string-length text)))

    (define (loop ti matches)
      (if (fix:< ti tend)
	  (loop (fix:+ ti 1)
		(if (match-1 0 ti)
		    (cons ti matches)
		    matches))
	  (cons pattern (reverse! matches))))

    (define (match-1 pi ti)
      (if (and (fix:< pi pend)
	       (fix:< ti tend))
	  (and (char=? (string-ref pattern pi)
		       (string-ref text ti))
	       (match-1 (fix:+ pi 1)
			(fix:+ ti 1)))
	  #t))

    (loop 0 '())))

(define pattern-instances
  (map find-instances patterns))

(define-test 'search-tests
  (map (lambda (entry)
         (let ((pattern (car entry))
               (indices (cdr entry)))
           (list
            (lambda ()
              (with-test-properties
               (lambda ()
                 (assert-equal (string-search-forward pattern text)
                               (car indices)))
               'expression `(string-search-forward ,pattern)))
            (lambda ()
              (with-test-properties
               (lambda ()
                 (assert-equal (string-search-backward pattern text)
                               (last indices)))
               'expression `(string-search-backward ,pattern)))
            (lambda ()
              (with-test-properties
               (lambda ()
                 (assert-equal (string-search-all pattern text)
                               indices))
               'expression `(string-search-all ,pattern))))))
       pattern-instances))
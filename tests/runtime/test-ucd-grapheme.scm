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

;;;; Tests of Unicode grapheme-cluster segmentation

(declare (usual-integrations))

(include "test-ucd-data/segmentation-support.scm")

(define-test 'grapheme-cluster-breaks
  (map (lambda (test-case)
	 (let ((string (test-case->string test-case))
	       (breaks (test-case->breaks test-case)))
	   (lambda ()
	     (with-test-properties
	      (lambda ()
		(assert-equal (grapheme-cluster-breaks string) breaks))
	      'expression string))))
       '((#t #\x0020 #t #\x0020 #t)
	 (#t #\x0020 #f #\x0308 #t #\x0020 #t)
	 (#t #\x0020 #t #\x000D #t)
	 (#t #\x0020 #f #\x0308 #t #\x000D #t)
	 (#t #\x0020 #t #\x000A #t)
	 (#t #\x0020 #f #\x0308 #t #\x000A #t)
	 (#t #\x0020 #t #\x0001 #t)
	 (#t #\x0020 #f #\x0308 #t #\x0001 #t)
	 (#t #\x0020 #f #\x0300 #t)
	 (#t #\x0020 #f #\x0308 #f #\x0300 #t)
	 (#t #\x0020 #t #\x0600 #t)
	 (#t #\x0020 #f #\x0308 #t #\x0600 #t)
	 (#t #\x0020 #f #\x0903 #t)
	 (#t #\x0020 #f #\x0308 #f #\x0903 #t)
	 (#t #\x0020 #t #\x1100 #t)
	 (#t #\x0020 #f #\x0308 #t #\x1100 #t)
	 (#t #\x0020 #t #\x1160 #t)
	 (#t #\x0020 #f #\x0308 #t #\x1160 #t)
	 (#t #\x0020 #t #\x11A8 #t)
	 (#t #\x0020 #f #\x0308 #t #\x11A8 #t)
	 (#t #\x0020 #t #\xAC00 #t)
	 (#t #\x0020 #f #\x0308 #t #\xAC00 #t)
	 (#t #\x0020 #t #\xAC01 #t)
	 (#t #\x0020 #f #\x0308 #t #\xAC01 #t)
	 (#t #\x0020 #t #\x1F1E6 #t)
	 (#t #\x0020 #f #\x0308 #t #\x1F1E6 #t)
	 (#t #\x0020 #t #\x261D #t)
	 (#t #\x0020 #f #\x0308 #t #\x261D #t)
	 (#t #\x0020 #t #\x1F3FB #t)
	 (#t #\x0020 #f #\x0308 #t #\x1F3FB #t)
	 (#t #\x0020 #f #\x200D #t)
	 (#t #\x0020 #f #\x0308 #f #\x200D #t)
	 (#t #\x0020 #t #\x2764 #t)
	 (#t #\x0020 #f #\x0308 #t #\x2764 #t)
	 (#t #\x0020 #t #\x1F466 #t)
	 (#t #\x0020 #f #\x0308 #t #\x1F466 #t)
	 (#t #\x0020 #t #\x0378 #t)
	 (#t #\x0020 #f #\x0308 #t #\x0378 #t)
	 (#t #\x0020 #t #\xD800 #t)
	 (#t #\x0020 #f #\x0308 #t #\xD800 #t)
	 (#t #\x000D #t #\x0020 #t)
	 (#t #\x000D #t #\x0308 #t #\x0020 #t)
	 (#t #\x000D #t #\x000D #t)
	 (#t #\x000D #t #\x0308 #t #\x000D #t)
	 (#t #\x000D #f #\x000A #t)
	 (#t #\x000D #t #\x0308 #t #\x000A #t)
	 (#t #\x000D #t #\x0001 #t)
	 (#t #\x000D #t #\x0308 #t #\x0001 #t)
	 (#t #\x000D #t #\x0300 #t)
	 (#t #\x000D #t #\x0308 #f #\x0300 #t)
	 (#t #\x000D #t #\x0600 #t)
	 (#t #\x000D #t #\x0308 #t #\x0600 #t)
	 (#t #\x000D #t #\x0903 #t)
	 (#t #\x000D #t #\x0308 #f #\x0903 #t)
	 (#t #\x000D #t #\x1100 #t)
	 (#t #\x000D #t #\x0308 #t #\x1100 #t)
	 (#t #\x000D #t #\x1160 #t)
	 (#t #\x000D #t #\x0308 #t #\x1160 #t)
	 (#t #\x000D #t #\x11A8 #t)
	 (#t #\x000D #t #\x0308 #t #\x11A8 #t)
	 (#t #\x000D #t #\xAC00 #t)
	 (#t #\x000D #t #\x0308 #t #\xAC00 #t)
	 (#t #\x000D #t #\xAC01 #t)
	 (#t #\x000D #t #\x0308 #t #\xAC01 #t)
	 (#t #\x000D #t #\x1F1E6 #t)
	 (#t #\x000D #t #\x0308 #t #\x1F1E6 #t)
	 (#t #\x000D #t #\x261D #t)
	 (#t #\x000D #t #\x0308 #t #\x261D #t)
	 (#t #\x000D #t #\x1F3FB #t)
	 (#t #\x000D #t #\x0308 #t #\x1F3FB #t)
	 (#t #\x000D #t #\x200D #t)
	 (#t #\x000D #t #\x0308 #f #\x200D #t)
	 (#t #\x000D #t #\x2764 #t)
	 (#t #\x000D #t #\x0308 #t #\x2764 #t)
	 (#t #\x000D #t #\x1F466 #t)
	 (#t #\x000D #t #\x0308 #t #\x1F466 #t)
	 (#t #\x000D #t #\x0378 #t)
	 (#t #\x000D #t #\x0308 #t #\x0378 #t)
	 (#t #\x000D #t #\xD800 #t)
	 (#t #\x000D #t #\x0308 #t #\xD800 #t)
	 (#t #\x000A #t #\x0020 #t)
	 (#t #\x000A #t #\x0308 #t #\x0020 #t)
	 (#t #\x000A #t #\x000D #t)
	 (#t #\x000A #t #\x0308 #t #\x000D #t)
	 (#t #\x000A #t #\x000A #t)
	 (#t #\x000A #t #\x0308 #t #\x000A #t)
	 (#t #\x000A #t #\x0001 #t)
	 (#t #\x000A #t #\x0308 #t #\x0001 #t)
	 (#t #\x000A #t #\x0300 #t)
	 (#t #\x000A #t #\x0308 #f #\x0300 #t)
	 (#t #\x000A #t #\x0600 #t)
	 (#t #\x000A #t #\x0308 #t #\x0600 #t)
	 (#t #\x000A #t #\x0903 #t)
	 (#t #\x000A #t #\x0308 #f #\x0903 #t)
	 (#t #\x000A #t #\x1100 #t)
	 (#t #\x000A #t #\x0308 #t #\x1100 #t)
	 (#t #\x000A #t #\x1160 #t)
	 (#t #\x000A #t #\x0308 #t #\x1160 #t)
	 (#t #\x000A #t #\x11A8 #t)
	 (#t #\x000A #t #\x0308 #t #\x11A8 #t)
	 (#t #\x000A #t #\xAC00 #t)
	 (#t #\x000A #t #\x0308 #t #\xAC00 #t)
	 (#t #\x000A #t #\xAC01 #t)
	 (#t #\x000A #t #\x0308 #t #\xAC01 #t)
	 (#t #\x000A #t #\x1F1E6 #t)
	 (#t #\x000A #t #\x0308 #t #\x1F1E6 #t)
	 (#t #\x000A #t #\x261D #t)
	 (#t #\x000A #t #\x0308 #t #\x261D #t)
	 (#t #\x000A #t #\x1F3FB #t)
	 (#t #\x000A #t #\x0308 #t #\x1F3FB #t)
	 (#t #\x000A #t #\x200D #t)
	 (#t #\x000A #t #\x0308 #f #\x200D #t)
	 (#t #\x000A #t #\x2764 #t)
	 (#t #\x000A #t #\x0308 #t #\x2764 #t)
	 (#t #\x000A #t #\x1F466 #t)
	 (#t #\x000A #t #\x0308 #t #\x1F466 #t)
	 (#t #\x000A #t #\x0378 #t)
	 (#t #\x000A #t #\x0308 #t #\x0378 #t)
	 (#t #\x000A #t #\xD800 #t)
	 (#t #\x000A #t #\x0308 #t #\xD800 #t)
	 (#t #\x0001 #t #\x0020 #t)
	 (#t #\x0001 #t #\x0308 #t #\x0020 #t)
	 (#t #\x0001 #t #\x000D #t)
	 (#t #\x0001 #t #\x0308 #t #\x000D #t)
	 (#t #\x0001 #t #\x000A #t)
	 (#t #\x0001 #t #\x0308 #t #\x000A #t)
	 (#t #\x0001 #t #\x0001 #t)
	 (#t #\x0001 #t #\x0308 #t #\x0001 #t)
	 (#t #\x0001 #t #\x0300 #t)
	 (#t #\x0001 #t #\x0308 #f #\x0300 #t)
	 (#t #\x0001 #t #\x0600 #t)
	 (#t #\x0001 #t #\x0308 #t #\x0600 #t)
	 (#t #\x0001 #t #\x0903 #t)
	 (#t #\x0001 #t #\x0308 #f #\x0903 #t)
	 (#t #\x0001 #t #\x1100 #t)
	 (#t #\x0001 #t #\x0308 #t #\x1100 #t)
	 (#t #\x0001 #t #\x1160 #t)
	 (#t #\x0001 #t #\x0308 #t #\x1160 #t)
	 (#t #\x0001 #t #\x11A8 #t)
	 (#t #\x0001 #t #\x0308 #t #\x11A8 #t)
	 (#t #\x0001 #t #\xAC00 #t)
	 (#t #\x0001 #t #\x0308 #t #\xAC00 #t)
	 (#t #\x0001 #t #\xAC01 #t)
	 (#t #\x0001 #t #\x0308 #t #\xAC01 #t)
	 (#t #\x0001 #t #\x1F1E6 #t)
	 (#t #\x0001 #t #\x0308 #t #\x1F1E6 #t)
	 (#t #\x0001 #t #\x261D #t)
	 (#t #\x0001 #t #\x0308 #t #\x261D #t)
	 (#t #\x0001 #t #\x1F3FB #t)
	 (#t #\x0001 #t #\x0308 #t #\x1F3FB #t)
	 (#t #\x0001 #t #\x200D #t)
	 (#t #\x0001 #t #\x0308 #f #\x200D #t)
	 (#t #\x0001 #t #\x2764 #t)
	 (#t #\x0001 #t #\x0308 #t #\x2764 #t)
	 (#t #\x0001 #t #\x1F466 #t)
	 (#t #\x0001 #t #\x0308 #t #\x1F466 #t)
	 (#t #\x0001 #t #\x0378 #t)
	 (#t #\x0001 #t #\x0308 #t #\x0378 #t)
	 (#t #\x0001 #t #\xD800 #t)
	 (#t #\x0001 #t #\x0308 #t #\xD800 #t)
	 (#t #\x0300 #t #\x0020 #t)
	 (#t #\x0300 #f #\x0308 #t #\x0020 #t)
	 (#t #\x0300 #t #\x000D #t)
	 (#t #\x0300 #f #\x0308 #t #\x000D #t)
	 (#t #\x0300 #t #\x000A #t)
	 (#t #\x0300 #f #\x0308 #t #\x000A #t)
	 (#t #\x0300 #t #\x0001 #t)
	 (#t #\x0300 #f #\x0308 #t #\x0001 #t)
	 (#t #\x0300 #f #\x0300 #t)
	 (#t #\x0300 #f #\x0308 #f #\x0300 #t)
	 (#t #\x0300 #t #\x0600 #t)
	 (#t #\x0300 #f #\x0308 #t #\x0600 #t)
	 (#t #\x0300 #f #\x0903 #t)
	 (#t #\x0300 #f #\x0308 #f #\x0903 #t)
	 (#t #\x0300 #t #\x1100 #t)
	 (#t #\x0300 #f #\x0308 #t #\x1100 #t)
	 (#t #\x0300 #t #\x1160 #t)
	 (#t #\x0300 #f #\x0308 #t #\x1160 #t)
	 (#t #\x0300 #t #\x11A8 #t)
	 (#t #\x0300 #f #\x0308 #t #\x11A8 #t)
	 (#t #\x0300 #t #\xAC00 #t)
	 (#t #\x0300 #f #\x0308 #t #\xAC00 #t)
	 (#t #\x0300 #t #\xAC01 #t)
	 (#t #\x0300 #f #\x0308 #t #\xAC01 #t)
	 (#t #\x0300 #t #\x1F1E6 #t)
	 (#t #\x0300 #f #\x0308 #t #\x1F1E6 #t)
	 (#t #\x0300 #t #\x261D #t)
	 (#t #\x0300 #f #\x0308 #t #\x261D #t)
	 (#t #\x0300 #t #\x1F3FB #t)
	 (#t #\x0300 #f #\x0308 #t #\x1F3FB #t)
	 (#t #\x0300 #f #\x200D #t)
	 (#t #\x0300 #f #\x0308 #f #\x200D #t)
	 (#t #\x0300 #t #\x2764 #t)
	 (#t #\x0300 #f #\x0308 #t #\x2764 #t)
	 (#t #\x0300 #t #\x1F466 #t)
	 (#t #\x0300 #f #\x0308 #t #\x1F466 #t)
	 (#t #\x0300 #t #\x0378 #t)
	 (#t #\x0300 #f #\x0308 #t #\x0378 #t)
	 (#t #\x0300 #t #\xD800 #t)
	 (#t #\x0300 #f #\x0308 #t #\xD800 #t)
	 (#t #\x0600 #f #\x0020 #t)
	 (#t #\x0600 #f #\x0308 #t #\x0020 #t)
	 (#t #\x0600 #t #\x000D #t)
	 (#t #\x0600 #f #\x0308 #t #\x000D #t)
	 (#t #\x0600 #t #\x000A #t)
	 (#t #\x0600 #f #\x0308 #t #\x000A #t)
	 (#t #\x0600 #t #\x0001 #t)
	 (#t #\x0600 #f #\x0308 #t #\x0001 #t)
	 (#t #\x0600 #f #\x0300 #t)
	 (#t #\x0600 #f #\x0308 #f #\x0300 #t)
	 (#t #\x0600 #f #\x0600 #t)
	 (#t #\x0600 #f #\x0308 #t #\x0600 #t)
	 (#t #\x0600 #f #\x0903 #t)
	 (#t #\x0600 #f #\x0308 #f #\x0903 #t)
	 (#t #\x0600 #f #\x1100 #t)
	 (#t #\x0600 #f #\x0308 #t #\x1100 #t)
	 (#t #\x0600 #f #\x1160 #t)
	 (#t #\x0600 #f #\x0308 #t #\x1160 #t)
	 (#t #\x0600 #f #\x11A8 #t)
	 (#t #\x0600 #f #\x0308 #t #\x11A8 #t)
	 (#t #\x0600 #f #\xAC00 #t)
	 (#t #\x0600 #f #\x0308 #t #\xAC00 #t)
	 (#t #\x0600 #f #\xAC01 #t)
	 (#t #\x0600 #f #\x0308 #t #\xAC01 #t)
	 (#t #\x0600 #f #\x1F1E6 #t)
	 (#t #\x0600 #f #\x0308 #t #\x1F1E6 #t)
	 (#t #\x0600 #f #\x261D #t)
	 (#t #\x0600 #f #\x0308 #t #\x261D #t)
	 (#t #\x0600 #f #\x1F3FB #t)
	 (#t #\x0600 #f #\x0308 #t #\x1F3FB #t)
	 (#t #\x0600 #f #\x200D #t)
	 (#t #\x0600 #f #\x0308 #f #\x200D #t)
	 (#t #\x0600 #f #\x2764 #t)
	 (#t #\x0600 #f #\x0308 #t #\x2764 #t)
	 (#t #\x0600 #f #\x1F466 #t)
	 (#t #\x0600 #f #\x0308 #t #\x1F466 #t)
	 (#t #\x0600 #f #\x0378 #t)
	 (#t #\x0600 #f #\x0308 #t #\x0378 #t)
	 (#t #\x0600 #t #\xD800 #t)
	 (#t #\x0600 #f #\x0308 #t #\xD800 #t)
	 (#t #\x0903 #t #\x0020 #t)
	 (#t #\x0903 #f #\x0308 #t #\x0020 #t)
	 (#t #\x0903 #t #\x000D #t)
	 (#t #\x0903 #f #\x0308 #t #\x000D #t)
	 (#t #\x0903 #t #\x000A #t)
	 (#t #\x0903 #f #\x0308 #t #\x000A #t)
	 (#t #\x0903 #t #\x0001 #t)
	 (#t #\x0903 #f #\x0308 #t #\x0001 #t)
	 (#t #\x0903 #f #\x0300 #t)
	 (#t #\x0903 #f #\x0308 #f #\x0300 #t)
	 (#t #\x0903 #t #\x0600 #t)
	 (#t #\x0903 #f #\x0308 #t #\x0600 #t)
	 (#t #\x0903 #f #\x0903 #t)
	 (#t #\x0903 #f #\x0308 #f #\x0903 #t)
	 (#t #\x0903 #t #\x1100 #t)
	 (#t #\x0903 #f #\x0308 #t #\x1100 #t)
	 (#t #\x0903 #t #\x1160 #t)
	 (#t #\x0903 #f #\x0308 #t #\x1160 #t)
	 (#t #\x0903 #t #\x11A8 #t)
	 (#t #\x0903 #f #\x0308 #t #\x11A8 #t)
	 (#t #\x0903 #t #\xAC00 #t)
	 (#t #\x0903 #f #\x0308 #t #\xAC00 #t)
	 (#t #\x0903 #t #\xAC01 #t)
	 (#t #\x0903 #f #\x0308 #t #\xAC01 #t)
	 (#t #\x0903 #t #\x1F1E6 #t)
	 (#t #\x0903 #f #\x0308 #t #\x1F1E6 #t)
	 (#t #\x0903 #t #\x261D #t)
	 (#t #\x0903 #f #\x0308 #t #\x261D #t)
	 (#t #\x0903 #t #\x1F3FB #t)
	 (#t #\x0903 #f #\x0308 #t #\x1F3FB #t)
	 (#t #\x0903 #f #\x200D #t)
	 (#t #\x0903 #f #\x0308 #f #\x200D #t)
	 (#t #\x0903 #t #\x2764 #t)
	 (#t #\x0903 #f #\x0308 #t #\x2764 #t)
	 (#t #\x0903 #t #\x1F466 #t)
	 (#t #\x0903 #f #\x0308 #t #\x1F466 #t)
	 (#t #\x0903 #t #\x0378 #t)
	 (#t #\x0903 #f #\x0308 #t #\x0378 #t)
	 (#t #\x0903 #t #\xD800 #t)
	 (#t #\x0903 #f #\x0308 #t #\xD800 #t)
	 (#t #\x1100 #t #\x0020 #t)
	 (#t #\x1100 #f #\x0308 #t #\x0020 #t)
	 (#t #\x1100 #t #\x000D #t)
	 (#t #\x1100 #f #\x0308 #t #\x000D #t)
	 (#t #\x1100 #t #\x000A #t)
	 (#t #\x1100 #f #\x0308 #t #\x000A #t)
	 (#t #\x1100 #t #\x0001 #t)
	 (#t #\x1100 #f #\x0308 #t #\x0001 #t)
	 (#t #\x1100 #f #\x0300 #t)
	 (#t #\x1100 #f #\x0308 #f #\x0300 #t)
	 (#t #\x1100 #t #\x0600 #t)
	 (#t #\x1100 #f #\x0308 #t #\x0600 #t)
	 (#t #\x1100 #f #\x0903 #t)
	 (#t #\x1100 #f #\x0308 #f #\x0903 #t)
	 (#t #\x1100 #f #\x1100 #t)
	 (#t #\x1100 #f #\x0308 #t #\x1100 #t)
	 (#t #\x1100 #f #\x1160 #t)
	 (#t #\x1100 #f #\x0308 #t #\x1160 #t)
	 (#t #\x1100 #t #\x11A8 #t)
	 (#t #\x1100 #f #\x0308 #t #\x11A8 #t)
	 (#t #\x1100 #f #\xAC00 #t)
	 (#t #\x1100 #f #\x0308 #t #\xAC00 #t)
	 (#t #\x1100 #f #\xAC01 #t)
	 (#t #\x1100 #f #\x0308 #t #\xAC01 #t)
	 (#t #\x1100 #t #\x1F1E6 #t)
	 (#t #\x1100 #f #\x0308 #t #\x1F1E6 #t)
	 (#t #\x1100 #t #\x261D #t)
	 (#t #\x1100 #f #\x0308 #t #\x261D #t)
	 (#t #\x1100 #t #\x1F3FB #t)
	 (#t #\x1100 #f #\x0308 #t #\x1F3FB #t)
	 (#t #\x1100 #f #\x200D #t)
	 (#t #\x1100 #f #\x0308 #f #\x200D #t)
	 (#t #\x1100 #t #\x2764 #t)
	 (#t #\x1100 #f #\x0308 #t #\x2764 #t)
	 (#t #\x1100 #t #\x1F466 #t)
	 (#t #\x1100 #f #\x0308 #t #\x1F466 #t)
	 (#t #\x1100 #t #\x0378 #t)
	 (#t #\x1100 #f #\x0308 #t #\x0378 #t)
	 (#t #\x1100 #t #\xD800 #t)
	 (#t #\x1100 #f #\x0308 #t #\xD800 #t)
	 (#t #\x1160 #t #\x0020 #t)
	 (#t #\x1160 #f #\x0308 #t #\x0020 #t)
	 (#t #\x1160 #t #\x000D #t)
	 (#t #\x1160 #f #\x0308 #t #\x000D #t)
	 (#t #\x1160 #t #\x000A #t)
	 (#t #\x1160 #f #\x0308 #t #\x000A #t)
	 (#t #\x1160 #t #\x0001 #t)
	 (#t #\x1160 #f #\x0308 #t #\x0001 #t)
	 (#t #\x1160 #f #\x0300 #t)
	 (#t #\x1160 #f #\x0308 #f #\x0300 #t)
	 (#t #\x1160 #t #\x0600 #t)
	 (#t #\x1160 #f #\x0308 #t #\x0600 #t)
	 (#t #\x1160 #f #\x0903 #t)
	 (#t #\x1160 #f #\x0308 #f #\x0903 #t)
	 (#t #\x1160 #t #\x1100 #t)
	 (#t #\x1160 #f #\x0308 #t #\x1100 #t)
	 (#t #\x1160 #f #\x1160 #t)
	 (#t #\x1160 #f #\x0308 #t #\x1160 #t)
	 (#t #\x1160 #f #\x11A8 #t)
	 (#t #\x1160 #f #\x0308 #t #\x11A8 #t)
	 (#t #\x1160 #t #\xAC00 #t)
	 (#t #\x1160 #f #\x0308 #t #\xAC00 #t)
	 (#t #\x1160 #t #\xAC01 #t)
	 (#t #\x1160 #f #\x0308 #t #\xAC01 #t)
	 (#t #\x1160 #t #\x1F1E6 #t)
	 (#t #\x1160 #f #\x0308 #t #\x1F1E6 #t)
	 (#t #\x1160 #t #\x261D #t)
	 (#t #\x1160 #f #\x0308 #t #\x261D #t)
	 (#t #\x1160 #t #\x1F3FB #t)
	 (#t #\x1160 #f #\x0308 #t #\x1F3FB #t)
	 (#t #\x1160 #f #\x200D #t)
	 (#t #\x1160 #f #\x0308 #f #\x200D #t)
	 (#t #\x1160 #t #\x2764 #t)
	 (#t #\x1160 #f #\x0308 #t #\x2764 #t)
	 (#t #\x1160 #t #\x1F466 #t)
	 (#t #\x1160 #f #\x0308 #t #\x1F466 #t)
	 (#t #\x1160 #t #\x0378 #t)
	 (#t #\x1160 #f #\x0308 #t #\x0378 #t)
	 (#t #\x1160 #t #\xD800 #t)
	 (#t #\x1160 #f #\x0308 #t #\xD800 #t)
	 (#t #\x11A8 #t #\x0020 #t)
	 (#t #\x11A8 #f #\x0308 #t #\x0020 #t)
	 (#t #\x11A8 #t #\x000D #t)
	 (#t #\x11A8 #f #\x0308 #t #\x000D #t)
	 (#t #\x11A8 #t #\x000A #t)
	 (#t #\x11A8 #f #\x0308 #t #\x000A #t)
	 (#t #\x11A8 #t #\x0001 #t)
	 (#t #\x11A8 #f #\x0308 #t #\x0001 #t)
	 (#t #\x11A8 #f #\x0300 #t)
	 (#t #\x11A8 #f #\x0308 #f #\x0300 #t)
	 (#t #\x11A8 #t #\x0600 #t)
	 (#t #\x11A8 #f #\x0308 #t #\x0600 #t)
	 (#t #\x11A8 #f #\x0903 #t)
	 (#t #\x11A8 #f #\x0308 #f #\x0903 #t)
	 (#t #\x11A8 #t #\x1100 #t)
	 (#t #\x11A8 #f #\x0308 #t #\x1100 #t)
	 (#t #\x11A8 #t #\x1160 #t)
	 (#t #\x11A8 #f #\x0308 #t #\x1160 #t)
	 (#t #\x11A8 #f #\x11A8 #t)
	 (#t #\x11A8 #f #\x0308 #t #\x11A8 #t)
	 (#t #\x11A8 #t #\xAC00 #t)
	 (#t #\x11A8 #f #\x0308 #t #\xAC00 #t)
	 (#t #\x11A8 #t #\xAC01 #t)
	 (#t #\x11A8 #f #\x0308 #t #\xAC01 #t)
	 (#t #\x11A8 #t #\x1F1E6 #t)
	 (#t #\x11A8 #f #\x0308 #t #\x1F1E6 #t)
	 (#t #\x11A8 #t #\x261D #t)
	 (#t #\x11A8 #f #\x0308 #t #\x261D #t)
	 (#t #\x11A8 #t #\x1F3FB #t)
	 (#t #\x11A8 #f #\x0308 #t #\x1F3FB #t)
	 (#t #\x11A8 #f #\x200D #t)
	 (#t #\x11A8 #f #\x0308 #f #\x200D #t)
	 (#t #\x11A8 #t #\x2764 #t)
	 (#t #\x11A8 #f #\x0308 #t #\x2764 #t)
	 (#t #\x11A8 #t #\x1F466 #t)
	 (#t #\x11A8 #f #\x0308 #t #\x1F466 #t)
	 (#t #\x11A8 #t #\x0378 #t)
	 (#t #\x11A8 #f #\x0308 #t #\x0378 #t)
	 (#t #\x11A8 #t #\xD800 #t)
	 (#t #\x11A8 #f #\x0308 #t #\xD800 #t)
	 (#t #\xAC00 #t #\x0020 #t)
	 (#t #\xAC00 #f #\x0308 #t #\x0020 #t)
	 (#t #\xAC00 #t #\x000D #t)
	 (#t #\xAC00 #f #\x0308 #t #\x000D #t)
	 (#t #\xAC00 #t #\x000A #t)
	 (#t #\xAC00 #f #\x0308 #t #\x000A #t)
	 (#t #\xAC00 #t #\x0001 #t)
	 (#t #\xAC00 #f #\x0308 #t #\x0001 #t)
	 (#t #\xAC00 #f #\x0300 #t)
	 (#t #\xAC00 #f #\x0308 #f #\x0300 #t)
	 (#t #\xAC00 #t #\x0600 #t)
	 (#t #\xAC00 #f #\x0308 #t #\x0600 #t)
	 (#t #\xAC00 #f #\x0903 #t)
	 (#t #\xAC00 #f #\x0308 #f #\x0903 #t)
	 (#t #\xAC00 #t #\x1100 #t)
	 (#t #\xAC00 #f #\x0308 #t #\x1100 #t)
	 (#t #\xAC00 #f #\x1160 #t)
	 (#t #\xAC00 #f #\x0308 #t #\x1160 #t)
	 (#t #\xAC00 #f #\x11A8 #t)
	 (#t #\xAC00 #f #\x0308 #t #\x11A8 #t)
	 (#t #\xAC00 #t #\xAC00 #t)
	 (#t #\xAC00 #f #\x0308 #t #\xAC00 #t)
	 (#t #\xAC00 #t #\xAC01 #t)
	 (#t #\xAC00 #f #\x0308 #t #\xAC01 #t)
	 (#t #\xAC00 #t #\x1F1E6 #t)
	 (#t #\xAC00 #f #\x0308 #t #\x1F1E6 #t)
	 (#t #\xAC00 #t #\x261D #t)
	 (#t #\xAC00 #f #\x0308 #t #\x261D #t)
	 (#t #\xAC00 #t #\x1F3FB #t)
	 (#t #\xAC00 #f #\x0308 #t #\x1F3FB #t)
	 (#t #\xAC00 #f #\x200D #t)
	 (#t #\xAC00 #f #\x0308 #f #\x200D #t)
	 (#t #\xAC00 #t #\x2764 #t)
	 (#t #\xAC00 #f #\x0308 #t #\x2764 #t)
	 (#t #\xAC00 #t #\x1F466 #t)
	 (#t #\xAC00 #f #\x0308 #t #\x1F466 #t)
	 (#t #\xAC00 #t #\x0378 #t)
	 (#t #\xAC00 #f #\x0308 #t #\x0378 #t)
	 (#t #\xAC00 #t #\xD800 #t)
	 (#t #\xAC00 #f #\x0308 #t #\xD800 #t)
	 (#t #\xAC01 #t #\x0020 #t)
	 (#t #\xAC01 #f #\x0308 #t #\x0020 #t)
	 (#t #\xAC01 #t #\x000D #t)
	 (#t #\xAC01 #f #\x0308 #t #\x000D #t)
	 (#t #\xAC01 #t #\x000A #t)
	 (#t #\xAC01 #f #\x0308 #t #\x000A #t)
	 (#t #\xAC01 #t #\x0001 #t)
	 (#t #\xAC01 #f #\x0308 #t #\x0001 #t)
	 (#t #\xAC01 #f #\x0300 #t)
	 (#t #\xAC01 #f #\x0308 #f #\x0300 #t)
	 (#t #\xAC01 #t #\x0600 #t)
	 (#t #\xAC01 #f #\x0308 #t #\x0600 #t)
	 (#t #\xAC01 #f #\x0903 #t)
	 (#t #\xAC01 #f #\x0308 #f #\x0903 #t)
	 (#t #\xAC01 #t #\x1100 #t)
	 (#t #\xAC01 #f #\x0308 #t #\x1100 #t)
	 (#t #\xAC01 #t #\x1160 #t)
	 (#t #\xAC01 #f #\x0308 #t #\x1160 #t)
	 (#t #\xAC01 #f #\x11A8 #t)
	 (#t #\xAC01 #f #\x0308 #t #\x11A8 #t)
	 (#t #\xAC01 #t #\xAC00 #t)
	 (#t #\xAC01 #f #\x0308 #t #\xAC00 #t)
	 (#t #\xAC01 #t #\xAC01 #t)
	 (#t #\xAC01 #f #\x0308 #t #\xAC01 #t)
	 (#t #\xAC01 #t #\x1F1E6 #t)
	 (#t #\xAC01 #f #\x0308 #t #\x1F1E6 #t)
	 (#t #\xAC01 #t #\x261D #t)
	 (#t #\xAC01 #f #\x0308 #t #\x261D #t)
	 (#t #\xAC01 #t #\x1F3FB #t)
	 (#t #\xAC01 #f #\x0308 #t #\x1F3FB #t)
	 (#t #\xAC01 #f #\x200D #t)
	 (#t #\xAC01 #f #\x0308 #f #\x200D #t)
	 (#t #\xAC01 #t #\x2764 #t)
	 (#t #\xAC01 #f #\x0308 #t #\x2764 #t)
	 (#t #\xAC01 #t #\x1F466 #t)
	 (#t #\xAC01 #f #\x0308 #t #\x1F466 #t)
	 (#t #\xAC01 #t #\x0378 #t)
	 (#t #\xAC01 #f #\x0308 #t #\x0378 #t)
	 (#t #\xAC01 #t #\xD800 #t)
	 (#t #\xAC01 #f #\x0308 #t #\xD800 #t)
	 (#t #\x1F1E6 #t #\x0020 #t)
	 (#t #\x1F1E6 #f #\x0308 #t #\x0020 #t)
	 (#t #\x1F1E6 #t #\x000D #t)
	 (#t #\x1F1E6 #f #\x0308 #t #\x000D #t)
	 (#t #\x1F1E6 #t #\x000A #t)
	 (#t #\x1F1E6 #f #\x0308 #t #\x000A #t)
	 (#t #\x1F1E6 #t #\x0001 #t)
	 (#t #\x1F1E6 #f #\x0308 #t #\x0001 #t)
	 (#t #\x1F1E6 #f #\x0300 #t)
	 (#t #\x1F1E6 #f #\x0308 #f #\x0300 #t)
	 (#t #\x1F1E6 #t #\x0600 #t)
	 (#t #\x1F1E6 #f #\x0308 #t #\x0600 #t)
	 (#t #\x1F1E6 #f #\x0903 #t)
	 (#t #\x1F1E6 #f #\x0308 #f #\x0903 #t)
	 (#t #\x1F1E6 #t #\x1100 #t)
	 (#t #\x1F1E6 #f #\x0308 #t #\x1100 #t)
	 (#t #\x1F1E6 #t #\x1160 #t)
	 (#t #\x1F1E6 #f #\x0308 #t #\x1160 #t)
	 (#t #\x1F1E6 #t #\x11A8 #t)
	 (#t #\x1F1E6 #f #\x0308 #t #\x11A8 #t)
	 (#t #\x1F1E6 #t #\xAC00 #t)
	 (#t #\x1F1E6 #f #\x0308 #t #\xAC00 #t)
	 (#t #\x1F1E6 #t #\xAC01 #t)
	 (#t #\x1F1E6 #f #\x0308 #t #\xAC01 #t)
	 (#t #\x1F1E6 #f #\x1F1E6 #t)
	 (#t #\x1F1E6 #f #\x0308 #t #\x1F1E6 #t)
	 (#t #\x1F1E6 #t #\x261D #t)
	 (#t #\x1F1E6 #f #\x0308 #t #\x261D #t)
	 (#t #\x1F1E6 #t #\x1F3FB #t)
	 (#t #\x1F1E6 #f #\x0308 #t #\x1F3FB #t)
	 (#t #\x1F1E6 #f #\x200D #t)
	 (#t #\x1F1E6 #f #\x0308 #f #\x200D #t)
	 (#t #\x1F1E6 #t #\x2764 #t)
	 (#t #\x1F1E6 #f #\x0308 #t #\x2764 #t)
	 (#t #\x1F1E6 #t #\x1F466 #t)
	 (#t #\x1F1E6 #f #\x0308 #t #\x1F466 #t)
	 (#t #\x1F1E6 #t #\x0378 #t)
	 (#t #\x1F1E6 #f #\x0308 #t #\x0378 #t)
	 (#t #\x1F1E6 #t #\xD800 #t)
	 (#t #\x1F1E6 #f #\x0308 #t #\xD800 #t)
	 (#t #\x261D #t #\x0020 #t)
	 (#t #\x261D #f #\x0308 #t #\x0020 #t)
	 (#t #\x261D #t #\x000D #t)
	 (#t #\x261D #f #\x0308 #t #\x000D #t)
	 (#t #\x261D #t #\x000A #t)
	 (#t #\x261D #f #\x0308 #t #\x000A #t)
	 (#t #\x261D #t #\x0001 #t)
	 (#t #\x261D #f #\x0308 #t #\x0001 #t)
	 (#t #\x261D #f #\x0300 #t)
	 (#t #\x261D #f #\x0308 #f #\x0300 #t)
	 (#t #\x261D #t #\x0600 #t)
	 (#t #\x261D #f #\x0308 #t #\x0600 #t)
	 (#t #\x261D #f #\x0903 #t)
	 (#t #\x261D #f #\x0308 #f #\x0903 #t)
	 (#t #\x261D #t #\x1100 #t)
	 (#t #\x261D #f #\x0308 #t #\x1100 #t)
	 (#t #\x261D #t #\x1160 #t)
	 (#t #\x261D #f #\x0308 #t #\x1160 #t)
	 (#t #\x261D #t #\x11A8 #t)
	 (#t #\x261D #f #\x0308 #t #\x11A8 #t)
	 (#t #\x261D #t #\xAC00 #t)
	 (#t #\x261D #f #\x0308 #t #\xAC00 #t)
	 (#t #\x261D #t #\xAC01 #t)
	 (#t #\x261D #f #\x0308 #t #\xAC01 #t)
	 (#t #\x261D #t #\x1F1E6 #t)
	 (#t #\x261D #f #\x0308 #t #\x1F1E6 #t)
	 (#t #\x261D #t #\x261D #t)
	 (#t #\x261D #f #\x0308 #t #\x261D #t)
	 (#t #\x261D #f #\x1F3FB #t)
	 (#t #\x261D #f #\x0308 #f #\x1F3FB #t)
	 (#t #\x261D #f #\x200D #t)
	 (#t #\x261D #f #\x0308 #f #\x200D #t)
	 (#t #\x261D #t #\x2764 #t)
	 (#t #\x261D #f #\x0308 #t #\x2764 #t)
	 (#t #\x261D #t #\x1F466 #t)
	 (#t #\x261D #f #\x0308 #t #\x1F466 #t)
	 (#t #\x261D #t #\x0378 #t)
	 (#t #\x261D #f #\x0308 #t #\x0378 #t)
	 (#t #\x261D #t #\xD800 #t)
	 (#t #\x261D #f #\x0308 #t #\xD800 #t)
	 (#t #\x1F3FB #t #\x0020 #t)
	 (#t #\x1F3FB #f #\x0308 #t #\x0020 #t)
	 (#t #\x1F3FB #t #\x000D #t)
	 (#t #\x1F3FB #f #\x0308 #t #\x000D #t)
	 (#t #\x1F3FB #t #\x000A #t)
	 (#t #\x1F3FB #f #\x0308 #t #\x000A #t)
	 (#t #\x1F3FB #t #\x0001 #t)
	 (#t #\x1F3FB #f #\x0308 #t #\x0001 #t)
	 (#t #\x1F3FB #f #\x0300 #t)
	 (#t #\x1F3FB #f #\x0308 #f #\x0300 #t)
	 (#t #\x1F3FB #t #\x0600 #t)
	 (#t #\x1F3FB #f #\x0308 #t #\x0600 #t)
	 (#t #\x1F3FB #f #\x0903 #t)
	 (#t #\x1F3FB #f #\x0308 #f #\x0903 #t)
	 (#t #\x1F3FB #t #\x1100 #t)
	 (#t #\x1F3FB #f #\x0308 #t #\x1100 #t)
	 (#t #\x1F3FB #t #\x1160 #t)
	 (#t #\x1F3FB #f #\x0308 #t #\x1160 #t)
	 (#t #\x1F3FB #t #\x11A8 #t)
	 (#t #\x1F3FB #f #\x0308 #t #\x11A8 #t)
	 (#t #\x1F3FB #t #\xAC00 #t)
	 (#t #\x1F3FB #f #\x0308 #t #\xAC00 #t)
	 (#t #\x1F3FB #t #\xAC01 #t)
	 (#t #\x1F3FB #f #\x0308 #t #\xAC01 #t)
	 (#t #\x1F3FB #t #\x1F1E6 #t)
	 (#t #\x1F3FB #f #\x0308 #t #\x1F1E6 #t)
	 (#t #\x1F3FB #t #\x261D #t)
	 (#t #\x1F3FB #f #\x0308 #t #\x261D #t)
	 (#t #\x1F3FB #t #\x1F3FB #t)
	 (#t #\x1F3FB #f #\x0308 #t #\x1F3FB #t)
	 (#t #\x1F3FB #f #\x200D #t)
	 (#t #\x1F3FB #f #\x0308 #f #\x200D #t)
	 (#t #\x1F3FB #t #\x2764 #t)
	 (#t #\x1F3FB #f #\x0308 #t #\x2764 #t)
	 (#t #\x1F3FB #t #\x1F466 #t)
	 (#t #\x1F3FB #f #\x0308 #t #\x1F466 #t)
	 (#t #\x1F3FB #t #\x0378 #t)
	 (#t #\x1F3FB #f #\x0308 #t #\x0378 #t)
	 (#t #\x1F3FB #t #\xD800 #t)
	 (#t #\x1F3FB #f #\x0308 #t #\xD800 #t)
	 (#t #\x200D #t #\x0020 #t)
	 (#t #\x200D #f #\x0308 #t #\x0020 #t)
	 (#t #\x200D #t #\x000D #t)
	 (#t #\x200D #f #\x0308 #t #\x000D #t)
	 (#t #\x200D #t #\x000A #t)
	 (#t #\x200D #f #\x0308 #t #\x000A #t)
	 (#t #\x200D #t #\x0001 #t)
	 (#t #\x200D #f #\x0308 #t #\x0001 #t)
	 (#t #\x200D #f #\x0300 #t)
	 (#t #\x200D #f #\x0308 #f #\x0300 #t)
	 (#t #\x200D #t #\x0600 #t)
	 (#t #\x200D #f #\x0308 #t #\x0600 #t)
	 (#t #\x200D #f #\x0903 #t)
	 (#t #\x200D #f #\x0308 #f #\x0903 #t)
	 (#t #\x200D #t #\x1100 #t)
	 (#t #\x200D #f #\x0308 #t #\x1100 #t)
	 (#t #\x200D #t #\x1160 #t)
	 (#t #\x200D #f #\x0308 #t #\x1160 #t)
	 (#t #\x200D #t #\x11A8 #t)
	 (#t #\x200D #f #\x0308 #t #\x11A8 #t)
	 (#t #\x200D #t #\xAC00 #t)
	 (#t #\x200D #f #\x0308 #t #\xAC00 #t)
	 (#t #\x200D #t #\xAC01 #t)
	 (#t #\x200D #f #\x0308 #t #\xAC01 #t)
	 (#t #\x200D #t #\x1F1E6 #t)
	 (#t #\x200D #f #\x0308 #t #\x1F1E6 #t)
	 (#t #\x200D #t #\x261D #t)
	 (#t #\x200D #f #\x0308 #t #\x261D #t)
	 (#t #\x200D #t #\x1F3FB #t)
	 (#t #\x200D #f #\x0308 #t #\x1F3FB #t)
	 (#t #\x200D #f #\x200D #t)
	 (#t #\x200D #f #\x0308 #f #\x200D #t)
	 (#t #\x200D #f #\x2764 #t)
	 (#t #\x200D #f #\x0308 #t #\x2764 #t)
	 (#t #\x200D #f #\x1F466 #t)
	 (#t #\x200D #f #\x0308 #t #\x1F466 #t)
	 (#t #\x200D #t #\x0378 #t)
	 (#t #\x200D #f #\x0308 #t #\x0378 #t)
	 (#t #\x200D #t #\xD800 #t)
	 (#t #\x200D #f #\x0308 #t #\xD800 #t)
	 (#t #\x2764 #t #\x0020 #t)
	 (#t #\x2764 #f #\x0308 #t #\x0020 #t)
	 (#t #\x2764 #t #\x000D #t)
	 (#t #\x2764 #f #\x0308 #t #\x000D #t)
	 (#t #\x2764 #t #\x000A #t)
	 (#t #\x2764 #f #\x0308 #t #\x000A #t)
	 (#t #\x2764 #t #\x0001 #t)
	 (#t #\x2764 #f #\x0308 #t #\x0001 #t)
	 (#t #\x2764 #f #\x0300 #t)
	 (#t #\x2764 #f #\x0308 #f #\x0300 #t)
	 (#t #\x2764 #t #\x0600 #t)
	 (#t #\x2764 #f #\x0308 #t #\x0600 #t)
	 (#t #\x2764 #f #\x0903 #t)
	 (#t #\x2764 #f #\x0308 #f #\x0903 #t)
	 (#t #\x2764 #t #\x1100 #t)
	 (#t #\x2764 #f #\x0308 #t #\x1100 #t)
	 (#t #\x2764 #t #\x1160 #t)
	 (#t #\x2764 #f #\x0308 #t #\x1160 #t)
	 (#t #\x2764 #t #\x11A8 #t)
	 (#t #\x2764 #f #\x0308 #t #\x11A8 #t)
	 (#t #\x2764 #t #\xAC00 #t)
	 (#t #\x2764 #f #\x0308 #t #\xAC00 #t)
	 (#t #\x2764 #t #\xAC01 #t)
	 (#t #\x2764 #f #\x0308 #t #\xAC01 #t)
	 (#t #\x2764 #t #\x1F1E6 #t)
	 (#t #\x2764 #f #\x0308 #t #\x1F1E6 #t)
	 (#t #\x2764 #t #\x261D #t)
	 (#t #\x2764 #f #\x0308 #t #\x261D #t)
	 (#t #\x2764 #t #\x1F3FB #t)
	 (#t #\x2764 #f #\x0308 #t #\x1F3FB #t)
	 (#t #\x2764 #f #\x200D #t)
	 (#t #\x2764 #f #\x0308 #f #\x200D #t)
	 (#t #\x2764 #t #\x2764 #t)
	 (#t #\x2764 #f #\x0308 #t #\x2764 #t)
	 (#t #\x2764 #t #\x1F466 #t)
	 (#t #\x2764 #f #\x0308 #t #\x1F466 #t)
	 (#t #\x2764 #t #\x0378 #t)
	 (#t #\x2764 #f #\x0308 #t #\x0378 #t)
	 (#t #\x2764 #t #\xD800 #t)
	 (#t #\x2764 #f #\x0308 #t #\xD800 #t)
	 (#t #\x1F466 #t #\x0020 #t)
	 (#t #\x1F466 #f #\x0308 #t #\x0020 #t)
	 (#t #\x1F466 #t #\x000D #t)
	 (#t #\x1F466 #f #\x0308 #t #\x000D #t)
	 (#t #\x1F466 #t #\x000A #t)
	 (#t #\x1F466 #f #\x0308 #t #\x000A #t)
	 (#t #\x1F466 #t #\x0001 #t)
	 (#t #\x1F466 #f #\x0308 #t #\x0001 #t)
	 (#t #\x1F466 #f #\x0300 #t)
	 (#t #\x1F466 #f #\x0308 #f #\x0300 #t)
	 (#t #\x1F466 #t #\x0600 #t)
	 (#t #\x1F466 #f #\x0308 #t #\x0600 #t)
	 (#t #\x1F466 #f #\x0903 #t)
	 (#t #\x1F466 #f #\x0308 #f #\x0903 #t)
	 (#t #\x1F466 #t #\x1100 #t)
	 (#t #\x1F466 #f #\x0308 #t #\x1100 #t)
	 (#t #\x1F466 #t #\x1160 #t)
	 (#t #\x1F466 #f #\x0308 #t #\x1160 #t)
	 (#t #\x1F466 #t #\x11A8 #t)
	 (#t #\x1F466 #f #\x0308 #t #\x11A8 #t)
	 (#t #\x1F466 #t #\xAC00 #t)
	 (#t #\x1F466 #f #\x0308 #t #\xAC00 #t)
	 (#t #\x1F466 #t #\xAC01 #t)
	 (#t #\x1F466 #f #\x0308 #t #\xAC01 #t)
	 (#t #\x1F466 #t #\x1F1E6 #t)
	 (#t #\x1F466 #f #\x0308 #t #\x1F1E6 #t)
	 (#t #\x1F466 #t #\x261D #t)
	 (#t #\x1F466 #f #\x0308 #t #\x261D #t)
	 (#t #\x1F466 #f #\x1F3FB #t)
	 (#t #\x1F466 #f #\x0308 #f #\x1F3FB #t)
	 (#t #\x1F466 #f #\x200D #t)
	 (#t #\x1F466 #f #\x0308 #f #\x200D #t)
	 (#t #\x1F466 #t #\x2764 #t)
	 (#t #\x1F466 #f #\x0308 #t #\x2764 #t)
	 (#t #\x1F466 #t #\x1F466 #t)
	 (#t #\x1F466 #f #\x0308 #t #\x1F466 #t)
	 (#t #\x1F466 #t #\x0378 #t)
	 (#t #\x1F466 #f #\x0308 #t #\x0378 #t)
	 (#t #\x1F466 #t #\xD800 #t)
	 (#t #\x1F466 #f #\x0308 #t #\xD800 #t)
	 (#t #\x0378 #t #\x0020 #t)
	 (#t #\x0378 #f #\x0308 #t #\x0020 #t)
	 (#t #\x0378 #t #\x000D #t)
	 (#t #\x0378 #f #\x0308 #t #\x000D #t)
	 (#t #\x0378 #t #\x000A #t)
	 (#t #\x0378 #f #\x0308 #t #\x000A #t)
	 (#t #\x0378 #t #\x0001 #t)
	 (#t #\x0378 #f #\x0308 #t #\x0001 #t)
	 (#t #\x0378 #f #\x0300 #t)
	 (#t #\x0378 #f #\x0308 #f #\x0300 #t)
	 (#t #\x0378 #t #\x0600 #t)
	 (#t #\x0378 #f #\x0308 #t #\x0600 #t)
	 (#t #\x0378 #f #\x0903 #t)
	 (#t #\x0378 #f #\x0308 #f #\x0903 #t)
	 (#t #\x0378 #t #\x1100 #t)
	 (#t #\x0378 #f #\x0308 #t #\x1100 #t)
	 (#t #\x0378 #t #\x1160 #t)
	 (#t #\x0378 #f #\x0308 #t #\x1160 #t)
	 (#t #\x0378 #t #\x11A8 #t)
	 (#t #\x0378 #f #\x0308 #t #\x11A8 #t)
	 (#t #\x0378 #t #\xAC00 #t)
	 (#t #\x0378 #f #\x0308 #t #\xAC00 #t)
	 (#t #\x0378 #t #\xAC01 #t)
	 (#t #\x0378 #f #\x0308 #t #\xAC01 #t)
	 (#t #\x0378 #t #\x1F1E6 #t)
	 (#t #\x0378 #f #\x0308 #t #\x1F1E6 #t)
	 (#t #\x0378 #t #\x261D #t)
	 (#t #\x0378 #f #\x0308 #t #\x261D #t)
	 (#t #\x0378 #t #\x1F3FB #t)
	 (#t #\x0378 #f #\x0308 #t #\x1F3FB #t)
	 (#t #\x0378 #f #\x200D #t)
	 (#t #\x0378 #f #\x0308 #f #\x200D #t)
	 (#t #\x0378 #t #\x2764 #t)
	 (#t #\x0378 #f #\x0308 #t #\x2764 #t)
	 (#t #\x0378 #t #\x1F466 #t)
	 (#t #\x0378 #f #\x0308 #t #\x1F466 #t)
	 (#t #\x0378 #t #\x0378 #t)
	 (#t #\x0378 #f #\x0308 #t #\x0378 #t)
	 (#t #\x0378 #t #\xD800 #t)
	 (#t #\x0378 #f #\x0308 #t #\xD800 #t)
	 (#t #\xD800 #t #\x0020 #t)
	 (#t #\xD800 #t #\x0308 #t #\x0020 #t)
	 (#t #\xD800 #t #\x000D #t)
	 (#t #\xD800 #t #\x0308 #t #\x000D #t)
	 (#t #\xD800 #t #\x000A #t)
	 (#t #\xD800 #t #\x0308 #t #\x000A #t)
	 (#t #\xD800 #t #\x0001 #t)
	 (#t #\xD800 #t #\x0308 #t #\x0001 #t)
	 (#t #\xD800 #t #\x0300 #t)
	 (#t #\xD800 #t #\x0308 #f #\x0300 #t)
	 (#t #\xD800 #t #\x0600 #t)
	 (#t #\xD800 #t #\x0308 #t #\x0600 #t)
	 (#t #\xD800 #t #\x0903 #t)
	 (#t #\xD800 #t #\x0308 #f #\x0903 #t)
	 (#t #\xD800 #t #\x1100 #t)
	 (#t #\xD800 #t #\x0308 #t #\x1100 #t)
	 (#t #\xD800 #t #\x1160 #t)
	 (#t #\xD800 #t #\x0308 #t #\x1160 #t)
	 (#t #\xD800 #t #\x11A8 #t)
	 (#t #\xD800 #t #\x0308 #t #\x11A8 #t)
	 (#t #\xD800 #t #\xAC00 #t)
	 (#t #\xD800 #t #\x0308 #t #\xAC00 #t)
	 (#t #\xD800 #t #\xAC01 #t)
	 (#t #\xD800 #t #\x0308 #t #\xAC01 #t)
	 (#t #\xD800 #t #\x1F1E6 #t)
	 (#t #\xD800 #t #\x0308 #t #\x1F1E6 #t)
	 (#t #\xD800 #t #\x261D #t)
	 (#t #\xD800 #t #\x0308 #t #\x261D #t)
	 (#t #\xD800 #t #\x1F3FB #t)
	 (#t #\xD800 #t #\x0308 #t #\x1F3FB #t)
	 (#t #\xD800 #t #\x200D #t)
	 (#t #\xD800 #t #\x0308 #f #\x200D #t)
	 (#t #\xD800 #t #\x2764 #t)
	 (#t #\xD800 #t #\x0308 #t #\x2764 #t)
	 (#t #\xD800 #t #\x1F466 #t)
	 (#t #\xD800 #t #\x0308 #t #\x1F466 #t)
	 (#t #\xD800 #t #\x0378 #t)
	 (#t #\xD800 #t #\x0308 #t #\x0378 #t)
	 (#t #\xD800 #t #\xD800 #t)
	 (#t #\xD800 #t #\x0308 #t #\xD800 #t)
	 (#t #\x000D #f #\x000A #t #\x0061 #t #\x000A #t #\x0308 #t)
	 (#t #\x0061 #f #\x0308 #t)
	 (#t #\x0020 #f #\x200D #t #\x0646 #t)
	 (#t #\x0646 #f #\x200D #t #\x0020 #t)
	 (#t #\x1100 #f #\x1100 #t)
	 (#t #\xAC00 #f #\x11A8 #t #\x1100 #t)
	 (#t #\xAC01 #f #\x11A8 #t #\x1100 #t)
	 (#t #\x1F1E6 #f #\x1F1E7 #t #\x1F1E8 #t #\x0062 #t)
	 (#t #\x0061 #t #\x1F1E6 #f #\x1F1E7 #t #\x1F1E8 #t #\x0062 #t)
	 (#t #\x0061 #t #\x1F1E6 #f #\x1F1E7 #f #\x200D #t #\x1F1E8 #t #\x0062 #t)
	 (#t #\x0061 #t #\x1F1E6 #f #\x200D #t #\x1F1E7 #f #\x1F1E8 #t #\x0062 #t)
	 (#t #\x0061 #t #\x1F1E6 #f #\x1F1E7 #t #\x1F1E8 #f #\x1F1E9 #t #\x0062 #t)
	 (#t #\x0061 #f #\x200D #t)
	 (#t #\x0061 #f #\x0308 #t #\x0062 #t)
	 (#t #\x0061 #f #\x0903 #t #\x0062 #t)
	 (#t #\x0061 #t #\x0600 #f #\x0062 #t)
	 (#t #\x261D #f #\x1F3FB #t #\x261D #t)
	 (#t #\x1F466 #f #\x1F3FB #t)
	 (#t #\x200D #f #\x1F466 #f #\x1F3FB #t)
	 (#t #\x200D #f #\x2764 #t)
	 (#t #\x200D #f #\x1F466 #t)
	 (#t #\x1F466 #t #\x1F466 #t))))
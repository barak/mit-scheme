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

;;;; Runtime-system adapters
;;; package: (edwin adapters)

;;; This file contains adapters that bridge between Edwin and the runtime
;;; system.  These are necessary because Edwin has assumptions about the
;;; behavior of the runtime system that are no longer valid.  Since some of
;;; these assumptions are deeply rooted, it's easier to adapt around them rather
;;; than rewrite Edwin.

;;; The most critical assumption is that Edwin relies on an older model of
;;; strings, in which each character is a byte, and the default coding is ISO
;;; 8859-1, while the runtime system now supports full Unicode and uses a
;;; default coding of UTF 8.  Most of that is taken care of by the (edwin
;;; string) package, which contains a copy of the runtime's old string
;;; implementation.  Other things, like file I/O, are handled here.

(declare (usual-integrations))

(define (call-with-file-adapter procedure)
  (lambda (pathname receiver)
    (procedure pathname
               (lambda (port)
                 (port/set-coding port 'iso-8859-1)
                 (receiver port)))))


(define edwin:call-with-append-file
  (call-with-file-adapter call-with-append-file))

(define edwin:call-with-input-file
  (call-with-file-adapter call-with-input-file))

(define edwin:call-with-output-file
  (call-with-file-adapter call-with-output-file))
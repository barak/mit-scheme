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

;;;; X Terminal
;;; Package: (edwin screen x-screen)

(declare (usual-integrations))

;;;; An X display type that autoloads the x11-screen plugin.

(define x-display-type
  (make-display-type
   'X #t
   (named-lambda (x11-screen-available?)
     (and (let ((display (get-environment-variable "DISPLAY")))
	    (and (string? display)
		 (not (string-null? display))))
	  (or (let ((dirpath (system-library-directory-pathname "x11-screen/")))
		(and dirpath
		     (file-directory? dirpath)))
	      ;; The subsystem is on the library path (not in a subdirectory on
	      ;; the path) when testing.
	      (let ((filepath (system-library-pathname "x11-screen.bin" #f)))
		(and filepath
		     (file-loadable? filepath))))))
   (named-lambda (make-x11-screen #!optional geometry)
     (load-option-quietly 'x11-screen)
     (make-xterm-screen geometry))
   (named-lambda (get-x11-screen-input-operations screen)
     screen
     (get-xterm-input-operations))
   (named-lambda (with-x11-display-grabbed receiver)
     (with-editor-interrupts-from-x receiver))
   (named-lambda (with-x11-interrupts-enabled thunk)
     (with-x-interrupts-enabled thunk))
   (named-lambda (with-x11-interrupts-disabled thunk)
     (with-x-interrupts-disabled thunk))))

(define (make-xterm-screen #!optional geometry)
  geometry
  (error "Not yet autoloaded."))

(define (get-xterm-input-operations)
  (error "Not yet autoloaded."))

(define (with-editor-interrupts-from-x receiver)
  receiver
  (error "Not yet autoloaded."))

(define (with-x-interrupts-enabled thunk)
  thunk
  (error "Not yet autoloaded."))

(define (with-x-interrupts-disabled thunk)
  thunk
  (error "Not yet autoloaded."))
#| -*-Scheme-*-

$Id: tscript.scm,v 1.2 1999/01/02 06:19:10 cph Exp $

Copyright (c) 1990, 1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; Transcript File
;;; package: (runtime transcript)

(declare (usual-integrations))

(define transcript-port)

(define (initialize-package!)
  (set! transcript-port false)
  unspecific)

(define (transcript-on filename)
  (if transcript-port
      (error "transcript already turned on"))
  (set! transcript-port (open-output-file filename))
  unspecific)

(define (transcript-off)
  (if transcript-port
      (let ((port transcript-port))
	(set! transcript-port false)
	(close-output-port port))))
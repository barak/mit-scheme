#| -*-Scheme-*-

$Id: tscript.scm,v 1.9 2004/11/23 19:38:48 cph Exp $

Copyright 1990,1999,2004 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Transcript File
;;; package: (runtime transcript)

(declare (usual-integrations))

(define (transcript-on filename #!optional port)
  (let ((port (if (default-object? port) (nearest-cmdl/port) port)))
    (if (port/transcript port)
	(error "Transcript already turned on."))
    (set-port/transcript! port (open-output-file filename))))

(define (transcript-off #!optional port)
  (let ((port (if (default-object? port) (nearest-cmdl/port) port)))
    (let ((transcript-port (port/transcript port)))
      (if transcript-port
	  (begin
	    (set-port/transcript! port #f)
	    (close-port transcript-port))))))
#| -*-Scheme-*-

$Id: compile.scm,v 1.5 2004/11/01 19:14:15 cph Exp $

Copyright 2003 Massachusetts Institute of Technology

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

;;;; SSP/XDOC compilation

(load-option 'CREF)
(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (for-each compile-file
	      '("expenv"
		"mod-lisp"
		"xhtml-expander"
		"xmlrpc"))
    (cref/generate-constructors "ssp")))
#| -*-Scheme-*-

$Id: compile.scm,v 1.14 2004/07/24 03:45:29 cph Exp $

Copyright 2001,2003,2004 Massachusetts Institute of Technology

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

(load-option 'CREF)
(load-option '*PARSER)
(load-option 'SOS)

(if (not (environment-bound? system-global-environment 'XML-PARSER-MACROS))
    (environment-define system-global-environment
			'XML-PARSER-MACROS
			(make-parser-macros #f)))

(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (with-current-parser-macros xml-parser-macros
      (lambda ()
	(load "parser-macro")
	(for-each compile-file
		  '("xml-names"
		    "xml-struct"
		    "xml-chars"
		    "xml-output"
		    "xml-parser"
		    "xhtml"
		    "xhtml-entities"))))
    (cref/generate-constructors "xml")))
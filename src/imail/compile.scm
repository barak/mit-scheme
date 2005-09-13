#| -*-Scheme-*-

$Id: compile.scm,v 1.21 2005/01/11 03:13:23 cph Exp $

Copyright 2000,2001,2003,2005 Massachusetts Institute of Technology

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

;;;; IMAIL mail reader: compilation

(load-option 'CREF)
(load-option 'SOS)
(load-option '*PARSER)
(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (for-each (lambda (filename)
		(compile-file filename '() (->environment '(EDWIN))))
	      '("imail-browser"
		"imail-core"
		"imail-file"
		"imail-imap"
		"imail-rmail"
		"imail-summary"
		"imail-top"
		"imail-umail"
		"imail-util"
		"imap-response"
		"imap-syntax"))
    (cref/generate-constructors "imail" 'ALL)))
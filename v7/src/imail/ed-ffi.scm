#| -*-Scheme-*-

$Id: ed-ffi.scm,v 1.23 2005/12/10 06:45:32 riastradh Exp $

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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02111-1301,
USA.

|#

;;;; IMAIL mail reader: Edwin buffer packaging info

(standard-scheme-find-file-initialization
 '#(("imail-browser"	(edwin imail front-end folder-browser))
    ("imail-core"	(edwin imail))
    ("imail-file"	(edwin imail file-folder))
    ("imail-imap"	(edwin imail imap-folder))
    ("imail-mime"       (edwin imail mime))
    ("imail-rmail"	(edwin imail file-folder rmail-folder))
    ("imail-summary"	(edwin imail front-end summary))
    ("imail-top"	(edwin imail front-end))
    ("imail-umail"	(edwin imail file-folder umail-folder))
    ("imail-util"	(edwin imail))
    ("imap-response"	(edwin imail imap-response))
    ("imap-syntax"	(edwin imail imap-syntax))))
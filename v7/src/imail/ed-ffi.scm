#| -*-Scheme-*-

$Id: ed-ffi.scm,v 1.21 2003/02/14 18:25:21 cph Exp $

Copyright 2000,2001,2003 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; IMAIL mail reader: Edwin buffer packaging info

(standard-scheme-find-file-initialization
 '#(("imail-browser"	(edwin imail front-end folder-browser))
    ("imail-core"	(edwin imail))
    ("imail-file"	(edwin imail file-folder))
    ("imail-imap"	(edwin imail imap-folder))
    ("imail-rmail"	(edwin imail file-folder rmail-folder))
    ("imail-summary"	(edwin imail front-end summary))
    ("imail-top"	(edwin imail front-end))
    ("imail-umail"	(edwin imail file-folder umail-folder))
    ("imail-util"	(edwin imail))
    ("imap-response"	(edwin imail imap-response))
    ("imap-syntax"	(edwin imail imap-syntax))))
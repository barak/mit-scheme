#| -*- Scheme -*-

$Id: ed-ffi.scm,v 1.3 2001/12/19 20:50:01 cph Exp $

Copyright (c) 1997, 1999, 2001 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;;; Edwin buffer packaging info

(standard-scheme-find-file-initialization
 '#(
    ("class"	(sos class))
    ("instance"	(sos instance))
    ("macros"	(sos macros))
    ("method"	(sos method))
    ("printer"	(sos printer))
    ("slot"	(sos slot))))
#| -*- Scheme -*-

$Id: ed-ffi.scm,v 1.2 1999/01/02 06:19:10 cph Exp $

Copyright (c) 1997, 1999 Massachusetts Institute of Technology

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

;;;; Edwin buffer packaging info

(standard-scheme-find-file-initialization
 '#(
    ("class"	(sos class)	system-global-syntax-table)
    ("instance"	(sos instance)	syntax-table/system-internal)
    ("macros"	(sos macros)	system-global-syntax-table)
    ("method"	(sos method)	system-global-syntax-table)
    ("printer"	(sos printer)	system-global-syntax-table)
    ("slot"	(sos slot)	system-global-syntax-table)))
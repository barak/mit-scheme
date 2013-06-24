#| -*-Scheme-*-

$Id: sbuild.scm,v 1.5 1999/01/02 06:19:10 cph Exp $

Copyright (c) 1987-1999 Massachusetts Institute of Technology

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

;;;; 6.001 Student Environment

(declare (usual-integrations))

(for-each (lambda (filename)
	    (load filename system-global-environment))
	  '("compat" "graphics" "strmac" "stream" "genenv" "studen"))
(add-identification! "Student (6.001)" 14 3)

"Student environment loaded."
#| -*-Scheme-*-

$Id: sbuild.scm,v 1.9 2007/01/05 15:33:10 cph Exp $

Copyright 1990,1991,1998,2004 Massachusetts Institute of Technology

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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; 6.001 Student Environment

(declare (usual-integrations))

(for-each (lambda (filename)
	    (load filename system-global-environment))
	  '("compat" "graphics" "strmac" "stream" "genenv" "studen"))
(add-subsystem-identification! "Student (6.001)" '(14 3))

"Student environment loaded."
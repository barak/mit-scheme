#| -*-Scheme-*-

$Id: mklogs.scm,v 1.24 2003/02/14 18:28:32 cph Exp $

Copyright (c) 1988-2000 Massachusetts Institute of Technology

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

;;;; Update the RCS log files in the standard Scheme directories.

(let ((changelog-map
       '("zurich.ai.mit.edu"
	 ("adams" "Stephen Adams")
	 ("arthur" "Arthur Gleckler")
	 ("bal" "Brian A. LaMacchia")
	 ("boogles" "Brian K. Zuzga")
	 ("cph" "Chris Hanson")
	 ("gjr" "Guillermo J. Rozas")
	 ("gjs" "Gerald Jay Sussman")
	 ("hal" "Hal Abelson")
	 ("jacob" "Jacob Katzenelson")
	 ("jawilson" "Jason Wilson")
	 ("jbank" "Joe Bank")
	 ("jinx" "Guillermo J. Rozas" "gjr@zurich.ai.mit.edu")
	 ("jmiller" "Jim Miller")
	 ("jrm" "Joe Marshall")
	 ("markf" "Mark Friedman")
	 ("mhwu" "Henry M. Wu")
	 ("nick" "Nick Papadakis")
	 ("pas" "Panayotis Skordos")
	 ("thanos" "Thanos Siapas")
	 ("ziggy" "Michael R. Blair"))))
  (for-each (lambda (directory)
	      (rcs-directory-log directory
				 `((CHANGELOG? #t)
				   (CHANGELOG-MAP ,changelog-map))))
	    '("/scheme/v7/src"
	      "/scheme/v7/doc"
	      "/scheme/etc")))
(for-each (lambda (directory)
	    (rcs-directory-log directory '()))
	  '("/scheme/v8/src/bench"
	    "/scheme/v8/src/compiler"
	    "/scheme/v8/src/microcode"
	    "/scheme/v8/src/runtime"
	    "/scheme/v8/src/sf"))
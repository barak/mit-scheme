#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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
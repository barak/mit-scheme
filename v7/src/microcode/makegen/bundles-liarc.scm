#| -*-Scheme-*-

$Id: bundles-liarc.scm,v 1.2 2007/04/09 16:42:20 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

;;;; Bundles for LIARC to be linked into dynamically loadable shared
;;;; objects.

;;; Format: (bundle (package*) source-directoory*)
;;;   bundle ::= string
;;;   package ::= pathname | (pathname os-type*)
;;;   source-directory ::= (pathname exclude-filename*)

;++ This is fragile: excluding filenames is bogus.  But it will work
;++ until we get a real module system.

("sf+compiler"
 ("../sf/sf"
  ("../compiler/compiler" "unx")
  "../star-parser/parser")
 ("../sf")
 ("../compiler")
 ("../compiler/base")
 ("../compiler/back")
 ("../compiler/fggen")
 ("../compiler/fgopt")
 ("../compiler/machines/C")
 ("../compiler/rtlbase")
 ("../compiler/rtlgen")
 ("../compiler/rtlopt")
 ("../star-parser" "compile" "ed-ffi" "load" "test-parser"))

("edwin"
 ("../edwin/edwin")
 ("../edwin" "decls"))

;; These are listed alphabetically; I don't think the order matters.

("6001"
 ("../6001/6001")
 ("../6001"))

("cref"
 ("../cref/cref")
 ("../cref"))

("imail"
 ("../imail/imail")
 ("../imail" "compile" "ed-ffi" "fake-env" "load"))

("sos"
 ("../sos/sos")
 ("../sos" "compile" "ed-ffi" "load" "microbench"))

("ssp"
 ("../ssp/ssp")
 ("../ssp" "compile" "load"))

("xdoc"
 ("../xdoc/xdoc")
 ("../xdoc" "compile" "load" "validate-xdoc"))

("xml"
 ("../xml/xml")
 ("../xml"
  "compile" "ed-ffi" "load" "parser-macro" "test-parser" "test-turtle" "xpath"))

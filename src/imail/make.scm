#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

This file is part of the IMail option for MIT/GNU Scheme.

IMail is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.

IMail is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with IMail; if not, write to the Free Software Foundation, Inc.,
51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.

|#

;;;; IMAIL mail reader: loader

(load-option 'regular-expression)
(load-option 'wt-tree)
(load-option 'sos)
(load-option 'edwin)
(with-loader-base-uri (system-library-uri "imail/")
  (lambda ()
    (fluid-let ((*allow-package-redefinition?* #t))
      (load-package-set "imail"))))
(add-subsystem-identification! "IMAIL" '(1 22))
#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; XDOC loader

(load-option 'ssp)
;; Backwards compatibility:
(let ((from-env (->environment '(runtime xml html)))
      (to-env (->environment '(runtime ssp-expander-environment))))
  (let ((export
	 (lambda (from to)
	   (link-variables to-env to from-env from))))
    (for-each (lambda (name)
		(let ((name (xml-name-local name)))
		  (if (not (memq name '(map style)))
		      (export (symbol 'html: name) name))))
	      (html-element-names))
    (for-each (lambda (name)
		(export (symbol 'html: name) name))
	      '(href id-def id-ref rel-link style-link http-equiv))
    (export 'html:style-attr 'style)
    (export 'xml-attrs 'attributes)
    (export 'xml-comment 'comment)))
(with-loader-base-uri (system-library-uri "xdoc/")
  (lambda ()
    (load-package-set "xdoc")))
(add-subsystem-identification! "XDOC" '(0 3))
#| -*-Scheme-*-

$Id: load.scm,v 1.3 2004/12/13 03:22:21 cph Exp $

Copyright 2004 Massachusetts Institute of Technology

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
(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (load-package-set "xdoc")))
(add-subsystem-identification! "XDOC" '(0 3))
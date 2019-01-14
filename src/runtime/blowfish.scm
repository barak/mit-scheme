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

;;;; Interface to Blowfish
;;; package: (runtime blowfish)

(declare (usual-integrations))

;;; Access to blowfish functions is now accomplished with the FFI
;;; rather than a microcode module.  The bindings in this package are
;;; linked to those in the (blowfish) package after the plugin is
;;; loaded.

(define linked? #f)

(define (blowfish-available?)
  (and (plugin-available? "blowfish")
       (or linked?
	   (begin
	     (load-option 'blowfish)
	     (link!)
	     #t))))

(define (link!)
  (for-each
    (let ((runtime (->environment '(runtime blowfish)))
	  (blowfish (->environment '(blowfish))))
      (lambda (name)
	(environment-link-name runtime blowfish name)))
    names)
  (set! linked? #t))

(define names
  '(blowfish-cbc
    blowfish-cfb64
    blowfish-ecb
    blowfish-encrypt-port
    blowfish-file?
    blowfish-ofb64
    blowfish-set-key
    compute-blowfish-init-vector
    read-blowfish-file-header
    write-blowfish-file-header))

(define blowfish-cbc)
(define blowfish-cfb64)
(define blowfish-ecb)
(define blowfish-encrypt-port)
(define blowfish-file?)
(define blowfish-ofb64)
(define blowfish-set-key)
(define compute-blowfish-init-vector)
(define read-blowfish-file-header)
(define write-blowfish-file-header)
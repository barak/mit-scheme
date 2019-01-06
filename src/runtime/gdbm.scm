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

;;;; gdbm Database Library Interface
;;; package: (runtime gdbm)

(declare (usual-integrations))

;;; Access to the gdbm library is now accomplished with the FFI
;;; rather than a microcode module.  The bindings in this package are
;;; linked to those in the (gdbm) package after the plugin is loaded.

(define linked? #f)

(define (gdbm-available?)
  (and (plugin-available? "gdbm")
       (or linked?
	   (begin
	     (load-option 'gdbm)
	     (link!)
	     #t))))

(define (link!)
  (for-each
    (let ((runtime (->environment '(runtime gdbm)))
	  (gdbm (->environment '(gdbm))))
      (lambda (name)
	(environment-link-name runtime gdbm name)))
    names)
  (set! linked? #t))

(define names
  '(gdbm-close
    gdbm-delete
    gdbm-exists?
    gdbm-fetch
    gdbm-firstkey
    gdbm-nextkey
    gdbm-open
    gdbm-reorganize
    gdbm-setopt
    gdbm-store
    gdbm-sync
    gdbm-version
    gdbm_cachesize
    gdbm_fast
    gdbm_fastmode
    gdbm_insert
    gdbm_newdb
    gdbm_reader
    gdbm_replace
    gdbm_wrcreat
    gdbm_writer))

(define gdbm-close)
(define gdbm-delete)
(define gdbm-exists?)
(define gdbm-fetch)
(define gdbm-firstkey)
(define gdbm-nextkey)
(define gdbm-open)
(define gdbm-reorganize)
(define gdbm-setopt)
(define gdbm-store)
(define gdbm-sync)
(define gdbm-version)
(define gdbm_cachesize)
(define gdbm_fast)
(define gdbm_fastmode)
(define gdbm_insert)
(define gdbm_newdb)
(define gdbm_reader)
(define gdbm_replace)
(define gdbm_wrcreat)
(define gdbm_writer)
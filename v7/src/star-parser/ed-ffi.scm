;;; -*-Scheme-*-
;;;
;;; $Id: ed-ffi.scm,v 1.1 2001/06/26 18:51:22 cph Exp $
;;;
;;; Copyright (c) 2001 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

;;;; Parser language: Edwin buffer packaging info

(standard-scheme-find-file-initialization
 '#(("buffer"	(runtime *parser)
		system-global-syntax-table)
    ("matcher"	(runtime *parser)
		system-global-syntax-table)
    ("parser"	(runtime *parser)
		system-global-syntax-table)
    ("shared"	(runtime *parser)
		system-global-syntax-table)
    ("synchk"	(runtime *parser)
		system-global-syntax-table)))
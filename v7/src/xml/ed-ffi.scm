;;; -*-Scheme-*-
;;;
;;; $Id: ed-ffi.scm,v 1.2 2001/07/12 03:24:32 cph Exp $
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

;;;; XML: Edwin buffer packaging info

(standard-scheme-find-file-initialization
 '#(("xml-chars" (runtime xml parser) system-global-syntax-table)
    ("xml-struct" (runtime xml structure) system-global-syntax-table)
    ("xml-parser" (runtime xml parser) system-global-syntax-table)))
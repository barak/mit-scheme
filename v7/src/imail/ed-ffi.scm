;;; -*-Scheme-*-
;;;
;;; $Id: ed-ffi.scm,v 1.17 2001/11/05 21:20:20 cph Exp $
;;;
;;; Copyright (c) 2000-2001 Massachusetts Institute of Technology
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

;;;; IMAIL mail reader: Edwin buffer packaging info

(standard-scheme-find-file-initialization
 '#(("imail-browser"	(edwin imail front-end folder-browser)
			edwin-syntax-table)
    ("imail-core"	(edwin imail)
			system-global-syntax-table)
    ("imail-file"	(edwin imail file-folder)
			system-global-syntax-table)
    ("imail-imap"	(edwin imail imap-folder)
			system-global-syntax-table)
    ("imail-rmail"	(edwin imail file-folder rmail-folder)
			system-global-syntax-table)
    ("imail-summary"	(edwin imail front-end summary)
			edwin-syntax-table)
    ("imail-top"	(edwin imail front-end)
			edwin-syntax-table)
    ("imail-umail"	(edwin imail file-folder umail-folder)
			system-global-syntax-table)
    ("imail-util"	(edwin imail)
			system-global-syntax-table)
    ("imap-response"	(edwin imail imap-response)
			system-global-syntax-table)
    ("imap-syntax"	(edwin imail imap-syntax)
			system-global-syntax-table)
    ("url"		(runtime url)
			system-global-syntax-table)))
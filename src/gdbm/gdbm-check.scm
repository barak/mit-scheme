#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Massachusetts
    Institute of Technology

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

;;;; Test the GDBM wrapper.

(if (not (gdbm-available?))
    (warn "gdbm wrapper not found")
    (let ((filename.db "gdbm-check.db"))
      (ignore-errors (lambda () (delete-file filename.db)))
      (let ((dbf (gdbm-open filename.db 0 GDBM_WRCREAT #o660)))
	;; Must be set before first store.
	(gdbm-setopt dbf GDBM_CACHESIZE 101)

	(gdbm-store dbf "Silly String" "Testing 1 2 3." GDBM_REPLACE)
        (if (not (condition?
		  (ignore-errors
		   (lambda () (gdbm-store dbf "NullString" "" GDBM_INSERT)))))
	    (error "storing null content did not signal"))
	(if (not (condition?
		  (ignore-errors
		   (lambda () (gdbm-store dbf "" "NullString" GDBM_INSERT)))))
	    (error "storing null key did not signal"))
	(if (not (eq? #t (gdbm-store dbf "Silly String" "Ahoy!" GDBM_REPLACE)))
	    (error "replace produced wrong indication"))
	(if (not (eq? #f (gdbm-store dbf "Silly String" "Oy!" GDBM_INSERT)))
	    (error "double insert produced no indication"))

	(gdbm-setopt dbf GDBM_SYNCMODE 1)

	(let ((content (gdbm-fetch dbf "Silly String")))
	  (if (not (string=? "Ahoy!" content))
	      (error "fetched:" content)))
	(let ((content (gdbm-fetch dbf "Missing String")))
	  (if (not (eq? #f content))
	      (error "missing fetched:" content)))

	(if (gdbm-exists? dbf "Missing String")
	    (error "exists"))
	(if (not (gdbm-exists? dbf "Silly String"))
	    (error "not exists"))

	(gdbm-delete dbf "Silly String")
	(if (gdbm-exists? dbf "Silly String")
	    (error "not deleted"))
	(if (gdbm-delete dbf "Missing String")
	    (error "deleted"))

	(let ((k (gdbm-firstkey dbf)))
	  (if k
	      (error "empty database returned a firstkey:" k)))
	(gdbm-store dbf "AString" "Testing 1 2 3." GDBM_INSERT)
	(gdbm-store dbf "ASecondString" "Testing 1 2 3." GDBM_REPLACE)
	(gdbm-store dbf "AThirdString" "Testing 1 2 3." GDBM_INSERT)
	#;(let ((keys (sort (gdbm-keys dbf) string<?)))
	  (if (not (equal? keys '("ASecondString" "AString" "AThirdString")))
	      (error "keys:" keys)))

	(gdbm-reorganize dbf)
	(gdbm-sync dbf)
	(gdbm-setopt dbf 'SYNCMODE #f)
	(gdbm-version)
	(gdbm-close dbf))

      (if (not (condition?
		(ignore-errors
		 (lambda () (gdbm-open "notfound.db" 0 GDBM_READER 0)))))
	  (error "opened a nonexistent database file:" gdbf))
      (let ((dbf2 (gdbm-open filename.db 0 GDBM_READER 0)))
	  (let ((keys (sort (gdbm-keys dbf2) string<?)))
	    (if (not (equal? keys '("ASecondString" "AString" "AThirdString")))
		(error "bogus keys:" keys))
	    (map (lambda (key)
		   (if (not (string=? "Testing 1 2 3." (gdbm-fetch dbf2 key)))
		       (error "bogus content:" key)))
		 keys))
	  (gdbm-close dbf2))))
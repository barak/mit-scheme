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

;;;; Test the PGSQL option.

(let ((conn (ignore-errors (lambda () (open-pgsql-conn "")))))

  (define (query . strings)
    (let ((query (string-append* strings)))
      (exec-pgsql-query conn query)))

  (define (cmd . strings)
    (let ((result (apply query strings)))
      (let ((status (pgsql-cmd-status result)))
	(pgsql-clear result)
	status)))

  (if (and (not (condition? conn))
	   (pgsql-conn-open? conn))
      (begin
	(ignore-errors
	 (lambda () (cmd "DROP TABLE test_table;")))
	(cmd "CREATE TABLE test_table ( name varchar (10) PRIMARY KEY );")
	(cmd "INSERT INTO test_table (name) VALUES ('apple');")
	(cmd "INSERT INTO test_table (name) VALUES ('banana');")
	(cmd "INSERT INTO test_table (name) VALUES ('cherry');")
	(let* ((result (query "SELECT * FROM test_table;"))
	       (n (pgsql-n-tuples result))
	       (fruits
		(do ((i 0 (+ i 1))
		     (fruits '()
			     (cons (pgsql-get-value result i 0) fruits)))
		    ((= i n)
		     (pgsql-clear result)
		     (reverse! fruits)))))
	  (if (not (equal? fruits '("apple" "banana" "cherry")))
	      (error "wrong fruits")))
	(close-pgsql-conn conn)
	(if (pgsql-conn-open? conn)
	    (error "could not pgsql close:" conn))
	(if (not (condition?
		  (ignore-errors
		   (lambda ()
		     (exec-pgsql-query conn "SELECT * FROM test_table;")))))
	    (error "not signaling an error when closed:" conn))
	(let* ((sample    " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")
	       (escaped (escape-pgsql-string sample))
	       (expected " !\"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"))
	  (if (not (equal? escaped expected))
	      (error "not escaped properly:" escaped))))
      (warn "could not connect to the default Postgres database")))
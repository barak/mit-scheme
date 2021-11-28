#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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

;;;; Tests for library import management

(declare (usual-integrations))

(include "test-library-data/support-code.scm")

(define-test 'expand-parsed-imports:ex1
  (lambda ()
    (let ((library (parse-define-library-form ex1 test-pathname))
	  (db (new-library-db 'test)))
      (register-library! library db)
      (register-libraries! (read-dependencies) db)
      (assert-lset= library-ixport=?
		    (filter (lambda (import)
			      (eq? 'foo
				   (car (library-ixport-from-library import))))
			    (library-imports library))
		    (list (make-library-ixport '(foo mumble) 'foo-mumble?)
			  (make-library-ixport '(foo mumble) 'make-foo-mumble)
			  (make-library-ixport '(foo mumble) 'foo-mumble-a)
			  (make-library-ixport '(foo mumble) 'foo-mumble-b)
			  (make-library-ixport '(foo grumble)
					       'foo-grumble?
					       'grumble-foo-grumble?)
			  (make-library-ixport '(foo grumble)
					       'make-foo-grumble
					       'grumble-make-foo-grumble)
			  (make-library-ixport '(foo grumble)
					       'foo-grumble-a
					       'grumble-foo-grumble-a)
			  (make-library-ixport '(foo grumble)
					       'foo-grumble-b
					       'grumble-foo-grumble-b)
			  (make-library-ixport '(foo quux) 'foo-quux?)
			  (make-library-ixport '(foo quux) 'foo-quux-a)
			  (make-library-ixport '(foo quux) 'foo-quux-b)
			  (make-library-ixport '(foo quux)
					       'make-foo-quux
					       'create-foo-quux)))
      (assert-lset= library-ixport=?
		    (library-exports library)
		    (convert-exports (library-name library) ex1-exports)))))

(define-test 'mit-libraries:srfi-140
  (lambda ()
    (let ((source (read-r7rs-source srfi-140-example-filename))
	  (db (copy-library-db host-library-db)))
      (register-r7rs-source! source db)
      (let ((scheme-base (exports-of '(scheme base) db))
	    (scheme-char (exports-of '(scheme char) db))
	    (srfi-140 (exports-of '(srfi 140) db)))
	(let ((library (registered-library '(test srfi 140 base) db))
	      (expected
	       (ixport-union (drop-collisions scheme-base srfi-140)
			     srfi-140)))
	  (assert-lset= library-ixport=?
			(library-imports library)
			expected)
	  (assert-lset= library-ixport=?
			(library-exports library)
			expected))
	(let ((library (registered-library '(test srfi 140 char) db))
	      (expected
	       (ixport-union (drop-collisions scheme-char srfi-140)
			     (take-collisions srfi-140 scheme-char))))
	  (assert-lset= library-ixport=?
			(library-imports library)
			expected)
	  (assert-lset= library-ixport=?
			(library-exports library)
			expected))
	(let ((library (registered-library '(test srfi 140 istrings) db))
	      (expected
	       (filter (lambda (ixport)
			 (assq (library-ixport-to ixport)
			       mstring-names))
		       srfi-140)))
	  (assert-lset= library-ixport=?
			(library-imports library)
			expected)
	  (assert-lset= library-ixport=?
			(library-exports library)
			expected))
	(let ((library (registered-library '(test srfi 140 mstrings) db)))
	  (assert-lset= library-ixport=?
			(library-imports library)
			(filter (lambda (ixport)
				  (eq? 'define (library-ixport-to ixport)))
				scheme-base))
	  (assert-lset= library-ixport=?
			(library-exports library)
			(map (lambda (p)
			       (make-library-ixport '(test srfi 140 mstrings)
						    (cadr p)
						    (car p)))
			     mstring-names)))))))

(define mstring-names
  '((list->string list->mstring)
    (reverse-list->string reverse-list->mstring)
    (string mstring)
    (string-append mstring-append)
    (string-concatenate mstring-concatenate)
    (string-concatenate-reverse mstring-concatenate-reverse)
    (string-downcase mstring-downcase)
    (string-drop mstring-drop)
    (string-drop-right mstring-drop-right)
    (string-filter mstring-filter)
    (string-foldcase mstring-foldcase)
    (string-join mstring-join)
    (string-map mstring-map)
    (string-map-index mstring-map-index)
    (string-pad mstring-pad)
    (string-pad-right mstring-pad-right)
    (string-remove mstring-remove)
    (string-repeat mstring-repeat)
    (string-replace mstring-replace)
    (string-tabulate mstring-tabulate)
    (string-take mstring-take)
    (string-take-right mstring-take-right)
    (string-titlecase mstring-titlecase)
    (string-trim mstring-trim)
    (string-trim-both mstring-trim-both)
    (string-trim-right mstring-trim-right)
    (string-unfold mstring-unfold)
    (string-unfold-right mstring-unfold-right)
    (string-upcase mstring-upcase)
    (substring msubstring)
    (utf16->string utf16->mstring)
    (utf16be->string utf16be->mstring)
    (utf16le->string utf16le->mstring)
    (utf8->string utf8->mstring)
    (vector->string vector->mstring)
    (xsubstring xmsubstring)))

(define-test 'test-private-exports
  (lambda ()
    (let ((source (read-r7rs-source private-exports-example-filename))
	  (db (copy-library-db host-library-db)))
      (register-r7rs-source! source db)
      (let ((amap-base (registered-library '(test amap) db))
	    (amap-impl (registered-library '(test amap impl) db)))
	(assert-lset= library-ixport=?
		      (library-exports amap-base)
		      (convert-exports '(test amap)
				       expected-amap-base-exports))
	(assert-lset= library-ixport=?
		      (library-exports amap-impl)
		      (convert-exports '(test amap impl)
				       expected-amap-impl-exports))
	(assert-lset= library-ixport=?
		      (library-exports amap-impl amap-base)
		      (convert-exports '(test amap impl)
				       expected-amap-impl-exports-private))))))

(define expected-amap-base-exports
  '(alist->amap
    amap->alist
    amap-args
    amap-clean!
    amap-clear!
    amap-comparator
    amap-contains?
    amap-copy
    amap-count
    amap-delete!
    amap-difference!
    amap-empty-copy
    amap-empty?
    amap-entries
    amap-find
    amap-fold
    amap-for-each
    amap-implementation-name
    amap-intern!
    amap-intersection!
    amap-keys
    amap-map
    amap-map!
    amap-map->list
    amap-mutable?
    amap-pop!
    amap-prune!
    amap-ref
    amap-ref/default
    amap-set!
    amap-size
    amap-unfold
    amap-union!
    amap-update!
    amap-update!/default
    amap-values
    amap-xor!
    amap=?
    amap?
    make-amap))

(define expected-amap-impl-exports
  '(all-amap-args
    amap-implementation-names
    amap-implementation-supported-args
    amap-implementation-supports-args?
    amap-implementation-supports-comparator?
    define-amap-implementation
    define-amap-implementation-selector
    make-amap-implementation))

(define expected-amap-impl-exports-private
  (cons 'select-impl expected-amap-impl-exports))
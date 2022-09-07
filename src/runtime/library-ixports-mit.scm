#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

;;;; MIT libraries: import/export
;;; package: (runtime library import/export mit)

(declare (usual-integrations))

(add-boot-deps! '(runtime comparator))

(define (mit-parsed-import-libraries parsed-import library libraries)
  (fold (lambda (import-set libraries)
	  (if (library-name? import-set)
	      (library-name-adjoin import-set libraries)
	      (fold nameset-libraries
		    (library-name-adjoin (cadr import-set) libraries)
		    (cddr import-set))))
	(if library
	    (defines-libraries (library-get 'parsed-defines library) libraries)
	    libraries)
	(cdr parsed-import)))

(define (mit-parsed-export-libraries parsed-export library libraries)
  (fold (lambda (export-spec libraries)
	  (case (car export-spec)
	    ((nameset) (nameset-libraries (cdr export-spec) libraries))
	    (else libraries)))
	(defines-libraries (library-get 'parsed-defines library) libraries)
	(cddr parsed-export)))

(define (nameset-libraries nameset libraries)
  (if (pair? nameset)
      (case (car nameset)
	((exports)
	 (library-name-adjoin (cadr nameset) libraries))
	((intersection union difference)
	 (fold nameset-libraries libraries (cdr nameset)))
	((re-filter re-remove)
	 (nameset-libraries (caddr nameset) libraries))
	((re-rename)
	 (nameset-libraries (cadddr nameset) libraries))
	(else libraries))
      libraries))

(define (defines-libraries parsed-defines libraries)
  (fold (lambda (parsed-define libraries)
	  (if (eq? 'mit-define (car parsed-define))
	      (nameset-libraries (caddr parsed-define) libraries)
	      libraries))
	libraries
	parsed-defines))

(define (mit-expand-parsed-import parsed-import db library imports)
  (fold (lambda (import-set imports)
	  (expand-parsed-import-set import-set db library imports))
	imports
	(cdr parsed-import)))

(define (expand-parsed-import-set import-set db library imports)
  (if (library-name? import-set)
      (fold (lambda (export imports)
	      (cons (make-library-ixport (library-ixport-from-library export)
					 (library-ixport-to export))
		    imports))
	    imports
	    (get-exports import-set db library))
      (case (car import-set)
	((drop)
	 (expand-exclusions (get-exports (cadr import-set) db library)
			    (cddr import-set)
			    db
			    library
			    imports))
	((take)
	 (expand-inclusions (get-exports (cadr import-set) db library)
			    (cddr import-set)
			    db
			    library
			    imports))
	(else
	 (error "Unrecognized import set:" import-set)))))

(define (mit-expand-parsed-export parsed-export names library)
  (make-export-group
   (cadr parsed-export)
   (expand-inclusions (map (lambda (name)
			     (make-library-ixport (library-name library) name))
			   names)
		      (cddr parsed-export)
		      (library-db library)
		      library
		      '())))

(define (expand-exclusions sources exclusions db library acc)
  (let ((part (partition-ixclusions exclusions))
	(name-matcher (make-name-matcher sources exclusions library db)))
    (lset-union library-ixport=?
		(remove-sources (re-match-matcher (part 're-match))
				(remove-sources (name-matcher (part 'nameset))
						sources))
		acc)))

(define (expand-inclusions sources inclusions db library acc)
  (let ((part (partition-ixclusions inclusions))
	(name-matcher (make-name-matcher sources inclusions library db))
	(rename-renamer (make-rename-renamer sources inclusions)))
    (lset-union library-ixport=?
		(keep-sources (name-matcher (part 'nameset)) sources)
		(rename-sources (rename-renamer (part 'rename)) sources)
		(keep-sources (re-match-matcher (part 're-match)) sources)
		(rename-sources (re-rename-renamer (part 're-rename)) sources)
		acc)))

(define-deferred partition-ixclusions
  (partition-generator car
		       (make-eq-comparator)
		       (lambda (item items)
			 (cons-last! (cdr item) items))
		       '()))

(define (keep-sources matcher sources)
  (filter-map (lambda (source)
		(let ((name (library-ixport-to source)))
		  (and (matcher name)
		       (make-library-ixport (library-ixport-from-library source)
					    name))))
	      sources))

(define (remove-sources matcher sources)
  (filter-map (lambda (source)
		(let ((name (library-ixport-to source)))
		  (and (not (matcher name))
		       (make-library-ixport (library-ixport-from-library source)
					    name))))
	      sources))

(define (rename-sources renamer sources)
  (filter-map (lambda (source)
		(let* ((name (library-ixport-to source))
		       (name* (renamer name)))
		  (and (not (eq? name name*))
		       (make-library-ixport (library-ixport-from-library source)
					    name
					    name*))))
	      sources))

(define (make-rename-renamer sources inclusions)
  (let ((available-names (map library-ixport-to sources)))
    (lambda (renames)
      (check-for-missing-names (map car renames) available-names inclusions)
      (make-renamer renames))))

(define (re-match-matcher sres)
  (make-re-matcher `(or ,@sres)))

(define (re-rename-renamer renames)
  (combine-renamers
   (map (lambda (r)
	  (make-re-renamer (car r) (cadr r)))
	renames)))

;;; Track explicit and implicit (exports) names separately.  Only explicit names
;;; are required to be available.

(define (make-name-matcher sources ixclusions library db)
  (let ((lookup (get-lookup library))
	(available-names (map library-ixport-to sources)))
    (lambda (namesets)
      (let ((nss
	     (apply nss-union
		    (map (lambda (nameset)
			   (expand-nameset nameset lookup library db))
			 namesets))))
	(check-for-missing-names (explicit-names nss)
				 available-names
				 ixclusions)
	(make-matcher (append (explicit-names nss) (implicit-names nss)))))))

(define (expand-nameset nameset lookup library db)
  (let loop ((nameset nameset))
    (if (symbol? nameset)
	(or (lookup nameset)
	    (make-nss (list nameset) '()))
	(case (car nameset)
	  ((exports)
	   (make-nss '()
		     (map library-ixport-to
			  (get-exports (cadr nameset) db library))))
	  ((intersection)
	   (apply nss-intersection (map loop (cdr nameset))))
	  ((union)
	   (apply nss-union (map loop (cdr nameset))))
	  ((difference)
	   (nss-difference (loop (cadr nameset))
			   (loop (caddr nameset))))
	  ((re-filter)
	   (filter-nss (make-re-matcher (cadr nameset))
		       (loop (caddr nameset))))
	  ((re-remove)
	   (remove-nss (make-re-matcher (cadr nameset))
		       (loop (cadr nameset))))
	  ((re-rename)
	   (rename-nss (make-re-renamer (cadr nameset) (caddr nameset))
		       (loop (cadddr nameset))))
	  (else
	   (error "Unrecognized nameset:" nameset))))))

(define (get-lookup library)
  (make-lookup (if library (library-get 'mit-defines library) '())))

(define (make-lookup defines)
  (lambda (name)
    (let ((p (assq name defines)))
      (and p
	   (cdr p)))))

(define-automatic-property 'mit-defines '(parsed-defines db library)
  #f
  (lambda (parsed-defines db library)
    (fold (lambda (def defs)
	    (if (eq? 'mit-define (car def))
		(cons (cons (cadr def)
			    (expand-nameset (caddr def)
					    (make-lookup defs)
					    library
					    db))
		      defs)
		defs))
	  '()
	  parsed-defines)))

(define-record-type <nss>
    (make-nss explicit implicit)
    nss?
  (explicit explicit-names)
  (implicit implicit-names))

(define null-nss
  (make-nss '() '()))

(define (nss-union . args)
  (fold (lambda (a b)
	  (let ((explicit
		 (lset-union eq?
			     (explicit-names a)
			     (explicit-names b)))
		(implicit
		 (lset-union eq?
			     (implicit-names a)
			     (implicit-names b))))
	    (make-nss explicit
		      (lset-difference eq? implicit explicit))))
	null-nss
	args))

(define (nss-intersection . args)
  (fold (lambda (a b)
	  (make-nss (lset-intersection eq?
				       (explicit-names a)
				       (explicit-names b))
		    (lset-intersection eq?
				       (implicit-names a)
				       (implicit-names b))))
	null-nss
	args))

(define (nss-difference a b)
  (make-nss (lset-difference eq?
			     (explicit-names a)
			     (explicit-names b)
			     (implicit-names b))
	    (lset-difference eq?
			     (implicit-names a)
			     (explicit-names b)
			     (implicit-names b))))

(define (filter-nss pred nss)
  (make-nss (filter pred (explicit-names nss))
	    (filter pred (implicit-names nss))))

(define (remove-nss pred nss)
  (make-nss (remove pred (explicit-names nss))
	    (remove pred (implicit-names nss))))

(define (rename-nss proc nss)
  (let ((explicit (map proc (explicit-names nss)))
	(implicit (map proc (implicit-names nss))))
    (make-nss explicit
	      (lset-difference eq? implicit explicit))))

(define (make-matcher names)
  (lambda (name)
    (memq name names)))

(define (make-renamer renames)
  (lambda (name)
    (let ((p (assq name renames)))
      (if p
	  (cadr p)
	  name))))

(define (make-re-matcher sre)
  (let ((regexp (regexp sre)))
    (lambda (name)
      (regexp-matches-some? regexp (symbol->string name)))))

(define (make-re-renamer sre repl)
  (let ((regexp (regexp sre)))
    (lambda (name)
      (let ((match (regexp-matches-some regexp (symbol->string name))))
	(if match
	    (string->symbol (regexp-match-replace match repl))
	    name)))))

(define (combine-renamers renamers)
  (lambda (name)
    (let loop ((renamers renamers))
      (if (pair? renamers)
	  (let ((name* ((car renamers) name)))
	    (if (eq? name name*)
		(loop (cdr renamers))
		name*))
	  name))))

(define (check-for-missing-names needed available ixclusions)
  (let ((missing (lset-difference eq? needed available)))
    (if (pair? missing)
	(error "Required names aren't available:"
	       missing
	       (error-irritant/noise " from clauses:")
	       (map cdr ixclusions)))))
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

;;;; Compiler File Dependencies
;;; package: (compiler declarations)

(declare (usual-integrations))

(define (initialize-package!)
  (add-event-receiver! event:after-restore reset-source-nodes!)
  (reset-source-nodes!))

(define (reset-source-nodes!)
  (set! source-filenames '())
  (set! source-hash)
  (set! source-nodes)
  (set! source-nodes/by-rank)
  unspecific)

(define (maybe-setup-source-nodes!)
  (if (null? source-filenames)
      (setup-source-nodes!)))

(define (setup-source-nodes!)
  (let ((filenames
	 (append-map!
	  (lambda (subdirectory)
	    (map (lambda (pathname)
		   (string-append subdirectory
				  "/"
				  (pathname-name pathname)))
		 (directory-read
		  (string-append subdirectory
				 "/"
				 source-file-expression))))
	  '("back" "base" "fggen" "fgopt" "rtlbase" "rtlgen" "rtlopt"
		   "machines/C"))))
    (if (null? filenames)
	(error "Can't find source files of compiler"))
    (set! source-filenames
	  ;; Don't process these excluded files.  It would be better to extract
	  ;; the filenames from the package description, but that's hard to do
	  ;; without extending the package abstraction a bit.
	  (lset-difference string=? filenames '("base/asstop" "base/crsend"))))
  (set! source-hash (make-string-hash-table))
  (set! source-nodes
	(map (lambda (filename)
	       (let ((node (make/source-node filename)))
		 (hash-table-set! source-hash filename node)
		 node))
	     source-filenames))
  (initialize/syntax-dependencies!)
  (initialize/integration-dependencies!)
  (source-nodes/rank!))

(define source-file-expression "*.scm")
(define source-filenames)
(define source-hash)
(define source-nodes)
(define source-nodes/by-rank)

(define (filename/append directory . names)
  (map (lambda (name) (string-append directory "/" name)) names))

(define-structure (source-node
		   (conc-name source-node/)
		   (constructor %make/source-node (filename pathname)))
  (filename #f read-only #t)
  (pathname #f read-only #t)
  (forward-links '())
  (backward-links '())
  (forward-closure '())
  (backward-closure '())
  (dependencies '())
  (dependents '())
  (rank #f)
  (syntax-table #f)
  (declarations '())
  (modification-time #f))

(define (make/source-node filename)
  (%make/source-node filename (->pathname filename)))

(define (filename->source-node filename)
  (let ((node (hash-table-ref/default source-hash filename #f)))
    (if (not node)
	(error "Unknown source file:" filename))
    node))

(define (source-node/circular? node)
  (memq node (source-node/backward-closure node)))

(define (source-node/link! node dependency)
  (if (not (memq dependency (source-node/backward-links node)))
      (begin
	(set-source-node/backward-links!
	 node
	 (cons dependency (source-node/backward-links node)))
	(set-source-node/forward-links!
	 dependency
	 (cons node (source-node/forward-links dependency)))
	(source-node/close! node dependency))))

(define (source-node/close! node dependency)
  (if (not (memq dependency (source-node/backward-closure node)))
      (begin
	(set-source-node/backward-closure!
	 node
	 (cons dependency (source-node/backward-closure node)))
	(set-source-node/forward-closure!
	 dependency
	 (cons node (source-node/forward-closure dependency)))
	(for-each (lambda (dependency)
		    (source-node/close! node dependency))
		  (source-node/backward-closure dependency))
	(for-each (lambda (node)
		    (source-node/close! node dependency))
		  (source-node/forward-closure node)))))

;;;; Rank

(define (source-nodes/rank!)
  (compute-dependencies! source-nodes)
  (compute-ranks! source-nodes)
  (set! source-nodes/by-rank (source-nodes/sort-by-rank source-nodes))
  unspecific)

(define (compute-dependencies! nodes)
  (for-each (lambda (node)
	      (set-source-node/dependencies!
	       node
	       (remove (lambda (node*)
			 (memq node (source-node/backward-closure node*)))
		       (source-node/backward-closure node)))
	      (set-source-node/dependents!
	       node
	       (remove (lambda (node*)
			 (memq node (source-node/forward-closure node*)))
		       (source-node/forward-closure node))))
	    nodes))

(define (compute-ranks! nodes)
  (let loop ((nodes nodes) (unranked-nodes '()))
    (if (null? nodes)
	(if (not (null? unranked-nodes))
	    (loop unranked-nodes '()))
	(loop (cdr nodes)
	      (let ((node (car nodes)))
		(let ((rank (source-node/rank* node)))
		  (if rank
		      (begin
			(set-source-node/rank! node rank)
			unranked-nodes)
		      (cons node unranked-nodes))))))))

(define (source-node/rank* node)
  (let loop ((nodes (source-node/dependencies node)) (rank -1))
    (if (null? nodes)
	(1+ rank)
	(let ((rank* (source-node/rank (car nodes))))
	  (and rank*
	       (loop (cdr nodes) (max rank rank*)))))))

(define (source-nodes/sort-by-rank nodes)
  (sort nodes (lambda (x y) (< (source-node/rank x) (source-node/rank y)))))

;;;; File Syntaxer

(define (syntax-files!)
  (maybe-setup-source-nodes!)
  (for-each
   (lambda (node)
     (let ((modification-time
	    (receive (scm bin spec)
		     (sf/pathname-defaulting (source-node/pathname node) #f #f)
	      spec
	      (let ((source (file-modification-time scm))
                    (binary (file-modification-time bin)))
                (if (not source)
                    (error "Missing source file" (source-node/filename node)))
                (and binary (< source binary) binary)))))
     (set-source-node/modification-time! node modification-time)
     (if (not modification-time)
	 (write-notification-line
	  (lambda (port)
	    (write-string "Source file newer than binary: " port)
	    (write (source-node/filename node) port))))))
   source-nodes)
  (if compiler:enable-integration-declarations?
      (begin
	(for-each
	 (lambda (node)
	   (let ((time (source-node/modification-time node)))
	     (if (and time
		      (any (lambda (node*)
			     (let ((newer?
				    (let ((time*
					   (source-node/modification-time
					    node*)))
				      (or (not time*)
					  (> time* time)))))
			       (if newer?
				   (write-notification-line
				    (lambda (port)
				      (write-string "Binary file " port)
				      (write (source-node/filename node) port)
				      (write-string " newer than dependency "
						    port)
				      (write (source-node/filename node*)
					     port))))
			       newer?))
			   (source-node/dependencies node)))
		 (set-source-node/modification-time! node #f))))
	 source-nodes)
	(for-each
	 (lambda (node)
	   (if (not (source-node/modification-time node))
	       (for-each (lambda (node*)
			   (if (source-node/modification-time node*)
			       (write-notification-line
				(lambda (port)
				  (write-string "Binary file " port)
				  (write (source-node/filename node*) port)
				  (write-string " depends on " port)
				  (write (source-node/filename node) port))))
			   (set-source-node/modification-time! node* #f))
			 (source-node/forward-closure node))))
	 source-nodes)))
  (for-each (lambda (node)
	      (if (not (source-node/modification-time node))
		  (pathname-delete!
		   (pathname-new-type (source-node/pathname node) "ext"))))
	    source-nodes/by-rank)
  (write-notification-line
   (lambda (port)
     (write-string "Begin pass 1:" port)))
  (bind-condition-handler (list condition-type:simple-warning)
      (lambda (condition)
	(if (string=? (access-condition condition 'MESSAGE)
		      "Missing externs file:")
	    (muffle-warning)))
    (lambda ()
      (for-each (lambda (node)
		  (if (not (source-node/modification-time node))
		      (source-node/syntax! node)))
		source-nodes/by-rank)))
  (if (any (lambda (node)
	     (and (not (source-node/modification-time node))
		  (source-node/circular? node)))
	   source-nodes/by-rank)
      (begin
	(write-notification-line
	 (lambda (port)
	   (write-string "Begin pass 2:" port)))
	(for-each (lambda (node)
		    (if (not (source-node/modification-time node))
			(if (source-node/circular? node)
			    (source-node/syntax! node)
			    (source-node/touch! node))))
		  source-nodes/by-rank))))

(define (source-node/touch! node)
  (receive (input-pathname bin-pathname spec-pathname)
      (sf/pathname-defaulting (source-node/pathname node) "" #f)
    input-pathname
    (pathname-touch! bin-pathname)
    (pathname-touch! (pathname-new-type bin-pathname "ext"))
    (if spec-pathname (pathname-touch! spec-pathname))))

(define (pathname-touch! pathname)
  (if (file-exists? pathname)
      (begin
	(write-notification-line
	 (lambda (port)
	   (write-string "Touch file: " port)
	   (write (enough-namestring pathname) port)))
	(file-touch pathname))))

(define (pathname-delete! pathname)
  (if (file-exists? pathname)
      (begin
	(write-notification-line
	 (lambda (port)
	   (write-string "Delete file: " port)
	   (write (enough-namestring pathname) port)))
	(delete-file pathname))))

(define (sc filename)
  (maybe-setup-source-nodes!)
  (source-node/syntax! (filename->source-node filename)))

(define (source-node/syntax! node)
  (receive (input-pathname bin-pathname spec-pathname)
      (sf/pathname-defaulting (source-node/pathname node) "" #f)
    (sf/internal
     input-pathname bin-pathname spec-pathname
     (source-node/syntax-table node)
     ((if compiler:enable-integration-declarations?
	  identity-procedure
	  (lambda (declarations)
	    (remove integration-declaration? declarations)))
      (source-node/declarations node)))))

;;;; Syntax dependencies

(define (initialize/syntax-dependencies!)
  (let ((file-dependency/syntax/join
	 (lambda (filenames syntax-table)
	   (for-each (lambda (filename)
		       (set-source-node/syntax-table!
			(filename->source-node filename)
			syntax-table))
		     filenames))))
    (file-dependency/syntax/join
     (append (filename/append "base"
			      "toplev"
			      "blocks" "cfg1" "cfg2" "cfg3" "constr"
			      "contin" "ctypes" "debug" "enumer"
			      "infnew" "lvalue" "object" "pmerly" "proced"
			      "refctx" "rvalue" "scode" "sets" "subprb"
			      "switch" "utils")
	     (filename/append "back"
			      "insseq" "lapgn1" "lapgn2" "linear" "regmap")
	     (filename/append "machines/C"
			      "cout" "ctop" "traditional" "stackify" "stackops"
			      "machin" "rulrew" "rgspcm")
	     (filename/append "fggen"
			      "declar" "fggen" "canon")
	     (filename/append "fgopt"
			      "blktyp" "closan" "conect" "contan" "delint"
			      "desenv" "envopt" "folcon" "offset" "operan"
			      "order" "outer" "param" "reord" "reteqv" "reuse"
			      "sideff" "simapp" "simple" "subfre" "varind")
	     (filename/append "rtlbase"
			      "regset" "rgraph" "rtlcfg" "rtlcon" "rtlexp"
			      "rtline" "rtlobj" "rtlreg" "rtlty1" "rtlty2"
			      "valclass")
	     (filename/append "rtlgen"
			      "fndblk" "fndvar" "opncod" "rgcomb" "rgproc"
			      "rgretn" "rgrval" "rgstmt" "rtlgen")
	     (filename/append "rtlopt"
			      "ralloc" "rcompr" "rcse1" "rcse2" "rcseep"
			      "rcseht" "rcserq" "rcsesr" "rdebug" "rdflow"
			      "rerite" "rinvex" "rlife" "rtlcsm"))
     (->environment '(COMPILER)))
    (file-dependency/syntax/join
     (filename/append "machines/C"
		      "lapgen"
		      "rules1" "rules2" "rules3" "rules4" "rulfix" "rulflo"
		      "cout" "traditional" "stackify" "stackops")
     (->environment '(COMPILER LAP-SYNTAXER)))))

;;;; Integration Dependencies

(define (initialize/integration-dependencies!)

  #|
  (define (add-declaration! declaration filenames)
    (for-each (lambda (filenames)
		(let ((node (filename->source-node filenames)))
		  (set-source-node/declarations!
		   node
		   (cons declaration
			 (source-node/declarations node)))))
	      filenames))
  |#

  (let* ((front-end-base
	  (filename/append "base"
			   "blocks" "cfg1" "cfg2" "cfg3"
			   "contin" "ctypes" "enumer" "lvalue"
			   "object" "proced" "rvalue"
			   "scode" "subprb" "utils"))
	 (C-base
	  (filename/append "machines/C" "machin"))
	 (rtl-base
	  (filename/append "rtlbase"
			   "rgraph" "rtlcfg" "rtlobj" "rtlreg" "rtlty1"
			   "rtlty2"))
	 (cse-base
	  (filename/append "rtlopt"
			   "rcse1" "rcseht" "rcserq" "rcsesr"))
	 (cse-all
	  (append (filename/append "rtlopt"
				   "rcse2" "rcseep")
		  cse-base))
	 (instruction-base
	  (filename/append "machines/C" "machin"))
	 (lapgen-base
	  (append (filename/append "back" "linear" "regmap")
		  (filename/append "machines/C" "lapgen")))
	 (lapgen-body
	  (append
	   (filename/append "back" "lapgn1" "lapgn2")
	   (filename/append "machines/C"
			    "rules1" "rules2" "rules3" "rules4"
			    "rulfix" "rulflo"
			    "cout" "traditional" "stackify" "stackops"
			    ))))

    (define (file-dependency/integration/join filenames dependencies)
      (for-each (lambda (filename)
		  (file-dependency/integration/make filename dependencies))
		filenames))

    (define (file-dependency/integration/make filename dependencies)
      (let ((node (filename->source-node filename)))
	(for-each (lambda (dependency)
		    (let ((node* (filename->source-node dependency)))
		      (if (not (eq? node node*))
			  (source-node/link! node node*))))
		  dependencies)))

    (define (define-integration-dependencies directory name directory* . names)
      (file-dependency/integration/make
       (string-append directory "/" name)
       (apply filename/append directory* names)))

    (define-integration-dependencies "base" "object" "base" "enumer")
    (define-integration-dependencies "base" "enumer" "base" "object")
    (define-integration-dependencies "base" "cfg1" "base" "object")
    (define-integration-dependencies "base" "cfg2" "base"
      "cfg1" "cfg3" "object")
    (define-integration-dependencies "base" "cfg3" "base" "cfg1" "cfg2")
    (define-integration-dependencies "base" "ctypes" "base"
      "blocks" "cfg1" "cfg2" "cfg3" "contin" "lvalue" "object" "subprb")
    (define-integration-dependencies "base" "rvalue" "base"
      "blocks" "cfg1" "cfg2" "cfg3" "enumer" "lvalue" "object" "utils")
    (define-integration-dependencies "base" "lvalue" "base"
      "blocks" "object" "proced" "rvalue" "utils")
    (define-integration-dependencies "base" "blocks" "base"
      "enumer" "lvalue" "object" "proced" "rvalue")
    (define-integration-dependencies "base" "proced" "base"
      "blocks" "cfg1" "cfg2" "cfg3" "contin" "enumer" "lvalue" "object"
      "rvalue" "utils")
    (define-integration-dependencies "base" "contin" "base"
      "blocks" "cfg3" "ctypes")
    (define-integration-dependencies "base" "subprb" "base"
      "cfg3" "contin" "enumer" "object" "proced")

    (define-integration-dependencies "machines/C" "machin" "rtlbase"
      "rtlreg" "rtlty1" "rtlty2")

    (define-integration-dependencies "rtlbase" "rgraph" "base" "cfg1" "cfg2")
    (define-integration-dependencies "rtlbase" "rgraph" "machines/C"
      "machin")
    (define-integration-dependencies "rtlbase" "rtlcfg" "base"
      "cfg1" "cfg2" "cfg3")
    (define-integration-dependencies "rtlbase" "rtlcon" "base" "cfg3" "utils")
    (define-integration-dependencies "rtlbase" "rtlcon" "machines/C"
      "machin")
    (file-dependency/integration/join (filename/append "rtlbase" "rtlcon")
				      rtl-base)
    (define-integration-dependencies "rtlbase" "rtlexp" "rtlbase"
      "rtlreg" "rtlty1")
    (define-integration-dependencies "rtlbase" "rtline" "base" "cfg1" "cfg2")
    (define-integration-dependencies "rtlbase" "rtline" "rtlbase"
      "rtlcfg" "rtlty2")
    (define-integration-dependencies "rtlbase" "rtlobj" "base"
      "cfg1" "object" "utils")
    (define-integration-dependencies "rtlbase" "rtlreg" "machines/C"
      "machin")
    (define-integration-dependencies "rtlbase" "rtlreg" "rtlbase"
      "rgraph" "rtlty1")
    (define-integration-dependencies "rtlbase" "rtlty1" "rtlbase" "rtlcfg")
    (define-integration-dependencies "rtlbase" "rtlty2" "machines/C"
      "machin")
    (define-integration-dependencies "rtlbase" "rtlty2" "rtlbase" "rtlty1")

    (file-dependency/integration/join
     (append
      (filename/append "base" "refctx")
      (filename/append "fggen"
		       "declar" "fggen") ; "canon" needs no integrations
      (filename/append "fgopt"
		       "blktyp" "closan" "conect" "contan" "delint" "desenv"
		       "envopt" "folcon" "offset" "operan" "order" "param"
		       "outer" "reuse" "reteqv" "sideff" "simapp" "simple"
		       "subfre" "varind"))
     (append C-base front-end-base))

    (define-integration-dependencies "fgopt" "reuse" "fgopt" "reord")

    (file-dependency/integration/join
     (filename/append "rtlgen"
		      "fndblk" "fndvar" "opncod" "rgcomb" "rgproc" "rgretn"
		      "rgrval" "rgstmt" "rtlgen")
     (append C-base front-end-base rtl-base))

    (file-dependency/integration/join
     (append cse-all
	     (filename/append "rtlopt" "ralloc" "rcompr" "rdebug" "rdflow"
			      "rerite" "rinvex" "rlife" "rtlcsm")
	     (filename/append "machines/C" "rulrew"))
     (append C-base rtl-base))

    (file-dependency/integration/join cse-all cse-base)

    (file-dependency/integration/join
     (filename/append "rtlopt" "ralloc" "rcompr" "rdebug" "rlife")
     (filename/append "rtlbase" "regset"))

    (file-dependency/integration/join
     (filename/append "rtlopt" "rcseht" "rcserq")
     (filename/append "base" "object"))

    (define-integration-dependencies "rtlopt" "rlife"  "base" "cfg2")

    (let ((dependents
	   (append instruction-base
		   lapgen-base
		   lapgen-body
		   (filename/append "back" "linear"))))
      #|
      (add-declaration! '(USUAL-DEFINITION (SET EXPT)) dependents)
      |#
      (file-dependency/integration/join dependents instruction-base))

    (file-dependency/integration/join (append lapgen-base lapgen-body)
				      lapgen-base)

    (define-integration-dependencies "back" "lapgn1" "base"
      "cfg1" "cfg2" "utils")
    (define-integration-dependencies "back" "lapgn1" "rtlbase"
      "rgraph" "rtlcfg")
    (define-integration-dependencies "back" "lapgn2" "rtlbase" "rtlreg")
    (define-integration-dependencies "back" "linear" "base" "cfg1" "cfg2")
    (define-integration-dependencies "back" "linear" "rtlbase" "rtlcfg")
    (define-integration-dependencies "back" "mermap" "back" "regmap")
    (define-integration-dependencies "back" "regmap" "base" "utils"))

  (for-each (lambda (node)
	      (let ((links (source-node/backward-links node)))
		(if (not (null? links))
		    (set-source-node/declarations!
		     node
		     (cons (make-integration-declaration
			    (source-node/pathname node)
			    (map source-node/pathname links))
			   (source-node/declarations node))))))
	    source-nodes))

(define (make-integration-declaration pathname integration-dependencies)
  `(INTEGRATE-EXTERNAL
    ,@(map (let ((default
		  (make-pathname
		   #f
		   #f
		   (cons 'RELATIVE
			 (make-list
			  (length (cdr (pathname-directory pathname)))
			  'UP))
		   #f
		   #f
		   #f)))
	     (lambda (pathname)
	       (merge-pathnames pathname default)))
	   integration-dependencies)))

(define (integration-declaration? declaration)
  (eq? (car declaration) 'INTEGRATE-EXTERNAL))
#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/decls.scm,v 4.1 1987/12/30 07:03:02 cph Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Compiler File Dependencies

(declare (usual-integrations))

(define-structure (source-node
		   (conc-name source-node/)
		   (constructor make/source-node (filename)))
  (filename false read-only true)
  (forward-links '())
  (backward-links '())
  (forward-closure '())
  (backward-closure '())
  (dependencies '())
  (rank false))

(define source-filenames
  (mapcan (lambda (subdirectory)
	    (map (lambda (pathname)
		   (string-append subdirectory "/" (pathname-name pathname)))
		 (directory-read (string-append subdirectory "/*.scm"))))
	  '("back" "base" "fggen" "fgopt" "rtlbase" "rtlgen" "rtlopt"
		    "machines/bobcat")))

(define source-hash
  (make/hash-table 101
		   string-hash-mod
		   (lambda (filename source-node)
		     (string=? filename (source-node/filename source-node)))
		   make/source-node))

(define source-nodes
  (map (lambda (filename)
	 (hash-table/intern! source-hash
			     filename
			     identity-procedure
			     identity-procedure))
       source-filenames))

(define (filename->source-node filename)
  (hash-table/lookup source-hash
		     filename
		     identity-procedure
		     (lambda () (error "Unknown source file" filename))))

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

(define (source-files-by-rank)
  (source-nodes/rank! source-nodes)
  (map source-node/filename (source-nodes/sort-by-rank source-nodes)))

(define (source-files-with-circular-dependencies)
  (map source-node/filename
       (list-transform-positive source-nodes
	 (lambda (node)
	   (memq node (source-node/backward-closure node))))))

(define source-nodes/rank!)
(let ()

(set! source-nodes/rank!
  (lambda (nodes)
    (compute-dependencies! nodes)
    (compute-ranks! nodes)))

(define (compute-dependencies! nodes)
  (for-each (lambda (node)
	      (set-source-node/dependencies!
	       node
	       (list-transform-negative (source-node/backward-closure node)
		 (lambda (node*)
		   (memq node (source-node/backward-closure node*))))))
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

)

(define (source-nodes/sort-by-rank nodes)
  (sort nodes (lambda (x y) (< (source-node/rank x) (source-node/rank y)))))

(define (file-dependency/syntax/join filenames dependency)
  (for-each (lambda (filename)
	      (sf/set-file-syntax-table! filename dependency))
	    filenames))

(define (define-integration-dependencies directory name directory* . names)
  (file-dependency/integration/make (string-append directory "/" name)
				    (apply filename/append directory* names)))

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

(define (finish-integration-dependencies!)
  (if compiler:enable-integration-declarations?
      (for-each (lambda (node)
		  (let ((links (source-node/backward-links node)))
		    (if (not (null? links))
			(sf/add-file-declarations!
			 (source-node/filename node)
			 `((INTEGRATE-EXTERNAL
			    ,@(map (lambda (node*)
				     (filename->absolute-pathname
				      (source-node/filename node*)))
				   links)))))))
		source-nodes)))

(define (file-dependency/expansion/join filenames expansions)
  (if compiler:enable-expansion-declarations?
      (for-each (lambda (filename)
		  (sf/add-file-declarations!
		   filename
		   `((EXPAND-OPERATOR ,@expansions))))
		filenames)))

(define (filename/append directory . names)
  (map (lambda (name) (string-append directory "/" name)) names))

(define (filename->absolute-pathname filename)
  (pathname->absolute-pathname (->pathname filename)))

;;;; Syntax dependencies

(file-dependency/syntax/join
 (append (filename/append "base"
			  "blocks" "cfg1" "cfg2" "cfg3" "contin" "ctypes"
			  "debug" "enumer" "infgen" "infutl" "lvalue" "object"
			  "pmerly" "proced" "queue" "rvalue" "scode" "sets"
			  "subprb" "switch" "toplev" "utils")
	 (filename/append "back"
			  "asmmac" "bittop" "bitutl" "insseq" "lapgn1" "lapgn2"
			  "lapgn3" "linear" "regmap" "symtab" "syntax")
	 (filename/append "machines/bobcat"
			  "insmac" "machin" "rgspcm")
	 (filename/append "fggen"
			  "declar" "fggen")
	 (filename/append "fgopt"
			  "blktyp" "closan" "conect" "contan" "desenv" "folcon"
			  "offset" "operan" "order" "outer" "simapp" "simple")
	 (filename/append "rtlbase"
			  "regset" "rgraph" "rtlcfg" "rtlcon" "rtlexp" "rtline"
			  "rtlobj" "rtlreg" "rtlty1" "rtlty2")
	 (filename/append "rtlgen"
			  "fndblk" "opncod" "rgcomb" "rgproc" "rgretn" "rgrval"
			  "rgstmt" "rtlgen")
	 (filename/append "rtlopt"
			  "ralloc" "rcse1" "rcse2" "rcseep" "rcseht" "rcserq"
			  "rcsesr" "rdeath" "rdebug" "rlife"))
 compiler-syntax-table)

(file-dependency/syntax/join
 (filename/append "machines/bobcat"
		  "lapgen" "rules1" "rules2" "rules3" "rules4")
 lap-generator-syntax-table)

(file-dependency/syntax/join
 (filename/append "machines/bobcat"
		  "insutl" "instr1" "instr2" "instr3" "instr4")
 assembler-syntax-table)

;;;; Integration Dependencies

(define-integration-dependencies "base" "object" "base" "enumer")
(define-integration-dependencies "base" "enumer" "base" "object")
(define-integration-dependencies "base" "utils" "base" "scode")
(define-integration-dependencies "base" "cfg1" "base" "object")
(define-integration-dependencies "base" "cfg2" "base" "cfg1" "cfg3" "object")
(define-integration-dependencies "base" "cfg3" "base" "cfg1" "cfg2")
(define-integration-dependencies "base" "ctypes" "base"
  "blocks" "cfg1" "cfg2" "cfg3" "contin" "lvalue" "object" "subprb")
(define-integration-dependencies "base" "rvalue" "base"
  "blocks" "cfg1" "cfg2" "cfg3" "enumer" "lvalue" "object" "utils")
(define-integration-dependencies "base" "lvalue" "base"
  "blocks" "object" "proced" "rvalue" "utils")
(define-integration-dependencies "base" "blocks" "base"
  "enumer" "lvalue" "object" "proced" "rvalue" "scode")
(define-integration-dependencies "base" "proced" "base"
  "blocks" "cfg1" "cfg2" "cfg3" "contin" "enumer" "lvalue" "object" "rvalue"
  "utils")
(define-integration-dependencies "base" "contin" "base"
  "blocks" "cfg3" "ctypes")
(define-integration-dependencies "base" "subprb" "base"
  "cfg3" "contin" "enumer" "object" "proced")
(define-integration-dependencies "base" "infnew" "base" "infutl")

(define front-end-base
  (filename/append "base"
		   "blocks" "cfg1" "cfg2" "cfg3" "contin" "ctypes" "enumer"
		   "lvalue" "object" "proced" "queue" "rvalue" "scode"
		   "subprb" "utils"))

(define-integration-dependencies "machines/bobcat" "machin" "rtlbase"
  "rtlreg" "rtlty1" "rtlty2")

(define bobcat-base
  (filename/append "machines/bobcat" "machin"))

(define-integration-dependencies "rtlbase" "regset" "base")
(define-integration-dependencies "rtlbase" "rgraph" "base" "cfg1" "cfg2")
(define-integration-dependencies "rtlbase" "rgraph" "machines/bobcat" "machin")
(define-integration-dependencies "rtlbase" "rtlcfg" "base"
  "cfg1" "cfg2" "cfg3")
(define-integration-dependencies "rtlbase" "rtlcon" "base" "cfg3" "utils")
(define-integration-dependencies "rtlbase" "rtlcon" "machines/bobcat" "machin")
(define-integration-dependencies "rtlbase" "rtlexp" "base" "utils")
(define-integration-dependencies "rtlbase" "rtlexp" "rtlbase" "rtlreg")
(define-integration-dependencies "rtlbase" "rtline" "base" "cfg1" "cfg2")
(define-integration-dependencies "rtlbase" "rtline" "rtlbase"
  "rtlcfg" "rtlty2")
(define-integration-dependencies "rtlbase" "rtlobj" "base"
  "cfg1" "object" "utils")
(define-integration-dependencies "rtlbase" "rtlreg" "machines/bobcat" "machin")
(define-integration-dependencies "rtlbase" "rtlreg" "rtlbase"
  "rgraph" "rtlty1")
(define-integration-dependencies "rtlbase" "rtlty1" "rtlbase" "rtlcfg")
(define-integration-dependencies "rtlbase" "rtlty2" "base" "scode")
(define-integration-dependencies "rtlbase" "rtlty2" "machines/bobcat" "machin")
(define-integration-dependencies "rtlbase" "rtlty2" "rtlbase" "rtlty1")

(define rtl-base
  (filename/append "rtlbase"
		   "regset" "rgraph" "rtlcfg" "rtlcon" "rtlexp" "rtlobj"
		   "rtlreg" "rtlty1" "rtlty2"))

(file-dependency/integration/join
 (append
  (filename/append "fggen"
		   "declar" "fggen")
  (filename/append "fgopt"
		   "blktyp" "closan" "conect" "contan" "desenv" "folcon"
		   "offset" "operan" "order" "outer" "simapp" "simple"))
 (append front-end-base bobcat-base))

(file-dependency/integration/join
 (filename/append "rtlgen"
		  "fndblk" "opncod" "rgcomb" "rgproc" "rgretn" "rgrval"
		  "rgstmt" "rtlgen")
 (append front-end-base bobcat-base rtl-base))

(define cse-base
  (filename/append "rtlopt"
		   "rcse1" "rcse2" "rcseep" "rcseht" "rcserq" "rcsesr"))

(file-dependency/integration/join
 (append cse-base
	 (filename/append "rtlopt" "ralloc" "rdeath" "rdebug" "rlife"))
 (append bobcat-base rtl-base))

(file-dependency/integration/join cse-base cse-base)

(define-integration-dependencies "rtlopt" "rcseht" "base" "object")
(define-integration-dependencies "rtlopt" "rcserq" "base" "object")
(define-integration-dependencies "rtlopt" "rlife"  "base" "cfg2")

(define instruction-base
  (append (filename/append "back" "insseq")
	  (filename/append "machines/bobcat" "assmd" "machin")))

(define lapgen-base
  (append (filename/append "back" "lapgn2" "lapgn3" "regmap")
	  (filename/append "machines/bobcat" "lapgen")))

(define assembler-base
  (append (filename/append "back" "bitutl" "symtab")
	  (filename/append "machines/bobcat" "insutl")))

(define lapgen-body
  (append
   (filename/append "back" "lapgn1" "syntax")
   (filename/append "machines/bobcat" "rules1" "rules2" "rules3" "rules4")))

(define assembler-body
  (append
   (filename/append "back" "bittop")
   (filename/append "machines/bobcat" "instr1" "instr2" "instr3" "instr4")))

(file-dependency/integration/join
 (append instruction-base
	 lapgen-base
	 lapgen-body
	 assembler-base
	 assembler-body
	 (filename/append "back" "linear" "syerly"))
 instruction-base)

(file-dependency/integration/join (append lapgen-base lapgen-body) lapgen-base)

(file-dependency/integration/join (append assembler-base assembler-body)
				  assembler-base)

(define-integration-dependencies "back" "lapgn1" "base" "cfg1" "cfg2" "utils")
(define-integration-dependencies "back" "lapgn1" "rtlbase"
  "regset" "rgraph" "rtlcfg")
(define-integration-dependencies "back" "lapgn2" "rtlbase" "rtlreg")
(define-integration-dependencies "back" "lapgn3" "rtlbase" "rtlcfg")
(define-integration-dependencies "back" "linear" "base" "cfg1" "cfg2")
(define-integration-dependencies "back" "linear" "rtlbase" "rtlcfg")
(define-integration-dependencies "back" "regmap" "base" "utils")
(define-integration-dependencies "back" "symtab" "base" "utils")

;;;; Expansion Dependencies

(file-dependency/expansion/join
 (filename/append "machines/bobcat"
		  "lapgen" "rules1" "rules2" "rules3" "rules4")
 '((LAP:SYNTAX-INSTRUCTION
    (ACCESS LAP:SYNTAX-INSTRUCTION-EXPANDER LAP-SYNTAX-PACKAGE
	    COMPILER-PACKAGE))
   (INSTRUCTION->INSTRUCTION-SEQUENCE
    (ACCESS INSTRUCTION->INSTRUCTION-SEQUENCE-EXPANDER LAP-SYNTAX-PACKAGE
	    COMPILER-PACKAGE))
   (SYNTAX-EVALUATION
    (ACCESS SYNTAX-EVALUATION-EXPANDER LAP-SYNTAX-PACKAGE COMPILER-PACKAGE))
   (CONS-SYNTAX
    (ACCESS CONS-SYNTAX-EXPANDER LAP-SYNTAX-PACKAGE COMPILER-PACKAGE))
   (OPTIMIZE-GROUP-EARLY
    (ACCESS OPTIMIZE-GROUP-EXPANDER LAP-SYNTAX-PACKAGE COMPILER-PACKAGE))
   (EA-KEYWORD-EARLY
    (ACCESS EA-KEYWORD-EXPANDER LAP-SYNTAX-PACKAGE COMPILER-PACKAGE))
   (EA-MODE-EARLY
    (ACCESS EA-MODE-EXPANDER LAP-SYNTAX-PACKAGE COMPILER-PACKAGE))
   (EA-REGISTER-EARLY
    (ACCESS EA-REGISTER-EXPANDER LAP-SYNTAX-PACKAGE COMPILER-PACKAGE))
   (EA-EXTENSION-EARLY
    (ACCESS EA-EXTENSION-EXPANDER LAP-SYNTAX-PACKAGE COMPILER-PACKAGE))
   (EA-CATEGORIES-EARLY
    (ACCESS EA-CATEGORIES-EXPANDER LAP-SYNTAX-PACKAGE COMPILER-PACKAGE))))

(finish-integration-dependencies!)
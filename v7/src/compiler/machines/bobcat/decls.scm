#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/decls.scm,v 1.10 1987/06/13 21:14:53 cph Exp $

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

(define (file-dependency/integration/chain filenames)
  (if (not (null? (cdr filenames)))
      (begin (file-dependency/integration/make (car filenames) (cdr filenames))
	     (file-dependency/integration/chain (cdr filenames)))))

(define (file-dependency/integration/join filenames dependencies)
  (for-each (lambda (filename)
	      (file-dependency/integration/make filename dependencies))
	    filenames))

(define (file-dependency/integration/make filename dependencies)
  (if enable-integration-declarations
      (sf/add-file-declarations!
       filename
       `((INTEGRATE-EXTERNAL
	  ,@(map (lambda (dependency)
		   (pathname->absolute-pathname (->pathname dependency)))
		 dependencies))))))

(define (filename/append directory . names)
  (map (lambda (name)
	 (string-append directory "/" name))
       names))

(define (file-dependency/syntax/join filenames dependency)
  (for-each (lambda (filename)
	      (sf/set-file-syntax-table! filename dependency))
	    filenames))

(define filenames/dependency-chain/base
  (filename/append "base"
		   "object" "cfg1" "cfg2" "cfg3" "ctypes" "dtypes" "bblock"
		   "dfg" "rtltyp" "rtlreg" "rtlcfg" "emodel" "rtypes"))

(define filenames/dependency-chain/rcse
  (filename/append "front-end" "rcseht" "rcserq" "rcse1" "rcse2"))

(define filenames/dependency-group/base
  (append (filename/append "base" "linear" "rtlcon" "rtlexp")
	  (filename/append "alpha" "dflow1" "dflow2" "dflow3" "dflow4" "dflow5"
			   "dflow6" "fggen1" "fggen2")
	  (filename/append "front-end"
			   "ralloc" "rcseep" "rcsesa" "rdeath" "rdebug"
			   "rgcomb" "rgpcom" "rgpred" "rgproc" "rgrval"
			   "rgstmt" "rlife" "rtlgen")
	  (filename/append "back-end" "lapgn1" "lapgn2" "lapgn3")))

(file-dependency/integration/chain
 (reverse
  (append filenames/dependency-chain/base
	  filenames/dependency-chain/rcse)))

(file-dependency/integration/join filenames/dependency-group/base
				  filenames/dependency-chain/base)

(file-dependency/integration/join
 (filename/append "machines/bobcat" "instr2" "instr3")
 (filename/append "machines/bobcat" "instr1"))

(file-dependency/syntax/join
 (append (filename/append "base"
			  "bblock" "cfg1" "cfg2" "cfg3" "ctypes" "dfg" "dtypes"
			  "emodel" "linear" "object" "queue" "rtlcfg" "rtlcon"
			  "rtlexp" "rtlreg" "rtltyp" "rtypes" "sets" "toplev"
			  "utils")
	 (filename/append "alpha" "dflow1" "dflow2" "dflow3" "dflow4" "dflow5"
			  "dflow6" "fggen1" "fggen2")
	 (filename/append "front-end"
			  "ralloc" "rcse1" "rcse2" "rcseep" "rcseht" "rcserq"
			  "rcsesa" "rdeath" "rdebug" "rgcomb" "rgpcom" "rgpred"
			  "rgproc" "rgrval" "rgstmt" "rlife" "rtlgen")
	 (filename/append "back-end"
			  "asmmac" "block" "lapgn1" "lapgn2" "lapgn3" "laptop"
			  "regmap" "symtab")
	 (filename/append "machines/bobcat" "insmac" "machin"))
 compiler-syntax-table)

(file-dependency/syntax/join
 (append (filename/append "machines/bobcat"
			  "lapgen" "rules1" "rules2" "rules3" "rules4")
	 (filename/append "machines/spectrum" "lapgen"))
 lap-generator-syntax-table)

(file-dependency/syntax/join
 (append (filename/append "machines/bobcat" "instr1" "instr2" "instr3")
	 (filename/append "machines/spectrum" "instrs"))
 assembler-syntax-table)
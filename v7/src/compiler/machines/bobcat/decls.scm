#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/decls.scm,v 1.24 1987/09/03 05:17:16 jinx Exp $

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
      (sf/add-file-declarations! filename
				 `((INTEGRATE-EXTERNAL ,@dependencies)))))

(define (file-dependency/expansion/join filenames expansions)
  (for-each (lambda (filename)
	      (file-dependency/expansion/make filename expansions))
	    filenames))			 

(define (file-dependency/expansion/make filename expansions)
  (if enable-expansion-declarations
      (sf/add-file-declarations! filename `((EXPAND-OPERATOR ,@expansions)))))

(define (filename/append directory . names)
  (map (lambda (name)
	 (pathname->absolute-pathname
	  (string->pathname (string-append directory "/" name))))
       names))

(define (file-dependency/syntax/join filenames dependency)
  (for-each (lambda (filename)
	      (sf/set-file-syntax-table! filename dependency))
	    filenames))

;;;; Integration and expansion dependencies

(define filenames/dependency-chain/base
  (filename/append "base"
		   "object" "cfg1" "cfg2" "cfg3" "rgraph" "ctypes" "dtype1"
		   "dtype2" "dtype3" "dfg" "rtlty1" "rtlty2" "rtlreg" "rtlcfg"
		   "emodel" "rtypes" "regset" "infutl" "infgen"))

(define filenames/dependency-chain/rcse
  (filename/append "front-end" "rcseht" "rcserq" "rcse1" "rcse2"))

(define filenames/dependency-group/base
  (append (filename/append "base" "linear" "rtlcon" "rtlexp")
	  (filename/append "alpha" "declar" "dflow1" "dflow2" "dflow3" "dflow4"
			   "dflow5" "dflow6" "fggen1" "fggen2")
	  (filename/append "front-end"
			   "ralloc" "rcseep" "rdeath" "rdebug" "rgcomb"
			   "rgpcom" "rgpred" "rgproc" "rgrval" "rgstmt" "rlife"
			   "rtlgen")
	  (filename/append "back-end" "lapgn1" "lapgn2" "lapgn3")
	  (filename/append "machines/bobcat" "rgspcm")))

(define filenames/dependency-chain/bits
  (filename/append "back-end" "symtab" "bitutl" "bittop"))

(file-dependency/integration/chain
 (reverse
  (append filenames/dependency-chain/base
	  filenames/dependency-chain/rcse)))

(file-dependency/integration/chain
 (reverse filenames/dependency-chain/bits))

(file-dependency/integration/join filenames/dependency-group/base
				  filenames/dependency-chain/base)

(file-dependency/integration/chain
 (append (filename/append "machines/bobcat" "dassm1")
	 (filename/append "base" "infutl")))

(file-dependency/integration/join
 (filename/append "machines/bobcat" "dassm2" "dassm3")
 (append (filename/append "machines/bobcat" "dassm1")
	 (filename/append "base" "infutl")))

;;;; Lap level integration and expansion dependencies

(define filenames/dependency-group/lap
  (filename/append "machines/bobcat" "instr1" "instr2" "instr3" "instr4"))

(define filenames/dependency-group/lap-syn1
  (append (filename/append "back-end" "lapgn1" "lapgn2" "lapgn3" "regmap")
	  (filename/append "base" "linear")))

(define filenames/dependency-group/lap-syn2
  (filename/append "machines/bobcat" "lapgen"))

(define filenames/dependency-group/lap-syn3
  (filename/append "machines/bobcat" "rules1" "rules2" "rules3" "rules4"))

(define filenames/dependency-group/lap-syn4
  (append filenames/dependency-group/lap-syn2
	  filenames/dependency-group/lap-syn3))

(file-dependency/integration/join filenames/dependency-group/lap-syn3
				  filenames/dependency-group/lap-syn2)

(file-dependency/integration/join filenames/dependency-group/lap-syn4
				  (append
				   (filename/append "machines/bobcat" "machin")
				   (filename/append "base" "utils")))

(file-dependency/integration/join (append filenames/dependency-group/lap-syn1
					  filenames/dependency-group/lap-syn4)
				  (filename/append "back-end" "insseq"))

(file-dependency/integration/join (append filenames/dependency-group/lap
					  filenames/dependency-group/lap-syn4)
				  (filename/append "machines/bobcat" "insutl"))

(file-dependency/expansion/join
 filenames/dependency-group/lap-syn4
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

;;;; Syntax dependencies

(file-dependency/syntax/join
 (append (filename/append "base"
			  "cfg1" "cfg2" "cfg3" "ctypes" "dfg" "dtype1" "dtype2"
			  "dtype3" "emodel" "infutl" "infgen" "linear" "object"
			  "pmerly" "queue" "regset" "rgraph" "rtlcfg" "rtlcon"
			  "rtlexp" "rtlreg" "rtlty1" "rtlty2" "rtypes" "sets"
			  "toplv1" "toplv2" "toplv3" "utils")
	 (filename/append "alpha" "declar" "dflow1" "dflow2" "dflow3" "dflow4"
			  "dflow5" "dflow6" "fggen1" "fggen2")
	 (filename/append "front-end"
			  "ralloc" "rcse1" "rcse2" "rcseep" "rcseht" "rcserq"
			  "rdeath" "rdebug" "rgcomb" "rgpcom" "rgpred" "rgproc"
			  "rgrval" "rgstmt" "rlife" "rtlgen")
	 (filename/append "back-end"
			  "asmmac" "bittop" "bitutl" "insseq" "lapgn1" "lapgn2"
			  "lapgn3" "regmap" "symtab" "syntax")
	 (filename/append "machines/bobcat" "dassm1" "dassm2" "dassm3" "insmac"
			  "machin"))
 compiler-syntax-table)

(file-dependency/syntax/join
 (append (filename/append "machines/spectrum" "lapgen")
	 filenames/dependency-group/lap-syn4)
 lap-generator-syntax-table)

(file-dependency/syntax/join
 (append (filename/append "machines/bobcat" "insutl" "instr1" "instr2"
			  "instr3" "instr4")
	 (filename/append "machines/spectrum" "instrs"))
 assembler-syntax-table)
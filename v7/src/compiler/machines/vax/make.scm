#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/vax/make.scm,v 1.0 1988/01/05 15:53:49 bal Exp $

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

;;;; Compiler Make File for DEC VAX

(declare (usual-integrations))

;(set-working-directory-pathname! "$zcomp")
;(load "base/rcs" system-global-environment)
(load "base/pkging" system-global-environment)

(in-package compiler-package

  (define compiler-system
    (make-environment
      (define :name "Liar (DEC VAX)")
      (define :version 3)
      (define :modification 0)
      (define :files)

;      (parse-rcs-header
;       "$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/vax/make.scm,v 1.0 1988/01/05 15:53:49 bal Exp $"
;       (lambda (filename version date time zone author state)
;	 (set! :version (car version))
;	 (set! :modification (cadr version))))

      (define :files-lists
	(list
	 (cons system-global-environment
	       '("base/pbs.bin"		   ;bit-string read/write syntax
		 ))

	 (cons compiler-package
	       '("base/macros.bin"	   ;compiler syntax
		 "base/decls.bin"	   ;declarations

		 "base/object.bin"	   ;tagged object support
		 "base/queue.bin"	   ;queue abstraction
		 "base/sets.bin"	   ;set abstraction
		 "base/mvalue.bin"	   ;multiple-value support

		 "machines/vax/machin.bin" ;machine dependent stuff
		 "base/toplv1.bin"	   ;top level
		 "base/toplv2.bin"
		 "base/toplv3.bin"
		 "base/utils.bin"	   ;odds and ends
		 "base/cfg1.bin"	   ;control flow graph
		 "base/cfg2.bin"
		 "base/cfg3.bin"
		 "base/rgraph.bin"	   ;program graph abstraction
		 "base/ctypes.bin"	   ;CFG datatypes
		 "base/dtype1.bin"	   ;DFG datatypes
		 "base/dtype2.bin"
		 "base/dtype3.bin"
		 "base/dfg.bin"		   ;data flow graph
		 "base/rtlty1.bin"	   ;RTL: type definitions
		 "base/rtlty2.bin"
		 "base/rtlexp.bin"	   ;RTL: expression operations
		 "base/rtlcon.bin"	   ;RTL: complex constructors
		 "base/rtlreg.bin"	   ;RTL: registers
		 "base/rtlcfg.bin"	   ;RTL: CFG types
		 "base/emodel.bin"	   ;environment model
		 "base/rtypes.bin"	   ;RTL Registers
		 "base/regset.bin"	   ;RTL Register Sets
		 "base/pmlook.bin"	   ;pattern matcher: lookup
		 "base/pmpars.bin"	   ;pattern matcher: parser
		 "base/infutl.bin"	   ;utilities for info generation, shared
		 "back-end/insseq.bin"	   ;lap instruction sequences
		 "machines/vax/dassm1.bin" ;disassembler
		 "base/linear.bin"	   ;linearization
		 ))

	 (cons disassembler-package
	       '("machines/vax/dsyn.bin" ; disassembler instruction syntax
		 "machines/vax/dassm2.bin" ;disassembler
		 "machines/vax/dassm3.bin"
		 "machines/vax/instr1.dbin" ;disassembler instruction definitions
		 "machines/vax/instr2.dbin"
		 "machines/vax/instr3.dbin"
		 ))

	 (cons converter-package
	       '("alpha/fggen1.bin"	   ;SCode->flow-graph converter
		 "alpha/fggen2.bin"
		 "alpha/declar.bin"	   ;Declaration handling
		 ))

	 (cons dataflow-package
	       '("alpha/dflow1.bin"	   ;Dataflow analyzer
		 "alpha/dflow2.bin"
		 "alpha/dflow3.bin"
		 "alpha/dflow4.bin"
		 "alpha/dflow5.bin"
		 "alpha/dflow6.bin"
		 ))

	 (cons rtl-generator-package
	       '("front-end/rtlgen.bin"	   ;RTL generator
		 "front-end/rgproc.bin"	   ;RTL generator: Procedure Headers
		 "front-end/rgstmt.bin"	   ;RTL generator: Statements
		 "front-end/rgpred.bin"	   ;RTL generator: Predicates
		 "front-end/rgrval.bin"	   ;RTL generator: RValues
		 "front-end/rgcomb.bin"	   ;RTL generator: Combinations
		 "front-end/rgpcom.bin"	   ;RTL generator: Primitive open-coding
		 ))

	 (cons rtl-cse-package
	       '("front-end/rcse1.bin"	   ;RTL common subexpression eliminator
		 "front-end/rcse2.bin"
		 "front-end/rcseep.bin"	   ;CSE expression predicates
		 "front-end/rcseht.bin"	   ;CSE hash table
		 "front-end/rcserq.bin"	   ;CSE register/quantity abstractions
		 ))

	 (cons rtl-analyzer-package
	       '("front-end/rlife.bin"	   ;RTL register lifetime analyzer
		 "front-end/rdeath.bin"	   ;RTL dead code eliminations
		 "front-end/rdebug.bin"	   ;RTL optimizer debugging output
		 "front-end/ralloc.bin"	   ;RTL register allocator
		 ))

	 (cons debugging-information-package
	       '("base/infgen.bin"	   ;debugging information generation
		 ))

	 (cons lap-syntax-package
	       '("back-end/lapgn1.bin"	   ;LAP generator.
		 "back-end/lapgn2.bin"
		 "back-end/lapgn3.bin"
		 "back-end/regmap.bin"	   ;Hardware register allocator.
		 "machines/vax/lapgen.bin" ;code generation rules.
		 "machines/vax/rules1.bin"
		 "machines/vax/rules2.bin"
		 "machines/vax/rules3.bin"
		 "machines/vax/rules4.bin"
		 "back-end/syntax.bin"	   ;Generic syntax phase
		 "machines/vax/coerce.bin" ;Coercions: integer -> bit string
		 "back-end/asmmac.bin"	   ;Macros for hairy syntax
		 "machines/vax/insmac.bin" ;Macros for hairy syntax
		 "machines/vax/insutl.bin" ;Utilities and effective addressing
		 "machines/vax/instr1.bin" ;VAX Instructions
		 "machines/vax/instr2.bin" ; "        "
		 "machines/vax/instr3.bin" ; "        "
		 ))

	 (cons bit-package
	       '("machines/vax/assmd.bin"  ;Machine dependent
		 "back-end/symtab.bin"	   ;Symbol tables
		 "back-end/bitutl.bin"	   ;Assembly blocks
		 "back-end/bittop.bin"	   ;Assembler top level
		 ))

	 ))

      ))

  (load-system! compiler-system true))

(for-each (lambda (name)
	    (local-assignment system-global-environment name
			      (lexical-reference compiler-package name)))
	  '(COMPILE-BIN-FILE COMPILE-PROCEDURE COMPILER:RESET!))
(toggle-gc-notification!)
#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/spectrum/make.scm,v 1.3 1987/03/19 00:56:02 cph Exp $

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

;;;; Compiler Make File for HP Precision Architecture

(declare (usual-integrations))

(set-working-directory-pathname! "$zcomp")
(load "rcs" system-global-environment)
(load "load" system-global-environment)

(load-system system-global-environment
	     'COMPILER-PACKAGE
	     '(SYSTEM-GLOBAL-ENVIRONMENT)
	     '(
	       (SYSTEM-GLOBAL-ENVIRONMENT
		"macros.bin"		;compiler syntax
		"pbs.bin"		;bit-string read/write syntax
		)

	       (COMPILER-PACKAGE
		"spectrum/machin.bin"	;machine dependent stuff
		"toplev.bin"		;top level
		"utils.bin"		;odds and ends
		"cfg.bin"		;control flow graph
		"ctypes.bin"		;CFG datatypes
		"dtypes.bin"		;DFG datatypes
		"bblock.bin"		;Basic block datatype
		"dfg.bin"		;data flow graph
		"rtl.bin"		;register transfer language
		"emodel.bin"		;environment model
		"rtypes.bin"		;RTL analyzer datatypes
		"nmatch.bin"		;simple pattern matcher
		)

	       (CONVERTER-PACKAGE
		"graphc.bin"		;SCode->flow-graph converter
		)

	       (DATAFLOW-PACKAGE
		"dflow.bin"		;Dataflow analyzer
		)

	       (RTL-GENERATOR-PACKAGE
		"rtlgen.bin"		;RTL generator
		"rgcomb.bin"		;RTL generator: combinations
		"linear.bin"		;linearization
		)

	       (RTL-CSE-PACKAGE
		"rcse.bin"		;RTL common subexpression eliminator
		)

	       (RTL-ANALYZER-PACKAGE
		"rlife.bin"		;RTL register lifetime analyzer
		"ralloc.bin"		;RTL register allocator
		)

	       (LAP-GENERATOR-PACKAGE
		"lapgen.bin"		;LAP generator.
		"regmap.bin"		;Hardware register allocator.
		"spectrum/lapgen.bin"	;code generation rules.
		)

	       (LAP-SYNTAXER-PACKAGE
		"syntax.bin"		;Generic syntax phase
		"spectrum/insutl.bin"	;Utilities for spectrum
		"spectrum/coerce.bin"	;Coercions: integer -> bit string
		"asmmac.bin"		;Macros for hairy syntax
		"spectrum/instrs.bin"	;Spectrum instructions
		)

	       (LAP-PACKAGE
		"spectrum/assmd.bin"	;Machine dependent
		"symtab.bin"		;Symbol tables
		"block.bin"		;Assembly blocks
		"laptop.bin"		;Assembler top level
		"spectrum/asmops.bin"	;Spectrum assembly operators
		)

	       ))

(in-package compiler-package

  (define compiler-system
    (make-environment
      (define :name "Liar (Spectrum)")
      (define :version)
      (define :modification)

      (parse-rcs-header "$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/spectrum/make.scm,v 1.3 1987/03/19 00:56:02 cph Exp $"
	(lambda (filename version date time author state)
	  (set! :version (car version))
	  (set! :modification (cadr version))))))

  (add-system! compiler-system))

(%ge compiler-package)
(%gst (access compiler-syntax-table compiler-package))
(disk-save "$zcomp/machines/spectrum/compiler")
#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sf/make.scm,v 3.3 1987/03/13 04:12:41 cph Exp $

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

;;;; SCode Optimizer: System Construction

(in-package system-global-environment
(declare (usual-integrations))

(define sf)
(define sf/set-file-syntax-table!)
(define sf/add-file-declarations!)
(load "$zcomp/base/load" system-global-environment)

(load-system system-global-environment
	     'PACKAGE/SCODE-OPTIMIZER
	     '(SYSTEM-GLOBAL-ENVIRONMENT)
	     '(
	       (PACKAGE/SCODE-OPTIMIZER
		"mvalue"		;Multiple Value Support
		"eqsets"		;Set Data Abstraction

		"object"		;Data Structures
		"emodel"		;Environment Model
		"gconst"		;Global Primitives List
		"usicon"		;Usual Integrations: Constants
		"tables"		;Table Abstractions
		"packag"		;Global packaging
		)

	       (PACKAGE/TOP-LEVEL
		"toplev"		;Top Level
		)

	       (PACKAGE/TRANSFORM
		"xform"			;SCode -> Internal
		)

	       (PACKAGE/INTEGRATE
		"subst"			;Beta Substitution Optimizer
		)

	       (PACKAGE/CGEN
		"cgen"			;Internal -> SCode
		)

	       (PACKAGE/EXPANSION
		"usiexp"		;Usual Integrations: Expanders
		)

	       (PACKAGE/DECLARATIONS
		"pardec"		;Declaration Parser
		)

	       (PACKAGE/COPY
		"copy"			;Copy Expressions
		)

	       (PACKAGE/FREE
		"free"			;Free Variable Analysis
		)

	       (PACKAGE/SAFE?
		"safep"			;Safety Analysis
		)

	       (PACKAGE/CHANGE-TYPE
		"chtype"		;Type interning
		)

	       ))

(in-package package/scode-optimizer
  (define integrations
    "$zcomp/source/object")

  (define scode-optimizer/system
    (make-environment
      (define :name "SF")
      (define :version 3)
      (define :modification 2)))

  (add-system! scode-optimizer/system)

  (scode-optimizer/initialize!))

;;; end IN-PACKAGE SYSTEM-GLOBAL-ENVIRONMENT
)
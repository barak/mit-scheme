#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v8/src/sf/make.scm,v 3.12 1987/07/08 04:55:33 jinx Rel $

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
(define sfu? false)
(define sf/set-file-syntax-table!)
(define sf/add-file-declarations!)

(define package/scode-optimizer
  (make-environment
    (define package/top-level	(make-environment))
    (define package/transform	(make-environment))
    (define package/integrate	(make-environment))
    (define package/cgen	(make-environment))
    (define package/expansion	(make-environment))
    (define package/declarations (make-environment))
    (define package/copy	(make-environment))
    (define package/free	(make-environment))
    (define package/change-type	(make-environment))))

(in-package package/scode-optimizer

  (define scode-optimizer/system
    (make-environment
      (define :name "SF")
      (define :version 3)
      (define :modification 12)
      (define :files)

      (define :files-lists
	(list
	 (cons package/scode-optimizer
	       '("mvalue.bin"		;Multiple Value Support
		 "eqsets.bin"		;Set Data Abstraction
		 "pthmap.bin"		;Pathname Map Abstraction
		 "object.bin"		;Data Structures
		 "emodel.bin"		;Environment Model
		 "gconst.bin"		;Global Primitives List
		 "usicon.bin"		;Usual Integrations: Constants
		 "tables.bin"		;Table Abstractions
		 "packag.bin"		;Global packaging
		 ))
	 (cons package/top-level
	       '("toplev.bin"))		;Top Level
	 (cons package/transform
	       '("xform.bin"))		;SCode -> Internal
	 (cons package/integrate
	       '("subst.bin"))		;Beta Substitution Optimizer
	 (cons package/cgen
	       '("cgen.bin"))		;Internal -> SCode
	 (cons package/expansion
	       '("usiexp.bin"))		;Usual Integrations: Expanders
	 (cons package/declarations
	       '("pardec.bin"))		;Declaration Parser
	 (cons package/copy
	       '("copy.bin"))		;Copy Expressions
	 (cons package/free
	       '("free.bin"))		;Free Variable Analysis
	 (cons package/change-type
	       '("chtype.bin"))		;Type interning
	 ))))

  (load-system! scode-optimizer/system true)

  (scode-optimizer/initialize!))

;;; end IN-PACKAGE SYSTEM-GLOBAL-ENVIRONMENT
)
;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Machine Dependent Type Tables

;;; $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v8/src/microcode/utabmd.scm,v 9.48 1989/09/24 14:47:07 cph Exp $

(declare (usual-integrations))

;;; For quick access to any given table,
;;; search for the following strings:
;;;
;;; [] Fixed
;;; [] Types
;;; [] Returns
;;; [] Errors
;;; [] Identification

;;; [] Fixed

(vector-set! (get-fixed-objects-vector)
	     #x0F ;(fixed-objects-vector-slot 'MICROCODE-FIXED-OBJECTS-SLOTS)
	     #(NON-OBJECT				;00
	       SYSTEM-INTERRUPT-VECTOR			;01
	       SYSTEM-ERROR-VECTOR			;02
	       OBARRAY					;03
	       MICROCODE-TYPES-VECTOR			;04
	       MICROCODE-RETURNS-VECTOR			;05
	       #F					;06
	       MICROCODE-ERRORS-VECTOR			;07
	       MICROCODE-IDENTIFICATION-VECTOR		;08
	       #F					;09
	       #F					;0A
	       GC-DAEMON				;0B
	       TRAP-HANDLER				;0C
	       #F					;0D
	       STEPPER-STATE				;0E
	       MICROCODE-FIXED-OBJECTS-SLOTS		;0F
	       #F					;10
	       STATE-SPACE-TAG				;11
	       STATE-POINT-TAG				;12
	       DUMMY-HISTORY				;13
               BIGNUM-ONE				;14
	       SCHEDULER				;15
	       MICROCODE-TERMINATIONS-VECTOR            ;16
	       MICROCODE-TERMINATIONS-PROCEDURES        ;17
	       FIXED-OBJECTS-VECTOR			;18
	       THE-WORK-QUEUE				;19
	       FUTURE-READS-LOGGER			;1A
	       TOUCHED-FUTURES-VECTOR			;1B
	       PRECIOUS-OBJECTS				;1C
	       ERROR-PROCEDURE				;1D
	       #F #| UNSNAPPED-LINK |#	                ;1E
	       #F #| MICROCODE-UTILITIES-VECTOR |#	;1F
	       COMPILER-ERROR-PROCEDURE			;20
	       LOST-OBJECT-BASE				;21
	       STATE-SPACE-ROOT				;22
	       PRIMITIVE-PROFILING-TABLE		;23
	       ))

;;; [] Types

(vector-set! (get-fixed-objects-vector)
	     4 ;(fixed-objects-vector-slot 'MICROCODE-TYPES-VECTOR)
	     #((NULL FALSE MANIFEST-VECTOR GLOBAL-ENVIRONMENT) ;00
	       (PAIR LIST)				;01
	       CHARACTER		       		;02
	       QUOTATION				;03
	       PRIMITIVE-COMBINATION-2 	                ;04
	       UNINTERNED-SYMBOL			;05
	       (FLONUM BIG-FLONUM)			;06
	       COMBINATION-1				;07
	       TRUE					;08
	       EXTENDED-PROCEDURE			;09		
	       VECTOR					;0A
	       (RETURN-CODE RETURN-ADDRESS)		;0B
	       COMBINATION-2				;0C
	       MANIFEST-CLOSURE 	       		;0D
	       (BIGNUM BIG-FIXNUM)			;0E
	       PROCEDURE				;0F
	       (ENTITY)					;10
	       DELAY					;11
	       ENVIRONMENT		      		;12
	       (PROMISE DELAYED)			;13
	       EXTENDED-LAMBDA				;14
	       COMMENT					;15
	       NON-MARKED-VECTOR			;16
	       LAMBDA					;17
	       PRIMITIVE				;18
	       SEQUENCE-2				;19
	       (FIXNUM ADDRESS)		       		;1A
	       PRIMITIVE-COMBINATION-1			;1B
	       CONTROL-POINT	       			;1C
	       INTERNED-SYMBOL				;1D
	       (STRING CHARACTER-STRING VECTOR-8B)	;1E
	       ACCESS					;1F
	       (HUNK3-A UNMARKED-HISTORY)		;20
	       DEFINITION				;21
	       BROKEN-HEART		       		;22
	       ASSIGNMENT				;23
	       (TRIPLE HUNK3 HUNK3-B MARKED-HISTORY)	;24
	       IN-PACKAGE				;25
	       COMBINATION	       			;26
	       MANIFEST-NM-VECTOR	       		;27
	       COMPILED-ENTRY				;28
	       LEXPR					;29
	       PRIMITIVE-COMBINATION-3		       	;2A
	       MANIFEST-SPECIAL-NM-VECTOR	  	;2B
	       VARIABLE					;2C
	       THE-ENVIRONMENT	      			;2D
	       FUTURE					;2E
	       VECTOR-1B	          		;2F
	       PRIMITIVE-COMBINATION-0	       	       	;30
	       VECTOR-16B		       		;31
	       (REFERENCE-TRAP UNASSIGNED)     		;32
	       SEQUENCE-3	       			;33
	       CONDITIONAL				;34
	       DISJUNCTION				;35
	       CELL					;36
	       WEAK-CONS				;37
	       QUAD        				;38
	       LINKAGE-SECTION				;39
	       RATNUM					;3A
	       STACK-ENVIRONMENT			;3B
	       (RECNUM COMPLEX)				;3C
	       COMPILED-CODE-BLOCK			;3D
	       #F					;3E
	       #F					;3F
	       #F        				;40
	       #F					;41
	       #F					;42
	       #F					;43
	       #F	                		;44
	       #F					;45
	       #F        				;46
	       #F					;47
	       #F					;48
	       #F               			;49
	       #F					;4A
	       #F					;4B
	       #F					;4C
	       #F					;4D
	       #F					;4E
	       #F					;4F
	       #F       				;50
	       #F					;51
	       #F					;52
	       #F					;53
	       #F					;54
	       #F					;55
	       #F		        		;56
	       #F					;57
	       #F         				;58
	       #F					;59
	       #F					;5A
	       #F					;5B
	       #F					;5C
	       #F					;5D
	       #F        				;5E
	       #F					;5F
	       #F               			;60
	       #F					;61
	       #F					;62
	       #F					;63
	       #F					;64
	       #F					;65
	       #F                       		;66
	       #F			 		;67
	       #F                       		;68
	       #F					;69
	       #F					;6A
	       #F					;6B
	       #F					;6C
	       #F					;6D
	       #F					;6E
	       #F					;6F
	       #F                			;70
	       #F					;71
	       #F					;72
	       #F					;73
	       #F					;74
	       #F					;75
	       #F                                       ;76
	       #F					;77
	       #F                			;78
	       #F					;79
	       #F					;7A
	       #F					;7B
	       #F					;7C
	       #F					;7D
	       #F                        		;7E
	       #F        				;7F
	       #F                			;80
	       #F					;81
	       #F					;82
	       #F					;83
	       #F					;84
	       #F					;85
	       #F                                       ;86
	       #F					;87
	       #F                			;88
	       #F					;89
	       #F					;8A
	       #F					;8B
	       #F					;8C
	       #F					;8D
	       #F                        		;8E
	       #F        				;8F
	       #F                			;90
	       #F					;91
	       #F					;92
	       #F					;93
	       #F					;94
	       #F					;95
	       #F                                       ;96
	       #F					;97
	       #F                			;98
	       #F					;99
	       #F					;9A
	       #F					;9B
	       #F					;9C
	       #F					;9D
	       #F                        		;9E
	       #F        				;9F
	       #F                			;A0
	       #F					;A1
	       #F					;A2
	       #F					;A3
	       #F					;A4
	       #F					;A5
	       #F                                       ;A6
	       #F					;A7
	       #F                			;A8
	       #F					;A9
	       #F					;AA
	       #F					;AB
	       #F					;AC
	       #F					;AD
	       #F                        		;AE
	       #F        				;AF
	       #F                			;B0
	       #F					;B1
	       #F					;B2
	       #F					;B3
	       #F					;B4
	       #F					;B5
	       #F                                       ;B6
	       #F					;B7
	       #F                			;B8
	       #F					;B9
	       #F					;BA
	       #F					;BB
	       #F					;BC
	       #F					;BD
	       #F                        		;BE
	       #F        				;BF
	       #F                			;C0
	       #F					;C1
	       #F					;C2
	       #F					;C3
	       #F					;C4
	       #F					;C5
	       #F                                       ;C6
	       #F					;C7
	       #F                			;C8
	       #F					;C9
	       #F					;CA
	       #F					;CB
	       #F					;CC
	       #F					;CD
	       #F                        		;CE
	       #F        				;CF
	       #F                			;D0
	       #F					;D1
	       #F					;D2
	       #F					;D3
	       #F					;D4
	       #F					;D5
	       #F                                       ;D6
	       #F					;D7
	       #F                			;D8
	       #F					;D9
	       #F					;DA
	       #F					;DB
	       #F					;DC
	       #F					;DD
	       #F                        		;DE
	       #F        				;DF
	       #F                			;E0
	       #F					;E1
	       #F					;E2
	       #F					;E3
	       #F					;E4
	       #F					;E5
	       #F                                       ;E6
	       #F					;E7
	       #F                			;E8
	       #F					;E9
	       #F					;EA
	       #F					;EB
	       #F					;EC
	       #F					;ED
	       #F                        		;EE
	       #F        				;EF
	       #F                			;F0
	       #F					;F1
	       #F					;F2
	       #F					;F3
	       #F					;F4
	       #F					;F5
	       #F                                       ;F6
	       #F					;F7
	       #F                			;F8
	       #F					;F9
	       #F					;FA
	       #F					;FB
	       #F					;FC
	       #F					;FD
	       #F                        		;FE
	       #F        				;FF
	       ))

;;; [] Returns

(vector-set! (get-fixed-objects-vector)
	     5 ;(fixed-objects-vector-slot 'MICROCODE-RETURNS-VECTOR)
	     #(NON-EXISTENT-CONTINUATION		;00
	       JOIN-STACKLETS				;01
	       RESTORE-CONTINUATION			;02
	       INTERNAL-APPLY				;03
	       BAD-INTERRUPT-CONTINUE			;04
	       RESTORE-HISTORY				;05
	       INVOKE-STACK-THREAD			;06
	       RESTART-EXECUTION			;07
	       ASSIGNMENT-CONTINUE			;08
	       DEFINITION-CONTINUE			;09
	       ACCESS-CONTINUE				;0A
	       IN-PACKAGE-CONTINUE			;0B
	       SEQUENCE-2-SECOND			;0C
	       SEQUENCE-3-SECOND			;0D
	       SEQUENCE-3-THIRD				;0E
	       CONDITIONAL-DECIDE			;0F
	       DISJUNCTION-DECIDE			;10
	       COMBINATION-1-PROCEDURE			;11
	       COMBINATION-APPLY			;12
	       COMBINATION-2-FIRST-OPERAND		;13
	       COMBINATION-2-PROCEDURE			;14
	       COMBINATION-SAVE-VALUE			;15
	       PRIMITIVE-COMBINATION-1-APPLY		;16
	       PRIMITIVE-COMBINATION-2-FIRST-OPERAND	;17
	       PRIMITIVE-COMBINATION-2-APPLY		;18
	       PRIMITIVE-COMBINATION-3-SECOND-OPERAND	;19
	       PRIMITIVE-COMBINATION-3-FIRST-OPERAND	;1A
	       PRIMITIVE-COMBINATION-3-APPLY		;1B
	       FORCE-SNAP-THUNK				;1C
	       REENTER-COMPILED-CODE			;1D
	       #F					;1E
	       COMPILER-REFERENCE-RESTART 		;1F
	       NORMAL-GARBAGE-COLLECT-DONE		;20
	       COMPLETE-GARBAGE-COLLECT-DONE		;21
	       PURIFY-AFTER-FIRST-GC			;22
	       PURIFY-AFTER-SECOND-GC			;23
	       AFTER-MEMORY-UPDATE			;24
	       RETRY-MICROCODE-TERMINATION-RESTARTABLE	;25
	       #F					;26
	       #F					;27
	       COMPILER-ASSIGNMENT-RESTART		;28
	       POP-FROM-COMPILED-CODE			;29
	       RETURN-TRAP-POINT			;2A
	       RESTORE-STEPPER				;2B
	       RESTORE-TO-STATE-POINT			;2C
	       MOVE-TO-ADJACENT-POINT			;2D
	       RESTORE-VALUE				;2E
	       RESTORE-DONT-COPY-HISTORY		;2F
	       #F					;30
	       #F					;31
	       #F					;32
	       #F					;33
	       #F					;34
	       #F					;35
	       #F					;36
	       #F					;37
	       #F					;38
	       #F					;39
	       #F					;3A
	       #F					;3B
	       #F					;3C
	       #F					;3D
	       #F					;3E
	       #F					;3F
	       POP-RETURN-ERROR				;40
	       EVAL-ERROR				;41
	       REPEAT-PRIMITIVE				;42
	       COMPILER-INTERRUPT-RESTART		;43
	       #F					;44
	       RESTORE-INTERRUPT-MASK			;45
	       HALT					;46
	       FINISH-GLOBAL-INTERRUPT			;47
	       REPEAT-DISPATCH				;48
	       GC-CHECK					;49
	       RESTORE-FLUIDS				;4A
	       COMPILER-LOOKUP-APPLY-RESTART		;4B
	       COMPILER-ACCESS-RESTART			;4C
	       COMPILER-UNASSIGNED?-RESTART		;4D
	       COMPILER-UNBOUND?-RESTART		;4E
	       COMPILER-DEFINITION-RESTART		;4F
	       #F					;50
	       COMPILER-SAFE-REFERENCE-RESTART		;51
	       #F					;52
	       COMPILER-REFERENCE-TRAP-RESTART		;53
	       COMPILER-ASSIGNMENT-TRAP-RESTART		;54
	       #F					;55
	       COMPILER-OPERATOR-LOOKUP-TRAP-RESTART	;56
	       COMPILER-LOOKUP-APPLY-TRAP-RESTART	;57
	       COMPILER-SAFE-REFERENCE-TRAP-RESTART	;58
	       COMPILER-UNASSIGNED?-TRAP-RESTART	;59
	       #F					;5A
	       COMPILER-LINK-CACHES-RESTART 		;5B
	       HARDWARE-TRAP				;5C
	       ))

;;; [] Errors

(vector-set! (get-fixed-objects-vector)
	     7	;(fixed-objects-vector-slot 'MICROCODE-ERRORS-VECTOR)
	     #(BAD-ERROR-CODE				;00
	       UNBOUND-VARIABLE				;01
	       UNASSIGNED-VARIABLE			;02
	       UNDEFINED-PROCEDURE			;03
	       #F					;04
	       #F					;05
	       BAD-FRAME				;06
	       BROKEN-CVARIABLE				;07
	       UNDEFINED-USER-TYPE			;08
	       UNDEFINED-PRIMITIVE-OPERATION		;09
	       EXTERNAL-RETURN				;0A
	       EXECUTE-MANIFEST-VECTOR			;0B
	       WRONG-NUMBER-OF-ARGUMENTS		;0C
	       WRONG-TYPE-ARGUMENT-0			;0D
	       WRONG-TYPE-ARGUMENT-1			;0E
	       WRONG-TYPE-ARGUMENT-2			;0F
	       BAD-RANGE-ARGUMENT-0			;10
	       BAD-RANGE-ARGUMENT-1			;11
	       BAD-RANGE-ARGUMENT-2			;12
	       #F					;13
	       #F					;14
	       BAD-INTERRUPT-CODE			;15
	       #F					;16
	       FASL-FILE-TOO-BIG			;17
	       FASL-FILE-BAD-DATA			;18
	       IMPURIFY-OBJECT-TOO-LARGE		;19
	       WRITE-INTO-PURE-SPACE                    ;1A
	       #F		                        ;1B
	       #F					;1C
	       BAD-ASSIGNMENT				;1D
	       FAILED-ARG-1-COERCION                    ;1E
	       FAILED-ARG-2-COERCION                    ;1F
	       OUT-OF-FILE-HANDLES			;20
	       #F					;21
	       BAD-RANGE-ARGUMENT-3			;22
	       BAD-RANGE-ARGUMENT-4			;23
	       BAD-RANGE-ARGUMENT-5			;24
	       BAD-RANGE-ARGUMENT-6			;25
	       BAD-RANGE-ARGUMENT-7			;26
	       BAD-RANGE-ARGUMENT-8			;27
	       BAD-RANGE-ARGUMENT-9			;28
	       WRONG-TYPE-ARGUMENT-3			;29
	       WRONG-TYPE-ARGUMENT-4			;2A
	       WRONG-TYPE-ARGUMENT-5			;2B
	       WRONG-TYPE-ARGUMENT-6			;2C
	       WRONG-TYPE-ARGUMENT-7			;2D
	       WRONG-TYPE-ARGUMENT-8			;2E
	       WRONG-TYPE-ARGUMENT-9			;2F
	       INAPPLICABLE-CONTINUATION		;30
	       COMPILED-CODE-ERROR			;31
	       FLOATING-OVERFLOW			;32
	       UNIMPLEMENTED-PRIMITIVE			;33
	       ILLEGAL-REFERENCE-TRAP			;34
	       BROKEN-VARIABLE-CACHE			;35
	       WRONG-ARITY-PRIMITIVES			;36
	       IO-ERROR					;37
	       FASDUMP-ENVIRONMENT			;38
	       FASLOAD-BAND				;39
	       FASLOAD-COMPILED-MISMATCH		;3A
	       ))

;;; [] Terminations

(vector-set! (get-fixed-objects-vector)
	     22 ;(fixed-objects-vector-slot 'MICROCODE-TERMINATIONS-VECTOR)
	     #(HALT                             ;00
	       DISK-RESTORE                     ;01
	       BROKEN-HEART                     ;02
	       NON-POINTER-RELOCATION           ;03
	       BAD-ROOT                         ;04
	       NON-EXISTENT-CONTINUATION        ;05
	       BAD-STACK                        ;06
	       STACK-OVERFLOW                   ;07
	       STACK-ALLOCATION-FAILED          ;08
	       NO-ERROR-HANDLER                 ;09
	       NO-INTERRUPT-HANDLER             ;0A
	       UNIMPLEMENTED-CONTINUATION       ;0B
	       EXIT                             ;0C
	       BAD-PRIMITIVE-DURING-ERROR       ;0D
	       EOF                              ;0E
	       BAD-PRIMITIVE                    ;0F
	       TERMINATION-HANDLER		;10
	       END-OF-CONTINUATION              ;11
	       INVALID-TYPE-CODE		;12
	       COMPILER-DEATH			;13
	       GC-OUT-OF-SPACE			;14
	       NO-SPACE				;15
	       SIGNAL				;16
	       TOUCH				;17
	       SAVE-AND-EXIT			;18
	       TRAP				;19
	       BAD-BACK-OUT			;20
	       ))

(vector-set! (get-fixed-objects-vector)
	     23 ;(fixed-objects-vector-slot 'MICROCODE-TERMINATION-PROCEDURES)
	     #())

;;; [] Identification

(vector-set! (get-fixed-objects-vector)
	     8 ;(fixed-objects-vector-slot 'MICROCODE-IDENTIFICATION-VECTOR)
	     #(SYSTEM-RELEASE-STRING		;00
	       MICROCODE-VERSION		;01
	       MICROCODE-MODIFICATION		;02
	       CONSOLE-WIDTH			;03
	       CONSOLE-HEIGHT			;04
	       NEWLINE-CHAR			;05
	       FLONUM-MANTISSA-LENGTH		;06
	       FLONUM-EPSILON			;07
	       OS-NAME-STRING			;08
	       OS-VARIANT-STRING		;09
	       STACK-TYPE-STRING		;0A
	       ))

;;; This identification string is saved by the system.

"$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v8/src/microcode/utabmd.scm,v 9.48 1989/09/24 14:47:07 cph Exp $"
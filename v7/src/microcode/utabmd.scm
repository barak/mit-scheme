#| -*-Scheme-*-

$Id: utabmd.scm,v 9.87 2003/02/14 18:28:24 cph Exp $

Copyright 1987-2002 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Machine Dependent Type Tables

(declare (usual-integrations))

;;; For quick access to any given table,
;;; search for the following strings:
;;;
;;; [] Fixed
;;; [] Types
;;; [] Returns
;;; [] Errors
;;; [] Terminations
;;; [] System-call names
;;; [] System-call errors
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
	       INTERRUPT-MASK-VECTOR			;06
	       MICROCODE-ERRORS-VECTOR			;07
	       MICROCODE-IDENTIFICATION-VECTOR		;08
	       SYSTEM-CALL-NAMES			;09
	       SYSTEM-CALL-ERRORS			;0A
	       GC-DAEMON				;0B
	       TRAP-HANDLER				;0C
	       EDWIN-AUTO-SAVE				;0D
	       STEPPER-STATE				;0E
	       MICROCODE-FIXED-OBJECTS-SLOTS		;0F
	       FILES-TO-DELETE				;10
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
	       GENERIC-TRAMPOLINE-ZERO?			;24
	       GENERIC-TRAMPOLINE-POSITIVE?		;25
	       GENERIC-TRAMPOLINE-NEGATIVE?		;26
	       GENERIC-TRAMPOLINE-ADD-1			;27
	       GENERIC-TRAMPOLINE-SUBTRACT-1		;28
	       GENERIC-TRAMPOLINE-EQUAL?		;29
	       GENERIC-TRAMPOLINE-LESS?			;2A
	       GENERIC-TRAMPOLINE-GREATER?		;2B
	       GENERIC-TRAMPOLINE-ADD			;2C
	       GENERIC-TRAMPOLINE-SUBTRACT		;2D
	       GENERIC-TRAMPOLINE-MULTIPLY		;2E
	       GENERIC-TRAMPOLINE-DIVIDE		;2F
	       GENERIC-TRAMPOLINE-QUOTIENT 		;30
	       GENERIC-TRAMPOLINE-REMAINDER 		;31
	       GENERIC-TRAMPOLINE-MODULO 		;32
	       ARITY-DISPATCHER-TAG			;33
	       PC-Sample/Builtin-Table			;34
	       PC-Sample/Utility-Table			;35
	       PC-Sample/Primitive-Table		;36
	       PC-Sample/Code-Block-Table		;37
	       PC-Sample/Purified-Code-Block-Block-Buffer  ;38
	       PC-Sample/Purified-Code-Block-Offset-Buffer ;39
	       PC-Sample/Heathen-Code-Block-Block-Buffer   ;3A
	       PC-Sample/Heathen-Code-Block-Offset-Buffer  ;3B
	       PC-Sample/Interp-Proc-Buffer		;3C
	       PC-Sample/Prob-Comp-Table		;3D
	       PC-Sample/UFO-Table			;3E
	       COMPILED-CODE-BKPT-HANDLER		;3F
	       GC-WABBIT-DESCWIPTOR			;40
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
	       (TRUE CONSTANT)				;08
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
	       (FIXNUM ADDRESS POSITIVE-FIXNUM NEGATIVE-FIXNUM) ;1A
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
	       RECORD					;3E
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
	       STACK-MARKER				;42
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
	       INTERNAL-APPLY-VAL			;5D
	       COMPILER-ERROR-RESTART			;5E
	       PRIMITIVE-CONTINUE			;5F
	       ))

;;; [] Errors

(vector-set! (get-fixed-objects-vector)
	     7	;(fixed-objects-vector-slot 'MICROCODE-ERRORS-VECTOR)
	     #(BAD-ERROR-CODE				;00
	       UNBOUND-VARIABLE				;01
	       UNASSIGNED-VARIABLE			;02
	       UNDEFINED-PROCEDURE			;03
	       SYSTEM-CALL				;04
	       ERROR-WITH-ARGUMENT			;05
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
	       MACRO-BINDING				;13
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
	       UNKNOWN-PRIMITIVE-CONTINUATION		;3B
	       ILLEGAL-CONTINUATION			;3C
	       STACK-HAS-SLIPPED			;3D
	       CANNOT-RECURSE				;3E
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

;;; [] System-call names and errors

(let-syntax
    ((ucode-primitive
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 (apply make-primitive-procedure (cdr form))))))
  (vector-set! (get-fixed-objects-vector)
	       #x09 ;(fixed-objects-vector-slot 'SYSTEM-CALL-NAMES)
	       ((ucode-primitive microcode-system-call-names 0)))
  (vector-set! (get-fixed-objects-vector)
	       #x0A ;(fixed-objects-vector-slot 'SYSTEM-CALL-ERRORS)
	       ((ucode-primitive microcode-system-error-names 0))))

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

"$Id: utabmd.scm,v 9.87 2003/02/14 18:28:24 cph Exp $"

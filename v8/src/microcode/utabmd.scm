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

;;; $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v8/src/microcode/utabmd.scm,v 9.32 1987/06/03 20:59:23 cph Exp $

(declare (usual-integrations))

;;; For quick access to any given table,
;;; search for the following strings:
;;;
;;; [] Fixed
;;; [] Types
;;; [] Returns
;;; [] Primitives
;;; [] External
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
	       MICROCODE-PRIMITIVES-VECTOR		;06
	       MICROCODE-ERRORS-VECTOR			;07
	       MICROCODE-IDENTIFICATION-VECTOR		;08
	       #F					;09
	       #F					;0A
	       GC-DAEMON				;0B
	       TRAP-HANDLER				;0C
	       #F					;0D
	       STEPPER-STATE				;0E
	       MICROCODE-FIXED-OBJECTS-SLOTS		;0F
	       MICROCODE-EXTERNAL-PRIMITIVES		;10
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
	       UNSNAPPED-LINK		                ;1E
	       MICROCODE-UTILITIES-VECTOR		;1F
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
	       COMPILED-PROCEDURE 	       		;0D
	       (BIGNUM BIG-FIXNUM)			;0E
	       PROCEDURE				;0F
	       PRIMITIVE-EXTERNAL			;10
	       DELAY					;11
	       ENVIRONMENT		      		;12
	       DELAYED					;13
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
	       #F					;20
	       DEFINITION				;21
	       BROKEN-HEART		       		;22
	       ASSIGNMENT				;23
	       (TRIPLE HUNK3)				;24
	       IN-PACKAGE				;25
	       COMBINATION	       			;26
	       MANIFEST-NM-VECTOR	       		;27
	       COMPILED-EXPRESSION			;28
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
	       COMPILER-RETURN-ADDRESS			;39
	       COMPILER-LINK				;3A
	       STACK-ENVIRONMENT			;3B
	       COMPLEX       				;3C
	       #F					;3D
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
	       COMPILER-LEXPR-INTERRUPT-RESTART		;50
	       COMPILER-SAFE-REFERENCE-RESTART		;51
	       COMPILER-CACHE-VARIABLE-RESTART		;52
	       COMPILER-REFERENCE-TRAP-RESTART		;53
	       COMPILER-ASSIGNMENT-TRAP-RESTART		;54
	       COMPILER-UUO-LINK-RESTART		;55
	       COMPILER-UUO-LINK-TRAP-RESTART		;56
	       COMPILER-CACHE-REFERENCE-APPLY-RESTART	;57
	       COMPILER-SAFE-REFERENCE-TRAP-RESTART	;58
	       COMPILER-UNASSIGNED?-TRAP-RESTART	;59
	       ))

;;; [] Primitives

(vector-set! (get-fixed-objects-vector)
	     6 ;(fixed-objects-vector-slot 'MICROCODE-PRIMITIVES-VECTOR)
	     #(LEXICAL-ASSIGNMENT			;$00
	       LOCAL-REFERENCE				;$01
	       LOCAL-ASSIGNMENT				;$02
	       CALL-WITH-CURRENT-CONTINUATION		;$03
	       SCODE-EVAL				;$04
	       APPLY					;$05
	       SET-INTERRUPT-ENABLES!			;$06
	       STRING->SYMBOL				;$07
	       GET-WORK					;$08
	       NON-REENTRANT-CALL-WITH-CURRENT-CONTINUATION	;$09
	       CURRENT-DYNAMIC-STATE			;$0A
	       SET-CURRENT-DYNAMIC-STATE!		;$0B
	       (NULL? NOT FALSE?)			;$0C
	       EQ?					;$0D
	       STRING-EQUAL?				;$0E
	       PRIMITIVE-TYPE?				;$0F
	       PRIMITIVE-TYPE				;$10
	       PRIMITIVE-SET-TYPE			;$11
	       LEXICAL-REFERENCE			;$12
	       LEXICAL-UNREFERENCEABLE?			;$13
	       MAKE-CHAR				;$14
	       CHAR-BITS				;$15
	       EXIT					;$16
	       CHAR-CODE				;$17
	       LEXICAL-UNASSIGNED?			;$18
	       INSERT-NON-MARKED-VECTOR!		;$19
	       HALT					;$1A
	       CHAR->INTEGER				;$1B
	       MEMQ					;$1C
	       INSERT-STRING				;$1D
	       ENABLE-INTERRUPTS!			;$1E
	       MAKE-EMPTY-STRING			;$1F
	       CONS					;$20
	       (CAR FIRST)				;$21
	       (CDR FIRST-TAIL)				;$22
	       (SET-CAR! SET-FIRST!)			;$23
	       (SET-CDR! SET-FIRST-TAIL!)		;$24
	       #F					;$25
	       TTY-GET-CURSOR				;$26
	       GENERAL-CAR-CDR				;$27
	       HUNK3-CONS				;$28
	       HUNK3-CXR				;$29
	       HUNK3-SET-CXR!				;$2A
	       INSERT-STRING!				;$2B
	       VECTOR-CONS				;$2C
	       (VECTOR-LENGTH VECTOR-SIZE)		;$2D
	       VECTOR-REF				;$2E
	       SET-CURRENT-HISTORY!			;$2F
	       VECTOR-SET!				;$30
	       NON-MARKED-VECTOR-CONS			;$31
	       #F					;$32
	       LEXICAL-UNBOUND?				;$33
	       INTEGER->CHAR				;$34
	       CHAR-DOWNCASE				;$35
	       CHAR-UPCASE				;$36
	       ASCII->CHAR				;$37
	       CHAR-ASCII?				;$38
	       CHAR->ASCII				;$39
	       GARBAGE-COLLECT				;$3A
	       PLUS-FIXNUM				;$3B
	       MINUS-FIXNUM				;$3C
	       MULTIPLY-FIXNUM				;$3D
	       DIVIDE-FIXNUM				;$3E
	       EQUAL-FIXNUM?				;$3F
	       LESS-THAN-FIXNUM?			;$40
	       POSITIVE-FIXNUM?				;$41
	       ONE-PLUS-FIXNUM				;$42
	       MINUS-ONE-PLUS-FIXNUM			;$43
	       TRUNCATE-STRING!				;$44
	       SUBSTRING				;$45
	       ZERO-FIXNUM?				;$46
	       MAKE-OBJECT-SAFE				;$47
	       MAKE-OBJECT-DANGEROUS			;$48
	       OBJECT-DANGEROUS?			;$49
	       SUBSTRING->LIST				;$4A
	       MAKE-FILLED-STRING			;$4B
	       PLUS-BIGNUM				;$4C
	       MINUS-BIGNUM				;$4D
	       MULTIPLY-BIGNUM				;$4E
	       DIVIDE-BIGNUM				;$4F
	       LISTIFY-BIGNUM				;$50
	       EQUAL-BIGNUM?				;$51
	       LESS-THAN-BIGNUM?			;$52
	       POSITIVE-BIGNUM?				;$53
	       FILE-OPEN-CHANNEL			;$54
	       FILE-CLOSE-CHANNEL			;$55
	       PRIMITIVE-FASDUMP			;$56
	       BINARY-FASLOAD				;$57
	       STRING-POSITION				;$58
	       STRING-LESS?				;$59
	       #F					;$5A
	       #F					;$5B
	       REHASH					;$5C
	       LENGTH					;$5D
	       ASSQ					;$5E
	       LIST->STRING				;$5F
	       EQUAL-STRING-TO-LIST?			;$60
	       MAKE-CELL				;$61
	       CELL-CONTENTS				;$62
	       CELL?					;$63
	       CHARACTER-UPCASE 			;$64
	       CHARACTER-LIST-HASH			;$65
	       GCD-FIXNUM				;$66
	       COERCE-FIXNUM-TO-BIGNUM			;$67
	       COERCE-BIGNUM-TO-FIXNUM			;$68
	       PLUS-FLONUM				;$69
	       MINUS-FLONUM				;$6A
	       MULTIPLY-FLONUM				;$6B
	       DIVIDE-FLONUM				;$6C
	       EQUAL-FLONUM?				;$6D
	       LESS-THAN-FLONUM?			;$6E
	       ZERO-BIGNUM?				;$6F
	       TRUNCATE-FLONUM				;$70
	       ROUND-FLONUM				;$71
	       COERCE-INTEGER-TO-FLONUM			;$72
	       SINE-FLONUM				;$73
	       COSINE-FLONUM				;$74
	       ARCTAN-FLONUM				;$75
	       EXP-FLONUM				;$76
	       LN-FLONUM				;$77
	       SQRT-FLONUM				;$78
	       PRIMITIVE-FASLOAD			;$79
	       GET-FIXED-OBJECTS-VECTOR			;$7A
	       SET-FIXED-OBJECTS-VECTOR!		;$7B
	       LIST->VECTOR				;$7C
	       SUBVECTOR->LIST				;$7D
	       PAIR?					;$7E
	       NEGATIVE-FIXNUM?				;$7F
	       NEGATIVE-BIGNUM?				;$80
	       GREATER-THAN-FIXNUM?			;$81
	       GREATER-THAN-BIGNUM?			;$82
	       STRING-HASH				;$83
	       SYSTEM-PAIR-CONS				;$84
	       SYSTEM-PAIR?				;$85
	       SYSTEM-PAIR-CAR				;$86
	       SYSTEM-PAIR-CDR				;$87
	       SYSTEM-PAIR-SET-CAR!			;$88
	       SYSTEM-PAIR-SET-CDR!			;$89
	       STRING-HASH-MOD				;$8A
	       #F					;$8B
	       SET-CELL-CONTENTS!			;$8C
	       &MAKE-OBJECT				;$8D
	       SYSTEM-HUNK3-CXR0			;$8E
	       SYSTEM-HUNK3-SET-CXR0!			;$8F
	       MAP-MACHINE-ADDRESS-TO-CODE		;$90
	       SYSTEM-HUNK3-CXR1			;$91
	       SYSTEM-HUNK3-SET-CXR1!			;$92
	       MAP-CODE-TO-MACHINE-ADDRESS		;$93
	       SYSTEM-HUNK3-CXR2			;$94
	       SYSTEM-HUNK3-SET-CXR2!			;$95
	       PRIMITIVE-PROCEDURE-ARITY		;$96
	       SYSTEM-LIST-TO-VECTOR			;$97
	       SYSTEM-SUBVECTOR-TO-LIST			;$98
	       SYSTEM-VECTOR?				;$99
	       SYSTEM-VECTOR-REF			;$9A
	       SYSTEM-VECTOR-SET!			;$9B
	       WITH-HISTORY-DISABLED			;$9C
	       SUBVECTOR-MOVE-RIGHT!			;$9D
	       SUBVECTOR-MOVE-LEFT!			;$9E
	       SUBVECTOR-FILL!				;$9F
	       #F					;$A0
	       #F					;$A1
	       #F					;$A2
	       VECTOR-8B-CONS				;$A3
	       VECTOR-8B?				;$A4
	       VECTOR-8B-REF				;$A5
	       VECTOR-8B-SET!				;$A6
	       ZERO-FLONUM?				;$A7
	       POSITIVE-FLONUM?				;$A8
	       NEGATIVE-FLONUM?				;$A9
	       GREATER-THAN-FLONUM?			;$AA
	       INTERN-CHARACTER-LIST			;$AB
	       #F					;$AC
	       (STRING-SIZE VECTOR-8B-SIZE)		;$AD
	       SYSTEM-VECTOR-SIZE			;$AE
	       FORCE					;$AF
	       PRIMITIVE-DATUM				;$B0
	       MAKE-NON-POINTER-OBJECT			;$B1
	       DEBUGGING-PRINTER			;$B2
	       STRING-UPCASE     			;$B3
	       PRIMITIVE-PURIFY				;$B4
	       RETURN-ADDRESS-BLOCK			;$B5
	       COMPLETE-GARBAGE-COLLECT			;$B6
	       DUMP-BAND				;$B7
	       SUBSTRING-SEARCH				;$B8
	       LOAD-BAND				;$B9
	       CONSTANT?				;$BA
	       PURE?					;$BB
	       PRIMITIVE-GC-TYPE			;$BC
	       PRIMITIVE-IMPURIFY			;$BD
	       WITH-THREADED-CONTINUATION		;$BE
	       WITHIN-CONTROL-POINT			;$BF
	       SET-RUN-LIGHT!				;$C0
	       FILE-EOF?				;$C1
	       FILE-READ-CHAR				;$C2
	       FILE-FILL-INPUT-BUFFER			;$C3
	       FILE-LENGTH				;$C4
	       FILE-WRITE-CHAR				;$C5
	       FILE-WRITE-STRING			;$C6
	       CLOSE-LOST-OPEN-FILES			;$C7
	       #F					;$C8
	       WITH-INTERRUPTS-REDUCED			;$C9
	       PRIMITIVE-EVAL-STEP			;$CA
	       PRIMITIVE-APPLY-STEP			;$CB
	       PRIMITIVE-RETURN-STEP			;$CC
	       TTY-READ-CHAR-READY?			;$CD
	       TTY-READ-CHAR				;$CE
	       TTY-READ-CHAR-IMMEDIATE			;$CF
	       TTY-READ-FINISH				;$D0
	       BIT-STRING-ALLOCATE			;$D1
	       MAKE-BIT-STRING				;$D2
	       BIT-STRING?				;$D3
	       BIT-STRING-LENGTH			;$D4
	       BIT-STRING-REF				;$D5
	       BIT-SUBSTRING-MOVE-RIGHT!		;$D6
	       BIT-STRING-SET!				;$D7
	       BIT-STRING-CLEAR!			;$D8
	       BIT-STRING-ZERO?				;$D9
	       BIT-SUBSTRING-FIND-NEXT-SET-BIT		;$DA
	       #F					;$DB
	       UNSIGNED-INTEGER->BIT-STRING		;$DC
	       BIT-STRING->UNSIGNED-INTEGER		;$DD
	       #F					;$DE
	       READ-BITS!				;$DF
	       WRITE-BITS!				;$E0
	       MAKE-STATE-SPACE				;$E1
	       EXECUTE-AT-NEW-STATE-POINT		;$E2
	       TRANSLATE-TO-STATE-POINT			;$E3
	       GET-NEXT-CONSTANT			;$E4
	       MICROCODE-IDENTIFY			;$E5
	       ZERO?					;$E6
	       POSITIVE?				;$E7
	       NEGATIVE?				;$E8
	       &=					;$E9
	       &<					;$EA
	       &>					;$EB
	       &+					;$EC
	       &-					;$ED
	       &*					;$EE
	       &/					;$EF
	       INTEGER-DIVIDE				;$F0
	       1+					;$F1
	       -1+					;$F2
	       TRUNCATE					;$F3
	       ROUND					;$F4
	       FLOOR					;$F5
	       CEILING					;$F6
	       SQRT					;$F7
	       EXP					;$F8
	       LOG					;$F9
	       SIN					;$FA
	       COS					;$FB
	       &ATAN					;$FC
	       TTY-WRITE-CHAR				;$FD
	       TTY-WRITE-STRING				;$FE
               TTY-BEEP					;$FF
	       TTY-CLEAR				;$100
	       GET-EXTERNAL-COUNTS			;$101
	       GET-EXTERNAL-NAME			;$102
	       GET-EXTERNAL-NUMBER			;$103
	       #F					;$104
	       #F					;$105
	       GET-NEXT-INTERRUPT-CHARACTER		;$106
	       CHECK-AND-CLEAN-UP-INPUT-CHANNEL		;$107
	       #F					;$108
	       SYSTEM-CLOCK				;$109
	       FILE-EXISTS?				;$10A
	       #F					;$10B
	       TTY-MOVE-CURSOR				;$10C
	       #F					;$10D
	       CURRENT-DATE				;$10E
	       CURRENT-TIME				;$10F
	       TRANSLATE-FILE				;$110
	       COPY-FILE				;$111
	       RENAME-FILE				;$112
	       REMOVE-FILE				;$113
	       LINK-FILE				;$114
	       MAKE-DIRECTORY				;$115
	       VOLUME-NAME				;$116
	       SET-WORKING-DIRECTORY-PATHNAME!		;$117
	       OPEN-CATALOG				;$118
	       CLOSE-CATALOG				;$119
	       NEXT-FILE				;$11A
	       CAT-NAME					;$11B
	       CAT-KIND					;$11C
	       CAT-PSIZE				;$11D
	       CAT-LSIZE				;$11E
	       CAT-INFO					;$11F
	       CAT-BLOCK				;$120
	       CAT-CREATE-DATE				;$121
	       CAT-CREATE-TIME				;$122
	       CAT-LAST-DATE				;$123
	       CAT-LAST-TIME				;$124
	       ERROR-MESSAGE				;$125
	       CURRENT-YEAR				;$126
	       CURRENT-MONTH				;$127
	       CURRENT-DAY				;$128
	       CURRENT-HOUR				;$129
	       CURRENT-MINUTE				;$12A
	       CURRENT-SECOND				;$12B
	       INIT-FLOPPY				;$12C
	       ZERO-FLOPPY				;$12D
	       PACK-VOLUME				;$12E
	       LOAD-PICTURE				;$12F
	       STORE-PICTURE				;$130
	       LOOKUP-SYSTEM-SYMBOL			;$131
	       #F					;$132
	       #F					;$133
	       CLEAR-TO-END-OF-LINE			;$134
	       #F					;$135
	       #F					;$136
	       WITH-INTERRUPT-MASK			;$137
	       STRING?					;$138
	       STRING-LENGTH				;$139
	       STRING-REF				;$13A
	       STRING-SET!				;$13B
	       SUBSTRING-MOVE-RIGHT!			;$13C
	       SUBSTRING-MOVE-LEFT!			;$13D
	       STRING-ALLOCATE				;$13E
	       STRING-MAXIMUM-LENGTH			;$13F
	       SET-STRING-LENGTH!			;$140
	       VECTOR-8B-FILL!				;$141
	       VECTOR-8B-FIND-NEXT-CHAR			;$142
	       VECTOR-8B-FIND-PREVIOUS-CHAR		;$143
	       VECTOR-8B-FIND-NEXT-CHAR-CI		;$144
	       VECTOR-8B-FIND-PREVIOUS-CHAR-CI		;$145
	       SUBSTRING-FIND-NEXT-CHAR-IN-SET		;$146
	       SUBSTRING-FIND-PREVIOUS-CHAR-IN-SET	;$147
	       SUBSTRING=?				;$148
	       SUBSTRING-CI=?				;$149
	       SUBSTRING<?				;$14A
	       SUBSTRING-UPCASE!			;$14B
	       SUBSTRING-DOWNCASE!			;$14C
	       SUBSTRING-MATCH-FORWARD			;$14D
	       SUBSTRING-MATCH-BACKWARD			;$14E
	       SUBSTRING-MATCH-FORWARD-CI		;$14F
	       SUBSTRING-MATCH-BACKWARD-CI		;$150
	       PHOTO-OPEN				;$151
	       PHOTO-CLOSE				;$152
	       SETUP-TIMER-INTERRUPT			;$153
	       #F					;$154
	       #F					;$155
	       #F					;$156
	       #F					;$157
	       #F					;$158
	       #F					;$159
	       #F					;$15A
	       #F					;$15B
	       #F					;$15C
	       #F					;$15D
	       #F					;$15E
	       #F					;$15F
	       #F					;$160
	       EXTRACT-NON-MARKED-VECTOR		;$161
	       UNSNAP-LINKS!				;$162
	       SAFE-PRIMITIVE?				;$163
	       SUBSTRING-READ				;$164
	       SUBSTRING-WRITE				;$165
	       SCREEN-X-SIZE				;$166
	       SCREEN-Y-SIZE				;$167
	       SCREEN-WRITE-CURSOR			;$168
	       SCREEN-WRITE-CHARACTER			;$169
	       SCREEN-WRITE-SUBSTRING			;$16A 
	       NEXT-FILE-MATCHING			;$16B
	       #F					;$16C
	       TTY-WRITE-BYTE				;$16D
	       FILE-READ-BYTE				;$16E
	       FILE-WRITE-BYTE				;$16F
	       #F #| SAVE-SCREEN |#			;$170
	       #F #| RESTORE-SCREEN! |#			;$171
	       #F #| SUBSCREEN-CLEAR! |#		;$172
	       #F #| &GCD |#				;$173
	       #F #| TTY-REDRAW-SCREEN |#		;$174
	       #F #| SCREEN-INVERSE-VIDEO! |#		;$175
	       STRING->SYNTAX-ENTRY			;$176
	       SCAN-WORD-FORWARD			;$177
	       SCAN-WORD-BACKWARD			;$178
	       SCAN-LIST-FORWARD			;$179
	       SCAN-LIST-BACKWARD			;$17A
	       SCAN-SEXPS-FORWARD			;$17B
	       SCAN-FORWARD-TO-WORD			;$17C
	       SCAN-BACKWARD-PREFIX-CHARS		;$17D
	       CHAR->SYNTAX-CODE			;$17E
	       QUOTED-CHAR?				;$17F
	       MICROCODE-TABLES-FILENAME		;$180
	       #F					;$181
	       #F #| FIND-PASCAL-PROGRAM |#		;$182
	       #F #| EXECUTE-PASCAL-PROGRAM |#		;$183
	       #F #| GRAPHICS-MOVE |#			;$184
	       #F #| GRAPHICS-LINE |#			;$185
	       #F #| GRAPHICS-PIXEL |#			;$186
	       #F #| GRAPHICS-SET-DRAWING-MODE |#	;$187
	       #F #| ALPHA-RASTER? |#			;$188
	       #F #| TOGGLE-ALPHA-RASTER |#		;$189
	       #F #| GRAPHICS-RASTER? |#		;$18A
	       #F #| TOGGLE-GRAPHICS-RASTER |#		;$18B
	       #F #| GRAPHICS-CLEAR |#			;$18C
	       #F #| GRAPHICS-SET-LINE-STYLE |#		;$18D
	       ERROR-PROCEDURE				;$18E
	       VOLUME-EXISTS?		                ;$18F
	       RE-CHAR-SET-ADJOIN!			;$190
	       RE-COMPILE-FASTMAP			;$191
	       RE-MATCH					;$192
	       RE-SEARCH-FORWARD			;$193
	       RE-SEARCH-BACKWARD			;$194
	       (SYSTEM-MEMORY-REF &OBJECT-REF)		;$195
	       (SYSTEM-MEMORY-SET! &OBJECT-SET!)	;$196
	       BIT-STRING-FILL!				;$197
	       BIT-STRING-MOVE!				;$198
	       BIT-STRING-MOVEC!			;$199
	       BIT-STRING-OR!				;$19A               
	       BIT-STRING-AND!				;$19B
	       BIT-STRING-ANDC!				;$19C
	       BIT-STRING=?				;$19D
	       WORKING-DIRECTORY-PATHNAME		;$19E
	       OPEN-DIRECTORY				;$19F
	       DIRECTORY-READ				;$1A0
	       UNDER-EMACS?				;$1A1
	       TTY-FLUSH-OUTPUT				;$1A2
	       RELOAD-BAND-NAME				;$1A3
	       ))

;;; [] External

(vector-set! (get-fixed-objects-vector)
	     16	;(fixed-objects-vector-slot 'MICROCODE-EXTERNAL-PRIMITIVES)
	     #())

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
	       #F					;1D
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
	       ))

;;; [] Terminations

(vector-set! (get-fixed-objects-vector)
	     22 ;(fixed-objects-vector-slot 'MICROCODE-TERMINATIONS-VECTOR)
	     #(HALT                              ;00
	       DISK-RESTORE                      ;01
	       BROKEN-HEART                      ;02
	       NON-POINTER-RELOCATION            ;03
	       BAD-ROOT                          ;04
	       NON-EXISTENT-CONTINUATION         ;05
	       BAD-STACK                         ;06
	       STACK-OVERFLOW                    ;07
	       STACK-ALLOCATION-FAILED           ;08
	       NO-ERROR-HANDLER                  ;09
	       NO-INTERRUPT-HANDLER              ;0A
	       UNIMPLEMENTED-CONTINUATION        ;0B
	       EXIT                              ;0C
	       BAD-PRIMITIVE-DURING-ERROR        ;0D
	       EOF                               ;0E
	       BAD-PRIMITIVE                     ;0F
	       TERMINATION-HANDLER		 ;10
	       END-OF-CONTINUATION               ;11
	       INVALID-TYPE-CODE		 ;12
	       COMPILER-DEATH			 ;13
	       GC-OUT-OF-SPACE			 ;14
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
	       FLONUM-EXPONENT-LENGTH		;07
	       OS-NAME-STRING			;08
	       OS-VARIANT-STRING		;09
	       ))

;;; This identification string is saved by the system.

"$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v8/src/microcode/utabmd.scm,v 9.32 1987/06/03 20:59:23 cph Exp $"

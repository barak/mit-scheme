;;; -*-Scheme-*-
;;;
;;; $Id: a33337b0923b9f48cd94253d1030e91551e0a775 $
;;;
;;; Copyright (c) 1987-1999 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;

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
	       LINKER-TRAMPOLINE-GENERATOR              ;41
	       ))

;;; [] Types

;;; This tagging arrangement has a rationale:
;;; 
;;; Numbers are arranged for efficent signed fixnums, and range-testing
;;; of the following predicates integer-type (i), machine-efficient (*)
;;; real (r), NUMBER? (n)
;;;   n r    * 111110 flonum       
;;;   n r  i * 111111 -ve fixnum (reserved)
;;;   n r  i * 000000 whole fixnum
;;;   n r  i   000001 bignum
;;;   n r      000010 rational
;;;   n        000011 complex
;;;
;;; The other important tag is COMPILED-ENTRY.  Sadly, this depends on
;;; both CPU architecture and operationg system.  Good choices are:
;;;
;;;   000100 (00)  MIPS
;;;   010000 (00)  spectrum
;;; Alpha, 68k and VAX all loose by prefering 0 or -1 -- already fixnums
;;; 
;;; This file is right for HP-PA spectrum. Tag RETURN-CODE, which is not
;;; position sensitive, is in the MIPS-favoured spot.

(vector-set! (get-fixed-objects-vector)
	     4 ;(fixed-objects-vector-slot 'MICROCODE-TYPES-VECTOR)
	     #(
#|1A|#         (POSITIVE-FIXNUM MANIFEST-VECTOR ADDRESS) ;00
#|0E|#         (BIGNUM BIG-FIXNUM)			;01
#|3A|#         RATNUM					;02
#|3C|#         (RECNUM COMPLEX)				;03

#|0B|#         (RETURN-CODE RETURN-ADDRESS)		;04

#|00|#         (NULL FALSE GLOBAL-ENVIRONMENT)          ;05
#|27|#         MANIFEST-NM-VECTOR	       		;06
#|02|#         CHARACTER		       		;07
#|08|#         (CONSTANT TRUE)				;08
#|18|#         PRIMITIVE				;09
#|0D|#         MANIFEST-CLOSURE 	       		;0A

#|36|#         CELL					;0B
#|01|#         (PAIR LIST)				;0C
#|37|#         WEAK-CONS				;0D
#|05|#         UNINTERNED-SYMBOL			;0E
#|1D|#         INTERNED-SYMBOL				;0F

#|28|#         COMPILED-ENTRY				;10

#|22|#         BROKEN-HEART		       		;11
#|20|#         (HUNK3-A UNMARKED-HISTORY)		;12
#|24|#         (TRIPLE HUNK3 HUNK3-B MARKED-HISTORY)	;13
#|38|#         QUAD        				;14

#|2B|#         MANIFEST-SPECIAL-NM-VECTOR	  	;15
#|16|#         NON-MARKED-VECTOR			;16
#|0A|#         VECTOR					;17
#|3E|#         RECORD					;18
#|2F|#         VECTOR-1B	          		;19
#|1E|#         (STRING CHARACTER-STRING VECTOR-8B)	;1A
#|31|#         VECTOR-16B		       		;1B

#|32|#         (REFERENCE-TRAP UNASSIGNED)     		;1C
#|3D|#         COMPILED-CODE-BLOCK			;1D
#|39|#         LINKAGE-SECTION				;1E
#|1C|#         CONTROL-POINT	       			;1F

#|3B|#         STACK-ENVIRONMENT			;20
#|0F|#         PROCEDURE				;21
#|09|#         EXTENDED-PROCEDURE			;22		
#|29|#         LEXPR					;23
#|10|#         (ENTITY)					;24
#|12|#         ENVIRONMENT		      		;25
#|13|#         (PROMISE DELAYED)			;26
#|2E|#         FUTURE					;27

           ;; scode structures
#|25|#         IN-PACKAGE				;28
#|15|#         COMMENT					;29
#|03|#         QUOTATION				;2A
#|2C|#         VARIABLE					;2B
#|1F|#         ACCESS					;2C
#|17|#         LAMBDA					;2D
#|14|#         EXTENDED-LAMBDA				;2E
#|19|#         SEQUENCE-2				;2F
#|33|#         SEQUENCE-3	       			;30
#|34|#         CONDITIONAL				;31
#|35|#         DISJUNCTION				;32
#|26|#         COMBINATION	       			;33
#|07|#         COMBINATION-1				;34
#|0C|#         COMBINATION-2				;35
#|30|#         PRIMITIVE-COMBINATION-0	       	       	;36
#|1B|#         PRIMITIVE-COMBINATION-1			;37
#|04|#         PRIMITIVE-COMBINATION-2 	                ;38
#|2A|#         PRIMITIVE-COMBINATION-3		       	;39
#|11|#         DELAY					;3A
#|21|#         DEFINITION				;3B
#|23|#         ASSIGNMENT				;3C
#|2D|#         THE-ENVIRONMENT	      			;3D

#|06|#         (FLONUM BIG-FLONUM)			;3E
#|3F|#         NEGATIVE-FIXNUM	                        ;3F

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
	       COMPILER-LINK-CACHES-CONTINUE		;60
	       ))

;;; [] Errors

(vector-set! (get-fixed-objects-vector)
	     7	;(fixed-objects-vector-slot 'MICROCODE-ERRORS-VECTOR)
	     #(BAD-ERROR-CODE				;00
	       UNBOUND-VARIABLE				;01
	       UNASSIGNED-VARIABLE			;02
	       UNDEFINED-PROCEDURE			;03
	       SYSTEM-CALL				;04
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

;;; [] System-call names

(define-macro (ucode-primitive . args)
  (apply make-primitive-procedure args))

(vector-set! (get-fixed-objects-vector)
	     #x09 ;(fixed-objects-vector-slot 'SYSTEM-CALL-NAMES)
	     (let ((prim (ucode-primitive microcode-system-call-names 0)))
	       (if (implemented-primitive-procedure? prim)
		   (prim)
		   ;; For compatibility with older microcodes:
		   '#(ACCEPT
		      BIND
		      CHDIR
		      CHMOD
		      CLOSE
		      CONNECT
		      FCNTL-GETFL
		      FCNTL-SETFL
		      FORK
		      FSTAT
		      FTRUNCATE
		      GETCWD
		      GETHOSTNAME
		      GETTIMEOFDAY
		      IOCTL-TIOCGPGRP
		      IOCTL-TIOCSIGSEND
		      KILL
		      LINK
		      LISTEN
		      LOCALTIME
		      LSEEK
		      MALLOC
		      MKDIR
		      OPEN
		      OPENDIR
		      PAUSE
		      PIPE
		      READ
		      READLINK
		      REALLOC
		      RENAME
		      RMDIR
		      SELECT
		      SETITIMER
		      SETPGID
		      SIGHOLD
		      SIGPROCMASK
		      SIGSUSPEND
		      SLEEP
		      SOCKET
		      SYMLINK
		      TCDRAIN
		      TCFLUSH
		      TCGETPGRP
		      TCSETPGRP
		      TERMINAL-GET-STATE
		      TERMINAL-SET-STATE
		      TIME
		      TIMES
		      UNLINK
		      UTIME
		      VFORK
		      WRITE
		      STAT
		      LSTAT
		      MKTIME
		      DYNAMIC-LOAD))))

;;; [] System-call errors

(vector-set! (get-fixed-objects-vector)
	     #x0A ;(fixed-objects-vector-slot 'SYSTEM-CALL-ERRORS)
	     (let ((prim (ucode-primitive microcode-system-error-names 0)))
	       (if (implemented-primitive-procedure? prim)
		   (prim)
		   ;; For compatibility with older microcodes:
		   '#(UNKNOWN
		      ARG-LIST-TOO-LONG
		      BAD-ADDRESS
		      BAD-FILE-DESCRIPTOR
		      BROKEN-PIPE
		      DIRECTORY-NOT-EMPTY
		      DOMAIN-ERROR
		      EXEC-FORMAT-ERROR
		      FILE-EXISTS
		      FILE-TOO-LARGE
		      FILENAME-TOO-LONG
		      FUNCTION-NOT-IMPLEMENTED
		      IMPROPER-LINK
		      INAPPROPRIATE-IO-CONTROL-OPERATION
		      INTERRUPTED-FUNCTION-CALL
		      INVALID-ARGUMENT
		      INVALID-SEEK
		      IO-ERROR
		      IS-A-DIRECTORY
		      NO-CHILD-PROCESSES
		      NO-LOCKS-AVAILABLE
		      NO-SPACE-LEFT-ON-DEVICE
		      NO-SUCH-DEVICE
		      NO-SUCH-DEVICE-OR-ADDRESS
		      NO-SUCH-FILE-OR-DIRECTORY
		      NO-SUCH-PROCESS
		      NOT-A-DIRECTORY
		      NOT-ENOUGH-SPACE
		      OPERATION-NOT-PERMITTED
		      PERMISSION-DENIED
		      READ-ONLY-FILE-SYSTEM
		      RESOURCE-BUSY
		      RESOURCE-DEADLOCK-AVOIDED
		      RESOURCE-TEMPORARILY-UNAVAILABLE
		      RESULT-TOO-LARGE
		      TOO-MANY-LINKS
		      TOO-MANY-OPEN-FILES
		      TOO-MANY-OPEN-FILES))))

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

"$Id: a33337b0923b9f48cd94253d1030e91551e0a775 $"

changecom(`;');;; -*-Midas-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/cmpauxmd/hppa.m4,v 1.15 1991/05/15 16:21:50 jinx Exp $
;;;
;;;	Copyright (c) 1989, 1990 Massachusetts Institute of Technology
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

;;;; HP Precision Architecture assembly language part of the compiled
;;;; code interface. See cmpint.txt, cmpint.c, cmpint-hppa.h, and
;;;; cmpgc.h for more documentation.
;;;;
;;;; NOTE:
;;;;	Assumptions:
;;;;
;;;;	1) All registers (except double floating point registers) and
;;;;	stack locations hold a C long object.
;;;;
;;;;	2) The C compiler divides registers into three groups:
;;;;	- Linkage registers, used for procedure calls and global
;;;;	references.  On HPPA: gr0 (always 0), gr2 (return address),
;;;;	gr27 (global data pointer), and gr30 (stack pointer).
;;;;	- super temporaries, not preserved accross procedure calls and
;;;;	always usable. On HPPA: gr1, gr19-gr26, gr28-29, gr31.
;;;;	gr26-23 are argument registers, gr28-29 are return registers.
;;;;	- preserved registers saved by the callee if they are written.
;;;;	On HPPA: gr3-gr18
;;;;
;;;;	3) Arguments, if passed on a stack, are popped by the caller
;;;;	or by the procedure return instruction (as on the VAX).  Thus
;;;;	most "leaf" procedures need not worry about them. On HPPA: All
;;;;	arguments have slots in the stack, allocated and popped by the
;;;;	caller, but the first four words are actually passed in gr26,
;;;;	gr25, gr24, gr23, unless they are floating point arguments, in
;;;;	which case they are passed in floating point registers.
;;;;
;;;;	4) There is a hardware or software maintained stack for
;;;;	control.  The procedure calling sequence may leave return
;;;;	addresses in registers, but they must be saved somewhere for
;;;;	nested calls and recursive procedures.  On HPPA: Passed in a
;;;;	register, but a slot on the stack exists, allocated by the
;;;;	caller.  The return link is in gr2 and immediately saved in
;;;;	-20(0,30) if the procedure makes further calls.  The stack
;;;;	pointer is in gr30.
;;;;
;;;;	5) C procedures return long values in a super temporary
;;;;    register.  Two word structures are returned in super temporary
;;;;    registers as well.  On HPPA: gr28 is used for long returns,
;;;;	gr28/gr29 are used for two word structure returns.
;;;;
;;;;	6) Floating point registers are not preserved by this
;;;;	interface.  The interface is only called from the Scheme
;;;;	interpreter, which does not use floating point data.  Thus
;;;;	although the calling convention would require us to preserve
;;;;	them, they contain garbage.  On HPPA: fr12-fr15 are
;;;;	callee-saves registers, fr4-fr7 are parameter registers, and
;;;;	fr8-fr11 are caller-saves registers.  fr0-fr3 are status
;;;;	registers.
;;;;
;;;; Compiled Scheme code uses the following register convention.
;;;; Note that scheme_to_interface_ble and the register block are
;;;; preserved by C calls, but the others are not, since they change
;;;; dynamically.  scheme_to_interface and trampoline_to_interface can
;;;; be reached at fixed offsets from scheme_to_interface_ble.
;;;;	- gr22 contains the Scheme stack pointer.
;;;;	- gr21 contains the Scheme free pointer.
;;;;	- gr20 contains a cached version of MemTop.
;;;;	- gr19 contains the dynamic link when needed.
;;;;	- gr5 contains the quad mask for machine pointers.
;;;;	- gr4 contains a pointer to the Scheme interpreter's
;;;;	"register" block.  This block contains the compiler's copy of
;;;;	MemTop, the interpreter's registers (val, env, exp, etc),
;;;;	temporary locations for compiled code.
;;;;	- gr3 contains the address of scheme_to_interface_ble.
;;;;
;;;;	All other registers are available to the compiler.  A
;;;;	caller-saves convention is used, so the registers need not be
;;;;	preserved by subprocedures.
;;;;
;;;; ADB mnemonics:
;;;;	arg3 = gr23; arg2 = gr24; arg1 = gr25; arg0 = gr26
;;;;	dp   = gr27; ret0 = gr28; ret1 = gr29; sp   = gr30; rp   = gr02

changequote(",")
define(HEX, "0x$1")
define(ASM_DEBUG, 0)
define(TC_LENGTH, ifdef("TYPE_CODE_LENGTH", TYPE_CODE_LENGTH, 8))
define(QUAD_MASK, eval(2 ** (TC_LENGTH - 2)))
define(LOW_TC_BIT, eval(TC_LENGTH - 1))
define(DATUM_LENGTH, eval(32 - TC_LENGTH))
define(FIXNUM_LENGTH, DATUM_LENGTH)
define(FIXNUM_POS, eval(FIXNUM_LENGTH - 1))
define(FIXNUM_BIT, eval(TC_LENGTH + 1))
define(TC_START, eval(TC_LENGTH - 1))
define(TC_FIXNUM, 0x1a)
define(TC_FLONUM, 0x6)
define(TC_CCENTRY, 0x28)
define(TC_NMV, 0x27)
define(FLONUM_VECTOR_HEADER, eval((TC_NMV * (2 ** DATUM_LENGTH)) + 2))
define(TC_FALSE, 0)
define(TC_TRUE, 0x8)
define(SHARP_F, eval(TC_FALSE * (2 ** DATUM_LENGTH)))
define(SHARP_T, eval(TC_TRUE * (2 ** DATUM_LENGTH)))

	.SPACE  $TEXT$
	.SUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY
C_to_interface
	.PROC
	.CALLINFO CALLER,FRAME=28,SAVE_RP
	.ENTRY
	STW	2,-20(0,30)		; Save return address
	STWM	3,112(30)		; Save first reg, allocate frame
	STW	4,-108(30)		; Save the other regs
	STW	5,-104(30)
	STW	6,-100(30)
	STW	7,-96(30)
	STW	8,-92(30)
	STW	9,-88(30)
	STW	10,-84(30)
	STW	11,-80(30)
	STW	12,-76(30)
	STW	13,-72(30)
	STW	14,-68(30)
	STW	15,-64(30)
	STW	16,-60(30)
	STW	17,-56(30)
	STW	18,-52(30)
	ADDIL	L'Registers-$global$,27
	LDO	R'Registers-$global$(1),4 ; Setup Regs
	LDI	QUAD_MASK,5

interface_to_scheme
	LDW	8(0,4),2		; Move interpreter reg to val
	LDW	0(0,4),20		; Setup memtop
	ADDIL	L'Ext_Stack_Pointer-$global$,27
	LDW	R'Ext_Stack_Pointer-$global$(1),22 ; Setup stack pointer
	ADDIL	L'Free-$global$,27
	LDW	R'Free-$global$(1),21	; Setup free
	COPY	2,19			; Restore dynamic link if any
	DEP	5,LOW_TC_BIT,TC_LENGTH,19
	.CALL	RTNVAL=GR		; out=28
	BLE	0(5,26)			; Invoke entry point
	COPY	31,3			; Setup scheme_to_interface_ble

scheme_to_interface_ble
	ADDI	4,31,31			; Skip over format word ...
trampoline_to_interface
	COPY	31,26
	DEP	0,31,2,26
scheme_to_interface
	STW	2,8(0,4)		; Move val to interpreter reg
	ADDIL	L'utility_table-$global$,27
	LDO	R'utility_table-$global$(1),29
	LDWX,S	28(0,29),29		; Find handler
	ADDIL	L'Ext_Stack_Pointer-$global$,27
	STW	22,R'Ext_Stack_Pointer-$global$(1) ; Update stack pointer
	ADDIL	L'Free-$global$,27
	STW	21,R'Free-$global$(1)	; Update free
	ifelse(ASM_DEBUG,1,"ADDIL	L'interface_counter-$global$,27
	LDW	R'interface_counter-$global$(0,1),21
	LDO	1(21),21
	STW	21,R'interface_counter-$global$(0,1)
	ADDIL	L'interface_limit-$global$,27
	LDW	R'interface_limit-$global$(0,1),22
	COMB,=,N	21,22,interface_break
interface_proceed")
	.CALL	ARGW0=GR,ARGW1=GR,ARGW2=GR,ARGW3=GR,RTNVAL=GR
	BLE	0(4,29)			; Call handler
	COPY	31,2			; Setup return address
	BV	0(28)			; Call receiver
	COPY	29,26			; Setup entry point

;; This sequence of NOPs is provided to allow for modification of
;; the sequence that appears above without having to recompile the
;; world.  The compiler "knows" the distance between
;; scheme_to_interface_ble and hook_jump_table (100 bytes)

	ifelse(ASM_DEBUG,1,"","NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP")
	NOP
	NOP
	NOP

hook_jump_table				; scheme_to_interface + 100
store_closure_code_hook
	B	store_closure_code+4
	LDIL	L'0x23400000,20		; LDIL opcode and register

store_closure_entry_hook
	B	store_closure_entry+4
	DEP	0,31,2,1		; clear PC protection bits

multiply_fixnum_hook
	B	multiply_fixnum+4
	EXTRS	26,FIXNUM_POS,FIXNUM_LENGTH,26		; arg1

fixnum_quotient_hook
	B	fixnum_quotient+4
	EXTRS	26,FIXNUM_POS,FIXNUM_LENGTH,26		; arg1

fixnum_remainder_hook
	B	fixnum_remainder+4
	EXTRS	26,FIXNUM_POS,FIXNUM_LENGTH,26		; arg1

fixnum_lsh_hook
	B	fixnum_lsh+4
	EXTRS	25,FIXNUM_POS,FIXNUM_LENGTH,25		; arg2

generic_plus_hook
	B	generic_plus+4
	LDW	0(0,22),6		; arg1

generic_subtract_hook
	B	generic_subtract+4
	LDW	0(0,22),6		; arg1

generic_times_hook
	B	generic_times+4
	LDW	0(0,22),6		; arg1

generic_divide_hook
	B	generic_divide+4
	LDW	0(0,22),6		; arg1

generic_equal_hook
	B	generic_equal+4
	LDW	0(0,22),6		; arg1

generic_less_hook
	B	generic_less+4
	LDW	0(0,22),6		; arg1

generic_greater_hook
	B	generic_greater+4
	LDW	0(0,22),6		; arg1

generic_increment_hook
	B	generic_increment+4
	LDW	0(0,22),6		; arg1

generic_decrement_hook
	B	generic_decrement+4
	LDW	0(0,22),6		; arg1

generic_zero_hook
	B	generic_zero+4
	LDW	0(0,22),6		; arg1

generic_positive_hook
	B	generic_positive+4
	LDW	0(0,22),6		; arg1

generic_negative_hook
	B	generic_negative+4
	LDW	0(0,22),6		; arg1

no_hook
;;
;; Provide dummy trapping hooks in case a newver version of compiled
;; code that expects more hooks is run.
;;
	BREAK	0,18
	NOP
	BREAK	0,19
	NOP
	BREAK	0,20
	NOP
	BREAK	0,21
	NOP
	BREAK	0,22
	NOP
	BREAK	0,23
	NOP
	BREAK	0,24
	NOP
	BREAK	0,25
	NOP
	BREAK	0,26
	NOP
	BREAK	0,27
	NOP
	BREAK	0,28
	NOP
	BREAK	0,28
	NOP
	BREAK	0,29
	NOP
	BREAK	0,30
	NOP
	BREAK	0,31
	NOP
	BREAK	0,32
	NOP
	BREAK	0,33
	NOP
	BREAK	0,34
	NOP
	BREAK	0,35
	NOP
	BREAK	0,36
	NOP
	BREAK	0,37
	NOP
	BREAK	0,38
	NOP
	BREAK	0,39
	NOP

ifelse(ASM_DEBUG,1,"interface_break
	COMB,=	21,22,interface_break
	NOP
	B,N	interface_proceed")

store_closure_entry
;;
;; On arrival, 31 has a return address and 1 contains the address to
;; which the closure should jump with pc protection bits.
;; 26 contains the format/gc-offset word for this entry.
;;
	DEP	0,31,2,1		; clear PC protection bits
	STWM	26,4(0,21)		; move format long to heap 
;; fall through to store_closure_code

store_closure_code
;;
;; On arrival, 31 has a return address and 1 contains the address to
;; which the closure should jump.  The appropriate instructions (LDIL
;; and BLE and SUBI) are pushed on the heap.
;;     Important:
;; 3 words in memory are modified, but only 2 FDC instructions and one FIC
;; instruction are issued.  The PDC_CACHE description in the I/O Architecture
;; manual specifies that each flush will flush a multiple of 16 bytes, thus
;; a flush of the first data word and a flush of the last data word suffice to
;; flush all three.  A single FIC of the first instruction word suffices since
;; the space is newly allocated and the whole I-cache was flushed at
;; exec and relocation(GC) time.
;; The SYNC is assumed to be separated by at least 7 instructions from
;; the first execution of the new instructions.
;;
	LDIL	L'0x23400000,20		; LDIL opcode and register
	EXTRU	1,0,1,5
	DEP	5,31,1,20
	EXTRU	1,11,11,5
	DEP	5,30,11,20
	EXTRU	1,13,2,5
	DEP	5,17,2,20
	EXTRU	1,18,5,5
	DEP	5,15,5,20
	STW	20,0(0,21)		; Store LDIL instruction
	LDIL	L'0xe7406000,20		; BLE opcode, register and nullify
	LDO	R'0xe7406000(20),20
	EXTRU	1,19,1,5
	DEP	5,29,1,20
	EXTRU	1,29,10,5
	DEP	5,28,10,20
	STW	20,4(0,21)		; Store BLE instruction
	LDIL	L'0xb7ff07e9,20
	LDO	R'0xb7ff07e9(20),20
	STW	20,8(0,21)		; Store ADDI instruction
	LDI	12,20
	FDC	0(0,21)			; flush 1st inst. from D-cache
	FDC	20(0,21)		; flush last inst. from D-cache
	SYNC
	FIC,M	20(5,21)		; flush 1st inst. from I-cache
	SYNC
	LDW	0(0,4),20		; Reload memtop
	BE	0(5,31)			; Return
	LDI	QUAD_MASK,5		; Restore register 5

multiply_fixnum
;;
;; On arrival, 31 has a return address and 26 and 25 have the fixnum arguments.
;;
	EXTRS	26,FIXNUM_POS,FIXNUM_LENGTH,26		; arg1
	STW	26,0(0,21)
	EXTRS	25,FIXNUM_POS,FIXNUM_LENGTH,25		; arg2
	STW	25,4(0,21)
	ZDEPI	1,TC_LENGTH,FIXNUM_BIT,26		; FIXNUM_LIMIT
	FLDWS	0(0,21),4
	FLDWS	4(0,21),5
	STW	26,8(0,21)				; FIXNUM_LIMIT
        FCNVXF,SGL,DBL  4,4				; arg1
        FCNVXF,SGL,DBL  5,5				; arg2
	FMPY,DBL	4,5,4
	FLDWS	8(0,21),5				; FIXNUM_LIMIT
        FCNVXF,SGL,DBL  5,5				; FIXNUM_LIMIT
	COPY	0,25					; signal no overflow
	FCMP,DBL,!>=	4,5				; result too large?
	FTEST
	B,N	multiply_fixnum_ovflw
	FSUB,DBL	0,5,5
	FCMP,DBL,!<	4,5				; result too small?
	FTEST
	B,N	multiply_fixnum_ovflw
	FCNVFXT,DBL,SGL	4,5
	FSTWS	5,0(0,21)				; result
	LDW	0(0,21),26
	BE	0(5,31)					; return
	ZDEP    26,FIXNUM_POS,FIXNUM_LENGTH,26		; make into fixnum
;;
multiply_fixnum_ovflw
	COPY	0,26
	LDO	1(0),25					; signal overflow
	BE	0(5,31)					; return
	ZDEP    26,FIXNUM_POS,FIXNUM_LENGTH,26		; make into fixnum

fixnum_quotient
;;
;; On arrival, 31 has a return address and 26 and 25 have the fixnum arguments.
;; Note that quotient only overflows when dividing by 0 and when the
;; divisor is -1 and the dividend is the most negative fixnum,
;; producing the most positive fixnum plus 1.
;;
	EXTRS	26,FIXNUM_POS,FIXNUM_LENGTH,26		; arg1
	COMB,=	0,25,fixnum_quotient_ovflw
	STW	26,0(0,21)
	EXTRS	25,FIXNUM_POS,FIXNUM_LENGTH,25		; arg2
	STW	25,4(0,21)
	ZDEPI	1,TC_LENGTH,FIXNUM_BIT,26		; FIXNUM_LIMIT
	FLDWS	0(0,21),4
	FLDWS	4(0,21),5
        FCNVXF,SGL,DBL  4,4				; arg1
        FCNVXF,SGL,DBL  5,5				; arg2
	FDIV,DBL	4,5,4
	STW	26,0(0,21)				; FIXNUM_LIMIT
	FCNVFXT,DBL,SGL	4,5
	FSTWS	5,4(0,21)				; result
	FLDWS	0(0,21),5				; FIXNUM_LIMIT
	FCNVXF,SGL,DBL	5,5
	FCMP,DBL,!>=	4,5				; result too large?
	LDW	4(0,21),26
	COPY	0,25					; signal no overflow
	FTEST
;;
fixnum_quotient_ovflw
	LDO	1(0),25					; signal overflow
	BE	0(5,31)					; return
	ZDEP    26,FIXNUM_POS,FIXNUM_LENGTH,26		; make into fixnum

fixnum_remainder
;;
;; On arrival, 31 has a return address and 26 and 25 have the fixnum arguments.
;; Note that remainder only overflows when dividing by 0.
;; Note also that the FREM instruction does not compute the same as
;; the Scheme remainder operation.  The sign of the result must
;; sometimes be adjusted.
;;
	EXTRS	26,FIXNUM_POS,FIXNUM_LENGTH,26		; arg1
	COMB,=,N	0,25,fixnum_remainder_ovflw
	STW	26,0(0,21)
	EXTRS	25,FIXNUM_POS,FIXNUM_LENGTH,25		; arg2
	STW	25,4(0,21)
	FLDWS	0(0,21),4
	FLDWS	4(0,21),5
        FCNVXF,SGL,DBL  4,4				; arg1
        FCNVXF,SGL,DBL  5,5				; arg2
	FREM,DBL	4,5,4
	FCNVFXT,DBL,SGL	4,5
	FSTWS	5,4(0,21)				; result
	LDW	4(0,21),1
	XOR,<	26,1,0					; skip if signs !=
	B,N	fixnum_remainder_done
	COMB,=,N	0,1,fixnum_remainder_done
	COMCLR,>	26,0,0				; skip if arg1 > 0
	SUB,TR	1,25,1					; result -= arg2
	ADD	1,25,1					; result += arg2
;;
fixnum_remainder_done
	ZDEP    1,FIXNUM_POS,FIXNUM_LENGTH,26		; make into fixnum
	BE	0(5,31)					; return
	COPY	0,25					; signal no overflow
;;
fixnum_remainder_ovflw
	BE	0(5,31)					; return
	LDO	1(0),25					; signal overflow

fixnum_lsh
;;
;; On arrival, 31 has a return address and 26 and 25 have the fixnum arguments.
;; If arg2 is negative, it is a right shift, otherwise a left shift.
;;
	EXTRS	25,FIXNUM_POS,FIXNUM_LENGTH,25		; arg2
	COMB,<,N	0,25,fixnum_lsh_positive
	SUB	0,25,25			; negate, for right shift
	COMICLR,>	FIXNUM_LENGTH,25,0
	LDI	31,25			; shift right completely
	MTSAR	25
	VSHD	0,26,26			; shift right
	DEP	0,31,TC_LENGTH,26	; normalize fixnum
	BE	0(5,31)			; return	
	COPY	0,25			; signal no overflow
;;
fixnum_lsh_positive
	SUBI,>	32,25,25		; shift amount for right shift
	COPY	0,25			; shift left completely
	MTSAR	25
	VSHD	26,0,26			; shift right (32 - arg2)
	BE	0(5,31)			; return	
	COPY	0,25			; signal no overflow

;;;; Generic arithmetic utilities.
;;;  On entry the arguments are on the Scheme stack, and the return
;;;  address immediately above them.

define(define_generic_binary,
"generic_$1
	LDW	0(0,22),6		; arg1
	LDW	4(0,22),8		; arg2
	EXTRU	6,TC_START,TC_LENGTH,7	; type of arg1
	EXTRU	8,TC_START,TC_LENGTH,9	; type of arg2
	COMIB,<>,N	TC_FLONUM,7,generic_$1_one_unk
	COMIB,<>,N	TC_FLONUM,9,generic_$1_two_unk
	DEP	5,TC_START,TC_LENGTH,6	; data segment quadrant bits
	FLDDS	4(0,6),4		; arg1 -> fr4
	DEP	5,TC_START,TC_LENGTH,8	; data segment quadrant bits
	FLDDS	4(0,8),5		; arg2 -> fr5
	LDO	8(22),22		; pop args from stack
	B	generic_flonum_result	; cons flonum and return
	$3,DBL	4,5,4			; operate

generic_$1_one_unk			; ~FLO * ??
	COMIB,<>,N	TC_FLONUM,9,generic_$1_fail
	COMICLR,=	TC_FIXNUM,7,0
	B,N	generic_$1_fail
	EXTRS	6,31,FIXNUM_LENGTH,6	; sign extend arg1
	STW	6,0(0,21)		; put in memory to reload into fpcp
	LDO	8(22),22		; pop args from stack
	DEP	5,TC_START,TC_LENGTH,8	; data segment quadrant bits
	FLDWS	0(0,21),4		; single precision int arg1 -> fr4
	FLDDS	4(0,8),5		; arg2 -> fr5
        FCNVXF,SGL,DBL  4,4		; convert to double float
	B	generic_flonum_result	; cons flonum and return
	$3,DBL	4,5,4			; operate
	
generic_$1_two_unk			; FLO * ~FLO
	COMICLR,=	TC_FIXNUM,9,0
	B,N	generic_$1_fail
	EXTRS	8,31,FIXNUM_LENGTH,8	; sign extend arg2
	STW	8,0(0,21)		; put in memory to reload into fpcp
	LDO	8(22),22		; pop args from stack
	DEP	5,TC_START,TC_LENGTH,6	; data segment quadrant bits
	FLDWS	0(0,21),5		; single precision int arg2 -> fr5
	FLDDS	4(0,6),4		; arg1 -> fr4
        FCNVXF,SGL,DBL  5,5		; convert to double float
	B	generic_flonum_result	; cons flonum and return
	$3,DBL	4,5,4			; operate

generic_$1_fail				; ?? * ??, out of line
	B	scheme_to_interface
	LDI	HEX($2),28		; operation code")

generic_flonum_result			; expects data in fr4.
	DEPI	4,31,3,21		; align free
	COPY	21,2			; result (untagged)
	LDWM	4(0,22),8		; return address
	LDIL	L'FLONUM_VECTOR_HEADER,7
	;	LDO	R'FLONUM_VECTOR_HEADER(7),7	; Assembler bug!
	ADDI	R'FLONUM_VECTOR_HEADER,7,7
	STWM	7,4(0,21)		; vector header
	DEPI	TC_FLONUM,TC_START,TC_LENGTH,2 ; tag flonum
	DEP	5,TC_START,TC_LENGTH,8	; data segment quadrant bits
	BLE	0(5,8)			; return!
	FSTDS,MA	4,8(0,21)	; store floating data

define(define_generic_binary_predicate,
"generic_$1
	LDW	0(0,22),6		; arg1
	LDW	4(0,22),8		; arg2
	EXTRU	6,TC_START,TC_LENGTH,7	; type of arg1
	EXTRU	8,TC_START,TC_LENGTH,9	; type of arg2
	COMIB,<>,N	TC_FLONUM,7,generic_$1_one_unk
	COMIB,<>,N	TC_FLONUM,9,generic_$1_two_unk
	DEP	5,TC_START,TC_LENGTH,6	; data segment quadrant bits
	FLDDS	4(0,6),4		; arg1 -> fr4
	DEP	5,TC_START,TC_LENGTH,8	; data segment quadrant bits
	FLDDS	4(0,8),5		; arg2 -> fr5
	LDO	8(22),22		; pop args from stack
	B	generic_boolean_result	; cons answer and return
	FCMP,DBL,$3	4,5		; compare

generic_$1_one_unk			; ~FLO * ??
	COMIB,<>,N	TC_FLONUM,9,generic_$1_fail
	COMICLR,=	TC_FIXNUM,7,0
	B,N	generic_$1_fail
	EXTRS	6,31,FIXNUM_LENGTH,6	; sign extend arg1
	STW	6,0(0,21)		; put in memory to reload into fpcp
	LDO	8(22),22		; pop args from stack
	DEP	5,TC_START,TC_LENGTH,8	; data segment quadrant bits
	FLDWS	0(0,21),4		; single precision int arg1 -> fr4
	FLDDS	4(0,8),5		; arg2 -> fr5
        FCNVXF,SGL,DBL  4,4		; convert to double float
	B	generic_boolean_result	; cons answer and return
	FCMP,DBL,$3	4,5		; compare
	
generic_$1_two_unk			; FLO * ~FLO
	COMICLR,=	TC_FIXNUM,9,0
	B,N	generic_$1_fail
	EXTRS	8,31,FIXNUM_LENGTH,8	; sign extend arg2
	STW	8,0(0,21)		; put in memory to reload into fpcp
	LDO	8(22),22		; pop args from stack
	DEP	5,TC_START,TC_LENGTH,6	; data segment quadrant bits
	FLDWS	0(0,21),5		; single precision int arg2 -> fr5
	FLDDS	4(0,6),4		; arg1 -> fr4
        FCNVXF,SGL,DBL  5,5		; convert to double float
	B	generic_boolean_result	; cons answer and return
	FCMP,DBL,$3	4,5		; compare

generic_$1_fail				; ?? * ??, out of line
	B	scheme_to_interface
	LDI	HEX($2),28		; operation code")

generic_boolean_result
	LDWM	4(0,22),8		; return address
	LDIL	L'SHARP_T,2
	FTEST
	LDIL	L'SHARP_F,2
	DEP	5,TC_START,TC_LENGTH,8	; data segment quadrant bits
	BLE,N	0(5,8)			; return!

define(define_generic_unary,
"generic_$1
	LDW	0(0,22),6		; arg
	EXTRU	6,TC_START,TC_LENGTH,7	; type of arg
	COMIB,<>,N	TC_FLONUM,7,generic_$1_fail
	LDI	1,7			; constant 1
	STW	7,0(0,21)		; into memory
	DEP	5,TC_START,TC_LENGTH,6	; data segment quadrant bits
	LDO	4(22),22		; pop arg from stack
	FLDWS	0(0,21),5		; 1 -> fr5
	FLDDS	4(0,6),4		; arg -> fr4
	FCNVXF,SGL,DBL	5,5		; convert to double float
	B	generic_flonum_result	; cons flonum and return
	$3,DBL	4,5,4			; operate

generic_$1_fail
	B	scheme_to_interface
	LDI	HEX($2),28		; operation code")

define(define_generic_unary_predicate,
"generic_$1
	LDW	0(0,22),6		; arg
	EXTRU	6,TC_START,TC_LENGTH,7	; type of arg
	COMIB,<>,N	TC_FLONUM,7,generic_$1_fail
	DEP	5,TC_START,TC_LENGTH,6	; data segment quadrant bits
	FLDDS	4(0,6),4		; arg -> fr4
	LDO	4(22),22		; pop arg from stack
	B	generic_boolean_result	; cons answer and return
	FCMP,DBL,$3	4,0		; compare

generic_$1_fail
	B	scheme_to_interface
	LDI	HEX($2),28		; operation code")

define_generic_unary(decrement,22,FSUB)
define_generic_binary(divide,23,FDIV)
define_generic_binary_predicate(equal,24,=)
define_generic_binary_predicate(greater,25,>)
define_generic_unary(increment,26,FADD)
define_generic_binary_predicate(less,27,<)
define_generic_binary(subtract,28,FSUB)
define_generic_binary(times,29,FMPY)
define_generic_unary_predicate(negative,2a,<)
define_generic_binary(plus,2b,FADD)
define_generic_unary_predicate(positive,2c,>)
define_generic_unary_predicate(zero,2d,=)

;;;; Assembly language entry point used by utilities in cmpint.c
;;;  to return to the interpreter.
;;;  It returns from C_to_interface.

interface_to_C
	COPY	29,28			; Setup C value
        LDW     -132(0,30),2		; Restore return address
        LDW     -52(0,30),18		; Restore saved registers
        LDW     -56(0,30),17
        LDW     -60(0,30),16
        LDW     -64(0,30),15
        LDW     -68(0,30),14
        LDW     -72(0,30),13
        LDW     -76(0,30),12
        LDW     -80(0,30),11
        LDW     -84(0,30),10
        LDW     -88(0,30),9
        LDW     -92(0,30),8
        LDW     -96(0,30),7
        LDW     -100(0,30),6
        LDW     -104(0,30),5
        LDW     -108(0,30),4
        BV      0(2)			; Return
        .EXIT
        LDWM    -112(0,30),3		; Restore last reg, pop frame
        .PROCEND			;in=26;out=28;

;;;; Routine to flush some locations from the processor cache.
;;; 
;;; Its C signature is
;;;
;;; void
;;; cache_flush_region (address, count, cache_set)
;;;     void *address;
;;;     long count;		/* in long words */
;;;	unsigned int cache_set;
;;;
;;; cache_set is a bit mask of the flags I_CACHE (1) and D_CACHE (2).
;;; the requested cache (or both) is flushed.
;;;
;;; We only need to flush every 16 bytes, since cache lines are
;;; architecturally required to have cache line sizes that are
;;; multiples of 16 bytes.  This is wasteful on processors with cache
;;; line sizes greater than 16 bytes, but this routine is typically
;;; called to flush very small ranges.
;;; We flush an additional time after flushing every 16 bytes since
;;; the start address may not be aligned with a cache line, and thus
;;; the end address may fall in a different cache line from the
;;; expected one.  The extra flush is harmless when not necessary.

cache_flush_region
	.PROC
        .CALLINFO CALLER,FRAME=0
	.ENTRY
	LDO	3(25),25		; add 3 to round up in next inst.
	SHD	0,25,2,25		; divide count (in longs) by 4
	COPY	25,28			; save for FIC loop
	COPY	26,29			; save for FIC loop
	LDI	16,1			; increment
	BB,>=,N	24,30,process_i_cache	; if D_CACHE is not set, skip d-cache
;;;
flush_cache_fdc_loop
	ADDIB,>=	-1,25,flush_cache_fdc_loop
	FDC,M	1(0,26)
	SYNC
;;;
process_i_cache
	BB,>=,N	24,31,L$exit2		; if I_CACHE is not set, return
;;;
flush_cache_fic_loop
	ADDIB,>=	-1,28,flush_cache_fic_loop
	FIC,M	1(5,29)
;;;
L$exit2
	BV	0(2)
	.EXIT
	SYNC
	.PROCEND			;in=25,26;

;;;; Routine to flush the processor cache.
;;;
;;; Its C signature is
;;;
;;; void
;;; cache_flush_all (cache_set, cache_info)
;;;      unsigned int cache_set;
;;;      struct pdc_cache_rtn_block *cache_info;
;;;
;;; cache_set is a bit mask of the flags I_CACHE (1) and D_CACHE (2).
;;; the requested cache (or both) is flushed.
;;;
;;; struct pdc_cache_rtn_block is defined in <machine/pdc_rqsts.h> and
;;; is the structure returned by the PDC_CACHE
;;; processor-dependent-code call, and stored in the kernel variable
;;; (HP-UX) "cache_tlb_parms".  Only the cache parameters (and not the
;;; TLB parameters) are used.

cache_flush_all
	.PROC
	.CALLINFO CALLER,FRAME=24
	.ENTRY

do_d_cache
	BB,>=,N	26,30,do_i_cache	; if D_CACHE is not set, skip d-cache

	LDW	32(0,25),31		; 31 <- address (initially base)
	LDW	44(0,25),29		; 29 <- loop
	LDW	36(0,25),23		; 23 <- stride
	LDW	40(0,25),19		; 19 <- count

	LDO	-1(19),19		; decrement count
	COMIB,>,N	0,19,d_sync	; if (count < 0), no flush
	COMIB,=,N	1,29,d_direct_l
	COMIB,=,N	2,29,d_assoc2_l
	COMIB,=,N	4,29,d_assoc4_l

d_assoc_l				; set-associative cache flush-loop
	COPY	29,20			; 20 (lcount) <- loop

d_set_l					; set flush-loop
	LDO	-1(20),20		; decrement lcount
	COMIB,<=,N	0,20,d_set_l	; if (lcount >= 0), continue set loop
	FDCE	0(0,31)			; flush entry at (address)

	LDO	-1(19),19		; decrement count
	COMIB,<=	0,19,d_assoc_l	; if (count >= 0), loop
	ADD	31,23,31		; address++

	B	do_i_cache		; next
	SYNC				; synchronize after flush

d_assoc4_l				; 4-way set-associative flush loop
	FDCE	0(0,31)			; flush entry at (*address)
	FDCE	0(0,31)			; flush entry at (*address)
	FDCE	0(0,31)			; flush entry at (*address)
	FDCE,M	23(0,31)		; flush entry at (*address++)
	COMIB,<		0,19,d_assoc4_l	; if (count > 0), loop
	LDO	-1(19),19		; decrement count

	B	do_i_cache		; next
	SYNC				; synchronize after flush

d_assoc2_l				; 2-way set-associative flush loop
	FDCE	0(0,31)			; flush entry at (*address)
	FDCE,M	23(0,31)		; flush entry at (*address++)
	COMIB,<		0,19,d_assoc2_l	; if (count > 0), loop
	LDO	-1(19),19		; decrement count

	B	do_i_cache		; next
	SYNC				; synchronize after flush

d_direct_l				; direct-mapped flush loop
	FDCE,M	23(0,31)		; flush entry at (*address++)
	COMIB,<		0,19,d_direct_l	; if (count > 0), loop
	LDO	-1(19),19		; decrement count

d_sync
	SYNC				; synchronize after flush

do_i_cache
	BB,>=,N	26,31,L$exit1		; if I_CACHE is not set, return

	LDW	8(0,25),31		; 31 <- address (initially base)
	LDW	20(0,25),29		; 29 <- loop
	LDW	12(0,25),23		; 23 <- stride
	LDW	16(0,25),19		; 19 <- count

	LDO	-1(19),19		; decrement count
	COMIB,>,N	0,19,i_sync	; if (count < 0), no flush
	COMIB,=,N	1,29,i_direct_l
	COMIB,=,N	2,29,i_assoc2_l
	COMIB,=,N	4,29,i_assoc4_l

i_assoc_l				; set-associative cache flush-loop
	COPY	29,20			; 20 (lcount) <- loop

i_set_l					; set flush-loop
	LDO	-1(20),20		; decrement lcount
	COMIB,<=,N	0,20,i_set_l	; if (lcount >= 0), continue set loop
	FICE	0(5,31)			; flush entry at (address)

	LDO	-1(19),19		; decrement count
	COMIB,<=	0,19,i_assoc_l	; if (count >= 0), loop
	ADD	31,23,31		; address++

	B	i_skips			; next
	SYNC				; synchronize after flush

i_assoc4_l				; 4-way set-associative flush loop
	FICE	0(5,31)			; flush entry at (*address)
	FICE	0(5,31)			; flush entry at (*address)
	FICE	0(5,31)			; flush entry at (*address)
	FICE,M	23(5,31)		; flush entry at (*address++)
	COMIB,<		0,19,i_assoc4_l	; if (count > 0), loop
	LDO	-1(19),19		; decrement count

	B	i_skips			; next
	SYNC				; synchronize after flush

i_assoc2_l				; 2-way set-associative flush loop
	FICE	0(5,31)			; flush entry at (*address)
	FICE,M	23(5,31)		; flush entry at (*address++)
	COMIB,<		0,19,i_assoc2_l	; if (count > 0), loop
	LDO	-1(19),19		; decrement count

	B	i_skips			; next
	SYNC				; synchronize after flush

i_direct_l				; direct-mapped flush loop
	FICE,M	23(5,31)		; flush entry at (*address++)
	COMIB,<		0,19,i_direct_l	; if (count > 0), loop
	LDO	-1(19),19		; decrement count

i_sync
	SYNC				; synchronize after flush

i_skips
	NOP				; 7 instructionss as prescribed
	NOP				; by the programming note in the 
	NOP				; description for SYNC.
	NOP
	NOP

L$exit1
	BV	0(2)
	.EXIT
	NOP
	.PROCEND ;in=25,26;

	.SPACE	$TEXT$
	.SUBSPA $LIT$,QUAD=0,ALIGN=8,ACCESS=44
;	.SUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY
	.SUBSPA $UNWIND$,QUAD=0,ALIGN=8,ACCESS=44
	.SUBSPA $CODE$
	.SPACE	$PRIVATE$
	.SUBSPA $DATA$,QUAD=1,ALIGN=8,ACCESS=31
$THISMODULE$
ifelse(ASM_DEBUG,1,"interface_counter
	.ALIGN	8
	.WORD	0
interface_limit
	.WORD	0")
	.SUBSPA $BSS$,QUAD=1,ALIGN=8,ACCESS=31,ZERO
	.IMPORT $global$,DATA
	.IMPORT	Registers,DATA
	.IMPORT	Ext_Stack_Pointer,DATA
	.IMPORT	Free,DATA
	.IMPORT	utility_table,DATA
	.SPACE	$TEXT$
	.SUBSPA $CODE$
	.EXPORT C_to_interface,PRIV_LEV=3,ARGW0=GR,RTNVAL=GR
	.EXPORT interface_to_scheme,PRIV_LEV=3
	.EXPORT interface_to_C,PRIV_LEV=3
	.EXPORT scheme_to_interface_ble,PRIV_LEV=3
	.EXPORT trampoline_to_interface,PRIV_LEV=3
	.EXPORT scheme_to_interface,PRIV_LEV=3
	.EXPORT hook_jump_table,PRIV_LEV=3
	.EXPORT cache_flush_region,PRIV_LEV=3
	.EXPORT cache_flush_all,PRIV_LEV=3
	.END

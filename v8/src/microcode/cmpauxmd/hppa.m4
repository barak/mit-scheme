changecom(`;');;; -*-Midas-*-
;;;
;;;	$Id: hppa.m4,v 1.37 1995/07/26 18:41:54 adams Exp $
;;;
;;;	Copyright (c) 1989-1995 Massachusetts Institute of Technology
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
;;;;	GCC returns two word structures differently: It passes
;;;;	the address of the structure in gr28!
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
;;;;    - gr19 contains the continuation
;;;;	[- gr19 used to contain the dynamic link when needed.]
;;;;    - gr18 contains the value '() which is either #F for compatibility
;;;;           with previous releases, or a distinct value for '().  This
;;;;           value is cached from the register block.
;;;;	- gr5 contains the quad mask for machine pointers in the low bits
;;;;          The value of gr5 as a whole is #F.
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

;;;; NOTEs on choosing calling conventions:
;;
;;   . Some hooks and utilities really want their stuff on the stack.  For
;;     example, the invoke_primitive ought to have the continuation and the
;;     actuals on the stack.
;;   . For Some hooks the arguments are best passed in registers, possibly
;;     using a special calling convention (eg fixnum multiply).
;;   . Some utilities MUST have their arguments on the stack - e.g.
;;     comutil_primitive_apply.
;;   . Some utilities want arguments in special registers (e.g.
;;     comutil_link).
;;
;;  So in each case the compiled (scheme) code must call the hook or
;;  utility with the appropriate calling convention for that hook.  If
;;  a hook (e.g. string-allocate) (tail-)calls a primitive, but is
;;  unlikely to do so, then a register calling convention is
;;  appropriate and the hook must make a valid stack frame before
;;  punting to the primitive.
;;
;;  Trampolines are tricky because the scheme code that executes a
;;  trampoline is using a register calling convention but the (current
;;  and portable trampoline invokes a utility to reformat the stack.
;;  Hence the trampoline_to_interface code must be able to find a
;;  call-site arity in the trampoline and push a continuation and
;;  stack frame.
;;


changequote(",")
define(HEX, "0x$1")
define(ASM_DEBUG, 0)
define(TC_LENGTH, ifdef("TYPE_CODE_LENGTH", TYPE_CODE_LENGTH, 8))
define(TC_START, eval(TC_LENGTH - 1))
define(TC_FLONUM, -2)
define(TC_VECTOR, 0x17)
define(TC_POSITIVE_FIXNUM, 0x00)
define(TC_NEGATIVE_FIXNUM, 0x3F)

define(TC_STRING, 0x1A)
define(TC_PAIR, 0x0C)
define(TC_NMV, 0x06)
define(TC_CCENTRY, 0x10)
define(QUAD_MASK, eval(2 ** (TC_LENGTH - 2)))
define(DATUM_LENGTH, eval(32 - TC_LENGTH))

changequote([,])
define(WHEN_CCENTRY_UNTAGGED,[ifelse(TC_CCENTRY,QUAD_MASK,"$1")])
define(WHEN_CCENTRY_TAGGED,[ifelse(TC_CCENTRY,QUAD_MASK,"","$1")])
changequote(",")

;;  FIXNUM_LENGTH is the number of bits in the fixnum including the sign
;;  FIXNUM_POS is the position of the fixnum field in a machine arithmetic
;;    fixnum (i.e. untagged, perhaps shifted)
;;  FIXNUM_BIT is the bit to set to generate a machine integer which is one
;;   greater than the maximum positive fixnum
;;
;;  Fixnums are machine values in which the tag bits are all 0 or all 1

define(FIXNUM_LENGTH, eval(DATUM_LENGTH+1))
define(FIXNUM_POS, 31)
define(FIXNUM_BIT, eval(TC_LENGTH + 0))

define(FLONUM_VECTOR_HEADER, eval((TC_NMV * (2 ** DATUM_LENGTH)) + 2))

;;#define SHARP_F		0x22000010
;;#define SHARP_T		0x23000000

define(SHARP_F, HEX(22000010))	; Available from r5
define(SHARP_T, HEX(23000000))	; Available via LDIL

;; Temporary registers for register-preserving utilities

define(gt1,26)
define(gt2,25)
define(gt3,28)
define(gt4,29)
define(ft1,4)
define(ft2,5)

;; Scheme register assignments
define(rs_val,2)  ;not really

define(rs_ble,3)
define(rs_regblock,4)
define(rs_false,5)
define(rs_quad,5)
define(rs_empty_list,18)
define(rs_continuation,19)
define(rs_memtop,20)
define(rs_free,21)
define(rs_stack,22)
define(rs_closure,25)

;; register argument homes: 2 6 7 8 9 10 11 12 13 14 15 16 17 23 24
define(ARGUMENT_REGISTERS,15)
;;define(ARGUMENT_REGISTERS,3)
define(rs_arg1,2)	define(rs_arg2,6)
define(rs_arg3,7)	define(rs_arg4,8)
define(rs_arg5,9)	define(rs_arg6,10)
define(rs_arg7,11)	define(rs_arg8,12)
define(rs_arg9,13)	define(rs_arg10,14)
define(rs_arg11,15)	define(rs_arg12,16)
define(rs_arg13,17)	define(rs_arg14,23)
define(rs_arg15,24)

;; scheme intepreter register block offsets, as in const.h but with block
;; indexes (i.e. word offsets) adjusted to byte offsets (i.e. *4)

define(REGBLOCK_MEMTOP,0(0,rs_regblock))
define(REGBLOCK_INT_MASK,4(0,rs_regblock))
define(REGBLOCK_VAL,8(0,rs_regblock))
define(REGBLOCK_ENV,12(0,rs_regblock))
define(REGBLOCK_COMPILER_TEMP,16(0,rs_regblock))
define(REGBLOCK_EXPR,20(0,rs_regblock))
define(REGBLOCK_RETURN,24(0,rs_regblock))
define(REGBLOCK_LEXPR_ACTUALS,28(0,rs_regblock))
define(REGBLOCK_PRIMITIVE,32(0,rs_regblock))
define(REGBLOCK_CLOSURE_FREE,36(0,rs_regblock))
define(REGBLOCK_CLOSURE_SPACE,40(0,rs_regblock))
define(REGBLOCK_STACK_GUARD,44(0,rs_regblock))
define(REGBLOCK_INT_CODE,48(0,rs_regblock))
define(REGBLOCK_REFLECT_TO_INTERFACE,52(0,rs_regblock))
define(REGBLOCK_EMPTY_LIST,56(0,rs_regblock))    ; allows '() = #F compatability
define(REGBLOCK_COMPILER_PSEUDOREGS,64(rs_regblock))

;; C register assignments

define(rc_return_address,2)
define(rc_arg1,26)
define(rc_arg2,25)
define(rc_arg3,24)
define(rc_arg4,23)

define(rc_static_area,27)
define(rc_val,28)
define(rc_val_2,29)
define(rc_stack,30)

;; Register assignments for generic arithmetic

define(gen_arg1,rs_arg1)
define(gen_arg2,rs_arg2)
define(gen_continuation,rs_continuation)

define(ENTRY_TO_ADDRESS, "")
;;WHEN_CCENTRY_UNTAGGED(define(ENTRY_TO_ADDRESS, ""))
;;WHEN_CCENTRY_TAGGED(define(ENTRY_TO_ADDRESS,
;;				 "DEP	rs_quad,TC_START,TC_LENGTH,$1"))


define(C_FRAME_SIZE,
       ifdef("HPC", 112,
	     ifdef("GCC", 120,
	           `Unknown C compiler: bad frame size')))
define(INT_BIT_STACK_OVERFLOW, 31)

	.SPACE  $TEXT$
	.SUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY

C_to_interface
;; rc_arg1 = compiled entry point
;;
	.PROC
	.CALLINFO CALLER,FRAME=28,SAVE_RP
	.ENTRY
	;; Save C callee-saves registers on C stack
	STW	rc_return_address,-20(0,rc_stack)	; Save return address
	STWM	3,eval(C_FRAME_SIZE)(rc_stack)		; Save first reg, 
	STW	4,-108(rc_stack)			;  and allocate frame
	STW	5,-104(rc_stack)			; Save the other regs..
	STW	6,-100(rc_stack)
	STW	7,-96(rc_stack)
	STW	8,-92(rc_stack)
	STW	9,-88(rc_stack)
	STW	10,-84(rc_stack)
	STW	11,-80(rc_stack)
	STW	12,-76(rc_stack)
	STW	13,-72(rc_stack)
	STW	14,-68(rc_stack)
	STW	15,-64(rc_stack)
	STW	16,-60(rc_stack)
	STW	17,-56(rc_stack)
	STW	18,-52(rc_stack)

	ADDIL	L'Registers-$global$,rc_static_area
	LDO	R'Registers-$global$(1),rs_regblock	; Setup Regs
	;; The Quad mask is the low bits of the SHARP_F
	LDIL	L'SHARP_F,rs_false
	;; ADDI avoids assember bug with:  LDO  R'SHARP_F(rs_false),rs_false
	ADDI	R'SHARP_F,rs_false,rs_false
	LDW	REGBLOCK_EMPTY_LIST(0,rs_regblock),rs_empty_list

ep_interface_to_scheme
	;;	LDW	REGBLOCK_VAL(0,rs_regblock),rs_arg1
	ADDIL	L'Ext_Stack_Pointer-$global$,rc_static_area
	LDW	R'Ext_Stack_Pointer-$global$(1),rs_stack

;; The Scheme stack holds: a count of actual parameters, that number of
;; actual parameters, a continuation.
;;
;;  hi		| ...            | 
;;		| continuation   |
;;		| argN           |
;;		| ...            |
;;		| arg2           |
;;		| arg1           |
;;  lo		| arg count N    |  <- rs_stack
;;
;; Pop values off stack into the appropriate places for the Scheme
;; calling convention.

	LDWM	4(0,rs_stack),gt3		; pop arg count

	;; Now reverse part of the stack like this:
	;; [deep] cont argN argN-1 ... argK+2 argK+1 argK ... arg1
	;; [deep] argK+1 argK+2 ... argN-1 argN cont argK ... arg1
	;; where K is ARGUMENT_REGISTERS
	LDO	eval(ARGUMENT_REGISTERS*4)(rs_stack),rs_arg1 ; addr(argK+1)
	SH2ADDL gt3,rs_stack,rs_arg2	; addr(cont)
	COMB,>=,N	rs_arg1,rs_arg2,popa_reverse_finished
popa_reverse_loop
	LDW	0(0,rs_arg2),rs_arg4
	LDW	0(0,rs_arg1),rs_arg3
	STWM	rs_arg4,4(rs_arg1)	; write then bump
	ADDI	-4,rs_arg2,rs_arg2
	COMB,<	rs_arg1,rs_arg2,popa_reverse_loop
	STW	rs_arg3,4(rs_arg2)	
popa_reverse_finished	

	ADDIB,<,N  -1,gt3,all_args_loaded
	LDWM	4(0,rs_stack),rs_arg1
	ADDIB,<,N  -1,gt3,all_args_loaded
	LDWM	4(0,rs_stack),rs_arg2
	ADDIB,<,N  -1,gt3,all_args_loaded
	LDWM	4(0,rs_stack),rs_arg3
	ADDIB,<,N  -1,gt3,all_args_loaded
	LDWM	4(0,rs_stack),rs_arg4
	ADDIB,<,N  -1,gt3,all_args_loaded
	LDWM	4(0,rs_stack),rs_arg5
	ADDIB,<,N  -1,gt3,all_args_loaded
	LDWM	4(0,rs_stack),rs_arg6
	ADDIB,<,N  -1,gt3,all_args_loaded
	LDWM	4(0,rs_stack),rs_arg7
	ADDIB,<,N  -1,gt3,all_args_loaded
	LDWM	4(0,rs_stack),rs_arg8
	ADDIB,<,N  -1,gt3,all_args_loaded
	LDWM	4(0,rs_stack),rs_arg9
	ADDIB,<,N  -1,gt3,all_args_loaded
	LDWM	4(0,rs_stack),rs_arg10
	ADDIB,<,N  -1,gt3,all_args_loaded
	LDWM	4(0,rs_stack),rs_arg11
	ADDIB,<,N  -1,gt3,all_args_loaded
	LDWM	4(0,rs_stack),rs_arg12
	ADDIB,<,N  -1,gt3,all_args_loaded
	LDWM	4(0,rs_stack),rs_arg13
	ADDIB,<,N  -1,gt3,all_args_loaded
	LDWM	4(0,rs_stack),rs_arg14
	ADDIB,<,N  -1,gt3,all_args_loaded
	LDWM	4(0,rs_stack),rs_arg15


all_args_loaded
	LDWM	4(0,rs_stack),rs_continuation	; finally, pop continuation

ep_interface_to_scheme_2
	;; A (modified) copy of this code is part of invoke_primitive
	LDW	REGBLOCK_MEMTOP(0,rs_regblock),rs_memtop
	ADDIL	L'Free-$global$,rc_static_area
	LDW	R'Free-$global$(1),rs_free
	.CALL	RTNVAL=GR		; out=28
	BLE	0(5,rc_arg1)		; Invoke entry point...
	LDO	scheme_to_interface_ble-trampoline_to_interface(31),rs_ble
					; with rs_ble = scheme_to_interface_ble

;; The compiler knows offsets from scheme_to_interface_ble including
;; offsets of trampoline_to_interface, scheme_to_interface and
;; hook_jump_table.

trampoline_to_interface
	;; On entry:  r31 points into trampoline
	;;            r28 is the utility index
	;; Note that we must preserve rc_arg[2-4]
	LDW	-3(31),29			; get trampoline arg count

bkpt_hook_to_interface
	;; On entry: r31 points to trampoline or breakpointed procedure/closure
	;;	     r28 is the utility index
	;;	     r29 is the frame size
	STWM	rs_continuation,-4(0,rs_stack)
	BL	push_arg_registers,rs_continuation
	ADDI	4,rs_continuation,rs_continuation    ; on return skip ADDI insn:

scheme_to_interface_ble
	ADDI	4,31,31				; Skip over format word ...

	COPY	31,rc_arg1
	DEP	0,31,2,rc_arg1			; clear privilege bits

scheme_to_interface
	;;STW	rs_val,REGBLOCK_VAL(0,rs_regblock)
	ADDIL	L'hppa_utility_table-$global$,rc_static_area
	LDW	R'hppa_utility_table-$global$(1),29
	ADDIL	L'Ext_Stack_Pointer-$global$,rc_static_area
	LDWX,S	28(0,29),29				; Find handler
	STW	rs_stack,R'Ext_Stack_Pointer-$global$(1)
	ADDIL	L'Free-$global$,rc_static_area
	STW	rs_free,R'Free-$global$(1)		; Update free

	ifelse(ASM_DEBUG,1,"ADDIL L'interface_counter-$global$,rc_static_area
	LDW	R'interface_counter-$global$(1),21
	LDO	1(21),21
	STW	21,R'interface_counter-$global$(1)
	ADDIL	L'interface_limit-$global$,rc_static_area
	LDW	R'interface_limit-$global$(1),22
	COMB,=,N	21,22,interface_break
interface_proceed")
	ifdef("GCC", "LDO	-116(rc_stack),28")

	.CALL	ARGW0=GR,ARGW1=GR,ARGW2=GR,ARGW3=GR,RTNVAL=GR
	BLE	0(4,29)				; Call handler
	COPY	31,2				; Setup return address
	ifdef("GCC", "LDW	-116(rc_stack),28
		      LDW	-112(rc_stack),29")
	BV	0(28)				; Call receiver
	COPY	29,rc_arg1			; Setup entry point

;; This sequence of NOPs is provided to allow for reassembly of the
;; sequence that appears above using different ifdefs without having
;; to recompile the world.  The compiler "knows" the distance between
;; scheme_to_interface_ble and hook_jump_table (100 bytes) and this
;; distance is incorporated in the instuctions invoking the hooks.

	ifelse(ASM_DEBUG,1,"","NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP")
	ifdef("GCC","","NOP
	NOP
	NOP")
	NOP

;; This label is used by the trap handler
;;
;; Note that in each entry we have copied the first instruction of the
;; hook into the delay slot of the branch and adjusted the branch by
;; +4 to skip that instruction in the hook code.

ep_scheme_hooks_low
hook_jump_table					; scheme_to_interface + 100
store_closure_code_hook
	B	store_closure_code+4
	LDIL	L'0x23400000,20			; LDIL opcode and register

store_closure_entry_hook
	B	store_closure_entry+4
	DEP	0,31,2,1			; clear PC protection bits

multiply_fixnum_hook
	B	multiply_fixnum+4
	STW	26,0(0,rs_free)

fixnum_quotient_hook
	B	fixnum_quotient+4
	STW	25,4(0,rs_free)			; yes, this is arg2

fixnum_remainder_hook
	B	fixnum_remainder+4
	STWM	29,-4(0,rs_stack)			; Preserve gr29

fixnum_lsh_hook
	B	fixnum_lsh+4
	NOP

generic_plus_hook
	B	generic_plus+4
	EXTRS	gen_arg1,TC_START,TC_LENGTH,gt3		; type of arg1

generic_subtract_hook
	B	generic_subtract+4
	EXTRS	gen_arg1,TC_START,TC_LENGTH,gt3		; type of arg1

generic_times_hook
	B	generic_times+4
	EXTRS	gen_arg1,TC_START,TC_LENGTH,gt3		; type of arg1

generic_divide_hook
	B	generic_divide+4
	EXTRS	gen_arg1,TC_START,TC_LENGTH,gt3		; type of arg1

generic_equal_hook
	B	generic_equal+4
	EXTRS	gen_arg1,TC_START,TC_LENGTH,gt3		; type of arg1

generic_less_hook
	B	generic_less+4
	EXTRS	gen_arg1,TC_START,TC_LENGTH,gt3		; type of arg1

generic_greater_hook
	B	generic_greater+4
	EXTRS	gen_arg1,TC_START,TC_LENGTH,gt3		; type of arg1

generic_increment_hook
	B	generic_increment+4
	EXTRS	gen_arg1,TC_START,TC_LENGTH,gt3		; type of arg1

generic_decrement_hook
	B	generic_decrement+4
	EXTRS	gen_arg1,TC_START,TC_LENGTH,gt3		; type of arg1

generic_zero_hook
	B	generic_zero+4
	EXTRS	gen_arg1,TC_START,TC_LENGTH,gt3		; type of arg1

generic_positive_hook
	B	generic_positive+4
	EXTRS	gen_arg1,TC_START,TC_LENGTH,gt3		; type of arg1

generic_negative_hook
	B	generic_negative+4
	EXTRS	gen_arg1,TC_START,TC_LENGTH,gt3		; type of arg1

shortcircuit_apply_hook
	B	shortcircuit_apply+4
	EXTRU	26,5,6,gt4			; procedure type -> gt4

shortcircuit_apply_1_hook
	B	shortcircuit_apply_1+4
	EXTRU	26,5,6,gt4			; procedure type -> gt4

shortcircuit_apply_2_hook
	B	shortcircuit_apply_2+4
	EXTRU	26,5,6,gt4			; procedure type -> gt4

shortcircuit_apply_3_hook
	B	shortcircuit_apply_3+4
	EXTRU	26,5,6,gt4			; procedure type -> gt4

shortcircuit_apply_4_hook
	B	shortcircuit_apply_4+4
	EXTRU	26,5,6,gt4			; procedure type -> gt4

shortcircuit_apply_5_hook
	B	shortcircuit_apply_5+4
	EXTRU	26,5,6,gt4			; procedure type -> gt4

shortcircuit_apply_6_hook
	B	shortcircuit_apply_6+4
	EXTRU	26,5,6,gt4			; procedure type -> gt4

shortcircuit_apply_7_hook
	B	shortcircuit_apply_7+4
	EXTRU	26,5,6,gt4			; procedure type -> gt4

shortcircuit_apply_8_hook
	B	shortcircuit_apply_8+4
	EXTRU	26,5,6,gt4			; procedure type -> gt4

stack_and_interrupt_check_hook
	B	stack_and_interrupt_check+4
	LDW	REGBLOCK_STACK_GUARD(0,rs_regblock),25

invoke_primitive_hook
	B	invoke_primitive+4
	DEPI	0,31,2,31			; clear privilege bits

vector_cons_hook
	B	vector_cons+4
	COPY	rs_free,gt1

string_allocate_hook
	B	string_allocate+4
	COPY	rs_free,gt1			; return value

floating_vector_cons_hook
	B	floating_vector_cons+4
	ZDEP	rs_arg1,31,DATUM_LENGTH,rs_arg1	; length as machine word

flonum_sin_hook
	B	flonum_sin+4
	COPY	rs_stack,17

flonum_cos_hook
	B	flonum_cos+4
	COPY	rs_stack,17

flonum_tan_hook
	B	flonum_tan+4
	COPY	rs_stack,17

flonum_asin_hook
	B	flonum_asin+4
	COPY	rs_stack,17

flonum_acos_hook
	B	flonum_acos+4
	COPY	rs_stack,17

flonum_atan_hook
	B	flonum_atan+4
	COPY	rs_stack,17

flonum_exp_hook
	B	flonum_exp+4
	COPY	rs_stack,17

flonum_log_hook
	B	flonum_log+4
	COPY	rs_stack,17

flonum_truncate_hook
	B	flonum_truncate+4
	COPY	rs_stack,17

flonum_ceiling_hook
	B	flonum_ceiling+4
	COPY	rs_stack,17

flonum_floor_hook
	B	flonum_floor+4
	COPY	rs_stack,17

flonum_atan2_hook
	B	flonum_atan2+4
	COPY	rs_stack,17

compiled_code_bkpt_hook				; hook 44 (offset 451 + 1)
	B	compiled_code_bkpt+4
	LDO	-8(31),31

compiled_closure_bkpt_hook			; hook 45 (offset 451 + 9)
	B	compiled_closure_bkpt+4
	LDO	-12(31),31

copy_closure_pattern_hook
	B	copy_closure_pattern+4
	LDW	-3(0,31),29			; offset

copy_multiclosure_pattern_hook
	B	copy_multiclosure_pattern+4
	LDW	-3(0,31),29			; offset	

closure_entry_bkpt_hook				; hook 48 (offset 451 + 33)
	B	closure_entry_bkpt+4
	LDO	-8(31),31			; bump back to entry point

new_procedure_interrupt_hook			; hook 49
	B	new_procedure_interrupt+4
	LDW	-3(0,31),20			; return address offset

new_continuation_interrupt_hook			; hook 50
	B	new_continuation_interrupt+4
	LDW	-3(0,31),20			; return address offset

new_closure_interrupt_hook
	B	new_interrupt_common		; hook 51
	COPY	rs_closure,20

generic_quotient_hook				; hook 52
	B	generic_quotient+4
	STWM	gen_continuation,-4(0,rs_stack)


generic_remainder_hook				; hook 53
	B	generic_remainder+4
	STWM	gen_continuation,-4(0,rs_stack)

interpreter_call_hook
	B	interpreter_call		; hook 54
	NOP

profile_count_hook
	LDW     -7(0,31),1
	ADDI    1,1,1
	BE      0(5,31)
	STW     1,-7(0,31)
	
;;
;; Provide dummy trapping hooks in case a newer version of compiled
;; code that expects more hooks is run.
;;

no_hook
	BREAK	0,55
	NOP
	BREAK	0,56
	NOP
	BREAK	0,57
	NOP
	BREAK	0,58
	NOP
	BREAK	0,59
	NOP
	BREAK	0,60
	NOP
	BREAK	0,61
	NOP
	BREAK	0,62
	NOP
	BREAK	0,63
	NOP
	BREAK	0,64
	NOP
	BREAK	0,65
	NOP
	BREAK	0,66
	NOP
	BREAK	0,67
	NOP
	BREAK	0,68
	NOP
	BREAK	0,69
	NOP
	BREAK	0,70
	NOP
	BREAK	0,71
	NOP
	BREAK	0,72
	NOP
	BREAK	0,73
	NOP


ifelse(ASM_DEBUG,1,"interface_break
	COMB,=	21,22,interface_break
	NOP
	B,N	interface_proceed")

;;  push_arg_registers saves the Scheme argument registers on the stack to
;;  make an interpreter compatible call frame.
;;  Inputs: Scheme register set rs_continuation + rs_arg1, rs_arg2, ... rs_argK
;;          count N  pa_count=r29
;;	    link     rs_continuation
;;  Stack [deep]: argK+1  (K = ARGUMENT_REGISTERS)
;;                argK+2
;;                ...
;;                argN    
;;        [top]   continuation
;;  Must preserve rc_arg[1-4], r28
;;
;;  Outputs:
;;  Stack [deep]: continuation
;;                argN
;;                argN-1
;;                ...
;;        [top]   arg1
;;
;; Uses: 1, rs_empty_list

define(pa_count,29)
define(pa_link,rs_continuation)
define(pa_ptr,rs_empty_list)	; auxillary pointer
define(pa_tmp,1)	; auxillary pointer

push_arg_registers

	SUBI,>=	ARGUMENT_REGISTERS,pa_count,1	; complement
	LDI	0,1	
	;; complement of number of regs is in 1, from ARGUMENT_REGISTERS down to
        ;; 0 inclusive
	BLR	1,0
	NOP

	STWM	rs_arg15,-4(0,rs_stack)
	NOP
	STWM	rs_arg14,-4(0,rs_stack)
	NOP
	STWM	rs_arg13,-4(0,rs_stack)
	NOP
	STWM	rs_arg12,-4(0,rs_stack)
	NOP
	STWM	rs_arg11,-4(0,rs_stack)
	NOP
	STWM	rs_arg10,-4(0,rs_stack)
	NOP
	STWM	rs_arg9,-4(0,rs_stack)
	NOP
	STWM	rs_arg8,-4(0,rs_stack)
	NOP
	STWM	rs_arg7,-4(0,rs_stack)
	NOP
	STWM	rs_arg6,-4(0,rs_stack)
	NOP
	STWM	rs_arg5,-4(0,rs_stack)
	NOP
	STWM	rs_arg4,-4(0,rs_stack)
	NOP
	STWM	rs_arg3,-4(0,rs_stack)
	NOP
	STWM	rs_arg2,-4(0,rs_stack)
	NOP
	STWM	rs_arg1,-4(0,rs_stack)
	NOP
	;; (This is where we land with 0 args in regs.)

	;; Now reverse part of the stack like this:
	;; [deep] argK+1 argK+2 ... argN-1 argN cont argK ... arg1
	;; [deep] cont argN argN-1 ... argK+2 argK+1 argK ... arg1
	;; where K is ARGUMENT_REGISTERS
	LDO	eval(ARGUMENT_REGISTERS*4)(rs_stack),rs_arg1 ; addr(argK+1)
	SH2ADDL pa_count,rs_stack,rs_arg2	; addr(cont)
	COMB,>=,N	rs_arg1,rs_arg2,pa_reverse_finished
pa_reverse_loop
	LDW	0(0,rs_arg2),rs_arg4
	LDW	0(0,rs_arg1),rs_arg3
	STWM	rs_arg4,4(rs_arg1)	; write then bump
	ADDI	-4,rs_arg2,rs_arg2
	COMB,<	rs_arg1,rs_arg2,pa_reverse_loop
	STW	rs_arg3,4(rs_arg2)	
pa_reverse_finished	

	BV	0(pa_link)
	NOP

store_closure_entry
;;
;; On arrival, 31 has a return address and 1 contains the address to
;; which the closure should jump with pc protection bits.
;; 26 contains the format/gc-offset word for this entry.
;;
	DEP	0,31,2,1			; clear PC protection bits
	STWM	26,4(0,rs_free)			; move format long to heap
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
	LDIL	L'0x23400000,20			; LDIL opcode and register
	EXTRU	1,0,1,5
	DEP	5,31,1,20
	EXTRU	1,11,11,5
	DEP	5,30,11,20
	EXTRU	1,13,2,5
	DEP	5,17,2,20
	EXTRU	1,18,5,5
	DEP	5,15,5,20
	STW	20,0(0,rs_free)			; Store LDIL instruction
	LDIL	L'0xe7406000,20			; BLE opcode, register
	LDO	R'0xe7406000(20),20		;  and nullify
	EXTRU	1,19,1,5
	DEP	5,29,1,20
	EXTRU	1,29,10,5
	DEP	5,28,10,20
	STW	20,4(0,rs_free)			; Store BLE instruction
	LDIL	L'0xb7ff07e9,20
	LDO	R'0xb7ff07e9(20),20
	STW	20,8(0,rs_free)			; Store ADDI instruction
	LDI	12,20
	FDC	0(0,rs_free)			; flush 1st inst. from D-cache
	FDC	20(0,rs_free)			; flush last inst. from D-cache
	SYNC
	FIC,M	20(5,rs_free)			; flush 1st inst. from I-cache
	SYNC
	;; Restore register 5
	LDIL	L'SHARP_F,5
	;	LDO	R'SHARP_F(5),5	; Assembler bug!
	ADDI	R'SHARP_F,5,5
	BE	0(5,31)				; Return
	LDW	REGBLOCK_MEMTOP(0,rs_regblock),rs_memtop  ;restore



multiply_fixnum
;; untagged integer version
;; On arrival, 31 has a return address and 26 and 25 have the fixnum arguments.
;;
	STW	26,0(0,rs_free)			; copy in jump table
	STW	25,4(0,rs_free)
	ZDEPI	1,TC_LENGTH-1,DATUM_LENGTH+1,26	; FIXNUM_LIMIT
	FLDWS	0(0,rs_free),4
	FLDWS	4(0,rs_free),5
	STW	26,8(0,rs_free)			; FIXNUM_LIMIT
        FCNVXF,SGL,DBL  4,4			; arg1
        FCNVXF,SGL,DBL  5,5			; arg2
	FMPY,DBL	4,5,4
	FLDWS	8(0,rs_free),5			; FIXNUM_LIMIT
        FCNVXF,SGL,DBL  5,5			; FIXNUM_LIMIT
	COPY	0,25				; signal no overflow
	FCMP,DBL,!>=	4,5			; result too large?
	FTEST
	B,N	multiply_fixnum_ovflw
	FSUB,DBL	0,5,5
	FCMP,DBL,!<	4,5			; result too small?
	FTEST
	B,N	multiply_fixnum_ovflw
	FCNVFXT,DBL,SGL	4,5
	FSTWS	5,0(0,rs_free)			; result
	BE	0(5,31)				; return
	LDW	0(0,rs_free),26

;;
multiply_fixnum_ovflw
	LDO	1(0),25				; signal overflow
	BE	0(5,31)				; return
	LDI	0,26 				; unused return `product'



fixnum_quotient
;;
;; On arrival, 31 has a return address and 26 and 25 have the fixnum arguments.
;; Note that quotient only overflows when dividing by 0 and when the
;; divisor is -1 and the dividend is the most negative fixnum,
;; producing the most positive fixnum plus 1.
;;
	STW	25,4(0,rs_free)			; arg2
	COMB,=	0,25,fixnum_quotient_ovflw
	STW	26,0(0,rs_free)
	ZDEPI	1,TC_LENGTH,FIXNUM_BIT,25	; FIXNUM_LIMIT
	FLDWS	0(0,rs_free),4
	FLDWS	4(0,rs_free),5
        FCNVXF,SGL,DBL  4,4			; arg1
        FCNVXF,SGL,DBL  5,5			; arg2
	FDIV,DBL	4,5,4
	STW	25,0(0,rs_free)			; FIXNUM_LIMIT
	FCNVFXT,DBL,SGL	4,5
	FSTWS	5,4(0,rs_free)			; result
	FLDWS	0(0,rs_free),5			; FIXNUM_LIMIT
	FCNVXF,SGL,DBL	5,5
	FCMP,DBL,!>=	4,5			; result too large?
	LDW	4(0,rs_free),26
	COPY	0,25				; signal no overflow
	FTEST
;;
fixnum_quotient_ovflw
	LDO	1(0),25				; signal overflow
	BE	0(5,31)				; return
	NOP



;; fixnum_remainder
;;
;; NOTE: The following code is disabled because the FREM instruction
;;	 has been dropped from the architecture and has never been
;;	 implemented in hardware.
;;
;; On arrival, 31 has a return address and 26 and 25 have the fixnum arguments.
;; Note that remainder only overflows when dividing by 0.
;; Note also that the FREM instruction does not compute the same as
;; the Scheme remainder operation.  The sign of the result must
;; sometimes be adjusted.
;;
;;	EXTRS	26,FIXNUM_POS,FIXNUM_LENGTH,26	; arg1
;;	COMB,=,N	0,25,fixnum_remainder_ovflw
;;	STW	26,0(0,21)
;;	EXTRS	25,FIXNUM_POS,FIXNUM_LENGTH,25	; arg2
;;	STW	25,4(0,21)
;;	FLDWS	0(0,21),4
;;	FLDWS	4(0,21),5
;;	FCNVXF,SGL,DBL  4,4			; arg1
;;	FCNVXF,SGL,DBL  5,5			; arg2
;;	FREM,DBL	4,5,4
;;	FCNVFXT,DBL,SGL	4,5
;;	FSTWS	5,4(0,21)			; result
;;	LDW	4(0,21),1
;;	XOR,<	26,1,0				; skip if signs !=
;;	B,N	fixnum_remainder_done
;;	COMB,=,N	0,1,fixnum_remainder_done
;;	XOR,<	26,25,0				; skip if signs !=
;;	ADD,TR	1,25,1				; result += arg2
;;	SUB	1,25,1				; result -= arg2
;;;;
;;fixnum_remainder_done
;;	ZDEP    1,FIXNUM_POS,FIXNUM_LENGTH,26	; make into fixnum
;;	BE	0(5,31)				; return
;;	COPY	0,25				; signal no overflow
;;;;
;;fixnum_remainder_ovflw
;;	BE	0(5,31)				; return
;;	LDO	1(0),25				; signal overflow


fixnum_remainder
;;
;; On arrival, 31 has a return address and 26 and 25 have the fixnum
;; arguments.
;; Remainder can overflow only if arg2 = 0.
;;
	STWM	29,-4(0,rs_stack)		; Preserve gr29
	COMB,=,N  0,25,fixnum_remainder_ovflw
	STWM	31,-4(0,rs_stack)		; Preserve ret. add.
	STWM	26,-4(0,rs_stack)		; Preserve arg1
        .CALL   				;in=25,26;out=29; (MILLICALL)
	BL	$$remI,31
	STWM	25,-4(0,rs_stack)		; Preserve arg2
;;
	LDWM	4(0,rs_stack),25		; Restore arg2
	LDWM	4(0,rs_stack),26		; Restore arg1
	XOR,<	26,29,0				; Skip if signs !=
	B,N	fixnum_remainder_done
	COMB,=,N	0,29,fixnum_remainder_done
	XOR,<	26,25,0
	ADD,TR	29,25,29			; setup result
	SUB	29,25,29
;;
fixnum_remainder_done
	COPY	29,26				; make into fixnum
	LDWM	4(0,rs_stack),31		; Restore ret. add.
	COPY	0,25				; signal no overflow
	BE	0(5,31)				; return
	LDWM	4(0,rs_stack),29		; Restore gr29
;;
fixnum_remainder_ovflw
	LDO	1(0),25				; signal overflow
	COPY	0,26				; bogus return value
	BE	0(5,31)				; return
	LDWM	4(0,rs_stack),29		; Restore gr29	


fixnum_lsh
;;
;; On arrival, 31 has a return address and 26 and 25 have the fixnum arguments.
;; If arg2 is negative, it is a right shift, otherwise a left shift.
;;
	NOP
	COMB,<,N	0,25,fixnum_lsh_positive
	;; Shifting by a non-positive amount: we want to right shift
	SUB	0,25,25				; negate, for right shift
	COMICLR,>	FIXNUM_LENGTH,25,0
	COPY	0,26
	MTSAR	25
	VSHD	0,26,26				; shift right
	BE	0(5,31)
	COPY	0,25

;;
fixnum_lsh_positive
	;; Shifting by a positive amount: we want to left shift
	SUBI,>	32,25,25			; shift amount for right shift
	COPY	0,26				; shift left completely
	MTSAR	25
	VSHD	26,0,26				; shift right (32 - arg2)
	EXTRS	26,31,FIXNUM_LENGTH,26		; normalize fixnum
	BE	0(5,31)				; return
	COPY	0,25				; signal no overflow

;;;; Generic arithmetic utilities.
;;;
;;;  On entry, arguments in gen_arg1 (and gen_arg2 in binary operators),
;;;  and return address in gen_continuation. The result is in rs_arg1(=rs_val)
;;;  Modifies: gen_arg1,gen_arg2,gen_continuation,gt1-gt4,ft1 and ft2

changequote([,])

;; JUMP_NOT_FIXNUM_FORWARD(reg,temp,label)
define(JUMP_NOT_FIXNUM_FORWARD,
	[EXTRS		$1,31,FIXNUM_LENGTH,$2
	COMB,<>,N	$1,$2,$3])


;; The next macros are used for building FLONUM results.  There are >1
;; macros to allow some flixibility in instruction scheduling.  They
;; must appear in order and after the last reference to rs_free and
;; rs_arg1, like this:
;;
;;	FLONUM_RESULT_PREP
;;	FLONUM_RESULT(float_reg)

define(FLONUM_RESULT_PREP,
       [DEPI	4,31,3,rs_free		; float align free
	COPY	rs_free,rs_val])	; prepare result
define(FLONUM_RESULT,
       [LDIL	L'FLONUM_VECTOR_HEADER,gt3
	;; Assembler bug:	LDO	R'FLONUM_VECTOR_HEADER(gt3),gt3
	ADDI	R'FLONUM_VECTOR_HEADER,gt3,gt3
	STWM	gt3,4(0,rs_free)			; vector header
	DEPI	TC_FLONUM,TC_START,TC_LENGTH,rs_val	; tag pointer to vector as flonum
	ENTRY_TO_ADDRESS(gen_continuation)
	BLE	0(5,gen_continuation)			; return!
	FSTDS,MA  $1,8(0,rs_free)])			; store floating data

;; Same idea for flonum comparisons:
;;	GENERIC_BOOLEAN_RESULT(test,freg1,freg2)
define(GENERIC_BOOLEAN_RESULT,
       [LDIL	L'SHARP_T,rs_val
	FCMP,DBL,$1	$2,$3
	FTEST
	COPY	rs_false,rs_val
	ENTRY_TO_ADDRESS(gen_continuation)
	BLE,N	0(5,gen_continuation)])		; return!

define(define_generic_binary,
[generic_$1
	EXTRS		gen_arg1,TC_START,TC_LENGTH,gt3	; type of arg1
	COMIB,<>	TC_FLONUM,gt3,generic_$1_notflo_unk
	EXTRS		gen_arg2,TC_START,TC_LENGTH,gt4	; type of arg2
	COMIB,<>,N	TC_FLONUM,gt4,generic_$1_flo_notflo
	DEP	rs_quad,TC_START,TC_LENGTH,gen_arg1	; object->address
	FLDDS	4(0,gen_arg1),ft1			; arg1 -> ft1
	DEP	rs_quad,TC_START,TC_LENGTH,gen_arg2	; object->address
	FLDDS	4(0,gen_arg2),ft2			; arg2 -> ft2
	FLONUM_RESULT_PREP
	$3,DBL	ft1,ft2,ft1				; operate
	FLONUM_RESULT(ft1)


generic_$1_notflo_unk					; ~FLO * ??
	JUMP_NOT_FIXNUM_FORWARD(gen_arg1,gt3,generic_$1_fail)
	COMIB,<>,N	TC_FLONUM,gt4,generic_$1_fix_unk
	STW	gen_arg1,0(0,rs_free)			; through heap memory into fp
	DEP	rs_quad,TC_START,TC_LENGTH,gen_arg2	; object->address
	FLDWS	0(0,rs_free),ft1			; single int arg1 -> ft1
	FLDDS	4(0,gen_arg2),ft2			; arg2 -> ft2
        FCNVXF,SGL,DBL  ft1,ft1				; convert to double float
	FLONUM_RESULT_PREP
	$3,DBL	ft1,ft2,ft1				; operate
	FLONUM_RESULT(ft1)

generic_$1_flo_notflo					; FLO * ~FLO
	JUMP_NOT_FIXNUM_FORWARD(gen_arg2,gt4,generic_$1_fail)
	STW	gen_arg2,0(0,rs_free)			; through heap memory into fpcp
	DEP	rs_quad,TC_START,TC_LENGTH,gen_arg1	; object->address
	FLDWS	0(0,rs_free),ft2			; single int arg2 -> ft2
	FLDDS	4(0,gen_arg1),ft1			; arg1 -> ft1
        FCNVXF,SGL,DBL  ft2,ft2				; convert to double float
	FLONUM_RESULT_PREP
	$3,DBL	ft1,ft2,ft1				; operate
	FLONUM_RESULT(ft1)

generic_$1_fix_unk					; FIX * ??
ifelse($4,NONE,[],[
	JUMP_NOT_FIXNUM_FORWARD(gen_arg2,gt4,generic_$1_fail)
	$4	gen_arg1,gen_arg2,gt4			; add/sub
	JUMP_NOT_FIXNUM_FORWARD(gt4,gt3,generic_$1_fail)
	ENTRY_TO_ADDRESS(gen_continuation)
	BLE	0(5,gen_continuation)			; return!
	COPY	gt4,rs_arg1])

generic_$1_fail						; ?? * ??, out of line
	;; Build stack frame
	STWM	gen_continuation,-4(0,rs_stack)
	STWM	gen_arg2,-4(0,rs_stack)
	STWM	gen_arg1,-4(0,rs_stack)
	BB,<	31,31,scheme_to_interface
	LDI	HEX($2),gt3				; operation code
	BL,N	do_preserve_2,gt1
	NOP
	B	scheme_to_interface
	LDI	HEX($2),gt3				; operation code])

;; Generic times is coded separately because we want to handle
;; multiplication by exact 0.

generic_times
	EXTRS		gen_arg1,TC_START,TC_LENGTH,gt3	; gt3 <- type of arg1
	COMIB,<>	TC_FLONUM,gt3,generic_times_notflo_unk
	EXTRS		gen_arg2,TC_START,TC_LENGTH,gt4	; gt4 <- type of arg2
	COMIB,<>,N	TC_FLONUM,gt4,generic_times_flo_notflo
generic_times_flo_flo
	DEP	rs_quad,TC_START,TC_LENGTH,gen_arg1	; object->address
	FLDDS	4(0,gen_arg1),ft1			; arg1 -> ft1
	DEP	rs_quad,TC_START,TC_LENGTH,gen_arg2	; object->address
	FLDDS	4(0,gen_arg2),ft2			; arg2 -> ft2
	FLONUM_RESULT_PREP
	FMPY,DBL	ft1,ft2,ft1
	FLONUM_RESULT(ft1)

generic_times_notflo_unk				; ~FLO * ??
	JUMP_NOT_FIXNUM_FORWARD(gen_arg1,gt3,generic_times_fail)
	;;  FIX * ??
	COMIB,<>,N	TC_FLONUM,gt4,generic_times_fix_unk
generic_times_fix_flo					; FIX * FLO
	COMIB,=,N	0,gen_arg1,generic_times_0_flo
	STW	gen_arg1,0(0,rs_free)			; through heap memory into fp
	DEP	rs_quad,TC_START,TC_LENGTH,gen_arg2	; object->address
	FLDWS	0(0,rs_free),ft1			; single int arg1 -> ft1
	FLDDS	4(0,gen_arg2),ft2			; arg2 -> ft2
        FCNVXF,SGL,DBL  ft1,ft1				; convert to double float
	FLONUM_RESULT_PREP
	FMPY,DBL	ft1,ft2,ft1
	FLONUM_RESULT(ft1)

generic_times_flo_notflo				; FLO * ~FLO
	JUMP_NOT_FIXNUM_FORWARD(gen_arg2,gt4,generic_times_fail)
generic_times_flo_fix					; FLO * FIX
	COMIB,=,N	0,gen_arg2,generic_times_flo_0
	STW	gen_arg2,0(0,rs_free)			; through heap memory into fpcp
	DEP	rs_quad,TC_START,TC_LENGTH,gen_arg1	; object->address
	FLDWS	0(0,rs_free),ft2			; single int arg2 -> ft2
	FLDDS	4(0,gen_arg1),ft1			; arg1 -> ft1
        FCNVXF,SGL,DBL  ft2,ft2				; convert to double float
	FLONUM_RESULT_PREP
	FMPY,DBL	ft1,ft2,ft1
	FLONUM_RESULT(ft1)

generic_times_fix_unk					; FIX * ~FLO
	JUMP_NOT_FIXNUM_FORWARD(gen_arg2,gt4,generic_times_fail)
generic_times_fix_fix					; FIX * FIX
	;;  This is similar to the multiply_fixnum code
	STW	gen_arg1,0(0,rs_free)
	STW	gen_arg2,4(0,rs_free)
	ZDEPI	1,TC_LENGTH-1,DATUM_LENGTH+1,gt3	; FIXNUM_LIMIT
	FLDWS	0(0,rs_free),ft1
	FLDWS	4(0,rs_free),ft2
	STW	gt3,8(0,rs_free)			; FIXNUM_LIMIT
        FCNVXF,SGL,DBL  ft1,ft1				; arg1
        FCNVXF,SGL,DBL  ft2,ft2				; arg2
	FMPY,DBL	ft1,ft2,ft1
	FLDWS		8(0,rs_free),ft2		; FIXNUM_LIMIT
        FCNVXF,SGL,DBL  ft2,ft2				; FIXNUM_LIMIT
	FCMP,DBL,!>=	ft1,ft2				; result too large?
	FTEST
	B,N	generic_times_fail
	FSUB,DBL	0,ft2,ft2
	FCMP,DBL,!<	ft1,ft2				; result too small?
	FTEST
	B,N	generic_times_fail
	FCNVFXT,DBL,SGL	ft1,ft2
	FSTWS	ft2,0(0,rs_free)			; result
	ENTRY_TO_ADDRESS(gen_continuation)
	BLE	0(5,gen_continuation)			; return with
	LDW	0(0,rs_free),rs_val

generic_times_fail					; ?? * ??, out of line
	;; Build stack frame
	STWM	gen_continuation,-4(0,rs_stack)
	STWM	gen_arg2,-4(0,rs_stack)
	STWM	gen_arg1,-4(0,rs_stack)
	BB,<	31,31,scheme_to_interface
	LDI	HEX(29),gt3				; operation code
	BL,N	do_preserve_2,gt1
	NOP
	B	scheme_to_interface
	LDI	HEX(29),gt3				; operation code

generic_times_0_flo
generic_times_flo_0
	ENTRY_TO_ADDRESS(gen_continuation)
	BLE	0(5,gen_continuation)			; return with
	LDI	0,rs_val				; fixnum 0 = exact 0 result

define(define_generic_binary_predicate,
[generic_$1
	EXTRS		gen_arg1,TC_START,TC_LENGTH,gt3	; type of arg1
	COMIB,<>	TC_FLONUM,gt3,generic_$1_notflo_unk
	EXTRS		gen_arg2,TC_START,TC_LENGTH,gt4	; type of arg2
	COMIB,<>,N	TC_FLONUM,gt4,generic_$1_flo_notflo
	DEP	rs_quad,TC_START,TC_LENGTH,gen_arg1	; object->address
	FLDDS	4(0,gen_arg1),ft1			; arg1 -> ft1
	DEP	rs_quad,TC_START,TC_LENGTH,gen_arg2	; object->address
	FLDDS	4(0,gen_arg2),ft2			; arg2 -> ft2
	GENERIC_BOOLEAN_RESULT($3,ft1,ft2)		; compare, cons answer and return

generic_$1_notflo_unk					; ~FLO * ??
	JUMP_NOT_FIXNUM_FORWARD(gen_arg1,gt3,generic_$1_fail)
	COMIB,<>,N	TC_FLONUM,gt4,generic_$1_fix_unk
generic_$1_fix_flo
	STW	gen_arg1,0(0,rs_free)			; through heap memory into fpcp
	DEP	rs_quad,TC_START,TC_LENGTH,gen_arg2	; object->address
	FLDWS	0(0,rs_free),ft1			; single int arg1 -> ft1
	FLDDS	4(0,gen_arg2),ft2			; arg2 -> ft2
        FCNVXF,SGL,DBL  ft1,ft1				; convert to double float
	GENERIC_BOOLEAN_RESULT($3,ft1,ft2)

generic_$1_flo_notflo					; FLO * ~FLO
	JUMP_NOT_FIXNUM_FORWARD(gen_arg2,gt4,generic_$1_fail)
generic_$1_flo_fix
	STW	gen_arg2,0(0,rs_free)			; through heap memory into fpcp
	DEP	rs_quad,TC_START,TC_LENGTH,gen_arg1	; object->address
	FLDWS	0(0,rs_free),ft2			; single int arg2 -> ft2
	FLDDS	4(0,gen_arg1),ft1			; arg1 -> ft1
        FCNVXF,SGL,DBL  ft2,ft2				; convert to double float
	GENERIC_BOOLEAN_RESULT($3,ft1,ft2)

generic_$1_fix_unk					; FIX * ??
	JUMP_NOT_FIXNUM_FORWARD(gen_arg2,gt4,generic_$1_fail)
generic_$1_fix_fix
	LDIL	L'SHARP_T,gt3
	COMCLR,$3	gen_arg1,gen_arg2,0
	COPY	rs_false,gt3
	ENTRY_TO_ADDRESS(gen_continuation)
	BLE	0(5,gen_continuation)
	COPY	gt3,rs_val

generic_$1_fail						; ?? * ??, out of line
	;; Build interpeter compatible stack frame
	STWM	gen_continuation,-4(0,rs_stack)
	STWM	gen_arg2,-4(0,rs_stack)
	STWM	gen_arg1,-4(0,rs_stack)
	BB,<	31,31,scheme_to_interface
	LDI	HEX($2),gt3				; operation code
	BL,N	do_preserve_2,gt1
	NOP
	B	scheme_to_interface
	LDI	HEX($2),gt3				; operation code])

define(define_generic_unary,
[generic_$1
	EXTRS	gen_arg1,TC_START,TC_LENGTH,gt3		; type of arg
	COMIB,<>,N	TC_FLONUM,gt3,generic_$1_not_flo
	LDI	1,gt3					; constant 1
	STW	gt3,0(0,rs_free)			; into memory
	DEP	rs_quad,TC_START,TC_LENGTH,gen_arg1	; object->address
	FLDWS	0(0,rs_free),ft2			; 1 -> ft2
	FLDDS	4(0,gen_arg1),ft1			; arg -> ft1
	FCNVXF,SGL,DBL	ft2,ft2				; convert to double float
	FLONUM_RESULT_PREP
	$3,DBL	ft1,ft2,ft1				; operate
	FLONUM_RESULT(ft1)

generic_$1_not_flo
	JUMP_NOT_FIXNUM_FORWARD(gen_arg1,gt3,generic_$1_fail)
	ADDI	$4,gen_arg1,gt4				; inc/dec
	JUMP_NOT_FIXNUM_FORWARD(gt4,gt3,generic_$1_fail)
	ENTRY_TO_ADDRESS(gen_continuation)
	BLE	0(5,gen_continuation)			; return!
	COPY	gt4,rs_arg1

generic_$1_fail
	STWM	gen_continuation,-4(0,rs_stack)
	STWM	gen_arg1,-4(0,rs_stack)
	BB,<	31,31,scheme_to_interface
	LDI	HEX($2),gt3				; operation code
	BL,N	do_preserve_1,gt1
	NOP
	B	scheme_to_interface
	LDI	HEX($2),gt3				; operation code])

define(define_generic_unary_predicate,
[generic_$1
	EXTRS		gen_arg1,TC_START,TC_LENGTH,gt3		; type of arg
	COMIB,<>,N	TC_FLONUM,gt3,generic_$1_not_flo
	DEP		rs_quad,TC_START,TC_LENGTH,gen_arg1	; object->address
	FLDDS		4(0,gen_arg1),ft1			; arg -> ft1
	GENERIC_BOOLEAN_RESULT($3,ft1,0)

generic_$1_not_flo
	JUMP_NOT_FIXNUM_FORWARD(gen_arg1,gt3,generic_$1_fail)
	LDIL	L'SHARP_T,gt3
	COMCLR,$3	gen_arg1,0,0
	COPY	rs_false,gt3
	ENTRY_TO_ADDRESS(gen_continuation)
	BLE	0(5,gen_continuation)
	COPY	gt3,rs_val

generic_$1_fail
	STWM	gen_continuation,-4(0,rs_stack)
	STWM	gen_arg1,-4(0,rs_stack)
	BB,<	31,31,scheme_to_interface
	LDI	HEX($2),gt3				; operation code
	BL,N	do_preserve_1,gt1
	NOP
	B	scheme_to_interface
	LDI	HEX($2),gt3				; operation code])

define_generic_unary(decrement,22,FSUB,-1)
define_generic_unary(increment,26,FADD,1)
define_generic_unary_predicate(negative,2a,<)
define_generic_unary_predicate(positive,2c,>)
define_generic_unary_predicate(zero,2d,=)
define_generic_binary(plus,2b,FADD,ADD)
define_generic_binary(subtract,28,FSUB,SUB)
;;define_generic_binary(times,29,FMPY,NONE)
define_generic_binary(divide,23,FDIV,NONE)
define_generic_binary_predicate(equal,24,=)
define_generic_binary_predicate(greater,25,>)
define_generic_binary_predicate(less,27,<)

define(define_generic_out_of_line,
[generic_$1
	STWM	gen_continuation,-4(0,rs_stack)
	STWM	gen_arg2,-4(0,rs_stack)
	STWM	gen_arg1,-4(0,rs_stack)
	BB,<	31,31,scheme_to_interface
	LDI	HEX($2),28			; operation code
	BL,N	do_preserve_2,gt1
	NOP
	B	scheme_to_interface
	LDI	HEX($2),28			; operation code])

define_generic_out_of_line(quotient,37)
define_generic_out_of_line(remainder,38)
changequote(",")

;;;; Optimized procedure application for unknown procedures.
;;;  Procedure in r26, arity (for shortcircuit-apply) in r25.

shortcircuit_apply
	EXTRU	26,5,6,gt4			; procedure type -> gt4
	COMICLR,=	TC_CCENTRY,gt4,0
	B,N	shortcircuit_apply_lose
	ENTRY_TO_ADDRESS(26)
	LDB	-3(0,26),gt3			; procedure's frame-size
	COMB,<>,N	25,gt3,shortcircuit_apply_lose
	BLE,N	0(5,26)				; invoke procedure

changequote([,])
define(define_shortcircuit_fixed,
[shortcircuit_apply_$1
	EXTRU	26,5,6,gt4			; procedure type -> gt4
	COMICLR,=	TC_CCENTRY,gt4,0
	B	shortcircuit_apply_lose
	LDI	$1,25				; NB: in delay slot of branch & skip dest.
	ENTRY_TO_ADDRESS(26)
	LDB	-3(0,26),gt3			; procedure's frame-size
	COMB,<>,N	25,gt3,shortcircuit_apply_lose
	BLE,N	0(5,26)				; invoke procedure])
changequote(",")

define_shortcircuit_fixed(1)
define_shortcircuit_fixed(2)
define_shortcircuit_fixed(3)
define_shortcircuit_fixed(4)
define_shortcircuit_fixed(5)
define_shortcircuit_fixed(6)
define_shortcircuit_fixed(7)
define_shortcircuit_fixed(8)

shortcircuit_apply_lose
	WHEN_CCENTRY_TAGGED("DEP	gt4,5,6,26")	; insert type back
	STWM	rs_continuation,-4(0,rs_stack)
	BL	push_arg_registers,rs_continuation
	ADDI	-1,25,29
	B	scheme_to_interface
	LDI	0x14,28

;;; Return address in r31.  r26 contains the offset from the return
;;; address to the interrupt invocation label.

stack_and_interrupt_check
	LDW	REGBLOCK_STACK_GUARD(0,rs_regblock),25
	LDW	REGBLOCK_MEMTOP(0,rs_regblock),rs_memtop
;;;
;;; If the Scheme stack pointer is <= Stack_Guard, then the stack has
;;; overflowed -- in which case we must signal a stack-overflow interrupt.

	COMB,<=,N rs_stack,25,stack_and_interrupt_check_stack_overflow
;;;
;;; If (Free >= MemTop), signal an interrupt.

	COMB,>=,N rs_free,rs_memtop,stack_and_interrupt_check_signal_interrupt
;;;
;;; Otherwise, return normally -- there's nothing to do.
	BE	0(5,31)
	NOP

stack_and_interrupt_check_stack_overflow
	LDW	REGBLOCK_INT_CODE(0,rs_regblock),25	; IntCode -> r25
	LDW	REGBLOCK_INT_MASK(0,rs_regblock),29	; IntEnb -> r29
;;;
;;; Set the stack-overflow interrupt bit and write the interrupt word
;;; back out to memory.  If the stack-overflow interrupt is disabled,
;;; skip forward to gc test.  Otherwise, set MemTop to -1 and signal
;;; the interrupt.

	DEPI	1,INT_BIT_STACK_OVERFLOW,1,25
	BB,>=	29,INT_BIT_STACK_OVERFLOW,stack_and_interrupt_check_no_overflow
	STW	25,REGBLOCK_INT_CODE(0,rs_regblock)	; r25 -> IntCode
	ADDI	-1,0,rs_memtop				; -1 -> r20
	STW	rs_memtop,REGBLOCK_MEMTOP(0,rs_regblock)
;;;
;;; If (Free >= MemTop), signal an interrupt.
stack_and_interrupt_check_no_overflow
	SUB,<	rs_free,rs_memtop,0		; skip next inst.
						;  if (Free < MemTop)
;;;
;;; To signal the interrupt, add the interrupt invocation offset to
;;; the return address, then return normally.
stack_and_interrupt_check_signal_interrupt
	ADD	26,31,31
	BE	0(5,31)				; return
	NOP

;;; These two are called differently from the old interrupt hooks.
;;; In order to make branch prediction work better, the interrupt code
;;; is moved downstream from the interrupt check, and the code
;;; is followed by a word containing the distance to the real return
;;; address.


new_continuation_interrupt
	LDW	-3(0,31),20			; return address offset
	COPY	rs_false,rs_continuation	; bogus object to protect GC
	B	new_interrupt_common
	ADD	20,31,20			; return address

new_closure_interrupt
	B	new_interrupt_common
	COPY	rs_closure,20			; return address is closure

new_procedure_interrupt
	LDW	-3(0,31),20			; return address offset
	ADD	20,31,20			; return address

new_interrupt_common
	;; r20 holds address of procedure object
	;; r1 = number of parameters
	ADDI	1,1,1				; add 1 for rs_continuation
	COPY	1,31				; preserve number of registers

	ADDIB,<,N	-1,1,save_regs_done
	STWM	rs_continuation,-4(0,rs_stack)
	ADDIB,<,N	-1,1,save_regs_done
	STWM	rs_arg1,-4(0,rs_stack)
	ADDIB,<,N	-1,1,save_regs_done
	STWM	rs_arg2,-4(0,rs_stack)
	ADDIB,<,N	-1,1,save_regs_done
	STWM	rs_arg3,-4(0,rs_stack)
	ADDIB,<,N	-1,1,save_regs_done
	STWM	rs_arg4,-4(0,rs_stack)
	ADDIB,<,N	-1,1,save_regs_done
	STWM	rs_arg5,-4(0,rs_stack)
	ADDIB,<,N	-1,1,save_regs_done
	STWM	rs_arg6,-4(0,rs_stack)
	ADDIB,<,N	-1,1,save_regs_done
	STWM	rs_arg7,-4(0,rs_stack)
	ADDIB,<,N	-1,1,save_regs_done
	STWM	rs_arg8,-4(0,rs_stack)
	ADDIB,<,N	-1,1,save_regs_done
	STWM	rs_arg9,-4(0,rs_stack)
	ADDIB,<,N	-1,1,save_regs_done
	STWM	rs_arg10,-4(0,rs_stack)
	ADDIB,<,N	-1,1,save_regs_done
	STWM	rs_arg11,-4(0,rs_stack)
	ADDIB,<,N	-1,1,save_regs_done
	STWM	rs_arg12,-4(0,rs_stack)
	ADDIB,<,N	-1,1,save_regs_done
	STWM	rs_arg13,-4(0,rs_stack)
	ADDIB,<,N	-1,1,save_regs_done
	STWM	rs_arg14,-4(0,rs_stack)
	ADDIB,<,N	-1,1,save_regs_done
	STWM	rs_arg15,-4(0,rs_stack)
	;;ADDIB,<,N	-1,1,save_regs_done
	;;STWM	25,-4(0,rs_stack)
	;;ADDIB,<,N	-1,1,save_regs_done
	;;STWM	26,-4(0,rs_stack)

;;	COPY	1,24				; n pseudo homes to save
;;	SUB,TR	31,24,31			; adjust number of registers

save_regs_done
	COPY	0,24				; n pseudo homes to save
	COPY	31,25				; number of registers
	COPY	20,26				; entry point
	B	scheme_to_interface
	LDI	0x3e,28				;comutil_new_interrupt_procedure

ep_interface_to_scheme_new
	ADDIL	L'Ext_Stack_Pointer-$global$,rc_static_area
	LDW	R'Ext_Stack_Pointer-$global$(1),rs_stack ; Setup stack pointer

	LDWM	4(0,rs_stack),1			; number of regs
	SUBI,>=	eval(ARGUMENT_REGISTERS+1),1,1	; complement
	B,N	interface_to_scheme_new_allregs
	BLR	1,0
	NOP

interface_to_scheme_new_allregs
	;;LDWM	4(0,rs_stack),26
	;;NOP
	;;LDWM	4(0,rs_stack),25
	;;NOP
	LDWM	4(0,rs_stack),rs_arg15
	NOP
	LDWM	4(0,rs_stack),rs_arg14
	NOP
	LDWM	4(0,rs_stack),rs_arg13
	NOP
	LDWM	4(0,rs_stack),rs_arg12
	NOP
	LDWM	4(0,rs_stack),rs_arg11
	NOP
	LDWM	4(0,rs_stack),rs_arg10
	NOP
	LDWM	4(0,rs_stack),rs_arg9
	NOP
	LDWM	4(0,rs_stack),rs_arg8
	NOP
	LDWM	4(0,rs_stack),rs_arg7
	NOP
	LDWM	4(0,rs_stack),rs_arg6
	NOP
	LDWM	4(0,rs_stack),rs_arg5
	NOP
	LDWM	4(0,rs_stack),rs_arg4
	NOP
	LDWM	4(0,rs_stack),rs_arg3
	NOP
	LDWM	4(0,rs_stack),rs_arg2
	NOP
	LDWM	4(0,rs_stack),rs_arg1
	NOP
	LDWM	4(0,rs_stack),rs_continuation	; return address
	NOP

	B	ep_interface_to_scheme_2
	NOP

;;; invoke_primitive and *cons all have the same interface:
;;; The "return address" in r31 points to a word containing
;;; the distance between itself and the word in memory containing
;;; the primitive object.
;;; All arguments are passed on the stack, ready for the primitive.

;;; The line marked *MAGIC* below is useful only when primitives do
;;; something other than a normal value-generating return (e.g. APPLY
;;; ends by tailing into a compiled procedure or WITH-INTERRUPT-MASK
;;; also tails into a compiled procedure but leaves cleanup code on
;;; the stack).  In these cases, the primitive actually returns the
;;; address of compiled code to be executed and has altered the return
;;; address on the stack so that it points to something like
;;; ep_interface_to_scheme.  These places expect the entry address in
;;; rc_arg1 rather than rc_val (mostly historical and worth changing),
;;; so the copy must happen here.

invoke_primitive
original_invoke_primitive
	DEPI	0,31,2,31			; clear privilege bits
	LDW	0(0,31),26			; get offset
	ADDIL	L'hppa_primitive_table-$global$,rc_static_area
	LDWX	26(0,31),26			; get primitive
	LDW	R'hppa_primitive_table-$global$(1),25
	EXTRU	26,31,DATUM_LENGTH,24		; get primitive index
	STW	26,REGBLOCK_PRIMITIVE(0,rs_regblock)
	ADDIL	L'Primitive_Arity_Table-$global$,rc_static_area
	LDW	R'Primitive_Arity_Table-$global$(1),17
	LDWX,S	24(0,25),25			; find primitive entry point
	ADDIL	L'Ext_Stack_Pointer-$global$,rc_static_area
	STW	rs_stack,R'Ext_Stack_Pointer-$global$(1) ; Update stack pointer
	ADDIL	L'Free-$global$,rc_static_area
	LDWX,S	24(0,17),17			; primitive arity
	STW	rs_free,R'Free-$global$(1)	; Update free	
	.CALL	RTNVAL=GR			; out=28
	BLE	0(4,25)				; Call primitive
	COPY	31,2				; Setup return address

	ADDIL	L'Ext_Stack_Pointer-$global$,rc_static_area
	LDW	R'Ext_Stack_Pointer-$global$(1),rs_stack ; Setup stack pointer
	COPY	rc_val,rs_val			; Move result to val
	SH2ADD	17,rs_stack,rs_stack		; pop frame
	LDWM	4(0,rs_stack),rc_arg1		; return address as object
	B	ep_interface_to_scheme_2
	;;DEP	rs_quad,TC_START,TC_LENGTH,rc_arg2
						; return address as address
	STW	0,REGBLOCK_PRIMITIVE(0,rs_regblock)

;;; The BLE in invoke_primitive can jump here.
;;; The primitive index is in gr24
;;; $$dyncall expects an address to be called in gr22

cross_segment_call
	ADDIL	L'Primitive_Procedure_Table-$global$,rc_static_area
	LDW	R'Primitive_Procedure_Table-$global$(1),22
	LDWX,S	24(0,22),22
	B,N	$$dyncall			; ignore the return address

invoke_primitive_1_preserving
	STWM	rs_continuation,-4(0,rs_stack)
	STWM	rs_arg1,-4(0,rs_stack)
	BB,<	31,31,invoke_primitive
	NOP
	BL,N	do_preserve_1,gt1
	NOP
	B,N	invoke_primitive

invoke_primitive_2_preserving
	STWM	rs_continuation,-4(0,rs_stack)
	STWM	rs_arg2,-4(0,rs_stack)
	STWM	rs_arg1,-4(0,rs_stack)
	BB,<	31,31,invoke_primitive
	NOP
	BL,N	do_preserve_2,gt1
	NOP
	B,N	invoke_primitive

new_pushing_args_invoke_primitive
;; not used because the compiled generates code to do the pushes
;; This will have to be modified to accomodate the *MAGIC* comment in
;; invoke_primitive.
	DEPI	0,31,2,31				; clear privilege bits
	STWM	rs_continuation,-4(0,rs_stack)

	LDW	0(0,31),26				; get offset
	ADDIL	L'hppa_primitive_table-$global$,rc_static_area
	LDWX	26(0,31),26				; get primitive
	LDW	R'hppa_primitive_table-$global$(1),25
	EXTRU	26,31,DATUM_LENGTH,24			; get primitive index
	STW	26,REGBLOCK_PRIMITIVE(0,rs_regblock)	; store primitive
	ADDIL	L'Primitive_Arity_Table-$global$,rc_static_area
	LDW	R'Primitive_Arity_Table-$global$(1),26
	LDWX,S	24(0,25),25			; find primitive entry point
	LDWX,S	24(0,26),26			; primitive arity

	STWM	rs_continuation,-4(0,rs_stack)
	BL	push_arg_registers,rs_continuation
	COPY	26,29

	ADDIL	L'Ext_Stack_Pointer-$global$,rc_static_area
	STW	rs_stack,R'Ext_Stack_Pointer-$global$(1) ; Update stack pointer
	COPY	26,17				; a C callee saves reg
	ADDIL	L'Free-$global$,rc_static_area
	STW	rs_free,R'Free-$global$(1)	; Update free	
	.CALL	RTNVAL=GR			; out=28
	BLE	0(4,25)				; Call primitive
	COPY	31,2				; Setup return address

	ADDIL	L'Ext_Stack_Pointer-$global$,rc_static_area
	LDW	R'Ext_Stack_Pointer-$global$(1),rs_stack ; Setup stack pointer
	COPY	28,2				; Move result to val
	SH2ADD	17,rs_stack,rs_stack		; pop frame
	LDWM	4(0,rs_stack),26		; return address as object
	STW	0,REGBLOCK_PRIMITIVE(0,rs_regblock)	; clear primitive
	B	ep_interface_to_scheme_2
	DEP	rs_quad,TC_START,TC_LENGTH,26	; return address as address


;; vector_cons:
;;	continuation 	in rs_continuation
;;	length		in rs_arg1
;;	fill_value	in rs_arg2
;; returns vector in rs_val
vector_cons
	COPY	rs_free,gt1
	ZDEP	rs_arg1,31,DATUM_LENGTH,rs_arg1	; length as machine word
	SH2ADD	rs_arg1,rs_free,gt2		; end of data (-1)
	;; no space, use primitive
	COMBF,<	gt2,rs_memtop,invoke_primitive_2_preserving
	;; start location, harmless in delay slot
	LDO	4(gt1),gt4	
	LDO	4(gt2),rs_free			; allocate!
	STW	rs_arg1,0(0,gt1)		; vector length (0-tagged)

vector_cons_loop
	COMBT,<,N	gt4,rs_free,vector_cons_loop
	STWM	rs_arg2,4(0,gt4)		; initialize

        LDI	TC_VECTOR,gt4			; load type code
	COPY	gt1,rs_val
	BLE	0(5,rs_continuation)		; return!
	DEP	gt4,TC_START,TC_LENGTH,rs_val   ; tag result

;; string_allocate:
;;	continuation in rs_continuation
;;	length in rs_arg1
;; returns string in rs_val
string_allocate
	COPY	rs_free,gt1			; return value
	ZDEP	rs_arg1,31,DATUM_LENGTH,rs_arg1	; length as machine word
	ADD	rs_arg1,rs_free,gt2		; end of data (-(9+round))
	;; if no space, use primitive
	COMBF,<	gt2,rs_memtop,invoke_primitive_1_preserving
	SHD	0,rs_arg1,2,gt3			; scale down to word
	STB	0,8(0,gt2)			; end-of-string #\NUL
	LDO	2(gt3),gt3			; total word size (-1)
	STWS,MB	rs_arg1,4(0,rs_free)		; store string length; free++
	SH2ADD	gt3,rs_free,rs_free		; allocate!
	DEPI	TC_NMV,TC_START,TC_LENGTH,gt3	; tag header
	STW	gt3,0(0,gt1)			; store nmv header
	LDI	TC_STRING,1
	COPY	gt1,rs_val
	BLE	0(5,rs_continuation)		; return!
	DEP	1,TC_START,TC_LENGTH,rs_val	; tag result

;; floating_vector_cons:
;;	continuation in rs_continuation
;;	length in rs_arg1
;; returns vector in rs_val
floating_vector_cons
	ZDEP	rs_arg1,31,DATUM_LENGTH,rs_arg1	; length as machine word
	; STW	0,0(0,rs_free)			; make heap parseable
	DEPI	4,31,3,rs_free			; bump free past header
	COPY	rs_free,gt1			; return value
	SH3ADD	rs_arg1,rs_free,gt2		; end of data (-1)
	;; if no space, use primitive
	COMBF,<	gt2,rs_memtop,invoke_primitive_1_preserving
	SHD	rs_arg1,0,31,gt3		; scale, harmless in delay slot
	LDO	4(gt2),rs_free			; allocate!
	DEPI	TC_NMV,TC_START,TC_LENGTH,gt3	; tag header
	STW	gt3,0(0,gt1)			; store nmv header
	COPY	gt1,rs_val
	BLE	0(5,rs_continuation)		; return!
	DEPI	TC_FLONUM,TC_START,TC_LENGTH,rs_val	; tag result

define(define_floating_point_util,
"flonum_$1
	COPY	22,17				; preserve regs
	; STW	2,REGBLOCK_VAL(0,rs_regblock)	; preserve val REMOVED by JSM
	COPY	22,17				; preserve regs
	COPY	21,16
	COPY	19,15
        .CALL   ARGW0=FR,ARGW1=FU,RTNVAL=FU     ;fpin=105;fpout=104;
	BL	$2,2
	COPY	31,14
	COPY	15,19
	COPY	16,21
	; LDW	REGBLOCK_VAL(0,rs_regblock),rs_val	; restore val REMOVED by JSM
	COPY	17,22
	BE	0(5,14)
	LDW	REGBLOCK_MEMTOP(0,rs_regblock),rs_memtop")

define_floating_point_util(sin,sin)
define_floating_point_util(cos,cos)
define_floating_point_util(tan,tan)
define_floating_point_util(asin,asin)
define_floating_point_util(acos,acos)
define_floating_point_util(atan,atan)
define_floating_point_util(exp,exp)
define_floating_point_util(log,log)
define_floating_point_util(truncate,double_truncate)
define_floating_point_util(ceiling,ceil)
define_floating_point_util(floor,floor)

flonum_atan2
	COPY	22,17				; preserve regs
	; STW	2,REGBLOCK_VAL(0,rs_regblock)	; preserve val REMOVED by JSM
	COPY	21,16
	COPY	19,15
        .CALL   ARGW0=FR,ARGW1=FU,ARGW2=FR,ARGW3=FU,RTNVAL=FU   ;fpin=105,107;fpout=104;
	BL	atan2,2
	COPY	31,14
	COPY	15,19
	COPY	16,21
	; LDW	REGBLOCK_VAL(0,rs_regblock),rs_val	; restore val REMOVED by JSM
	COPY	17,22
	BE	0(5,14)
	LDW	REGBLOCK_MEMTOP(0,rs_regblock),rs_memtop

compiled_code_bkpt
	LDO	-8(31),31			; bump back to entry point
	;;	COPY	19,25				; Preserve Dynamic link
	LDB	-6(0,31),29			; get entry frame size
	B	bkpt_hook_to_interface
	LDI	0x3c,28

compiled_closure_bkpt
	LDO	-12(31),31			; bump back to entry point
	LDB	-6(0,31),29			; frame size
	B	bkpt_hook_to_interface
	LDI	0x3d,28

closure_entry_bkpt
	LDO	-8(31),31			; bump back to entry point
	LDB	-6(0,31),29			; frame size
	B	bkpt_hook_to_interface
	LDI	0x3c,28

;; On arrival, 31 has a return address.  The word at the return
;; address has the offset between the return address and the
;; closure pattern.
;; Returns the address of the entry point in 25
;; Used: 29, 28, 26, 25, fp11, fp10 [31]

copy_closure_pattern
	LDW	-3(0,31),29			; offset
	DEPI	4,31,3,rs_free			; quad align
	ADD	29,31,29			; addr of pattern
	LDWS,MA	4(0,29),28			; load pattern header
	LDO	8(rs_free),25			; preserve for FDC & FIC
	STWS,MA	28,4(0,rs_free)			; store pattern header
	FLDDS,MA	8(0,29),10		; load entry
	FLDDS,MA	8(0,29),11
	FSTDS,MA	10,8(0,rs_free)		; store entry
	FSTDS,MA	11,8(0,rs_free)
	FDC	0(0,25)
	FDC	0(0,rs_free)
	SYNC
	FIC	0(5,25)
	BE	4(5,31)
	SYNC

;; On arrival, 31 has a return address and 1 contains the number of
;; entries in the closure.  The word at the return address has the
;; offset between the return address and the closure pattern.
;; Returns the address of the entry point in 25
;; Used: 29, 28, 26, 25, fp11, fp10 [31, 1]

copy_multiclosure_pattern
	LDW	-3(0,31),29			; offset
	DEPI	4,31,3,rs_free			; quad align
	ADD	29,31,29			; addr of pattern
	LDWS,MA	4(0,29),28			; load pattern header
	LDO	12(rs_free),25			; preserve for FIC
	STWS,MA	28,4(0,rs_free)			; store pattern header
	LDI	-16,26				; FDC index
	
copy_multiclosure_pattern_loop
	FLDDS,MA	8(0,29),10		; load entry
	FLDDS,MA	8(0,29),11
	FSTDS,MA	10,8(0,rs_free)		; store entry
	FSTDS,MA	11,8(0,rs_free)
	ADDIB,>	-1,1,copy_multiclosure_pattern_loop
	FDC	26(0,21)

	LDWS,MA	4(0,29),28			; load pattern tail
	COPY	rs_free,26
	STWS,MA 28,4(0,rs_free)			; store pattern tail
	FDC	0(0,26)
	SYNC
	FIC	0(5,25)
	BE	4(5,31)				; return
	SYNC

;;; This code clobbers 2,31,gt1-gt4.
;;; It is assumed that the register allocator will free them before
;;; going out of line.
;;; There are three return addresses to this code:
;;; - The real return address on the stack
;;; - gt1, the return address into the part of the interface that
;;;   can continue the operation by going out of line
;;; - r31, an address some small distance from the real return address.
;;; The immediately preceding word is the integer register mask.
;;; If bit 0 (MSB) is set, the preceding word is the floating register mask.
;;; If bit 1 is set, the preceding bytes are a list of the pseudo register
;;; homes that need to be preserved, all assumed to contain floating-point
;;; data.
;;; If bit 31 (LSB) is set, the preceding bytes are a list of the pseudo
;;; register homes that need to be preserved, all assumed to contain objects.
;;; The way preservations work is simple: a call to a special interface
;;; routine that needs to preserve registers in case of back out leaves
;;; something in r31 that does _not_ have the LSB set (which the BLE
;;; instruction sets by default).  When the interface determines that
;;; it must go into arbitrary code (e.g. primitive or runtime system),
;;; it checks that bit of r31 and invokes this as a subroutine if necessary.
;;; Note that r31 may be needed by the code pointed to by gt1 (e.g.
;;; invoke-primitive), so this code cannot clobber it.

preserve_registers
do_preserve_1
	ADD,TR	0,0,gt2				; extra words

do_preserve_2
	LDI	1,gt2				; extra words
	LDWM	4(0,rs_stack),1			; pop arg 1
	COMICLR,=	0,gt2,0
	LDWM	4(0,rs_stack),2			; pop arg 2
	STW	rs_stack,REGBLOCK_COMPILER_TEMP(0,rs_regblock)
	;; CHANGE?:
	LDW	0(0,rs_stack),gt3		; real return address
	LDW	-4(0,31),gt4			; integer register mask

;; The following sequence preserves all the integer registers (except 0,
;; 1, and 31) if specified in the mask.  Of course, certain bits set
;; in the mask will not be restored properly since the restoration
;; code uses a few of them.

	BB,>=,N	gt4,2,pskip2
	STWM	2,-4(0,rs_stack)
pskip2	BB,>=,N	gt4,3,pskip3
	STWM	3,-4(0,rs_stack)
pskip3	BB,>=,N	gt4,4,pskip4
	STWM	4,-4(0,rs_stack)
pskip4	BB,>=,N	gt4,5,pskip5
	STWM	5,-4(0,rs_stack)
pskip5	BB,>=,N	gt4,6,pskip6
	STWM	6,-4(0,rs_stack)
pskip6	BB,>=,N	gt4,7,pskip7
	STWM	7,-4(0,rs_stack)
pskip7	BB,>=,N	gt4,8,pskip8
	STWM	8,-4(0,rs_stack)
pskip8	BB,>=,N	gt4,9,pskip9
	STWM	9,-4(0,rs_stack)
pskip9	BB,>=,N	gt4,10,pskip10
	STWM	10,-4(0,rs_stack)

pskip10	BB,>=,N	gt4,11,pskip11
	STWM	11,-4(0,rs_stack)
pskip11	BB,>=,N	gt4,12,pskip12
	STWM	12,-4(0,rs_stack)
pskip12	BB,>=,N	gt4,13,pskip13
	STWM	13,-4(0,rs_stack)
pskip13	BB,>=,N	gt4,14,pskip14
	STWM	14,-4(0,rs_stack)
pskip14	BB,>=,N	gt4,15,pskip15
	STWM	15,-4(0,rs_stack)
pskip15	BB,>=,N	gt4,16,pskip16
	STWM	16,-4(0,rs_stack)
pskip16	BB,>=,N	gt4,17,pskip17
	STWM	17,-4(0,rs_stack)
pskip17	BB,>=,N	gt4,18,pskip18
	STWM	18,-4(0,rs_stack)
pskip18	BB,>=,N	gt4,19,pskip19
	STWM	19,-4(0,rs_stack)
pskip19	BB,>=,N	gt4,20,pskip20
	STWM	20,-4(0,rs_stack)
pskip20	BB,>=,N	gt4,21,pskip21
	STWM	21,-4(0,rs_stack)
pskip21	BB,>=,N	gt4,22,pskip22
	STWM	22,-4(0,rs_stack)
pskip22	BB,>=,N	gt4,23,pskip23
	STWM	23,-4(0,rs_stack)
pskip23	BB,>=,N	gt4,24,pskip24
	STWM	24,-4(0,rs_stack)
pskip24	BB,>=,N	gt4,25,pskip25
	STWM	25,-4(0,rs_stack)
pskip25	BB,>=,N	gt4,26,pskip26
	STWM	26,-4(0,rs_stack)
pskip26	BB,>=,N	gt4,27,pskip27
	STWM	27,-4(0,rs_stack)
pskip27	BB,>=,N	gt4,28,pskip28
	STWM	28,-4(0,rs_stack)
pskip28	BB,>=,N	gt4,29,pskip29
	STWM	29,-4(0,rs_stack)
pskip29	BB,>=,N	gt4,30,pskip30
	STWM	30,-4(0,rs_stack)
pskip30
	LDO	-4(31),6			; skip to previous word
	BB,>=,N	gt4,31,preserve_skip_object_homes
	
	LDBS,MB	-1(0,6),7			; count of pseudo regs
	LDO	64(4),8				; address of first home

pohloop	LDBS,MB	-1(0,6),9			; pseudo reg number
	LDO	-1(7),7
	SH3ADDL		9,8,9
	LDW	0(0,9),10			; value in home
	COMB,<>	0,7,pohloop
	STWM	10,-4(0,rs_stack)

preserve_skip_object_homes
	DEPI	0,31,2,6			; align to first word
	COPY	rs_stack,11				; preserve sp

	BB,>=,N	gt4,0,preserve_skip_float_regs
	DEPI	0,31,3,rs_stack			; align float
	LDWM	-4(0,6),8			; float mask

	BB,>=,N	8,0,pfskp0
	FSTDS,MB	0,-8(0,rs_stack)
pfskp0	BB,>=,N	8,1,pfskp1
	FSTDS,MB	1,-8(0,rs_stack)
pfskp1	BB,>=,N	8,2,pfskp2
	FSTDS,MB	2,-8(0,rs_stack)
pfskp2	BB,>=,N	8,3,pfskp3
	FSTDS,MB	3,-8(0,rs_stack)
pfskp3	BB,>=,N	8,4,pfskp4
	FSTDS,MB	4,-8(0,rs_stack)
pfskp4	BB,>=,N	8,5,pfskp5
	FSTDS,MB	5,-8(0,rs_stack)
pfskp5	BB,>=,N	8,6,pfskp6
	FSTDS,MB	6,-8(0,rs_stack)
pfskp6	BB,>=,N	8,7,pfskp7
	FSTDS,MB	7,-8(0,rs_stack)
pfskp7	BB,>=,N	8,8,pfskp8
	FSTDS,MB	8,-8(0,rs_stack)
pfskp8	BB,>=,N	8,9,pfskp9
	FSTDS,MB	9,-8(0,rs_stack)
pfskp9	BB,>=,N	8,10,pfskp10
	FSTDS,MB	10,-8(0,rs_stack)
pfskp10	BB,>=,N	8,11,pfskp11
	FSTDS,MB	11,-8(0,rs_stack)
pfskp11	BB,>=,N	8,12,pfskp12
	FSTDS,MB	12,-8(0,rs_stack)
pfskp12	BB,>=,N	8,13,pfskp13
	FSTDS,MB	13,-8(0,rs_stack)
pfskp13	BB,>=,N	8,14,pfskp14
	FSTDS,MB	14,-8(0,rs_stack)
pfskp14	BB,>=,N	8,15,pfskp15
	FSTDS,MB	15,-8(0,rs_stack)
pfskp15	BB,>=,N	8,16,pfskp16
	FSTDS,MB	16,-8(0,rs_stack)
pfskp16	BB,>=,N	8,17,pfskp17
	FSTDS,MB	17,-8(0,rs_stack)
pfskp17	BB,>=,N	8,18,pfskp18
	FSTDS,MB	18,-8(0,rs_stack)
pfskp18	BB,>=,N	8,19,pfskp19
	FSTDS,MB	19,-8(0,rs_stack)
pfskp19	BB,>=,N	8,20,pfskp20
	FSTDS,MB	20,-8(0,rs_stack)
pfskp20	BB,>=,N	8,21,pfskp21
	FSTDS,MB	21,-8(0,rs_stack)
pfskp21	BB,>=,N	8,22,pfskp22
	FSTDS,MB	22,-8(0,rs_stack)
pfskp22	BB,>=,N	8,23,pfskp23
	FSTDS,MB	23,-8(0,rs_stack)
pfskp23	BB,>=,N	8,24,pfskp24
	FSTDS,MB	24,-8(0,rs_stack)
pfskp24	BB,>=,N	8,25,pfskp25
	FSTDS,MB	25,-8(0,rs_stack)
pfskp25	BB,>=,N	8,26,pfskp26
	FSTDS,MB	26,-8(0,rs_stack)
pfskp26	BB,>=,N	8,27,pfskp27
	FSTDS,MB	27,-8(0,rs_stack)
pfskp27	BB,>=,N	8,28,pfskp28
	FSTDS,MB	28,-8(0,rs_stack)
pfskp28	BB,>=,N	8,29,pfskp29
	FSTDS,MB	29,-8(0,rs_stack)
pfskp29	BB,>=,N	8,30,pfskp30
	FSTDS,MB	30,-8(0,rs_stack)
pfskp30	BB,>=,N	8,31,pfskp31
	FSTDS,MB	31,-8(0,rs_stack)
pfskp31

preserve_skip_float_regs
	BB,>=,N	gt4,1,preserve_skip_float_homes
	DEPI	0,31,3,22			; align float

	LDBS,MB	-1(0,6),7
	LDO	64(4),8

pfhloop	LDBS,MB	-1(0,6),9			; pseudo reg number
	LDO	-1(7),7
	FLDDX,S	9(0,8),4			; value in home
	COMB,<>	0,7,pfhloop
	FSTDS,MB	4,-8(0,rs_stack)

preserve_skip_float_homes
	SUB	11,22,11			; non-marked bytes pushed
	SHD	0,11,2,11			; non-marked words pushed
	DEPI	TC_NMV,TC_START,TC_LENGTH,11	; -> non-marked vector header
	STWM	11,-4(0,rs_stack)			; push header or nothing
	LDW	REGBLOCK_COMPILER_TEMP(0,rs_regblock),6	; original stack pointer
	STWM	gt3,-4(0,rs_stack)			; re-push return address

	DEP	rs_quad,TC_START,TC_LENGTH,gt3	; object->address
	SUB	gt3,31,gt3			; offset of mask
	STWM	gt3,-4(0,rs_stack)		; push offset in stack
	SUB	6,22,6				; bytes pushed
	SHD	0,6,2,6				; words pushed
	STWM	6,-4(0,rs_stack)		; push number of words on stack
	LDI	5,gt3				; REFLECT_CODE_RESTORE_REGS
	LDI	TC_POSITIVE_FIXNUM,6
	DEP	6,TC_START,TC_LENGTH,gt3
	STWM	gt3,-4(0,rs_stack)		; push into stack
	LDW	REGBLOCK_REFLECT_TO_INTERFACE(0,rs_regblock),gt3
	STWM	gt3,-4(0,rs_stack)		; push into stack
	COMICLR,=	0,gt2,0			; extra words = 0?
	STWM	2,-4(0,rs_stack)		; push arg 2

	BV	0(gt1)
	STWM	1,-4(0,rs_stack)		; push arg 1

ep_interface_to_scheme_restore
restore_registers
	ADDIL	L'Ext_Stack_Pointer-$global$,rc_static_area
	LDW	R'Ext_Stack_Pointer-$global$(1),rs_stack ; Setup stack pointer

	LDWM	4(0,rs_stack),gt2		; offset of mask
	LDWM	4(0,rs_stack),gt1		; return address
	DEP	rs_quad,TC_START,TC_LENGTH,gt1	; object->address
	SUB	gt1,gt2,gt1			; address of mask
	LDW	-4(0,gt1),gt4			; mask
	LDWM	4(0,rs_stack),11		; non marked vector header
	
	LDO	-4(gt1),12			; addr of object mask
	BB,>=,N	gt4,31,restore_float_homes
	LDB	-1(0,12),7			; count of object homes -1
	LDO	5(7),7				; normalize
	SHD	0,7,2,7				; into words
	SH2ADDL	7,0,7				; into bytes
	SUB	12,7,12				; addr past floating mask


;; NOTE: The following sequence restores all the integer registers if
;; specified in the mask.  Of course, certain bits set in the mask
;; will just make the code break (those including the stack pointer,
;; gt4, 26).

restore_float_homes
	BB,>=,N	gt4,1,restore_skip_float_homes
	COPY	12,6
	BB,>=,N	gt4,0,restore_float_homes_continue
	LDO	-4(6),6				; skip past floating-poing mask

restore_float_homes_continue
	LDO	-1(6),6				; address of count
	LDO	64(4),8				; address of first home
	LDBS,MA	-1(0,6),7			; count of pseudo regs
	SUB	6,7,6				; address of first number

rfhloop	LDBS,MA	1(0,6),9			; pseudo reg number
	FLDDS,MA	8(0,rs_stack),4		; floating-point value
	LDO	-1(7),7
	COMB,<>	0,7,rfhloop
	FSTDX	4,9(0,8)

restore_skip_float_homes
	BB,>=,N gt4,0,restore_skip_float_regs
	LDW	-4(0,12),8			; floating-point mask
	BB,>=,N	8,31,rfskp31
	FLDDS,MA	8(0,rs_stack),31
rfskp31	BB,>=,N	8,30,rfskp30
	FLDDS,MA	8(0,rs_stack),30
rfskp30	BB,>=,N	8,29,rfskp29
	FLDDS,MA	8(0,rs_stack),29
rfskp29	BB,>=,N	8,28,rfskp28
	FLDDS,MA	8(0,rs_stack),28
rfskp28	BB,>=,N	8,27,rfskp27
	FLDDS,MA	8(0,rs_stack),27
rfskp27	BB,>=,N	8,26,rfskp26
	FLDDS,MA	8(0,rs_stack),26
rfskp26	BB,>=,N	8,25,rfskp25
	FLDDS,MA	8(0,rs_stack),25
rfskp25	BB,>=,N	8,24,rfskp24
	FLDDS,MA	8(0,rs_stack),24
rfskp24	BB,>=,N	8,23,rfskp23
	FLDDS,MA	8(0,rs_stack),23
rfskp23	BB,>=,N	8,22,rfskp22
	FLDDS,MA	8(0,rs_stack),22
rfskp22	BB,>=,N	8,21,rfskp21
	FLDDS,MA	8(0,rs_stack),21
rfskp21	BB,>=,N	8,20,rfskp20
	FLDDS,MA	8(0,rs_stack),20
rfskp20	BB,>=,N	8,19,rfskp19
	FLDDS,MA	8(0,rs_stack),19
rfskp19	BB,>=,N	8,18,rfskp18
	FLDDS,MA	8(0,rs_stack),18
rfskp18	BB,>=,N	8,17,rfskp17
	FLDDS,MA	8(0,rs_stack),17
rfskp17	BB,>=,N	8,16,rfskp16
	FLDDS,MA	8(0,rs_stack),16
rfskp16	BB,>=,N	8,15,rfskp15
	FLDDS,MA	8(0,rs_stack),15
rfskp15	BB,>=,N	8,14,rfskp14
	FLDDS,MA	8(0,rs_stack),14
rfskp14	BB,>=,N	8,13,rfskp13
	FLDDS,MA	8(0,rs_stack),13
rfskp13	BB,>=,N	8,12,rfskp12
	FLDDS,MA	8(0,rs_stack),12
rfskp12	BB,>=,N	8,11,rfskp11
	FLDDS,MA	8(0,rs_stack),11
rfskp11	BB,>=,N	8,10,rfskp10
	FLDDS,MA	8(0,rs_stack),10
rfskp10	BB,>=,N	8,9,rfskp9
	FLDDS,MA	8(0,rs_stack),9
rfskp9	BB,>=,N	8,8,rfskp8
	FLDDS,MA	8(0,rs_stack),8
rfskp8	BB,>=,N	8,7,rfskp7
	FLDDS,MA	8(0,rs_stack),7
rfskp7	BB,>=,N	8,6,rfskp6
	FLDDS,MA	8(0,rs_stack),6
rfskp6	BB,>=,N	8,5,rfskp5
	FLDDS,MA	8(0,rs_stack),5
rfskp5	BB,>=,N	8,4,rfskp4
	FLDDS,MA	8(0,rs_stack),4
rfskp4	BB,>=,N	8,3,rfskp3
	FLDDS,MA	8(0,rs_stack),3
rfskp3	BB,>=,N	8,2,rfskp2
	FLDDS,MA	8(0,rs_stack),2
rfskp2	BB,>=,N	8,1,rfskp1
	FLDDS,MA	8(0,rs_stack),1
rfskp1	BB,>=,N	8,0,rfskp0
	FLDDS,MA	8(0,rs_stack),0
rfskp0

restore_skip_float_regs
	BB,>=,N	11,31,rspop			; was there padding?
	LDO	4(22),22			; pop padding
rspop	BB,>=,N	gt4,31,restore_skip_object_homes

	LDO	-5(gt1),6			; address of count
	LDO	64(4),8				; address of first home
	LDBS,MA	-1(0,6),7			; count of pseudo regs
	SUB	6,7,6				; address of first number

rohloop	LDBS,MA	1(0,6),9			; pseudo reg number
	LDWM	4(0,rs_stack),10			; value
	LDO	-1(7),7
	SH3ADDL	9,8,9
	COMB,<>	0,7,rohloop
	STW	10,0(0,9)

restore_skip_object_homes
	BB,>=,N	gt4,30,rskip30
	LDWM	4(0,rs_stack),30
rskip30	BB,>=,N	gt4,29,rskip29
	LDWM	4(0,rs_stack),29
rskip29	BB,>=,N	gt4,28,rskip28
	LDWM	4(0,rs_stack),28
rskip28	BB,>=,N	gt4,27,rskip27
	LDWM	4(0,rs_stack),27
rskip27	BB,>=,N	gt4,26,rskip26
	LDWM	4(0,rs_stack),26
rskip26	BB,>=,N	gt4,25,rskip25
	LDWM	4(0,rs_stack),25
rskip25	BB,>=,N	gt4,24,rskip24
	LDWM	4(0,rs_stack),24
rskip24	BB,>=,N	gt4,23,rskip23
	LDWM	4(0,rs_stack),23
rskip23	BB,>=,N	gt4,22,rskip22
	LDWM	4(0,rs_stack),22
rskip22	BB,>=,N	gt4,21,rskip21
	LDWM	4(0,rs_stack),21
rskip21	BB,>=,N	gt4,20,rskip20
	LDWM	4(0,rs_stack),20
rskip20	BB,>=,N	gt4,19,rskip19
	LDWM	4(0,rs_stack),19
rskip19	BB,>=,N	gt4,18,rskip18
	LDWM	4(0,rs_stack),18
rskip18	BB,>=,N	gt4,17,rskip17
	LDWM	4(0,rs_stack),17
rskip17	BB,>=,N	gt4,16,rskip16
	LDWM	4(0,rs_stack),16
rskip16	BB,>=,N	gt4,15,rskip15
	LDWM	4(0,rs_stack),15
rskip15	BB,>=,N	gt4,14,rskip14
	LDWM	4(0,rs_stack),14
rskip14	BB,>=,N	gt4,13,rskip13
	LDWM	4(0,rs_stack),13
rskip13	BB,>=,N	gt4,12,rskip12
	LDWM	4(0,rs_stack),12
rskip12	BB,>=,N	gt4,11,rskip11
	LDWM	4(0,rs_stack),11
rskip11	BB,>=,N	gt4,10,rskip10
	LDWM	4(0,rs_stack),10
rskip10	BB,>=,N	gt4,9,rskip9
	LDWM	4(0,rs_stack),9
rskip9	BB,>=,N	gt4,8,rskip8
	LDWM	4(0,rs_stack),8
rskip8	BB,>=,N	gt4,7,rskip7
	LDWM	4(0,rs_stack),7
rskip7	BB,>=,N	gt4,6,rskip6
	LDWM	4(0,rs_stack),6
rskip6	BB,>=,N	gt4,5,rskip5
	LDWM	4(0,rs_stack),5
rskip5	BB,>=,N	gt4,4,rskip4
	LDWM	4(0,rs_stack),4
rskip4	BB,>=,N	gt4,3,rskip3
	LDWM	4(0,rs_stack),3
rskip3	BB,>=,N	gt4,2,rskip2
	LDWM	4(0,rs_stack),2

rskip2
	LDWM	4(0,rs_stack),26			; return address
	LDW	REGBLOCK_VAL(0,rs_regblock),2		; interpreter val -> val reg
	B	ep_interface_to_scheme_2
	DEP	rs_quad,TC_START,TC_LENGTH,26		; object->address	

;; Arguments in g26, g25, [and g24] (standard C argument registers)
;; utility code in g28
;; return address for scheme_to_interface_ble (and register mask) in g31

interpreter_call
	BB,<	31,31,scheme_to_interface_ble
	NOP
	STWM	25,-8(0,rs_stack)		; preserve gt2
	LDO	4(31),26			; bump past format word
	LDI	TC_CCENTRY,23
	DEP	23,TC_START,TC_LENGTH,26	; set tag
	STW	26,4(0,rs_stack)		; return address (gt1)
	BL	do_preserve_1,gt1
	COPY	28,23				; preserve code
	LDWM	4(0,rs_stack),25
	LDWM	4(0,rs_stack),26		; reflect_to_interface
	DEP	rs_quad,TC_START,TC_LENGTH,26	; untag
	B	scheme_to_interface
	COPY	23,28				; restore code

;; This label is used by the trap handler

ep_scheme_hooks_high

;;;; Assembly language entry point used by utilities in cmpint.c
;;;  to return to the interpreter.
;;;  It returns from C_to_interface.

ep_interface_to_C
	COPY	29,28				; Setup C value
        LDW     -eval(C_FRAME_SIZE+20)(0,rc_stack),2	; Restore return address
        LDW     -52(0,rc_stack),18			; Restore saved regs
        LDW     -56(0,rc_stack),17
        LDW     -60(0,rc_stack),16
        LDW     -64(0,rc_stack),15
        LDW     -68(0,rc_stack),14
        LDW     -72(0,rc_stack),13
        LDW     -76(0,rc_stack),12
        LDW     -80(0,rc_stack),11
        LDW     -84(0,rc_stack),10
        LDW     -88(0,rc_stack),9
        LDW     -92(0,rc_stack),8
        LDW     -96(0,rc_stack),7
        LDW     -100(0,rc_stack),6
        LDW     -104(0,rc_stack),5
        LDW     -108(0,rc_stack),4
        BV      0(2)				; Return
        .EXIT
        LDWM    -eval(C_FRAME_SIZE)(0,rc_stack),3	; Restore last reg, pop frame
        .PROCEND				;in=26;out=28;

;;;; Procedure to initialize this interface.
;;;
;;; C signature:
;;;
;;; void initialize_interface (void);

interface_initialize
	.PROC
	.CALLINFO CALLER,FRAME=4,SAVE_RP
	.ENTRY
	STW	2,-20(0,rc_stack)			; Preserve return address
	LDO	64(rc_stack),rc_stack			; Allocate stack frame
	STW	3,-64(rc_stack)			; Preserve gr3
	FSTWS	0,-4(rc_stack)
	LDW	-4(rc_stack),22
	LDI	30,21				; enable V, Z, O, U traps
	OR	21,22,22
	STW	22,-4(rc_stack)
	FLDWS	-4(rc_stack),0
						; Prepare entry points
	BL	known_pc,3			; get pc
	NOP
known_pc

define(store_entry_point,"ADDIL	L'ep_$1-known_pc,3
	LDO	R'ep_$1-known_pc(1),29
	ADDIL	L'$1-$global$,27
	STW	29,R'$1-$global$(1)")

	store_entry_point(interface_to_scheme_restore)
	store_entry_point(interface_to_scheme_new)
	store_entry_point(interface_to_scheme)
	store_entry_point(interface_to_C)

changequote([,])
define(builtin,[ADDIL	L'$1-known_pc,3
	LDO	R'$1-known_pc(1),26
	ADDIL	L'$1_string-$global$,27
	.CALL	ARGW0=GR
	BL	declare_builtin,2
	LDO	R'$1_string-$global$(1),25 divert(1)
$1_string
	.ALIGN	8
	.STRINGZ "$1" divert(0)])

	;; The builtins must be declared in the order defined.

	builtin(scheme_to_interface_ble)
	builtin(ep_scheme_hooks_low)
	builtin(store_closure_entry)
	builtin(store_closure_code)
	builtin(multiply_fixnum)
	builtin(fixnum_quotient)
	builtin(fixnum_remainder)
	builtin(fixnum_lsh)
	builtin(generic_times)
	builtin(generic_decrement)
	builtin(generic_increment)
	builtin(generic_negative)
	builtin(generic_positive)
	builtin(generic_zero)
	builtin(generic_plus)
	builtin(generic_subtract)
	builtin(generic_divide)
	builtin(generic_equal)
	builtin(generic_greater)
	builtin(generic_less)
	builtin(generic_quotient)
	builtin(generic_remainder)
	builtin(shortcircuit_apply)
	builtin(shortcircuit_apply_1)
	builtin(shortcircuit_apply_2)
	builtin(shortcircuit_apply_3)
	builtin(shortcircuit_apply_4)
	builtin(shortcircuit_apply_5)
	builtin(shortcircuit_apply_6)
	builtin(shortcircuit_apply_7)
	builtin(shortcircuit_apply_8)
	builtin(stack_and_interrupt_check)
	builtin(new_continuation_interrupt)
	builtin(new_closure_interrupt)
	builtin(new_procedure_interrupt)
	builtin(ep_interface_to_scheme_new)
	builtin(invoke_primitive)
	builtin(vector_cons)
	builtin(string_allocate)
	builtin(floating_vector_cons)
	builtin(flonum_sin)
	builtin(flonum_cos)
	builtin(flonum_tan)
	builtin(flonum_asin)
	builtin(flonum_acos)
	builtin(flonum_atan)
	builtin(flonum_exp)
	builtin(flonum_log)
	builtin(flonum_truncate)
	builtin(flonum_ceiling)
	builtin(flonum_floor)
	builtin(flonum_atan2)
	builtin(compiled_code_bkpt)
	builtin(compiled_closure_bkpt)
	builtin(copy_closure_pattern)
	builtin(copy_multiclosure_pattern)
	builtin(preserve_registers)
	builtin(restore_registers)
	builtin(interpreter_call)
	builtin(ep_scheme_hooks_high)
changequote(",")
						; Return
	LDW	-84(rc_stack),2			; Restore return address
	LDW	-64(rc_stack),3			; Restore gr3
	BV	0(2)
	.EXIT
	LDO	-64(rc_stack),rc_stack			; De-allocate stack frame
	.PROCEND

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
	LDO	3(25),25			; add 3 to round up
	SHD	0,25,2,25			; divide count (in longs) by 4
	COPY	25,28				; save for FIC loop
	COPY	26,29				; save for FIC loop
	LDI	16,1				; increment
	BB,>=,N	24,30,process_i_cache		; if D_CACHE is not set,
						;  skip d-cache
;;;
flush_cache_fdc_loop
	ADDIB,>=	-1,25,flush_cache_fdc_loop
	FDC,M	1(0,26)
	SYNC
;;;
process_i_cache
	BB,>=,N	24,31,L$exit2			; if I_CACHE is not set, return
;;;
flush_cache_fic_loop
	ADDIB,>=	-1,28,flush_cache_fic_loop
	FIC,M	1(5,29)
;;;
L$exit2
	BV	0(2)
	.EXIT
	SYNC
	.PROCEND				;in=25,26;

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
	BB,>=,N	26,30,do_i_cache		; if D_CACHE is not set,
						;  skip d-cache

	LDW	32(0,25),31			; 31 <- address (init. base)
	LDW	44(0,25),29			; 29 <- loop
	LDW	36(0,25),23			; 23 <- stride
	LDW	40(0,25),19			; 19 <- count

	LDO	-1(19),19			; decrement count
	COMIB,>,N	0,19,d_sync		; if (count < 0), no flush
	COMIB,=,N	1,29,d_direct_l
	COMIB,=,N	2,29,d_assoc2_l
	COMIB,=,N	4,29,d_assoc4_l

d_assoc_l					; set-associative flush-loop
	COPY	29,20				; 20 (lcount) <- loop

d_set_l						; set flush-loop
	LDO	-1(20),20			; decrement lcount
	COMIB,<=,N	0,20,d_set_l		; if (lcount >= 0), set loop
	FDCE	0(0,31)				; flush entry at (address)

	LDO	-1(19),19			; decrement count
	COMIB,<=	0,19,d_assoc_l		; if (count >= 0), loop
	ADD	31,23,31			; address++

	B	do_i_cache			; next
	SYNC					; synchronize after flush

d_assoc4_l					; 4-way set-associative loop
	FDCE	0(0,31)				; flush entry at (*address)
	FDCE	0(0,31)				; flush entry at (*address)
	FDCE	0(0,31)				; flush entry at (*address)
	FDCE,M	23(0,31)			; flush entry at (*address++)
	COMIB,<		0,19,d_assoc4_l		; if (count > 0), loop
	LDO	-1(19),19			; decrement count

	B	do_i_cache			; next
	SYNC					; synchronize after flush

d_assoc2_l					; 2-way set-associative loop
	FDCE	0(0,31)				; flush entry at (*address)
	FDCE,M	23(0,31)			; flush entry at (*address++)
	COMIB,<		0,19,d_assoc2_l		; if (count > 0), loop
	LDO	-1(19),19			; decrement count

	B	do_i_cache			; next
	SYNC					; synchronize after flush

d_direct_l					; direct-mapped flush loop
	FDCE,M	23(0,31)			; flush entry at (*address++)
	COMIB,<		0,19,d_direct_l		; if (count > 0), loop
	LDO	-1(19),19			; decrement count

d_sync
	SYNC					; synchronize after flush

do_i_cache
	BB,>=,N	26,31,L$exit1			; if I_CACHE is not set, return

	LDW	8(0,25),31			; 31 <- address (init. base)
	LDW	20(0,25),29			; 29 <- loop
	LDW	12(0,25),23			; 23 <- stride
	LDW	16(0,25),19			; 19 <- count

	LDO	-1(19),19			; decrement count
	COMIB,>,N	0,19,i_sync		; if (count < 0), no flush
	COMIB,=,N	1,29,i_direct_l
	COMIB,=,N	2,29,i_assoc2_l
	COMIB,=,N	4,29,i_assoc4_l

i_assoc_l					; set-associative flush-loop
	COPY	29,20				; 20 (lcount) <- loop

i_set_l						; set flush-loop
	LDO	-1(20),20			; decrement lcount
	COMIB,<=,N	0,20,i_set_l		; if (lcount >= 0), set loop
	FICE	0(5,31)				; flush entry at (address)

	LDO	-1(19),19			; decrement count
	COMIB,<=	0,19,i_assoc_l		; if (count >= 0), loop
	ADD	31,23,31			; address++

	B	i_skips				; next
	SYNC					; synchronize after flush

i_assoc4_l					; 4-way set-associative loop
	FICE	0(5,31)				; flush entry at (*address)
	FICE	0(5,31)				; flush entry at (*address)
	FICE	0(5,31)				; flush entry at (*address)
	FICE,M	23(5,31)			; flush entry at (*address++)
	COMIB,<		0,19,i_assoc4_l		; if (count > 0), loop
	LDO	-1(19),19			; decrement count

	B	i_skips				; next
	SYNC					; synchronize after flush

i_assoc2_l					; 2-way set-associative loop
	FICE	0(5,31)				; flush entry at (*address)
	FICE,M	23(5,31)			; flush entry at (*address++)
	COMIB,<		0,19,i_assoc2_l		; if (count > 0), loop
	LDO	-1(19),19			; decrement count

	B	i_skips				; next
	SYNC					; synchronize after flush

i_direct_l					; direct-mapped flush loop
	FICE,M	23(5,31)			; flush entry at (*address++)
	COMIB,<		0,19,i_direct_l		; if (count > 0), loop
	LDO	-1(19),19			; decrement count

i_sync
	SYNC					; synchronize after flush

i_skips
	NOP					; 7 instructionss as prescribed
	NOP					; by the programming note in
	NOP					; the description for SYNC.
	NOP
	NOP

L$exit1
	BV	0(2)
	.EXIT
	NOP
	.PROCEND ;in=25,26;

bkpt_normal_proceed
	BL	bkpt_normal_cont,1		; Get PC
	DEP	0,31,2,1
bkpt_normal_cont
	LDW	bkpt_normal_ep-bkpt_normal_cont(0,1),1		; entry point
	BV	0(1)				; Invoke
	NOP					; Slot for first instruction
bkpt_normal_ep
	NOP					; Slot for fall through

bkpt_plus_proceed
	COMB,=	1,1,bkpt_plus_t			; Slot for first instruction
	NOP					; Slot for second instruction
	STWM	1,-4(0,rs_stack)			; Preserve 1
	BL	bkpt_plus_cont_f,1		; Get PC
	DEP	0,31,2,1
bkpt_plus_cont_f
	LDW	bkpt_plus_ep-bkpt_plus_cont_f(0,1),1		; entry point
	BV	0(1)				; Invoke
	LDWM	4(0,rs_stack),1
bkpt_plus_t
	STWM	1,-4(0,rs_stack)			; Preserve 1
	BL	bkpt_plus_cont_t,1		; Get PC
	DEP	0,31,2,1
bkpt_plus_cont_t
	LDW	bkpt_plus_bt-bkpt_plus_cont_t(0,1),1		; entry point
	BV	0(1)				; Invoke
	LDWM	4(0,rs_stack),1
bkpt_plus_ep
	NOP					; Slot for fall through
bkpt_plus_bt
	NOP					; Slot for branch target

bkpt_minus_proceed_start
bkpt_minus_t
	STWM	1,-4(0,rs_stack)			; Preserve 1
	BL	bkpt_minus_cont_t,1		; Get PC
	DEP	0,31,2,1
bkpt_minus_cont_t
	LDW	bkpt_minus_bt-bkpt_minus_cont_t(0,1),1 ; entry point
	BV	0(1)				; Invoke
	LDWM	4(0,rs_stack),1
bkpt_minus_proceed
	COMB,=	1,1,bkpt_minus_t		; Slot for first instruction
	NOP					; Slot for second instruction
	STWM	1,-4(0,rs_stack)			; Preserve 1
	BL	bkpt_minus_cont_f,1		; Get PC
	DEP	0,31,2,1
bkpt_minus_cont_f
	LDW	bkpt_minus_ep-bkpt_minus_cont_f(0,1),1 ; entry point
	BV	0(1)				; Invoke
	LDWM	4(0,rs_stack),1
bkpt_minus_ep
	NOP					; Slot for fall through
bkpt_minus_bt
	NOP					; Slot for branch target

bkpt_closure_proceed
	BL	bkpt_closure_cont,1
	DEP	0,31,2,1
bkpt_closure_cont
	LDW	bkpt_closure_entry-bkpt_closure_cont(0,1),25
	LDW	bkpt_closure_closure-bkpt_closure_cont(0,1),31
	BV	0(25)	
	COPY	31,25
bkpt_closure_closure
	NOP					; Closure object pointer
bkpt_closure_entry
	NOP					; Eventual entry point
bkpt_closure_proceed_end
	NOP	

	.SPACE	$TEXT$
	.SUBSPA $LIT$,QUAD=0,ALIGN=8,ACCESS=44
;	.SUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY
	.SUBSPA $UNWIND$,QUAD=0,ALIGN=8,ACCESS=44
	.SUBSPA $CODE$
	.SPACE	$PRIVATE$
	.SUBSPA $SHORTBSS$
interface_to_scheme_restore .COMM 4
interface_to_scheme_new .COMM 4
interface_to_scheme .COMM 4
interface_to_C .COMM 4
scheme_hooks_low .COMM 4
scheme_hooks_high .COMM 4
	.SUBSPA $DATA$,QUAD=1,ALIGN=8,ACCESS=31
$THISMODULE$
ifelse(ASM_DEBUG,1,"interface_counter
	.ALIGN	8
	.WORD	0
interface_limit
	.WORD	0")
undivert(1)
	.SUBSPA $BSS$,QUAD=1,ALIGN=8,ACCESS=31,ZERO
	.IMPORT $global$,DATA
	.IMPORT	Registers,DATA
	.IMPORT	Ext_Stack_Pointer,DATA
	.IMPORT	Free,DATA
	.IMPORT	hppa_utility_table,DATA
	.IMPORT	hppa_primitive_table,DATA
	.IMPORT	Primitive_Arity_Table,DATA
	.IMPORT	Primitive_Procedure_Table,DATA
	.SPACE	$TEXT$
	.SUBSPA $CODE$
        .IMPORT $$dyncall,MILLICODE
        .IMPORT $$remI,MILLICODE
	.IMPORT declare_builtin,CODE
	.IMPORT	sin,CODE
	.IMPORT	cos,CODE
	.IMPORT	tan,CODE
	.IMPORT	asin,CODE
	.IMPORT	acos,CODE
	.IMPORT	atan,CODE
	.IMPORT	exp,CODE
	.IMPORT	log,CODE
	.IMPORT	double_truncate,CODE
	.IMPORT	ceil,CODE
	.IMPORT	floor,CODE
	.IMPORT	atan2,CODE
	.EXPORT C_to_interface,PRIV_LEV=3,ARGW0=GR,RTNVAL=GR
	.EXPORT ep_interface_to_scheme,PRIV_LEV=3
	.EXPORT ep_interface_to_scheme_new,PRIV_LEV=3
	.EXPORT ep_interface_to_scheme_restore,PRIV_LEV=3
	.EXPORT scheme_to_interface_ble,PRIV_LEV=3
	.EXPORT trampoline_to_interface,PRIV_LEV=3
	.EXPORT scheme_to_interface,PRIV_LEV=3
	.EXPORT hook_jump_table,PRIV_LEV=3
	.EXPORT cross_segment_call,PRIV_LEV=3
	.EXPORT	flonum_atan2,PRIV_LEV=3
	.EXPORT preserve_registers,PRIV_LEV=3
	.EXPORT restore_registers,PRIV_LEV=3
	.EXPORT interpreter_call,PRIV_LEV=3
	.EXPORT ep_interface_to_C,PRIV_LEV=3
	.EXPORT interface_initialize,PRIV_LEV=3
	.EXPORT cache_flush_region,PRIV_LEV=3
	.EXPORT cache_flush_all,PRIV_LEV=3
	.EXPORT bkpt_normal_proceed,PRIV_LEV=3
	.EXPORT bkpt_plus_proceed,PRIV_LEV=3
	.EXPORT bkpt_minus_proceed_start,PRIV_LEV=3
	.EXPORT bkpt_minus_proceed,PRIV_LEV=3
	.EXPORT bkpt_closure_proceed,PRIV_LEV=3
	.EXPORT bkpt_closure_proceed_end,PRIV_LEV=3
	.END

;;; Emacs magic:
;;; Local Variables:
;;; comment-column: 48
;;; comment-start: "; "
;;; comment-start-skip: ";+ "
;;; End:

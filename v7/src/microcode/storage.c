/* -*-C-*-

Copyright (c) 1986 Massachusetts Institute of Technology

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
MIT in each case. */

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/storage.c,v 5.2 1986/12/20 01:25:14 cph Exp $

This file defines the storage for global variables for
the Scheme Interpreter. */

#include "scheme.h"
#include "prims.h"
#include "gctype.c"

                         /*************/
                         /* REGISTERS */
                         /*************/

Pointer
  Env,		 	/* The environment */
  Val,		        /* The value returned from primitives or apply */
  Return,		/* The return address code */
  Expression,   	/* Expression to EVALuate */
 *History,              /* History register */
 *Free,			/* Next free word in storage */
 *MemTop,		/* Top of free space available */
 *Stack_Pointer,	/* Next available slot in control stack */
 *Stack_Top,		/* Top of control stack */
 *Stack_Guard,		/* Guard area at end of stack */
 *Free_Stacklets,	/* Free list of stacklets */
 *Constant_Space,	/* Bottom of constant+pure space */
 *Free_Constant,	/* Next free cell in constant+pure area */
 *Unused_Heap_Top, *Unused_Heap,
			/* Top and bottom of 'other' heap for GC */
 *Heap_Top, *Heap_Bottom, /* Top and bottom of current heap area */
 *Local_Heap_Base,	/* Per-processor CONSing area */
 *Heap,			/* Bottom of entire heap */
 Swap_Temp,		/* Used by Swap_Pointers in default.h */
 Lookup_Base,	        /* Slot lookup returns result here */
 Fluid_Bindings=NIL,	/* Fluid bindings AList */
 Current_State_Point=NIL, /* Used by dynamic winder */
 return_to_interpreter,	/* Return address/code left by interpreter
			   when calling compiled code */
 *last_return_code;	/* Address of the most recent return code in the stack.
			   This is only meaningful while in compiled code.
			   *** This must be changed when stacklets are used. ***
			 */

long IntCode,		/* Interrupts requesting */
     IntEnb,		/* Interrupts enabled */
     Lookup_Offset,	/* Slot lookup result return */
     GC_Reserve = 4500,	/* Scheme pointer overflow space in heap */
     GC_Space_Needed, 	/* Amount of space needed when GC triggered */
     /* Used to signal microcode errors from compiled code. */
     compiled_code_error_code;

Declare_Fixed_Objects();

FILE *(Channels[FILE_CHANNELS]), *File_Handle, *Photo_File_Handle;
int Saved_argc;
char **Saved_argv;
char *OS_Name, *OS_Variant;
Boolean Photo_Open = false; /* Photo file open */
Boolean Trapping, Can_Do_Cursor;
Pointer Old_Return_Code, *Return_Hook_Address,
        *Previous_Restore_History_Stacklet,
	Weak_Chain;
long Previous_Restore_History_Offset;
jmp_buf *Back_To_Eval; /* Buffer for set/longjmp */
long Heap_Size, Constant_Size, Stack_Size;
Pointer *Highest_Allocated_Address;

#ifndef Heap_In_Low_Memory
Pointer *Memory_Base;
#endif

                    /**********************/
                    /* DEBUGGING SWITCHES */
                    /**********************/

#ifdef ENABLE_DEBUGGING_TOOLS
Boolean Eval_Debug	= false;
Boolean Hex_Input_Debug	= false;
Boolean File_Load_Debug	= false;
Boolean Reloc_Debug	= false;	
Boolean Intern_Debug	= false;
Boolean Cont_Debug	= false;
Boolean Primitive_Debug	= false;
Boolean Lookup_Debug	= false;
Boolean Define_Debug	= false;
Boolean GC_Debug	= false;
Boolean Upgrade_Debug	= false;
Boolean Dump_Debug	= false;
Boolean Trace_On_Error	= false;
Boolean Bignum_Debug    = false;
Boolean Per_File	= true;
Boolean Fluids_Debug	= false;
More_Debug_Flag_Allocs();

int debug_slotno = 0;
int debug_nslots = 0;
int local_slotno = 0;
int local_nslots = 0;
/* MHWU
int debug_circle[debug_maxslots];
int local_circle[debug_maxslots];
*/
int debug_circle[100];
int local_circle[100];
#endif

		/****************************/
		/* Debugging Macro Messages */
		/****************************/

char *CONT_PRINT_RETURN_MESSAGE =   "Save_Cont, return code";
char *CONT_PRINT_EXPR_MESSAGE   =   "Save_Cont, expression";
char *RESTORE_CONT_RETURN_MESSAGE = "Restore_Cont, return code";
char *RESTORE_CONT_EXPR_MESSAGE =   "Restore_Cont, expression";


            /*********************************/
            /* Argument Count for Primitives */
            /*********************************/

char Arg_Count_Table[] = {
/* 000 */ (char) 3, /* LEXICAL-ASSIGNMENT */
/* 001 */ (char) 2, /* LOCAL-REFERENCE */
/* 002 */ (char) 3, /* LOCAL-ASSIGNMENT */
/* 003 */ (char) 1, /* CATCH */
/* 004 */ (char) 2, /* SCODE-EVAL */
/* 005 */ (char) 2, /* APPLY */
/* 006 */ (char) 1, /* SET!-INTERRUPT-ENABLES */
/* 007 */ (char) 1, /* STRING->SYMBOL */
/* 008 */ (char) 1, /* GET-WORK */
/* 009 */ (char) 1, /* NON-REENTRANT-CATCH */
/* 00A */ (char) 1, /* GET-CURRENT-DYNAMIC-STATE */
/* 00B */ (char) 1, /* SET-CURRENT-DYNAMIC-STATE! */
/* 00C */ (char) 1, /* NULL? NOT */
/* 00D */ (char) 2, /* EQ? */
/* 00E */ (char) 2, /* STRING-EQUAL? */
/* 00F */ (char) 2, /* PRIMITIVE-TYPE? */
/* 010 */ (char) 1, /* PRIMITIVE-TYPE */
/* 011 */ (char) 2, /* PRIMITIVE-SET-TYPE */
/* 012 */ (char) 2, /* LEXICAL-REFERENCE */
/* 013 */ (char) 2, /* LEXICAL-UNREFERENCEABLE-TEST */
/* 014 */ (char) 2, /* MAKE-CHAR */
/* 015 */ (char) 1, /* CHAR-BITS */
/* 016 */ (char) 0, /* NON-RESTARTABLE-EXIT */
/* 017 */ (char) 1, /* CHAR-CODE */
/* 018 */ (char) 2, /* UNASSIGNED-TEST */
/* 019 */ (char) 3, /* INSERT-NON-MARKED-VECTOR */
/* 01A */ (char) 0, /* RESTARTABLE-EXIT */
/* 01B */ (char) 1, /* CHAR->INTEGER */
/* 01C */ (char) 2, /* MEMQ */
/* 01D */ (char) 3, /* INSERT-STRING */
/* 01E */ (char) 1, /* ENABLE-INTERRUPTS */
/* 01F */ (char) 1, /* MAKE-EMPTY-STRING */
/* 020 */ (char) 2, /* CONS */
/* 021 */ (char) 1, /* CAR */
/* 022 */ (char) 1, /* CDR */
/* 023 */ (char) 2, /* SET!-CAR */
/* 024 */ (char) 2, /* SET!-CDR */
/* 025 */ (char) 2, /* PRINT-STRING */
/* 026 */ (char) 0, /* TTY-GET-CURSOR */
/* 027 */ (char) 2, /* GENERAL-CAR-CDR */
/* 028 */ (char) 3, /* HUNK3-CONS */

/* Argument Count Table continues on next page */

/* Argument Count Table, continued */
 
/* 029 */ (char) 2, /* HUNK3-CXR */
/* 02A */ (char) 3, /* HUNK3-SET!-CXR */
/* 02B */ (char) 3, /* OVERWRITE-STRING */
/* 02C */ (char) 2, /* VECTOR-CONS */
/* 02D */ (char) 1, /* VECTOR-SIZE */
/* 02E */ (char) 2, /* VECTOR-REF */
/* 02F */ (char) 1, /* SET-CURRENT-HISTORY */
/* 030 */ (char) 3, /* VECTOR-SET! */
/* 031 */ (char) 1, /* NON-MARKED-VECTOR-CONS */
/* 032 */ (char) 1, /* GET-CHARACTER */
/* 033 */ (char) 2, /* UNBOUND-TEST */
/* 034 */ (char) 1, /* INTEGER->CHAR */
/* 035 */ (char) 1, /* CHAR-DOWNCASE */
/* 036 */ (char) 1, /* CHAR-UPCASE */
/* 037 */ (char) 1, /* ASCII->CHAR */
/* 038 */ (char) 1, /* CHAR-ASCII? */
/* 039 */ (char) 1, /* CHAR->ASCII */
/* 03A */ (char) 1, /* GARBAGE-COLLECT */
/* 03B */ (char) 2, /* PLUS-FIXNUM */
/* 03C */ (char) 2, /* MINUS-FIXNUM */
/* 03D */ (char) 2, /* MULTIPLY-FIXNUM */
/* 03E */ (char) 2, /* DIVIDE-FIXNUM */
/* 03F */ (char) 2, /* EQUAL-FIXNUM? */
/* 040 */ (char) 2, /* LESS-THAN-FIXNUM? */
/* 041 */ (char) 1, /* POSITIVE-FIXNUM? */
/* 042 */ (char) 1, /* ONE-PLUS-FIXNUM */
/* 043 */ (char) 1, /* MINUS-ONE-PLUS-FIXNUM */
/* 044 */ (char) 2, /* TRUNCATE-STRING */
/* 045 */ (char) 3, /* SUBSTRING */
/* 046 */ (char) 1, /* ZERO-FIXNUM? */
/* 047 */ (char) 1, /* UNDANGERIZE */
/* 048 */ (char) 1, /* DANGERIZE */
/* 049 */ (char) 1, /* DANGEROUS? */
/* 04A */ (char) 3, /* SUBSTRING-TO-LIST */
/* 04B */ (char) 2, /* MAKE-FILLED-STRING */
/* 04C */ (char) 2, /* PLUS-BIGNUM */
/* 04D */ (char) 2, /* MINUS-BIGNUM */
/* 04E */ (char) 2, /* MULTIPLY-BIGNUM */
/* 04F */ (char) 2, /* DIVIDE-BIGNUM */
/* 050 */ (char) 2, /* LISTIFY-BIGNUM */
/* 051 */ (char) 2, /* EQUAL-BIGNUM? */
/* 052 */ (char) 2, /* LESS-THAN-BIGNUM? */
/* 053 */ (char) 1, /* POSITIVE-BIGNUM? */

/* Argument Count Table continues on next page */

/* Argument Count Table, continued */

/* 054 */ (char) 2, /* FILE-OPEN-CHANNEL */
/* 055 */ (char) 1, /* FILE-CLOSE-CHANNEL */
/* 056 */ (char) 3, /* PRIMITIVE-FASDUMP */
/* 057 */ (char) 1, /* BINARY-FASLOAD */
/* 058 */ (char) 3, /* STRING-POSITION */
/* 059 */ (char) 2, /* STRING-LESS? */
/* 05A */ (char) 1, /* OBJECT-HASH */
/* 05B */ (char) 1, /* OBJECT-UNHASH */
/* 05C */ (char) 0, /* GC-REHASH-DAEMON */
/* 05D */ (char) 1, /* LENGTH */
/* 05E */ (char) 2, /* ASSQ */
/* 05F */ (char) 1, /* BUILD-STRING-FROM-LIST */
/* 060 */ (char) 2, /* EQUAL-STRING-TO-LIST? */
/* 061 */ (char) 1, /* MAKE-CELL */
/* 062 */ (char) 1, /* CONTENTS */
/* 063 */ (char) 1, /* CELL? */
/* 064 */ (char) 1, /* CHARACTER-UPCASE */
/* 065 */ (char) 1, /* CHARACTER-LIST-HASH */
/* 066 */ (char) 2, /* GCD-FIXNUM */
/* 067 */ (char) 1, /* COERCE-FIXNUM-TO-BIGNUM */
/* 068 */ (char) 1, /* COERCE-BIGNUM-TO-FIXNUM */
/* 069 */ (char) 2, /* PLUS-FLONUM */
/* 06A */ (char) 2, /* MINUS-FLONUM */
/* 06B */ (char) 2, /* MULTIPLY-FLONUM */
/* 06C */ (char) 2, /* DIVIDE-FLONUM */
/* 06D */ (char) 2, /* EQUAL-FLONUM? */
/* 06E */ (char) 2, /* LESS-THAN-FLONUM? */
/* 06F */ (char) 1, /* ZERO-BIGNUM? */
/* 070 */ (char) 1, /* TRUNCATE-FLONUM */
/* 071 */ (char) 1, /* ROUND-FLONUM */
/* 072 */ (char) 1, /* COERCE-INTEGER-TO-FLONUM */
/* 073 */ (char) 1, /* SINE-FLONUM */
/* 074 */ (char) 1, /* COSINE-FLONUM */
/* 075 */ (char) 1, /* ARCTAN-FLONUM */
/* 076 */ (char) 1, /* EXP-FLONUM */
/* 077 */ (char) 1, /* LN-FLONUM */
/* 078 */ (char) 1, /* SQRT-FLONUM */
/* 079 */ (char) 1, /* PRIMITIVE-FASLOAD */
/* 07A */ (char) 0, /* GET-FIXED-OBJECTS-VECTOR */
/* 07B */ (char) 1, /* SET!-FIXED-OBJECTS-VECTOR */
/* 07C */ (char) 1, /* LIST-TO-VECTOR */
/* 07D */ (char) 3, /* SUBVECTOR-TO-LIST */
/* 07E */ (char) 1, /* PAIR? */
/* 07F */ (char) 1, /* NEGATIVE-FIXNUM? */
/* 080 */ (char) 1, /* NEGATIVE-BIGNUM? */

/* Argument Count Table continues on next page */

/* Argument Count Table, continued */

/* 081 */ (char) 2, /* GREATER-THAN-FIXNUM? */
/* 082 */ (char) 2, /* GREATER-THAN-BIGNUM? */
/* 083 */ (char) 1, /* STRING-HASH */
/* 084 */ (char) 3, /* Sys-PAIR-CONS */
/* 085 */ (char) 1, /* Sys-PAIR? */
/* 086 */ (char) 1, /* Sys-PAIR-CAR */
/* 087 */ (char) 1, /* Sys-PAIR-CDR */
/* 088 */ (char) 2, /* Sys-PAIR-SET!-CAR */
/* 089 */ (char) 2, /* Sys-PAIR-SET!-CDR */
/* 08A */ (char) 1, /* INITIALIZE-OBJECT-HASH */
/* 08B */ (char) 1, /* GET-CHARACTER-IMMEDIATE */
/* 08C */ (char) 2, /* SET-CONTENTS! */
/* 08D */ (char) 2, /* &MAKE-OBJECT */
/* 08E */ (char) 1, /* Sys-HUNK3-CXR0 */
/* 08F */ (char) 2, /* Sys-HUNK3-SET!-CXR0 */
/* 090 */ (char) 2, /* MAP-MACHINE-ADDRESS-TO-CODE */
/* 091 */ (char) 1, /* Sys-HUNK3-CXR1 */
/* 092 */ (char) 2, /* Sys-HUNK3-SET!-CXR1 */
/* 093 */ (char) 2, /* MAP-CODE-TO-MACHINE-ADDRESS */
/* 094 */ (char) 1, /* Sys-HUNK3-CXR2 */
/* 095 */ (char) 2, /* Sys-HUNK3-SET!-CXR2 */
/* 096 */ (char) 1, /* MAP-PRIMITIVE-ADDRESS-TO-ARITY */
/* 097 */ (char) 2, /* Sys-LIST-TO-VECTOR */
/* 098 */ (char) 3, /* Sys-SUBVECTOR-TO-LIST */
/* 099 */ (char) 1, /* Sys-VECTOR? */
/* 09A */ (char) 2, /* Sys-VECTOR-REF */
/* 09B */ (char) 3, /* Sys-VECTOR-SET! */
/* 09C */ (char) 1, /* WITH-HISTORY-DISABLED */
/* 09D */ (char) 0, /* unused */
/* 09E */ (char) 0, /* unused */
/* 09F */ (char) 0, /* unused */
/* 0A0 */ (char) 0, /* unused */
/* 0A1 */ (char) 0, /* unused */	
/* 0A2 */ (char) 0, /* unused */
/* 0A3 */ (char) 1, /* VECTOR-8B-CONS */
/* 0A4 */ (char) 1, /* VECTOR-8B? */
/* 0A5 */ (char) 2, /* VECTOR-8B-REF */
/* 0A6 */ (char) 3, /* VECTOR-8B-SET! */
/* 0A7 */ (char) 1, /* ZERO-FLONUM? */
/* 0A8 */ (char) 1, /* POSITIVE-FLONUM? */
/* 0A9 */ (char) 1, /* NEGATIVE-FLONUM? */
/* 0AA */ (char) 2, /* GREATER-THAN-FLONUM? */
/* 0AB */ (char) 1, /* INTERN-CHARACTER-LIST */

/* Argument Count Table continues on next page */

/* Argument Count Table, continued */

/* 0AC */ (char) 0, /* unused */
/* 0AD */ (char) 1, /* VECTOR-8B-SIZE */
/* 0AE */ (char) 1, /* Sys-VECTOR-SIZE */
/* 0AF */ (char) 1, /* FORCE */
/* 0B0 */ (char) 1, /* PRIMITIVE-DATUM */
/* 0B1 */ (char) 1, /* MAKE-NON-POINTER-OBJECT */
/* 0B2 */ (char) 1, /* DEBUGGING-PRINTER */
/* 0B3 */ (char) 1, /* STRING-UPCASE */
/* 0B4 */ (char) 2, /* PRIMITIVE-PURIFY */
/* 0B5 */ (char) 0, /* unused */
/* 0B6 */ (char) 2, /* COMPLETE-GARBAGE-COLLECT */
/* 0B7 */ (char) 2, /* BAND-DUMP */
/* 0B8 */ (char) 2, /* SUBSTRING-SEARCH */
/* 0B9 */ (char) 1, /* BAND-LOAD */
/* 0BA */ (char) 1, /* CONSTANT-P */
/* 0BB */ (char) 1, /* PURE-P */
/* 0BC */ (char) 1, /* GC-TYPE */
/* 0BD */ (char) 1, /* IMPURIFY */
/* 0BE */ (char) 2, /* WITH-THREADED-STACK */
/* 0BF */ (char) 2, /* WITHIN-CONTROL-POINT */
/* 0C0 */ (char) 1, /* SET-RUN-LIGHT */
/* 0C1 */ (char) 1, /* FILE-EOF? */
/* 0C2 */ (char) 1, /* FILE-READ-CHAR */
/* 0C3 */ (char) 2, /* FILE-FILL-INPUT-BUFFER */
/* 0C4 */ (char) 1, /* FILE-LENGTH */
/* 0C5 */ (char) 2, /* FILE-WRITE-CHAR */
/* 0C6 */ (char) 2, /* FILE-WRITE-STRING */
/* 0C7 */ (char) 0, /* CLOSE-LOST-OPEN-FILES */
/* 0C8 */ (char) 2, /* PUT-CHARACTER-TO-OUTPUT-CHANNEL */

/* Argument Count Table continues on next page */

/* Argument Count Table, continued */

/* 0C9 */ (char) 2, /* WITH-INTERRUPTS-REDUCED */
/* 0CA */ (char) 3, /* EVAL-STEP */
/* 0CB */ (char) 3, /* APPLY-STEP */
/* 0CC */ (char) 2, /* RETURN-STEP */
/* 0CD */ (char) 1, /* TTY-READ-CHAR-READY? */
/* 0CE */ (char) 0, /* TTY-READ-CHAR */
/* 0CF */ (char) 0, /* TTY-READ-CHAR-IMMEDIATE */
/* 0D0 */ (char) 0, /* TTY-READ-FINISH */
/* 0D1 */ (char) 1, /* BIT-STRING-ALLOCATE */
/* 0D2 */ (char) 2, /* MAKE-BIT-STRING */
/* 0D3 */ (char) 1, /* BIT-STRING-P */
/* 0D4 */ (char) 1, /* BIT-STRING-LENGTH */
/* 0D5 */ (char) 2, /* BIT-STRING-REF */
/* 0D6 */ (char) 5, /* BIT-SUBSTRING-MOVE-RIGHT-X */
/* 0D7 */ (char) 2, /* BIT-STRING-SET-X */
/* 0D8 */ (char) 2, /* BIT-STRING-CLEAR-X */
/* 0D9 */ (char) 1, /* BIT-STRING-ZERO-P */
/* 0DA */ (char) 0, /* unused */
/* 0DB */ (char) 0, /* unused */
/* 0DC */ (char) 2, /* UNSIGNED-INTEGER-TO-BIT-STRING */
/* 0DD */ (char) 1, /* BIT-STRING-TO-UNSIGNED-INTEGER */
/* 0DE */ (char) 0, /* unused */
/* 0DF */ (char) 3, /* READ-BITS-X */
/* 0E0 */ (char) 3, /* WRITE-BITS-X */
/* 0E1 */ (char) 1, /* MAKE-STATE-SPACE */
/* 0E2 */ (char) 4, /* EXECUTE-AT-NEW-POINT */
/* 0E3 */ (char) 1, /* TRANSLATE-TO-POINT */
/* 0E4 */ (char) 0, /* GET-NEXT-CONSTANT */
/* 0E5 */ (char) 0, /* MICROCODE-IDENTIFY */
/* 0E6 */ (char) 1, /* ZERO */
/* 0E7 */ (char) 1, /* POSITIVE */

/* Argument Count Table continues on next page */

/* Argument Count Table, continued */

/* 0E8 */ (char) 1, /* NEGATIVE */
/* 0E9 */ (char) 2, /* EQUAL-NUMBER */
/* 0EA */ (char) 2, /* LESS */
/* 0EB */ (char) 2, /* GREATER */
/* 0EC */ (char) 2, /* PLUS */
/* 0ED */ (char) 2, /* MINUS */
/* 0EE */ (char) 2, /* MULTIPLY */
/* 0EF */ (char) 2, /* DIVIDE */
/* 0F0 */ (char) 2, /* INTEGER-DIVIDE */
/* 0F1 */ (char) 1, /* ONE-PLUS */
/* 0F2 */ (char) 1, /* MINUS-ONE-PLUS */
/* 0F3 */ (char) 1, /* TRUNCATE */
/* 0F4 */ (char) 1, /* ROUND */
/* 0F5 */ (char) 1, /* FLOOR */
/* 0F6 */ (char) 1, /* CEILING */
/* 0F7 */ (char) 1, /* SQRT */
/* 0F8 */ (char) 1, /* EXP */
/* 0F9 */ (char) 1, /* LN */
/* 0FA */ (char) 1, /* SINE */
/* 0FB */ (char) 1, /* COSINE */
/* 0FB */ (char) 1, /* ARCTAN */
/* 0FD */ (char) 1, /* TTY-WRITE-CHAR */
/* 0FE */ (char) 1, /* TTY-WRITE-STRING */
/* 0FF */ (char) 0, /* TTY-BEEP */
/* 100 */ (char) 0, /* TTY-CLEAR */
/* 101 */ (char) 0, /* GET-EXTERNAL-COUNTS */
/* 102 */ (char) 1, /* GET-EXT-NAME */
/* 103 */ (char) 2, /* GET-EXT-NUMBER */
/* 104 */ (char) 0, /* unused */
/* 105 */ (char) 0, /* unused */
/* 106 */ (char) 0, /* GET-NEXT-INTERRUPT-CHARACTER */
/* 107 */ (char) 2, /* CHECK-AND-CLEAN-UP-INPUT-CHANNEL */
/* 108 */ (char) 0, /* unused */
/* 109 */ (char) 0, /* SYSTEM-CLOCK */
/* 10A */ (char) 1, /* FILE-EXISTS */
/* 10B */ (char) 0, /* unused */
/* 10C */ (char) 2, /* TTY-MOVE-CURSOR */
/* 10D */ (char) 0, /* unused */
/* 10E */ (char) 0, /* CURRENT-DATE */
/* 10F */ (char) 0, /* CURRENT-TIME */
/* 110 */ (char) 2, /* TRANSLATE-FILE */
/* 111 */ (char) 2, /* COPY-FILE */
/* 112 */ (char) 2, /* RENAME-FILE */
/* 113 */ (char) 1, /* REMOVE-FILE */
/* 114 */ (char) 3, /* LINK-FILE */
/* 115 */ (char) 1, /* MAKE-DIRECTORY */
/* 116 */ (char) 1, /* VOLUME-NAME */
/* 117 */ (char) 1, /* SET-WORKING-DIRECTORY-PATHNAME-X */
/* 118 */ (char) 1, /* OPEN-CATALOG */
/* 119 */ (char) 0, /* CLOSE-CATALOG */
/* 11A */ (char) 0, /* NEXT-FILE */
/* 11B */ (char) 0, /* CAT-NAME */
/* 11C */ (char) 0, /* CAT-KIND */
/* 11D */ (char) 0, /* CAT-PSIZE */
/* 11E */ (char) 0, /* CAT-LSIZE */
/* 11F */ (char) 0, /* CAT-INFO */
/* 120 */ (char) 0, /* CAT-BLOCK */
/* 121 */ (char) 0, /* CAT-CREATE-DATE */
/* 122 */ (char) 0, /* CAT-CREATE-TIME */
/* 123 */ (char) 0, /* CAT-LAST-DATE */
/* 124 */ (char) 0, /* CAT-LAST-TIME */
/* 125 */ (char) 0, /* ERROR-MESSAGE */
/* 126 */ (char) 0, /* CURRENT-YEAR */
/* 127 */ (char) 0, /* CURRENT-MONTH */
/* 128 */ (char) 0, /* CURRENT-DAY */
/* 129 */ (char) 0, /* CURRENT-HOUR */
/* 12A */ (char) 0, /* CURRENT-MINUTE */
/* 12B */ (char) 0, /* CURRENT-SECOND */
/* 12C */ (char) 1, /* INIT-FLOPPY */
/* 12D */ (char) 1, /* ZERO-FLOPPY */
/* 12E */ (char) 1, /* PACK-VOLUME */
/* 12F */ (char) 1, /* LOAD-PICTURE */
/* 130 */ (char) 1, /* STORE-PICTURE */
/* 131 */ (char) 1, /* LOOKUP-SYSTEM-SYMBOL */
/* 132 */ (char) 0, /* unused */
/* 133 */ (char) 0, /* unused */
/* 134 */ (char) 0, /* CLEAR-TO-END-OF-LINE */
/* 135 */ (char) 0, /* unused */
/* 136 */ (char) 0, /* unused */
/* 137 */ (char) 2, /* WITH-INTERRUPT-MASK */
/* 138 */ (char) 1, /* STRING? */
/* 139 */ (char) 1, /* STRING-LENGTH */
/* 13A */ (char) 2, /* STRING-REF */
/* 13B */ (char) 3, /* STRING-SET! */
/* 13C */ (char) 5, /* SUBSTRING-MOVE-RIGHT! */
/* 13D */ (char) 5, /* SUBSTRING-MOVE-LEFT! */
/* 13E */ (char) 1, /* STRING-ALLOCATE */
/* 13F */ (char) 1, /* STRING-MAXIMUM-LENGTH */
/* 140 */ (char) 2, /* SET-STRING-LENGTH! */
/* 141 */ (char) 4, /* VECTOR-8B-FILL! */
/* 142 */ (char) 4, /* VECTOR-8B-FIND-NEXT-CHAR */
/* 143 */ (char) 4, /* VECTOR-8B-FIND-PREVIOUS-CHAR */
/* 144 */ (char) 4, /* VECTOR-8B-FIND-NEXT-CHAR-CI */
/* 145 */ (char) 4, /* VECTOR-8B-FIND-PREVIOUS-CHAR-CI */
/* 146 */ (char) 4, /* SUBSTRING-FIND-NEXT-CHAR-IN-SET */
/* 147 */ (char) 4, /* SUBSTRING-FIND-PREVIOUS-CHAR-IN-SET */
/* 148 */ (char) 6, /* SUBSTRING=? */
/* 149 */ (char) 6, /* SUBSTRING-CI=? */
/* 14A */ (char) 6, /* SUBSTRING<? */
/* 14B */ (char) 3, /* SUBSTRING-UPCASE! */
/* 14C */ (char) 3, /* SUBSTRING-DOWNCASE! */
/* 14D */ (char) 6, /* SUBSTRING-MATCH-FORWARD */
/* 14E */ (char) 6, /* SUBSTRING-MATCH-BACKWARD */
/* 14F */ (char) 6, /* SUBSTRING-MATCH-FORWARD-CI */
/* 150 */ (char) 6, /* SUBSTRING-MATCH-BACKWARD-CI */
/* 151 */ (char) 1, /* PHOTO-OPEN */
/* 152 */ (char) 0, /* PHOTO-CLOSE */
/* 153 */ (char) 2, /* SETUP-TIMER-INTERRUPT */
/* 154 */ (char) 0, /* unused */
/* 155 */ (char) 0, /* unused */
/* 156 */ (char) 0, /* unused */
/* 157 */ (char) 0, /* unused */
/* 158 */ (char) 0, /* unused */
/* 159 */ (char) 0, /* unused */
/* 15A */ (char) 0, /* unused */
/* 15B */ (char) 0, /* unused */
/* 15C */ (char) 0, /* unused */
/* 15D */ (char) 0, /* unused */
/* 15E */ (char) 0, /* unused */
/* 15F */ (char) 0, /* unused */
/* 160 */ (char) 0, /* unused */
/* 161 */ (char) 0, /* EXTRACT-NON-MARKED-VECTOR */
/* 162 */ (char) 0, /* UNSNAP-LINKS */
/* 163 */ (char) 0, /* SAFE-PRIMITIVE-P */
/* 164 */ (char) 0, /* SUBSTRING-READ */
/* 165 */ (char) 0, /* SUBSTRING-WRITE */
/* 166 */ (char) 0, /* SCREEN-X-SIZE */
/* 167 */ (char) 0, /* SCREEN-Y-SIZE */
/* 168 */ (char) 0, /* SCREEN-WRITE-CURSOR */
/* 169 */ (char) 0, /* SCREEN-WRITE-CHARACTER */
/* 16A */ (char) 0, /* SCREEN-WRITE-SUBSTRING */
/* 16B */ (char) 0, /* NEXT-FILE-MATCHING */
/* 16C */ (char) 0, /* unused */
/* 16D */ (char) 0, /* TTY-WRITE-BYTE */
/* 16E */ (char) 0, /* FILE-READ-BYTE */
/* 16F */ (char) 0, /* FILE-WRITE-BYTE */
/* 170 */ (char) 0, /* unused: SAVE-SCREEN */
/* 171 */ (char) 0, /* unused: RESTORE-SCREEN */
/* 172 */ (char) 0, /* unused: SUBSCREEN-CLEAR */
/* 173 */ (char) 0, /* unused: AND-GCD */
/* 174 */ (char) 0, /* unused: TTY-REDRAW-SCREEN */
/* 175 */ (char) 0, /* unused: SCREEN-INVERSE-VIDEO */
/* 176 */ (char) 1, /* STRING-TO-SYNTAX-ENTRY */
/* 177 */ (char) 4, /* SCAN-WORD-FORWARD */
/* 178 */ (char) 4, /* SCAN-WORD-BACKWARD */
/* 179 */ (char) 7, /* SCAN-LIST-FORWARD */
/* 17A */ (char) 7, /* SCAN-LIST-BACKWARD */
/* 17B */ (char) 7, /* SCAN-SEXPS-FORWARD */
/* 17C */ (char) 4, /* SCAN-FORWARD-TO-WORD */
/* 17D */ (char) 4, /* SCAN-BACKWARD-PREFIX-CHARS */
/* 17E */ (char) 2, /* CHAR-TO-SYNTAX-CODE */
/* 17F */ (char) 4, /* QUOTED-CHAR-P */
/* 180 */ (char) 0, /* MICROCODE-TABLES-FILENAME */
/* 181 */ (char) 0, /* unused */
/* 182 */ (char) 0, /* unused: FIND-PASCAL-PROGRAM */
/* 183 */ (char) 0, /* unused: EXECUTE-PASCAL-PROGRAM */
/* 184 */ (char) 0, /* unused: GRAPHICS-MOVE */
/* 185 */ (char) 0, /* unused: GRAPHICS-LINE */
/* 186 */ (char) 0, /* unused: GRAPHICS-PIXEL */
/* 187 */ (char) 0, /* unused: GRAPHICS-SET-DRAWING-MODE */
/* 188 */ (char) 0, /* unused: ALPHA-RASTER-P */
/* 189 */ (char) 0, /* unused: TOGGLE-ALPHA-RASTER */
/* 18A */ (char) 0, /* unused: GRAPHICS-RASTER-P */
/* 18B */ (char) 0, /* unused: TOGGLE-GRAPHICS-RASTER */
/* 18C */ (char) 0, /* unused: GRAPHICS-CLEAR */
/* 18D */ (char) 0, /* unused: GRAPHICS-SET-LINE-STYLE */
/* 18E */ (char) 3, /* ERROR-PROCEDURE */
/* 18F */ (char) 0, /* VOLUME-EXISTS-P */
/* 190 */ (char) 0, /* RE-CHAR-SET-ADJOIN */
/* 191 */ (char) 0, /* RE-COMPILE-FASTMAP */
/* 192 */ (char) 0, /* RE-MATCH */
/* 193 */ (char) 0, /* RE-SEARCH-FORWARD */
/* 194 */ (char) 0, /* RE-SEARCH-BACKWARD */
/* 195 */ (char) 0, /* SYS-MEMORY-REF */
/* 196 */ (char) 0, /* SYS-MEMORY-SET */
/* 197 */ (char) 2, /* BIT-STRING-FILL-X */
/* 198 */ (char) 2, /* BIT-STRING-MOVE-X */
/* 199 */ (char) 2, /* BIT-STRING-MOVEC-X */
/* 19A */ (char) 2, /* BIT-STRING-OR-X */
/* 19B */ (char) 2, /* BIT-STRING-AND-X */
/* 19C */ (char) 2, /* BIT-STRING-ANDC-X */
/* 19D */ (char) 2, /* BIT-STRING-EQUAL-P */
/* 19E */ (char) 0, /* WORKING-DIRECTORY-PATHNAME */
/* 19F */ (char) 1, /* OPEN-DIRECTORY */
/* 1A0 */ (char) 0, /* DIRECTORY-READ */
/* 1A1 */ (char) 0, /* UNDER-EMACS? */
/* 1A2 */ (char) 0  /* TTY-FLUSH-OUTPUT */
};

#if (MAX_PRIMITIVE_NUMBER != 0x1A2)
/* Cause an error */
#include "prims.h and storage.c are inconsistent -- arity table"
#endif

/* Declare the primitives themselves to be Externs */

extern Pointer
  Prim_And_Make_Object(),
  Prim_Apply(), Prim_Apply_Step(), Prim_Arctan(), Prim_Arctan_Flonum(),
  Prim_Assq(), Prim_Band_Dump(), Prim_Band_Load(), Prim_Big_To_Fix(),
  Prim_Binary_Fasload(),
  Prim_Build_String_From_List(), 
  Prim_Car(), Prim_Cat_Block(), Prim_Cat_Create_Date(),
  Prim_Cat_Create_Time(), Prim_Cat_Info(), Prim_Cat_Kind(),
  Prim_Cat_Last_Date(), Prim_Cat_Last_Time(), Prim_Cat_Lsize(),
  Prim_Cat_Name(), Prim_Cat_Psize(), Prim_Catch(), Prim_Cdr(),
  Prim_Ceiling(), Prim_Cell(), Prim_Cell_Contents(),
  Prim_Character_List_Hash(),
  Prim_Chk_And_Cln_Input_Channel(),
  Prim_Close_Lost_Open_Files(),
  Prim_Clear_To_End_Of_Line(),
  Prim_Close_Catalog(),
  Prim_Complete_Garbage_Collect(), Prim_Cons(),
  Prim_Constant_P(), Prim_Copy_File(),
  Prim_Cosine(), Prim_Cosine_Flonum(),
  Prim_Current_Date(), Prim_Current_Day(),
  Prim_Current_Dynamic_State(), Prim_Current_Hour(),
  Prim_Current_Minute(), Prim_Current_Month(), Prim_Current_Second(),
  Prim_Current_Time(), Prim_Current_Year(),
  Prim_Dangerize(), Prim_Dangerous_QM(),
  Prim_Divide(), Prim_Divide_Bignum(), Prim_Divide_Fixnum(),
  Prim_Divide_Flonum(),
  Prim_Enable_Interrupts(), Prim_Eq(),
  Prim_Equal_Bignum(), Prim_Equal_Fixnum(),
  Prim_Equal_Flonum(), Prim_Equal_Number(),
  Prim_Equal_String_To_List(), Prim_Error_Message(),
  Prim_Eval_Step(),
  Prim_Execute_At_New_Point(),
  Prim_Exp(), Prim_Fix_To_Big(),
  Prim_Exp_Flonum(), Prim_File_Exists(), Prim_Floor(), Prim_Force(),
  Prim_Garbage_Collect(), Prim_Gcd_Fixnum(),
  Prim_Gc_Type(),
  Prim_General_Car_Cdr(), Prim_Get_Character(),
  Prim_Get_Char_Immediate(),
  Prim_Get_External_Count(), Prim_Get_Ext_Name(),
  Prim_Get_Ext_Number();

/* Externs continue on next page */

/* Externs, continued */

extern Pointer
  Prim_Get_Fixed_Objects_Vector(),
  Prim_Get_Next_Constant(), Prim_Get_Next_Interrupt_Char(),
#ifdef COMPILE_FUTURES
  Prim_Get_Work(),
#endif
  Prim_Greater_Bignum(), Prim_Greater(),
  Prim_Greater_Fixnum(), Prim_Greater_Flonum(),
  Prim_Hunk3_Cons(), Prim_Hunk3_Cxr(), Prim_Hunk3_Set_Cxr(),
  Prim_Impurify(), Prim_Init_Floppy(),
  Prim_Initialize_Object_Hash(),
  Prim_Ins_BStr(), Prim_Ins_BStr_Excl(),
  Prim_Insert_Non_Marked_Vector(), Prim_Insert_String(),
  Prim_Int_To_Float(), Prim_Integer_Divide(),
  Prim_Intern_Character_List(), Prim_String_To_Symbol(),
  Prim_Length(), Prim_Less(), Prim_Less_Bignum(), Prim_Less_Fixnum(),
  Prim_Less_Flonum(), Prim_Lexical_Assignment(),
  Prim_Lexical_Reference(), Prim_Link_File(),
  Prim_Listify_Bignum(), Prim_List_To_Vector(), Prim_Ln(),
  Prim_Ln_Flonum(), Prim_Load_Picture(), Prim_Local_Assignment(),
  Prim_Local_Reference(),
  Prim_Lookup_System_Symbol(), Prim_M_1_Plus(),
  Prim_M_1_Plus_Fixnum(), Prim_Make_Cell(),
  Prim_Make_Directory(), Prim_Make_Empty_String(),
  Prim_Make_Fld_String(),
  Prim_Make_Non_Pointer(), Prim_Make_State_Space(),
  Prim_Map_Code_To_Address(),
  Prim_Map_Address_To_Code(),
  Prim_Map_Prim_Address_To_Arity();

/* Externs continue on next page */

/* Externs, continued */

extern Pointer
  Prim_Memq(),
  Prim_Microcode_Identify(),
  Prim_Minus(), Prim_Minus_Bignum(), Prim_Minus_Fixnum(),
  Prim_Minus_Flonum(), Prim_Multiply_Bignum(),
  Prim_Multiply(), Prim_Multiply_Fixnum(), Prim_Multiply_Flonum(),
  Prim_Negative(), Prim_Negative_Bignum(), Prim_Negative_Fixnum(),
  Prim_Negative_Flonum(), Prim_Next_File(),
  Prim_Non_Marked_Vector_Cons(), Prim_Non_Reentrant_Catch(),
  Prim_Non_Restartable_Exit(), Prim_Null(),
  Prim_Object_Hash(), Prim_Object_Unhash(), 
  Prim_One_Plus(), Prim_One_Plus_Fixnum(),
  Prim_Open_Catalog(),
  Prim_Overwrite_String(), Prim_Pack_Volume(),
  Prim_Pair(), Prim_Plus_Bignum(),
  Prim_Plus(), Prim_Plus_Fixnum(), Prim_Plus_Flonum(), Prim_Positive(),
  Prim_Positive_Bignum(),
  Prim_Positive_Fixnum(), Prim_Positive_Flonum(),
  Prim_Primitive_Datum(), Prim_Prim_Fasdump(),
  Prim_Prim_Fasload(), Prim_Primitive_Purify(),
  Prim_Primitive_Set_Type(), Prim_Prim_Type(),
  Prim_Prim_Type_QM(), Prim_Print_String(), Prim_Pure_P(),
  Prim_Put_Char_To_Output_Channel(),
  Prim_Raise_Char(), Prim_Raise_String(),
  Prim_Rehash_Gc_Daemon(),
  Prim_Remove_File(), Prim_Rename_File(),
  Prim_Restartable_Exit(), Prim_Return_Step(),
  Prim_Round(),
  Prim_Round_Flonum(), Prim_Scode_Eval(), Prim_Set_Car(),
  Prim_Set_Cdr(), Prim_Set_Cell_Contents(),
  Prim_Set_Current_History(), Prim_Set_Dynamic_State(),
  Prim_Set_Fixed_Objects_Vector(), Prim_Set_Interrupt_Enables();

/* Externs continue on next page */

/* Externs, continued */

extern Pointer
  Prim_Set_Run_Light(), 
  Prim_Sine(), Prim_Sine_Flonum(),
  Prim_Sqrt(), Prim_Sqrt_Flonum(), Prim_Store_Picture(),
  Prim_String_Equal(), Prim_String_Hash(),
  Prim_String_Less(), Prim_String_Position(),
  Prim_Substring(), Prim_Substring_Search(),
  Prim_Substring_To_List(), Prim_Subvector_To_List(),
  Prim_Sys_H3_0(), Prim_Sys_H3_1(),
  Prim_Sys_H3_2(), Prim_SH3_Set_0(),
  Prim_SH3_Set_1(), Prim_SH3_Set_2(),
  Prim_Sys_List_To_Vector(), Prim_Sys_Pair(),
  Prim_Sys_Pair_Car(), Prim_Sys_Pair_Cdr(),
  Prim_Sys_Pair_Cons(), Prim_Sys_Set_Car(),
  Prim_Sys_Set_Cdr(), Prim_Sys_Subvector_To_List(),
  Prim_Sys_Vector(), Prim_Sys_Vector_Ref(),
  Prim_Sys_Vec_Set(), Prim_Sys_Vec_Size(),
  Prim_System_Clock(), Prim_Temp_Printer(), 
  Prim_Translate_File(),  Prim_Translate_To_Point(),
  Prim_Truncate(), Prim_Truncate_Flonum(), Prim_Truncate_String(),
  Prim_Unassigned_Test(), Prim_Unbound_Test(),
  Prim_Undangerize(), Prim_Unreferenceable_Test(),
  Prim_Unused(),
  Prim_Volume_Name(),
  Prim_Vector_8b(), Prim_Vector_8b_Cons(), Prim_Vector_8b_Ref(),
  Prim_Vec_8b_Size(), Prim_Vector_Cons(), Prim_Vector_Ref(),
  Prim_Vector_Set(), Prim_Vector_Size(), Prim_With_History_Disabled(),
  Prim_With_Interrupt_Mask(), Prim_With_Interrupts_Reduced(),
  Prim_With_Threaded_Stack(), Prim_Within_Control_Point(),
  Prim_Zero(),  Prim_Zero_Bignum(), Prim_Zero_Fixnum(),
  Prim_Zero_Flonum(), Prim_Zero_Floppy();

extern Pointer
  Prim_Make_Char(), Prim_Char_Bits(), Prim_Char_Code(),
  Prim_Char_To_Integer(), Prim_Integer_To_Char(),
  Prim_Char_Downcase(), Prim_Char_Upcase(), Prim_Ascii_To_Char(),
  Prim_Char_Ascii_P(), Prim_Char_To_Ascii(),

  Prim_File_Open_Channel(), Prim_File_Close_Channel(),
  Prim_File_Eof_P(), Prim_File_Read_Char(), 
  Prim_File_Fill_Input_Buffer(), Prim_File_Length(),
  Prim_File_Write_Char(), Prim_File_Write_String(),

  Prim_Tty_Read_Char_Ready_P(), Prim_Tty_Read_Char(),
  Prim_Tty_Read_Char_Immediate(), Prim_Tty_Read_Finish(),
  Prim_Tty_Write_Char(), Prim_Tty_Write_String(), Prim_tty_flush_output(),
  Prim_Tty_Beep(), Prim_Tty_Clear(), 
  Prim_Photo_Open(), Prim_Photo_Close(),
  Prim_Setup_Timer_Interrupt(),
  Prim_Tty_Move_Cursor(), Prim_Tty_Get_Cursor(),

  Prim_String_P(),Prim_String_Length(),Prim_String_Ref(),
  Prim_String_Set(), Prim_Substring_Move_Right(),
  Prim_Substring_Move_Left(), Prim_String_Allocate(),
  Prim_String_Maximum_Length(), Prim_Set_String_Length();

extern Pointer
  Prim_Vector_8b_Set(), Prim_Vector_8b_Fill(),
  Prim_Vector_8b_Find_Next_Char(),
  Prim_Vector_8b_Find_Previous_Char(),
  Prim_Vector_8b_Find_Next_Char_Ci(),
  Prim_Vector_8b_Find_Previous_Char_Ci(),
  Prim_Substring_Find_Next_Char_In_Set(),
  Prim_Substring_Find_Previous_Char_In_Set(),
  Prim_Substring_Equal(),
  Prim_Substring_Ci_Equal(),		
  Prim_Substring_Less(),
  Prim_Substring_Upcase(),	
  Prim_Substring_Downcase(),	
  Prim_Substring_Match_Forward(),
  Prim_Substring_Match_Backward(),
  Prim_Substring_Match_Forward_Ci(),
  Prim_Substring_Match_Backward_Ci(),
  Prim_Screen_X_Size(),
  Prim_Screen_Y_Size(),
/* Not yet implemented below here */
  Prim_Extract_Non_Marked_Vector(),
  Prim_Unsnap_Links(),
  Prim_Safe_Primitive_P(),
  Prim_Substring_Read(),
  Prim_Substring_Write(),
  Prim_Screen_Write_Cursor(),
  Prim_Screen_Write_Character(),
  Prim_Screen_Write_Substring(),
  Prim_Next_File_Matching(),
  Prim_Tty_Write_Byte(),
  Prim_File_Read_Byte(),
  Prim_File_Write_Byte(),
#if 0
  Prim_And_Gcd(),
  Prim_Save_Screen(),
  Prim_Restore_Screen(),
  Prim_Subscreen_Clear(),
  Prim_Tty_Redraw_Screen(),
  Prim_Screen_Inverse_Video(),
#endif
  Prim_String_To_Syntax_Entry(),
  Prim_Scan_Word_Forward(),
  Prim_Scan_Word_Backward(),
  Prim_Scan_List_Forward(),
  Prim_Scan_List_Backward(),
  Prim_Scan_Sexps_Forward(),
  Prim_Scan_Forward_To_Word(),
  Prim_Scan_Backward_Prefix_Chars(),
  Prim_Char_To_Syntax_Code(),
  Prim_Quoted_Char_P(),
  Prim_Microcode_Tables_Filename(),
#if 0
  Prim_Find_Pascal_Program(),
  Prim_Execute_Pascal_Program(),
  Prim_Graphics_Move(),
  Prim_Graphics_Line(),
  Prim_Graphics_Pixel(),
  Prim_Graphics_Set_Drawing_Mode(),
  Prim_Alpha_Raster_P(),
  Prim_Toggle_Alpha_Raster(),
  Prim_Graphics_Raster_P(),
  Prim_Toggle_Graphics_Raster(),
  Prim_Graphics_Clear(),
  Prim_Graphics_Set_Line_Style(),
#endif
  Prim_Error_Procedure(),
  Prim_Volume_Exists_P(),
  Prim_Re_Char_Set_Adjoin(),
  Prim_Re_Compile_Fastmap(),
  Prim_Re_Match(),
  Prim_Re_Search_Forward(),
  Prim_Re_Search_Backward(),
  Prim_Sys_Memory_Ref(),
  Prim_Sys_Memory_Set(),

/* new directory access primitives */
  Prim_working_directory_pathname(),
  Prim_set_working_directory_pathname_x(),
  Prim_open_directory(),
  Prim_directory_read(),

/* new bit string primitives */
  Prim_bit_string_allocate(), Prim_make_bit_string(),
  Prim_bit_string_p(), Prim_bit_string_length(),
  Prim_bit_string_ref(), Prim_bit_substring_move_right_x(),
  Prim_bit_string_set_x(), Prim_bit_string_clear_x(),
  Prim_unsigned_integer_to_bit_string(), Prim_bit_string_to_unsigned_integer(),
  Prim_read_bits_x(), Prim_write_bits_x(),
  Prim_bit_string_fill_x(), Prim_bit_string_move_x(),
  Prim_bit_string_movec_x(), Prim_bit_string_or_x(),
  Prim_bit_string_and_x(), Prim_bit_string_andc_x(),
  Prim_bit_string_equal_p(), Prim_bit_string_zero_p(),

  Prim_under_emacs_p();

/* The table of all primitive procedures */

Pointer (*(Primitive_Table[]))() = {
/* 000 */ Prim_Lexical_Assignment,
/* 001 */ Prim_Local_Reference,
/* 002 */ Prim_Local_Assignment,
/* 003 */ Prim_Catch,
/* 004 */ Prim_Scode_Eval,
/* 005 */ Prim_Apply,
/* 006 */ Prim_Set_Interrupt_Enables,
/* 007 */ Prim_String_To_Symbol,
#ifdef COMPILE_FUTURES
/* 008 */ Prim_Get_Work,
#else
/* 008 */ Prim_Unused,
#endif
/* 009 */ Prim_Non_Reentrant_Catch,
/* 00A */ Prim_Current_Dynamic_State,
/* 00B */ Prim_Set_Dynamic_State,
/* 00C */ Prim_Null,
/* 00D */ Prim_Eq,
/* 00E */ Prim_String_Equal,
/* 00F */ Prim_Prim_Type_QM,
/* 010 */ Prim_Prim_Type,
/* 011 */ Prim_Primitive_Set_Type,
/* 012 */ Prim_Lexical_Reference,
/* 013 */ Prim_Unreferenceable_Test,
/* 014 */ Prim_Make_Char,
/* 015 */ Prim_Char_Bits,
/* 016 */ Prim_Non_Restartable_Exit,
/* 017 */ Prim_Char_Code,
/* 018 */ Prim_Unassigned_Test,
/* 019 */ Prim_Insert_Non_Marked_Vector,
/* 01A */ Prim_Restartable_Exit,
/* 01B */ Prim_Char_To_Integer,
/* 01C */ Prim_Memq,
/* 01D */ Prim_Insert_String,
/* 01E */ Prim_Enable_Interrupts,
/* 01F */ Prim_Make_Empty_String,
/* 020 */ Prim_Cons,
/* 021 */ Prim_Car,
/* 022 */ Prim_Cdr,
/* 023 */ Prim_Set_Car,
/* 024 */ Prim_Set_Cdr,
/* 025 */ Prim_Print_String,
/* 026 */ Prim_Tty_Get_Cursor,
/* 027 */ Prim_General_Car_Cdr,
/* 028 */ Prim_Hunk3_Cons,

/* Primitive dispatch table continues on next page */

/* Primitive dispatch table, continued */

/* 029 */ Prim_Hunk3_Cxr,
/* 02A */ Prim_Hunk3_Set_Cxr,
/* 02B */ Prim_Overwrite_String,
/* 02C */ Prim_Vector_Cons,
/* 02D */ Prim_Vector_Size,
/* 02E */ Prim_Vector_Ref,
/* 02F */ Prim_Set_Current_History,
/* 030 */ Prim_Vector_Set,
/* 031 */ Prim_Non_Marked_Vector_Cons,
/* 032 */ Prim_Get_Character,
/* 033 */ Prim_Unbound_Test,
/* 034 */ Prim_Integer_To_Char,
/* 035 */ Prim_Char_Downcase,
/* 036 */ Prim_Char_Upcase,
/* 037 */ Prim_Ascii_To_Char,
/* 038 */ Prim_Char_Ascii_P,
/* 039 */ Prim_Char_To_Ascii,
/* 03A */ Prim_Garbage_Collect,
/* 03B */ Prim_Plus_Fixnum,
/* 03C */ Prim_Minus_Fixnum,
/* 03D */ Prim_Multiply_Fixnum,
/* 03E */ Prim_Divide_Fixnum,
/* 03F */ Prim_Equal_Fixnum,
/* 040 */ Prim_Less_Fixnum,
/* 041 */ Prim_Positive_Fixnum,
/* 042 */ Prim_One_Plus_Fixnum,
/* 043 */ Prim_M_1_Plus_Fixnum,
/* 044 */ Prim_Truncate_String,
/* 045 */ Prim_Substring,
/* 046 */ Prim_Zero_Fixnum,
/* 047 */ Prim_Undangerize,
/* 048 */ Prim_Dangerize,
/* 049 */ Prim_Dangerous_QM,
/* 04A */ Prim_Substring_To_List,
/* 04B */ Prim_Make_Fld_String,
/* 04C */ Prim_Plus_Bignum,
/* 04D */ Prim_Minus_Bignum,
/* 04E */ Prim_Multiply_Bignum,
/* 04F */ Prim_Divide_Bignum,
/* 050 */ Prim_Listify_Bignum,
/* 051 */ Prim_Equal_Bignum,
/* 052 */ Prim_Less_Bignum,
/* 053 */ Prim_Positive_Bignum,

/* Primitive dispatch table continues on next page */

/* Primitive dispatch table, continued */

/* 054 */ Prim_File_Open_Channel,
/* 055 */ Prim_File_Close_Channel,
/* 056 */ Prim_Prim_Fasdump,
/* 057 */ Prim_Binary_Fasload,
/* 058 */ Prim_String_Position,
/* 059 */ Prim_String_Less,
/* 05A */ Prim_Object_Hash,
/* 05B */ Prim_Object_Unhash,
/* 05C */ Prim_Rehash_Gc_Daemon,
/* 05D */ Prim_Length,
/* 05E */ Prim_Assq,
/* 05F */ Prim_Build_String_From_List,
/* 060 */ Prim_Equal_String_To_List,
/* 061 */ Prim_Make_Cell,
/* 062 */ Prim_Cell_Contents,
/* 063 */ Prim_Cell,
/* 064 */ Prim_Raise_Char,
/* 065 */ Prim_Character_List_Hash,
/* 066 */ Prim_Gcd_Fixnum,
/* 067 */ Prim_Fix_To_Big,
/* 068 */ Prim_Big_To_Fix,
/* 069 */ Prim_Plus_Flonum,
/* 06A */ Prim_Minus_Flonum,
/* 06B */ Prim_Multiply_Flonum,
/* 06C */ Prim_Divide_Flonum,
/* 06D */ Prim_Equal_Flonum,
/* 06E */ Prim_Less_Flonum,
/* 06F */ Prim_Zero_Bignum,
/* 070 */ Prim_Truncate_Flonum,
/* 071 */ Prim_Round_Flonum,
/* 072 */ Prim_Int_To_Float,
/* 073 */ Prim_Sine_Flonum,
/* 074 */ Prim_Cosine_Flonum,
/* 075 */ Prim_Arctan_Flonum,
/* 076 */ Prim_Exp_Flonum,
/* 077 */ Prim_Ln_Flonum,
/* 078 */ Prim_Sqrt_Flonum,
/* 079 */ Prim_Prim_Fasload,
/* 07A */ Prim_Get_Fixed_Objects_Vector,
/* 07B */ Prim_Set_Fixed_Objects_Vector,
/* 07C */ Prim_List_To_Vector,
/* 07D */ Prim_Subvector_To_List,
/* 07E */ Prim_Pair,
/* 07F */ Prim_Negative_Fixnum,
/* 080 */ Prim_Negative_Bignum,

/* Primitive dispatch table continues on next page */

/* Primitive dispatch table, continued */

/* 081 */ Prim_Greater_Fixnum,
/* 082 */ Prim_Greater_Bignum,
/* 083 */ Prim_String_Hash,
/* 084 */ Prim_Sys_Pair_Cons,
/* 085 */ Prim_Sys_Pair,
/* 086 */ Prim_Sys_Pair_Car,
/* 087 */ Prim_Sys_Pair_Cdr,
/* 088 */ Prim_Sys_Set_Car,
/* 089 */ Prim_Sys_Set_Cdr,
/* 08A */ Prim_Initialize_Object_Hash,
/* 08B */ Prim_Get_Char_Immediate,
/* 08C */ Prim_Set_Cell_Contents,
/* 08D */ Prim_And_Make_Object,
/* 08E */ Prim_Sys_H3_0,
/* 08F */ Prim_SH3_Set_0,
/* 090 */ Prim_Map_Address_To_Code,
/* 091 */ Prim_Sys_H3_1,
/* 092 */ Prim_SH3_Set_1,
/* 093 */ Prim_Map_Code_To_Address,
/* 094 */ Prim_Sys_H3_2,
/* 095 */ Prim_SH3_Set_2,
/* 096 */ Prim_Map_Prim_Address_To_Arity,
/* 097 */ Prim_Sys_List_To_Vector,
/* 098 */ Prim_Sys_Subvector_To_List,
/* 099 */ Prim_Sys_Vector,
/* 09A */ Prim_Sys_Vector_Ref,
/* 09B */ Prim_Sys_Vec_Set,
/* 09C */ Prim_With_History_Disabled,
/* 09D */ Prim_Unused,
/* 09E */ Prim_Unused,
/* 09F */ Prim_Unused,
/* 0A0 */ Prim_Unused,
/* 0A1 */ Prim_Unused,
/* 0A2 */ Prim_Unused,
/* 0A3 */ Prim_Vector_8b_Cons,
/* 0A4 */ Prim_Vector_8b,
/* 0A5 */ Prim_Vector_8b_Ref,
/* 0A6 */ Prim_Vector_8b_Set,
/* 0A7 */ Prim_Zero_Flonum,
/* 0A8 */ Prim_Positive_Flonum,
/* 0A9 */ Prim_Negative_Flonum,
/* 0AA */ Prim_Greater_Flonum,
/* 0AB */ Prim_Intern_Character_List,

/* Primitive dispatch table continues on next page */

/* Primitive dispatch table, continued */

/* 0AC */ Prim_Unused,
/* 0AD */ Prim_Vec_8b_Size,
/* 0AE */ Prim_Sys_Vec_Size,
/* 0AF */ Prim_Force,
/* 0B0 */ Prim_Primitive_Datum,
/* 0B1 */ Prim_Make_Non_Pointer,
/* 0B2 */ Prim_Temp_Printer,
/* 0B3 */ Prim_Raise_String,
/* 0B4 */ Prim_Primitive_Purify,
/* 0B5 */ Prim_Unused,
/* 0B6 */ Prim_Complete_Garbage_Collect,
/* 0B7 */ Prim_Band_Dump,
/* 0B8 */ Prim_Substring_Search,
/* 0B9 */ Prim_Band_Load,
/* 0BA */ Prim_Constant_P,
/* 0BB */ Prim_Pure_P,
/* 0BC */ Prim_Gc_Type,
/* 0BD */ Prim_Impurify,
/* 0BE */ Prim_With_Threaded_Stack,
/* 0BF */ Prim_Within_Control_Point,
/* 0C0 */ Prim_Set_Run_Light,
/* 0C1 */ Prim_File_Eof_P,
/* 0C2 */ Prim_File_Read_Char,
/* 0C3 */ Prim_File_Fill_Input_Buffer,
/* 0C4 */ Prim_File_Length,
/* 0C5 */ Prim_File_Write_Char,
/* 0C6 */ Prim_File_Write_String,
/* 0C7 */ Prim_Close_Lost_Open_Files,
/* 0C8 */ Prim_Put_Char_To_Output_Channel,
/* 0C9 */ Prim_With_Interrupts_Reduced,

/* Primitive dispatch table continues on next page */

/* Primitive dispatch table, continued */

/* 0CA */ Prim_Eval_Step,
/* 0CB */ Prim_Apply_Step,
/* 0CC */ Prim_Return_Step,
/* 0CD */ Prim_Tty_Read_Char_Ready_P,
/* 0CE */ Prim_Tty_Read_Char,
/* 0CF */ Prim_Tty_Read_Char_Immediate,
/* 0D0 */ Prim_Tty_Read_Finish,
/* 0D1 */ Prim_bit_string_allocate,
/* 0D2 */ Prim_make_bit_string,
/* 0D3 */ Prim_bit_string_p,
/* 0D4 */ Prim_bit_string_length,
/* 0D5 */ Prim_bit_string_ref,
/* 0D6 */ Prim_bit_substring_move_right_x,
/* 0D7 */ Prim_bit_string_set_x,
/* 0D8 */ Prim_bit_string_clear_x,
/* 0D9 */ Prim_bit_string_zero_p,
/* 0DA */ Prim_Unused,
/* 0DB */ Prim_Unused,
/* 0DC */ Prim_unsigned_integer_to_bit_string,
/* 0DD */ Prim_bit_string_to_unsigned_integer,
/* 0DE */ Prim_Unused,
/* 0DF */ Prim_read_bits_x,
/* 0E0 */ Prim_write_bits_x,
/* 0E1 */ Prim_Make_State_Space,
/* 0E2 */ Prim_Execute_At_New_Point,
/* 0E3 */ Prim_Translate_To_Point,
/* 0E4 */ Prim_Get_Next_Constant,
/* 0E5 */ Prim_Microcode_Identify,

/* Primitive dispatch table continues on next page */

/* Primitive dispatch table, continued */

/* 0E6 */ Prim_Zero,
/* 0E7 */ Prim_Positive,
/* 0E8 */ Prim_Negative,
/* 0E9 */ Prim_Equal_Number,
/* 0EA */ Prim_Less,
/* 0EB */ Prim_Greater,
/* 0EC */ Prim_Plus,
/* 0ED */ Prim_Minus,
/* 0EE */ Prim_Multiply,
/* 0EF */ Prim_Divide,
/* 0F0 */ Prim_Integer_Divide,
/* 0F1 */ Prim_One_Plus,
/* 0F2 */ Prim_M_1_Plus,
/* 0F3 */ Prim_Truncate,
/* 0F4 */ Prim_Round,
/* 0F5 */ Prim_Floor,
/* 0F6 */ Prim_Ceiling,
/* 0F7 */ Prim_Sqrt,
/* 0F8 */ Prim_Exp,
/* 0F9 */ Prim_Ln,
/* 0FA */ Prim_Sine,
/* 0FB */ Prim_Cosine,
/* 0FC */ Prim_Arctan,
/* 0FD */ Prim_Tty_Write_Char,
/* 0FE */ Prim_Tty_Write_String,
/* 0FF */ Prim_Tty_Beep,
/* 100 */ Prim_Tty_Clear,
/* 101 */ Prim_Get_External_Count,
/* 102 */ Prim_Get_Ext_Name,
/* 103 */ Prim_Get_Ext_Number,
/* 104 */ Prim_Unused,
/* 105 */ Prim_Unused,
/* 106 */ Prim_Get_Next_Interrupt_Char,
/* 107 */ Prim_Chk_And_Cln_Input_Channel,
/* 108 */ Prim_Unused,
/* 109 */ Prim_System_Clock,
/* 10a */ Prim_File_Exists,
/* 10b */ Prim_Unused,
/* 10c */ Prim_Tty_Move_Cursor,
/* 10d */ Prim_Unused,
/* 10e */ Prim_Current_Date,
/* 10f */ Prim_Current_Time,
/* 110 */ Prim_Translate_File,
/* 111 */ Prim_Copy_File,
/* 112 */ Prim_Rename_File,
/* 113 */ Prim_Remove_File,
/* 114 */ Prim_Link_File,
/* 115 */ Prim_Make_Directory,
/* 116 */ Prim_Volume_Name,
/* 117 */ Prim_set_working_directory_pathname_x,
/* 118 */ Prim_Open_Catalog,
/* 119 */ Prim_Close_Catalog,
/* 11a */ Prim_Next_File,
/* 11b */ Prim_Cat_Name,
/* 11c */ Prim_Cat_Kind,
/* 11d */ Prim_Cat_Psize,
/* 11e */ Prim_Cat_Lsize,
/* 11f */ Prim_Cat_Info,
/* 120 */ Prim_Cat_Block,
/* 121 */ Prim_Cat_Create_Date,
/* 122 */ Prim_Cat_Create_Time,
/* 123 */ Prim_Cat_Last_Date,
/* 124 */ Prim_Cat_Last_Time,
/* 125 */ Prim_Error_Message,
/* 126 */ Prim_Current_Year,
/* 127 */ Prim_Current_Month,
/* 128 */ Prim_Current_Day,
/* 129 */ Prim_Current_Hour,
/* 12a */ Prim_Current_Minute,
/* 12b */ Prim_Current_Second,
/* 12c */ Prim_Init_Floppy,
/* 12d */ Prim_Zero_Floppy,
/* 12e */ Prim_Pack_Volume,
/* 12f */ Prim_Load_Picture,
/* 130 */ Prim_Store_Picture,
/* 131 */ Prim_Lookup_System_Symbol,
/* 132 */ Prim_Unused,
/* 133 */ Prim_Unused,
/* 134 */ Prim_Clear_To_End_Of_Line,
/* 135 */ Prim_Unused,
/* 136 */ Prim_Unused,
/* 137 */ Prim_With_Interrupt_Mask,
/* 138 */ Prim_String_P,
/* 139 */ Prim_String_Length,
/* 13A */ Prim_String_Ref,
/* 13B */ Prim_String_Set,
/* 13C */ Prim_Substring_Move_Right,
/* 13D */ Prim_Substring_Move_Left,
/* 13E */ Prim_String_Allocate,
/* 13F */ Prim_String_Maximum_Length,
/* 140 */ Prim_Set_String_Length,
/* 141 */ Prim_Vector_8b_Fill,
/* 142 */ Prim_Vector_8b_Find_Next_Char,
/* 143 */ Prim_Vector_8b_Find_Previous_Char,
/* 144 */ Prim_Vector_8b_Find_Next_Char_Ci,
/* 145 */ Prim_Vector_8b_Find_Previous_Char_Ci,
/* 146 */ Prim_Substring_Find_Next_Char_In_Set,
/* 147 */ Prim_Substring_Find_Previous_Char_In_Set,
/* 148 */ Prim_Substring_Equal,
/* 149 */ Prim_Substring_Ci_Equal,
/* 14A */ Prim_Substring_Less,
/* 14B */ Prim_Substring_Upcase,
/* 14C */ Prim_Substring_Downcase,
/* 14D */ Prim_Substring_Match_Forward,
/* 14E */ Prim_Substring_Match_Backward,
/* 14F */ Prim_Substring_Match_Forward_Ci,
/* 150 */ Prim_Substring_Match_Backward_Ci,
/* 151 */ Prim_Photo_Open,
/* 152 */ Prim_Photo_Close,
/* 153 */ Prim_Setup_Timer_Interrupt,
/* 154 */ Prim_Unused,
/* 155 */ Prim_Unused,
/* 156 */ Prim_Unused,
/* 157 */ Prim_Unused,
/* 158 */ Prim_Unused,
/* 159 */ Prim_Unused,
/* 15A */ Prim_Unused,
/* 15B */ Prim_Unused,
/* 15C */ Prim_Unused,
/* 15D */ Prim_Unused,
/* 15E */ Prim_Unused,
/* 15F */ Prim_Unused,
/* 160 */ Prim_Unused,
/* 161 */ Prim_Extract_Non_Marked_Vector,
/* 162 */ Prim_Unsnap_Links,
/* 163 */ Prim_Safe_Primitive_P,
/* 164 */ Prim_Substring_Read,
/* 165 */ Prim_Substring_Write,
/* 166 */ Prim_Screen_X_Size,
/* 167 */ Prim_Screen_Y_Size,
/* 168 */ Prim_Screen_Write_Cursor,
/* 169 */ Prim_Screen_Write_Character,
/* 16a */ Prim_Screen_Write_Substring,
/* 16b */ Prim_Next_File_Matching,
/* 16c */ Prim_Unused,
/* 16d */ Prim_Tty_Write_Byte,
/* 16e */ Prim_File_Read_Byte,
/* 16f */ Prim_File_Write_Byte,
/* 170 */ Prim_Unused, /* Prim_Save_Screen, */
/* 171 */ Prim_Unused, /* Prim_Restore_Screen, */
/* 172 */ Prim_Unused, /* Prim_Subscreen_Clear, */
/* 173 */ Prim_Unused, /* Prim_And_Gcd, */
/* 174 */ Prim_Unused, /* Prim_Tty_Redraw_Screen, */
/* 175 */ Prim_Unused, /* Prim_Screen_Inverse_Video, */
/* 176 */ Prim_String_To_Syntax_Entry,
/* 177 */ Prim_Scan_Word_Forward,
/* 178 */ Prim_Scan_Word_Backward,
/* 179 */ Prim_Scan_List_Forward,
/* 17a */ Prim_Scan_List_Backward,
/* 17b */ Prim_Scan_Sexps_Forward,
/* 17c */ Prim_Scan_Forward_To_Word,
/* 17d */ Prim_Scan_Backward_Prefix_Chars,
/* 17e */ Prim_Char_To_Syntax_Code,
/* 17f */ Prim_Quoted_Char_P,
/* 180 */ Prim_Microcode_Tables_Filename,
/* 181 */ Prim_Unused,
/* 182 */ Prim_Unused, /* Prim_Find_Pascal_Program, */
/* 183 */ Prim_Unused, /* Prim_Execute_Pascal_Program, */
/* 184 */ Prim_Unused, /* Prim_Graphics_Move, */
/* 185 */ Prim_Unused, /* Prim_Graphics_Line, */
/* 186 */ Prim_Unused, /* Prim_Graphics_Pixel, */
/* 187 */ Prim_Unused, /* Prim_Graphics_Set_Drawing_Mode, */
/* 188 */ Prim_Unused, /* Prim_Alpha_Raster_P, */
/* 189 */ Prim_Unused, /* Prim_Toggle_Alpha_Raster, */
/* 18a */ Prim_Unused, /* Prim_Graphics_Raster_P, */
/* 18b */ Prim_Unused, /* Prim_Toggle_Graphics_Raster, */
/* 18c */ Prim_Unused, /* Prim_Graphics_Clear, */
/* 18d */ Prim_Unused, /* Prim_Graphics_Set_Line_Style, */
/* 18e */ Prim_Error_Procedure,
/* 18f */ Prim_Volume_Exists_P,
/* 190 */ Prim_Re_Char_Set_Adjoin,
/* 191 */ Prim_Re_Compile_Fastmap,
/* 192 */ Prim_Re_Match,
/* 193 */ Prim_Re_Search_Forward,
/* 194 */ Prim_Re_Search_Backward,
/* 195 */ Prim_Sys_Memory_Ref,
/* 196 */ Prim_Sys_Memory_Set,
/* 197 */ Prim_bit_string_fill_x,
/* 198 */ Prim_bit_string_move_x,
/* 199 */ Prim_bit_string_movec_x,
/* 19a */ Prim_bit_string_or_x,
/* 19b */ Prim_bit_string_and_x,
/* 19c */ Prim_bit_string_andc_x,
/* 19d */ Prim_bit_string_equal_p,
/* 19E */ Prim_working_directory_pathname,
/* 19F */ Prim_open_directory,
/* 1A0 */ Prim_directory_read,
/* 1A1 */ Prim_under_emacs_p,
/* 1A2 */ Prim_tty_flush_output
};

#if (MAX_PRIMITIVE_NUMBER != 0x1A2)
/* Cause an error */
#include "Prims.h and storage.c are inconsistent -- Procedure Table"
#endif

/* And, finally, the table of primitive names. */

static char No_Name[] = "";

char *Primitive_Names[] = {

/* 0x00 in lookup */	"LEXICAL-ASSIGNMENT",
/* 0x01 in lookup */	"LOCAL-REFERENCE",
/* 0x02 in lookup */	"LOCAL-ASSIGNMENT",
/* 0x03 in hooks */	"CALL-WITH-CURRENT-CONTINUATION",
/* 0x04 in hooks */	"SCODE-EVAL",
/* 0x05 in hooks */	"APPLY",
/* 0x06 in hooks */	"SET-INTERRUPT-ENABLES!",
/* 0x07 in fasload */	"STRING->SYMBOL",
/* 0x08 in prim */	"GET-WORK",
/* 0x09 in hooks */	"NON-REENTRANT-CALL-WITH-CURRENT-CONTINUATION",
/* 0x0A in hooks */	"CURRENT-DYNAMIC-STATE",
/* 0x0B in hooks */	"SET-CURRENT-DYNAMIC-STATE!",
/* 0x0C in prim */	"NULL?",
/* 0x0D in prim */	"EQ?",
/* 0x0E in string */	"STRING-EQUAL?",
/* 0x0F in prim */	"PRIMITIVE-TYPE?",
/* 0x10 in prim */	"PRIMITIVE-TYPE",
/* 0x11 in prim */	"PRIMITIVE-SET-TYPE",
/* 0x12 in lookup */	"LEXICAL-REFERENCE",
/* 0x13 in lookup */	"LEXICAL-UNREFERENCEABLE?",
/* 0x14 in character */	"MAKE-CHAR",
/* 0x15 in character */	"CHAR-BITS",
/* 0x16 in sysprim */	"EXIT",
/* 0x17 in character */	"CHAR-CODE",
/* 0x18 in lookup */	"LEXICAL-UNASSIGNED?",
/* 0x19 in prim */	"INSERT-NON-MARKED-VECTOR!",
/* 0x1A in sysprim */	"HALT",
/* 0x1B in character */	"CHAR->INTEGER",
/* 0x1C in list */	"MEMQ",
/* 0x1D in string */	"INSERT-STRING",
/* 0x1E in hooks */	"ENABLE-INTERRUPTS!",
/* 0x1F in string */	"MAKE-EMPTY-STRING",
/* 0x20 in list */	"CONS",
/* 0x21 in list */	"CAR",
/* 0x22 in list */	"CDR",
/* 0x23 in list */	"SET-CAR!",
/* 0x24 in list */	"SET-CDR!",

/* Primitive names continue on the next page */

/* Primitive names, continued */

/* 0x25 in io */	"PRINT-STRING",
/* 0x26 in ttyio */	"TTY-GET-CURSOR",
/* 0x27 in list */	"GENERAL-CAR-CDR",
/* 0x28 in hunk */	"HUNK3-CONS",
/* 0x29 in hunk */	"HUNK3-CXR",
/* 0x2A in hunk */	"HUNK3-SET-CXR!",
/* 0x2B in string */	"INSERT-STRING!",
/* 0x2C in vector */	"VECTOR-CONS",
/* 0x2D in vector */	"VECTOR-LENGTH",
/* 0x2E in vector */	"VECTOR-REF",
/* 0x2F in hooks */	"SET-CURRENT-HISTORY!",
/* 0x30 in vector */	"VECTOR-SET!",
/* 0x31 in prim */	"NON-MARKED-VECTOR-CONS",
/* 0x32 in io */	"GET-CHARACTER-FROM-INPUT-CHANNEL",
/* 0x33 in lookup */	"LEXICAL-UNBOUND?",
/* 0x34 in character */	"INTEGER->CHAR",
/* 0x35 in character */	"CHAR-DOWNCASE",
/* 0x36 in character */	"CHAR-UPCASE",
/* 0x37 in character */	"ASCII->CHAR",
/* 0x38 in character */	"CHAR-ASCII?",
/* 0x39 in character */	"CHAR->ASCII",
/* 0x3A in gcloop */	"GARBAGE-COLLECT",
/* 0x3B in fixnum */	"PLUS-FIXNUM",
/* 0x3C in fixnum */	"MINUS-FIXNUM",
/* 0x3D in fixnum */	"MULTIPLY-FIXNUM",
/* 0x3E in fixnum */	"DIVIDE-FIXNUM",
/* 0x3F in fixnum */	"EQUAL-FIXNUM?",
/* 0x40 in fixnum */	"LESS-THAN-FIXNUM?",
/* 0x41 in fixnum */	"POSITIVE-FIXNUM?",
/* 0x42 in fixnum */	"ONE-PLUS-FIXNUM",
/* 0x43 in fixnum */	"MINUS-ONE-PLUS-FIXNUM",
/* 0x44 in string */	"TRUNCATE-STRING!",
/* 0x45 in string */	"SUBSTRING",
/* 0x46 in fixnum */	"ZERO-FIXNUM?",
/* 0x47 in prim */	"MAKE-OBJECT-SAFE",
/* 0x48 in prim */	"MAKE-OBJECT-DANGEROUS",
/* 0x49 in prim */	"OBJECT-DANGEROUS?",
/* 0x4A in string */	"SUBSTRING->LIST",
/* 0x4B in string */	"MAKE-FILLED-STRING",
/* 0x4C in bignum */	"PLUS-BIGNUM",
/* 0x4D in bignum */	"MINUS-BIGNUM",
/* 0x4E in bignum */	"MULTIPLY-BIGNUM",
/* 0x4F in bignum */	"DIVIDE-BIGNUM",

/* Primitive names continue on the next page */

/* Primitive names, continued */

/* 0x50 in bignum */	"LISTIFY-BIGNUM",
/* 0x51 in bignum */	"EQUAL-BIGNUM?",
/* 0x52 in bignum */	"LESS-THAN-BIGNUM?",
/* 0x53 in bignum */	"POSITIVE-BIGNUM?",
/* 0x54 in fileio */	"FILE-OPEN-CHANNEL",
/* 0x55 in fileio */	"FILE-CLOSE-CHANNEL",
/* 0x56 in fasdump */	"PRIMITIVE-FASDUMP",
/* 0x57 in fasload */	"BINARY-FASLOAD",
/* 0x58 in string */	"STRING-POSITION",
/* 0x59 in string */	"STRING-LESS?",
/* 0x5A in daemon */	"OBJECT-HASH",
/* 0x5B in daemon */	"OBJECT-UNHASH",
/* 0x5C in daemon */	"REHASH-GC-DAEMON",
/* 0x5D in list */	"LENGTH",
/* 0x5E in list */	"ASSQ",
/* 0x5F in string */	"LIST->STRING",
/* 0x60 in string */	"EQUAL-STRING-TO-LIST?",
/* 0x61 in prim */	"MAKE-CELL",
/* 0x62 in prim */	"CELL-CONTENTS",
/* 0x63 in prim */	"CELL?",
/* 0x64 in string */	"CHARACTER-UPCASE",
/* 0x65 in fasload */	"CHARACTER-LIST-HASH",
/* 0x66 in fixnum */	"GCD-FIXNUM",
/* 0x67 in bignum */	"COERCE-FIXNUM-TO-BIGNUM",
/* 0x68 in bignum */	"COERCE-BIGNUM-TO-FIXNUM",
/* 0x69 in flonum */	"PLUS-FLONUM",
/* 0x6A in flonum */	"MINUS-FLONUM",
/* 0x6B in flonum */	"MULTIPLY-FLONUM",
/* 0x6C in flonum */	"DIVIDE-FLONUM",
/* 0x6D in flonum */	"EQUAL-FLONUM?",
/* 0x6E in flonum */	"LESS-THAN-FLONUM?",
/* 0x6F in bignum */	"ZERO-BIGNUM?",
/* 0x70 in flonum */	"TRUNCATE-FLONUM",
/* 0x71 in flonum */	"ROUND-FLONUM",
/* 0x72 in flonum */	"COERCE-INTEGER-TO-FLONUM",
/* 0x73 in flonum */	"SINE-FLONUM",
/* 0x74 in flonum */	"COSINE-FLONUM",
/* 0x75 in flonum */	"ARCTAN-FLONUM",
/* 0x76 in flonum */	"EXP-FLONUM",
/* 0x77 in flonum */	"LN-FLONUM",

/* Primitive names continue on the next page */

/* Primitive names, continued */

/* 0x78 in flonum */	"SQRT-FLONUM",
/* 0x79 in nihil */	"PRIMITIVE-FASLOAD",
/* 0x7A in hooks */	"GET-FIXED-OBJECTS-VECTOR",
/* 0x7B in hooks */	"SET-FIXED-OBJECTS-VECTOR!",
/* 0x7C in vector */	"LIST->VECTOR",
/* 0x7D in vector */	"SUBVECTOR->LIST",
/* 0x7E in list */	"PAIR?",
/* 0x7F in fixnum */	"NEGATIVE-FIXNUM?",
/* 0x80 in bignum */	"NEGATIVE-BIGNUM?",
/* 0x81 in fixnum */	"GREATER-THAN-FIXNUM?",
/* 0x82 in bignum */	"GREATER-THAN-BIGNUM?",
/* 0x83 in string */	"STRING-HASH",
/* 0x84 in list */	"SYSTEM-PAIR-CONS",
/* 0x85 in list */	"SYSTEM-PAIR?",
/* 0x86 in list */	"SYSTEM-PAIR-CAR",
/* 0x87 in list */	"SYSTEM-PAIR-CDR",
/* 0x88 in list */	"SYSTEM-PAIR-SET-CAR!",
/* 0x89 in list */	"SYSTEM-PAIR-SET-CDR!",
/* 0x8A in daemon */	"INITIALIZE-OBJECT-HASH",
/* 0x8B in io */	"GET-CHARACTER-FROM-INPUT-CHANNEL-IMMEDIATE",
/* 0x8C in prim */	"SET-CELL-CONTENTS!",
/* 0x8D in prim */	"&MAKE-OBJECT",
/* 0x8E in hunk */	"SYSTEM-HUNK3-CXR0",
/* 0x8F in hunk */	"SYSTEM-HUNK3-SET-CXR0!",
/* 0x90 in prim */	"MAP-MACHINE-ADDRESS-TO-CODE",
/* 0x91 in hunk */	"SYSTEM-HUNK3-CXR1",
/* 0x92 in hunk */	"SYSTEM-HUNK3-SET-CXR1!",
/* 0x93 in prim */	"MAP-CODE-TO-MACHINE-ADDRESS",
/* 0x94 in hunk */	"SYSTEM-HUNK3-CXR2",
/* 0x95 in hunk */	"SYSTEM-HUNK3-SET-CXR2!",
/* 0x96 in prim */	"PRIMITIVE-PROCEDURE-ARITY",
/* 0x97 in vector */	"SYSTEM-LIST-TO-VECTOR",
/* 0x98 in vector */	"SYSTEM-SUBVECTOR-TO-LIST",
/* 0x99 in vector */	"SYSTEM-VECTOR?",
/* 0x9A in vector */	"SYSTEM-VECTOR-REF",
/* 0x9B in vector */	"SYSTEM-VECTOR-SET!",
/* 0x9C in hooks */	"WITH-HISTORY-DISABLED",

/* Primitive names continue on the next page */

/* Primitive names, continued */

/* 0x9D not here */	No_Name,
/* 0x9E not here */	No_Name,
/* 0x9F not here */	No_Name,
/* 0xA0 not here */	No_Name,
/* 0xA1 not here */	No_Name,
/* 0xA2 not here */	No_Name,
/* 0xA3 in string */	"VECTOR-8B-CONS",
/* 0xA4 in string */	"VECTOR-8B?",
/* 0xA5 in string */	"VECTOR-8B-REF",
/* 0xA6 in string */	"VECTOR-8B-SET!",
/* 0xA7 in flonum */	"ZERO-FLONUM?",
/* 0xA8 in flonum */	"POSITIVE-FLONUM?",
/* 0xA9 in flonum */	"NEGATIVE-FLONUM?",
/* 0xAA in flonum */	"GREATER-THAN-FLONUM?",
/* 0xAB in fasload */	"INTERN-CHARACTER-LIST",
/* 0xAC not here */	No_Name,
/* 0xAD in string */	"STRING-LENGTH",
/* 0xAE in vector */	"SYSTEMTEM-VECTOR-SIZE",
/* 0xAF in hooks */	"FORCE",
/* 0xB0 in prim */	"PRIMITIVE-DATUM",
/* 0xB1 in prim */	"MAKE-NON-POINTER-OBJECT",
/* 0xB2 in debug */	"DEBUGGING-PRINTER",
/* 0xB3 in string */	"STRING-UPCASE",
/* 0xB4 in gcloop */	"PRIMITIVE-PURIFY",
/* 0xB5 not here */	No_Name,
/* 0xB6 in nihil */	"COMPLETE-GARBAGE-COLLECT",
/* 0xB7 in fasdump */	"DUMP-BAND",
/* 0xB8 in string */	"SUBSTRING-SEARCH",
/* 0xB9 in fasload */	"LOAD-BAND",
/* 0xBA in gcloop */	"CONSTANT?",
/* 0xBB in gcloop */	"PURE?",
/* 0xBC in gcloop */	"PRIMITIVE-GC-TYPE",
/* 0xBD in gcloop */	"PRIMITIVE-IMPURIFY",
/* 0xBE in hooks */	"WITH-THREADED-CONTINUATION",
/* 0xBF in hooks */	"WITHIN-CONTROL-POINT",
/* 0xC0 in sysprim */	"SET-RUN-LIGHT!",
/* 0xC1 in fileio */	"FILE-EOF?",
/* 0xC2 in fileio */	"FILE-READ-CHAR",
/* 0xC3 in fileio */	"FILE-FILL-INPUT-BUFFER",
/* 0xC4 in fileio */	"FILE-LENGTH",
/* 0xC5 in fileio */	"FILE-WRITE-CHAR",
/* 0xC6 in fileio */	"FILE-WRITE-STRING",

/* Primitive names continue on the next page */

/* Primitive names, continued */

/* 0xC7 in daemon */	"CLOSE-LOST-OPEN-FILES",
/* 0xC8 in io */	"PUT-CHARACTER-TO-OUTPUT-CHANNEL",
/* 0xC9 in hooks */	"WITH-INTERRUPTS-REDUCED",
/* 0xCA in step */	"PRIMITIVE-EVAL-STEP",
/* 0xCB in step */	"PRIMITIVE-APPLY-STEP",
/* 0xCC in step */	"PRIMITIVE-RETURN-STEP",
/* 0xCD in console */	"TTY-READ-CHAR-READY?",
/* 0xCE in console */	"TTY-READ-CHAR",
/* 0xCF in console */	"TTY-READ-CHAR-IMMEDIATE",
/* 0xD0 in console */	"TTY-READ-FINISH",
/* 0xD1 in bitstr */	"BIT-STRING-ALLOCATE",
/* 0xD2 in bitstr */	"MAKE-BIT-STRING",
/* 0xD3 in bitstr */	"BIT-STRING?",
/* 0xD4 in bitstr */	"BIT-STRING-LENGTH",
/* 0xD5 in bitstr */	"BIT-STRING-REF",
/* 0xD6 in bitstr */	"BIT-SUBSTRING-MOVE-RIGHT!",
/* 0xD7 in bitstr */	"BIT-STRING-SET!",
/* 0xD8 in bitstr */	"BIT-STRING-CLEAR!",
/* 0xD9 in bitstr */	"BIT-STRING-ZERO?",
/* 0xDA not here */	No_Name,
/* 0xDB not here */	No_Name,
/* 0xDC in bitstr */	"UNSIGNED-INTEGER->BIT-STRING",
/* 0xDD in bitstr */	"BIT-STRING->UNSIGNED-INTEGER",
/* 0xDE not here */	No_Name,
/* 0xDF in bitstr */	"READ-BITS!",
/* 0xE0 in bitstr */	"WRITE-BITS!",
/* 0xE1 in hooks */	"MAKE-STATE-SPACE",
/* 0xE2 in hooks */	"EXECUTE-AT-NEW-POINT",
/* 0xE3 in hooks */	"TRANSLATE-TO-POINT",
/* 0xE4 in gcloop */	"GET-NEXT-CONSTANT",

/* Primitive names continue on the next page */

/* Primitive names, continued */

/* 0xE5 in boot */	"MICROCODE-IDENTIFY",
/* 0xE6 in generic */	"ZERO?",
/* 0xE7 in generic */	"POSITIVE?",
/* 0xE8 in generic */	"NEGATIVE?",
/* 0xE9 in generic */	"&=",
/* 0xEA in generic */	"&<",
/* 0xEB in generic */	"&>",
/* 0xEC in generic */	"&+",
/* 0xED in generic */	"&-",
/* 0xEE in generic */	"&*",
/* 0xEF in generic */	"&/",
/* 0xF0 in generic */	"INTEGER-DIVIDE",
/* 0xF1 in generic */	"1+",
/* 0xF2 in generic */	"-1+",
/* 0xF3 in generic */	"TRUNCATE",
/* 0xF4 in generic */	"ROUND",
/* 0xF5 in generic */	"FLOOR",
/* 0xF6 in generic */	"CEILING",
/* 0xF7 in generic */	"SQRT",
/* 0xF8 in generic */	"EXP",
/* 0xF9 in generic */	"LOG",
/* 0xFA in generic */	"SIN",
/* 0xFB in generic */	"COS",
/* 0xFC in generic */	"&ATAN",
/* 0xFD in console */	"TTY-WRITE-CHAR",
/* 0xFE in console */	"TTY-WRITE-STRING",
/* 0xFF in console */	"TTY-BEEP",
/* 0x100 in console */	"TTY-CLEAR",
/* 0x101 in extern */	"GET-EXTERNAL-COUNTS",
/* 0x102 in extern */	"GET-EXTERNAL-NAME",
/* 0x103 in extern */	"GET-EXTERNAL-NUMBER",
/* 0x104 not here */	No_Name,
/* 0x105 not here */	No_Name,
/* 0x106 in sysprim */	"GET-NEXT-INTERRUPT-CHARACTER",
/* 0x107 in sysprim */	"CHECK-AND-CLEAN-UP-INPUT-CHANNEL",
/* 0x108 not here */	No_Name,
/* 0x109 in sysprim */	"SYSTEM-CLOCK",
/* 0x10A in fileio */	"FILE-EXISTS?",
/* 0x10B not here */	No_Name,
/* 0x10C in ttyio */	"TTY-MOVE-CURSOR",
/* 0x10D not here */	No_Name,
/* 0x10E in nihil */	"CURRENT-DATE",
/* 0x10F in nihil */	"CURRENT-TIME",
/* 0x110 in nihil */	"TRANSLATE-FILE",
/* 0x111 in fileio */	"COPY-FILE",
/* 0x112 in fileio */	"RENAME-FILE",
/* 0x113 in fileio */	"REMOVE-FILE",
/* 0x114 in fileio */	"LINK-FILE",
/* 0x115 in fileio */	"MAKE-DIRECTORY",
/* 0x116 in nihil */	"VOLUME-NAME",
/* 0x117 in fileio */	"SET-WORKING-DIRECTORY-PATHNAME!",
/* 0x118 in nihil */	"OPEN-CATALOG",
/* 0x119 in nihil */	"CLOSE-CATALOG",
/* 0x11A in nihil */	"NEXT-FILE",
/* 0x11B in nihil */	"CAT-NAME",
/* 0x11C in nihil */	"CAT-KIND",
/* 0x11D in nihil */	"CAT-PSIZE",
/* 0x11E in nihil */	"CAT-LSIZE",
/* 0x11F in nihil */	"CAT-INFO",
/* 0x120 in nihil */	"CAT-BLOCK",
/* 0x121 in nihil */	"CAT-CREATE-DATE",
/* 0x122 in nihil */	"CAT-CREATE-TIME",
/* 0x123 in nihil */	"CAT-LAST-DATE",
/* 0x124 in nihil */	"CAT-LAST-TIME",
/* 0x125 in nihil */	"ERROR-MESSAGE",
/* 0x126 in sysprim */	"CURRENT-YEAR",
/* 0x127 in sysprim */	"CURRENT-MONTH",
/* 0x128 in sysprim */	"CURRENT-DAY",
/* 0x129 in sysprim */	"CURRENT-HOUR",
/* 0x12A in sysprim */	"CURRENT-MINUTE",
/* 0x12B in sysprim */	"CURRENT-SECOND",
/* 0x12C in nihil */	"INIT-FLOPPY",
/* 0x12D in nihil */	"ZERO-FLOPPY",
/* 0x12E in nihil */	"PACK-VOLUME",
/* 0x12F in nihil */	"LOAD-PICTURE",
/* 0x130 in nihil */	"STORE-PICTURE",
/* 0x131 in nihil */	"LOOKUP-SYSTEM-SYMBOL",

/* Unix specialized primitives start here */

/* 0x132 not here */	No_Name,
/* 0x133 not here */	No_Name,
/* 0x134 in ttyio */	"CLEAR-TO-END-OF-LINE",
/* 0x135 not here */	No_Name,
/* 0x136 not here */	No_Name,
/* 0x137 in hooks */	"WITH-INTERRUPT-MASK",

/* 0x138 in stringprim */ "STRING?",
/* 0x139 in stringprim */ "STRING-LENGTH",
/* 0x13A in stringprim */ "STRING-REF",
/* 0x13B in stringprim */ "STRING-SET!",
/* 0x13C in stringprim */ "SUBSTRING-MOVE-RIGHT!",
/* 0x13D in stringprim */ "SUBSTRING-MOVE-LEFT!",
/* 0x13E in stringprim */ "STRING-ALLOCATE",
/* 0x13F in stringprim */ "STRING-MAXIMUM-LENGTH",
/* 0x140 in stringprim */ "SET-STRING-LENGTH!",
/* 0x141 in stringprim */ "VECTOR-8B-FILL!",
/* 0x142 in stringprim */ "VECTOR-8B-FIND-NEXT-CHAR",
/* 0x143 in stringprim */ "VECTOR-8B-FIND-PREVIOUS-CHAR",
/* 0x144 in stringprim */ "VECTOR-8B-FIND-NEXT-CHAR-CI",
/* 0x145 in stringprim */ "VECTOR-8B-FIND-PREVIOUS-CHAR-CI",
/* 0x146 in stringprim */ "SUBSTRING-FIND-NEXT-CHAR-IN-SET",
/* 0x147 in stringprim */ "SUBSTRING-FIND-PREVIOUS-CHAR-IN-SET",
/* 0x148 in stringprim */ "SUBSTRING=?",
/* 0x149 in stringprim */ "SUBSTRING-CI=?",
/* 0x14A in stringprim */ "SUBSTRING<?",
/* 0x14B in stringprim */ "SUBSTRING-UPCASE!",
/* 0x14C in stringprim */ "SUBSTRING-DOWNCASE!",
/* 0x14D in stringprim */ "SUBSTRING-MATCH-FORWARD",
/* 0x14E in stringprim */ "SUBSTRING-MATCH-BACKWARD",
/* 0x14F in stringprim */ "SUBSTRING-MATCH-FORWARD-CI",
/* 0x150 in stringprim */ "SUBSTRING-MATCH-BACKWARD-CI",
/* 0x151 in fileio */     "PHOTO-OPEN",
/* 0x152 in fileio */     "PHOTO-CLOSE",
/* 0x153 in sysprim */    "SETUP-TIMER-INTERRUPT",
/* 0x154 in nihil */      No_Name,
/* 0x155 in nihil */      No_Name,
/* 0x156 in nihil */      No_Name,
/* 0x157 in nihil */      No_Name,
/* 0x158 in nihil */      No_Name,
/* 0x159 in nihil */      No_Name,
/* 0x15A in nihil */      No_Name,
/* 0x15B in nihil */      No_Name,
/* 0x15C in nihil */      No_Name,
/* 0x15D in nihil */      No_Name,
/* 0x15E in nihil */      No_Name,
/* 0x15F in nihil */      No_Name,
/* 0x160 in nihil */      No_Name,
/* 0x161 in nihil */      "EXTRACT-NON-MARKED-VECTOR",
/* 0x162 in nihil */      "UNSNAP-LINKS!",
/* 0x163 in nihil */      "SAFE-PRIMITIVE?",
/* 0x164 in nihil */      "SUBSTRING-READ",
/* 0x165 in nihil */      "SUBSTRING-WRITE",
/* 0x166 in ttyio */      "SCREEN-X-SIZE",
/* 0x167 in ttyio */      "SCREEN-Y-SIZE",
/* 0x168 in nihil */      "SCREEN-WRITE-CURSOR",
/* 0x169 in nihil */      "SCREEN-WRITE-CHARACTER",
/* 0x16A in nihil */      "SCREEN-WRITE-SUBSTRING",
/* 0x16B in nihil */      "NEXT-FILE-MATCHING",
/* 0x16C in nihil */      No_Name,
/* 0x16D in nihil */      "TTY-WRITE-BYTE",
/* 0x16E in nihil */      "FILE-READ-BYTE",
/* 0x16F in nihil */      "FILE-WRITE-BYTE",
/* 0x170 not here */      No_Name, /* "SAVE-SCREEN", */
/* 0x171 not here */      No_Name, /* "RESTORE-SCREEN!", */
/* 0x172 not here */      No_Name, /* "SUBSCREEN-CLEAR!", */
/* 0x173 not here */      No_Name, /* "&GCD", */
/* 0x174 not here */      No_Name, /* "TTY-REDRAW-SCREEN", */
/* 0x175 not here */      No_Name, /* "SCREEN-INVERSE-VIDEO", */
/* 0x176 in nihil */      "STRING->SYNTAX-ENTRY",
/* 0x177 in scanprim */   "SCAN-WORD-FORWARD",
/* 0x178 in scanprim */   "SCAN-WORD-BACKWARD",
/* 0x179 in scanprim */   "SCAN-LIST-FORWARD",
/* 0x17A in scanprim */   "SCAN-LIST-BACKWARD",
/* 0x17B in scanprim */   "SCAN-SEXPS-FORWARD",
/* 0x17C in scanprim */   "SCAN-FORWARD-TO-WORD",
/* 0x17D in scanprim */   "SCAN-BACKWARD-PREFIX-CHARS",
/* 0x17E in scanprim */   "CHAR->SYNTAX-CODE",
/* 0x17F in scanprim */   "QUOTED-CHAR?",
/* 0x180 in boot */      "MICROCODE-TABLES-FILENAME",
/* 0x181 not here */      No_Name,
/* 0x182 not here */      No_Name, /* "FIND-PASCAL-PROGRAM", */
/* 0x183 not here */      No_Name, /* "EXECUTE-PASCAL-PROGRAM", */
/* 0x184 not here */      No_Name, /* "GRAPHICS-MOVE", */
/* 0x185 not here */      No_Name, /* "GRAPHICS-LINE", */
/* 0x186 not here */      No_Name, /* "GRAPHICS-PIXEL", */
/* 0x187 not here */      No_Name, /* "GRAPHICS-SET-DRAWING-MODE", */
/* 0x188 not here */      No_Name, /* "ALPHA-RASTER?", */
/* 0x189 not here */      No_Name, /* "TOGGLE-ALPHA-RASTER", */
/* 0x18A not here */      No_Name, /* "GRAPHICS-RASTER?", */
/* 0x18B not here */      No_Name, /* "TOGGLE-GRAPHICS-RASTER", */
/* 0x18C not here */      No_Name, /* "GRAPHICS-CLEAR", */
/* 0x18D not here */      No_Name, /* "GRAPHICS-SET-LINE-STYLE", */
/* 0x18E in hooks */      "ERROR-PROCEDURE",
/* 0x18F in nihil */      "VOLUME-EXISTS?",
/* 0x190 in nihil */      "RE-CHAR-SET-ADJOIN!",
/* 0x191 in nihil */      "RE-COMPILE-FASTMAP",
/* 0x192 in nihil */      "RE-MATCH",
/* 0x193 in nihil */      "RE-SEARCH-FORWARD",
/* 0x194 in nihil */      "RE-SEARCH-BACKWARD",
/* 0x195 in nihil */      "SYSTEM-MEMORY-REF",
/* 0x196 in nihil */      "SYSTEM-MEMORY-SET!",
/* 0x197 in bitstr */     "BIT-STRING-FILL!",
/* 0x198 in bitstr */     "BIT-STRING-MOVE!",
/* 0x199 in bitstr */     "BIT-STRING-MOVEC!",
/* 0x19A in bitstr */     "BIT-STRING-OR!",
/* 0x19B in bitstr */     "BIT-STRING-AND!",
/* 0x19C in bitstr */     "BIT-STRING-ANDC!",
/* 0x19D in bitstr */     "BIT-STRING=?",
/* 0x19E in fileio */     "WORKING-DIRECTORY-PATHNAME",
/* 0x19F in fileio */     "OPEN-DIRECTORY",
/* 0x1A0 in fileio */     "DIRECTORY-READ",
/* 0x1A1 in sysprim */    "UNDER-EMACS?",
/* 0x1A2 in ttyio */      "TTY-FLUSH-OUTPUT"
};

#if (MAX_PRIMITIVE_NUMBER != 0x1A2)
/* Cause an error */
#include "Error: prims.h and storage.c are inconsistent -- Names Table"
#endif

/* After passing all above checks */

long MAX_PRIMITIVE = MAX_PRIMITIVE_NUMBER;

char *Return_Names[] = {
/* 0x00 */		"END_OF_COMPUTATION",
/* 0x01 */		"JOIN_STACKLETS",
/* 0x02 */		"RESTORE_CONTINUATION",
/* 0x03 */		"INTERNAL_APPLY",
/* 0x04 */		"BAD_INTERRUPT_CONTINUE",
/* 0x05 */		"RESTORE_HISTORY",
/* 0x06 */		"INVOKE_STACK_THREAD",
/* 0x07 */		"RESTART_EXECUTION",
/* 0x08 */		"EXECUTE_ASSIGNMENT_FINISH",
/* 0x09 */		"EXECUTE_DEFINITION_FINISH",
/* 0x0A */		"EXECUTE_ACCESS_FINISH",
/* 0x0b */		"EXECUTE_IN_PACKAGE_CONTINUE",
/* 0x0C */		"SEQ_2_DO_2",
/* 0x0d */		"SEQ_3_DO_2",
/* 0x0E */		"SEQ_3_DO_3",
/* 0x0f */		"CONDITIONAL_DECIDE",
/* 0x10 */		"DISJUNCTION_DECIDE",
/* 0x11 */		"COMB_1_PROCEDURE",
/* 0x12 */		"COMB_APPLY_FUNCTION",
/* 0x13 */		"COMB_2_FIRST_OPERAND",
/* 0x14 */		"COMB_2_PROCEDURE",
/* 0x15 */		"COMB_SAVE_VALUE",
/* 0x16 */		"PCOMB1_APPLY",
/* 0x17 */		"PCOMB2_DO_1",
/* 0x18 */		"PCOMB2_APPLY",
/* 0x19 */		"PCOMB3_DO_2",
/* 0x1A */		"PCOMB3_DO_1",
/* 0x1B */		"PCOMB3_APPLY",
/* 0x1C */		"SNAP_NEED_THUNK",
/* 0x1D */		No_Name,
/* 0x1E */		No_Name,
/* 0x1F */		No_Name,
/* 0x20 */		"NORMAL_GC_DONE",
/* 0x21 */		"COMPLETE_GC_DONE",
/* 0x22 */		"PURIFY_GC_1",
/* 0x23 */		"PURIFY_GC_2",
/* 0x24 */		"AFTER_MEMORY_UPDATE",
/* 0x25 */		"RESTARTABLE_EXIT",
/* 0x26 */		No_Name,
/* 0x27 */		No_Name,

/* 0x28 */		No_Name,
/* 0x29 */		No_Name,
/* 0x2A */		"RETURN_TRAP_POINT",
/* 0x2B */		"RESTORE_STEPPER",
/* 0x2C */		"RESTORE_TO_STATE_POINT",
/* 0x2D */		"MOVE_TO_ADJACENT_POINT",
/* 0x2E */		"RESTORE_VALUE",
/* 0x2F */		"RESTORE_DONT_COPY_HISTORY",
/* 0x30 */		No_Name,
/* 0x31 */		No_Name,
/* 0x32 */		No_Name,
/* 0x33 */		No_Name,
/* 0x34 */		No_Name,
/* 0x35 */		No_Name,
/* 0x36 */		No_Name,
/* 0x37 */		No_Name,
/* 0x38 */		No_Name,
/* 0x39 */		No_Name,
/* 0x3A */		No_Name,
/* 0x3B */		No_Name,
/* 0x3C */		No_Name,
/* 0x3D */		No_Name,
/* 0x3E */		No_Name,
/* 0x3F */		No_Name,
/* 0x40 */		"POP_RETURN_ERROR",
/* 0x41 */		"EVAL_ERROR",
/* 0x42 */		"REPEAT_PRIMITIVE",
/* 0x43 */		"COMPILER_INTERRUPT_RESTART",
/* 0x44 */		No_Name,
/* 0x45 */		"RESTORE_INT_MASK",
/* 0x46 */		"HALT",
/* 0x47 */		"FINISH_GLOBAL_INT",
/* 0x48 */		"REPEAT_DISPATCH",
/* 0x49 */		"GC_CHECK",
/* 0x4A */		"RESTORE_FLUIDS",
/* 0x4B */		"COMPILER_LOOKUP_APPLY_RESTART",
/* 0x4C */		"COMPILER_ACCESS_RESTART",
/* 0x4D */		"COMPILER_UNASSIGNED_P_RESTART",
/* 0x4E */		"COMPILER_UNBOUND_P_RESTART",
/* 0x4F */		"COMPILER_DEFINITION_RESTART",
/* 0x50 */		"COMPILER_LEXPR_GC_RESTART"
};

#if (MAX_RETURN_CODE != 0x50)
/* Cause an error */
#include "Returns.h and storage.c are inconsistent -- Names Table"
#endif

long MAX_RETURN = MAX_RETURN_CODE;

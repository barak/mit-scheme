/* -*-C-*-

Copyright (c) 1987, 1988 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/sysprim.c,v 9.30 1988/08/15 20:56:21 cph Exp $
 *
 * Random system primitives.  Most are implemented in terms of
 * utilities in os.c
 *
 */
#include "scheme.h"
#include "prims.h"

/* Interrupt primitives */

DEFINE_PRIMITIVE ("CHECK-AND-CLEAN-UP-INPUT-CHANNEL", Prim_chk_and_cln_input_channel, 2, 2, 0)
{
  extern Boolean OS_Clean_Interrupt_Channel();
  PRIMITIVE_HEADER (2);

  PRIMITIVE_RETURN
    ((OS_Clean_Interrupt_Channel ((arg_nonnegative_integer (1)),
				  (arg_nonnegative_integer (2))))
     ? SHARP_T : NIL);
}

DEFINE_PRIMITIVE ("GET-NEXT-INTERRUPT-CHARACTER", Prim_get_next_interrupt_char, 0, 0, 0)
{
  int result;
  extern int OS_Get_Next_Interrupt_Character();
  PRIMITIVE_HEADER (0);

  result = (OS_Get_Next_Interrupt_Character ());
  if (result == -1)
    {
      Primitive_Error (ERR_EXTERNAL_RETURN);
      /*NOTREACHED*/
    }
  CLEAR_INTERRUPT (INT_Character);
  PRIMITIVE_RETURN (MAKE_UNSIGNED_FIXNUM (result));
}

/* Time primitives */

DEFINE_PRIMITIVE ("SYSTEM-CLOCK", Prim_system_clock, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);

  PRIMITIVE_RETURN (C_Integer_To_Scheme_Integer (OS_process_clock ()));
}

DEFINE_PRIMITIVE ("REAL-TIME-CLOCK", Prim_real_time_clock, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);

  PRIMITIVE_RETURN (C_Integer_To_Scheme_Integer (OS_real_time_clock ()));
}

DEFINE_PRIMITIVE ("SETUP-TIMER-INTERRUPT", Prim_setup_timer_interrupt, 2, 2, 0)
{
  extern void Clear_Int_Timer(), Set_Int_Timer();
  Primitive_2_Args();

  if ((Arg1 == NIL) && (Arg2==NIL))
    Clear_Int_Timer();
  else
  {
    long Days, Centi_Seconds;

    Arg_1_Type(TC_FIXNUM);
    Arg_2_Type(TC_FIXNUM);
    Sign_Extend(Arg1, Days);
    Sign_Extend(Arg2, Centi_Seconds);
    Set_Int_Timer(Days, Centi_Seconds);
  }
  CLEAR_INTERRUPT(INT_Timer);
  PRIMITIVE_RETURN(NIL);
}

/* Date and current time primitives */

#define Date_Primitive(OS_Name)						\
  int result;								\
  extern int OS_Name();							\
  PRIMITIVE_HEADER (0);							\
									\
  result = (OS_Name ());						\
  PRIMITIVE_RETURN ((result == -1) ? NIL : (MAKE_UNSIGNED_FIXNUM (result)))

DEFINE_PRIMITIVE ("CURRENT-YEAR", Prim_current_year, 0, 0, 0)
{ Date_Primitive (OS_Current_Year); }

DEFINE_PRIMITIVE ("CURRENT-MONTH", Prim_current_month, 0, 0, 0)
{ Date_Primitive (OS_Current_Month); }

DEFINE_PRIMITIVE ("CURRENT-DAY", Prim_current_day, 0, 0, 0)
{ Date_Primitive (OS_Current_Day); }

DEFINE_PRIMITIVE ("CURRENT-HOUR", Prim_current_hour, 0, 0, 0)
{ Date_Primitive (OS_Current_Hour); }

DEFINE_PRIMITIVE ("CURRENT-MINUTE", Prim_current_minute, 0, 0, 0)
{ Date_Primitive (OS_Current_Minute); }

DEFINE_PRIMITIVE ("CURRENT-SECOND", Prim_current_second, 0, 0, 0)
{ Date_Primitive (OS_Current_Second); }

/* Pretty random primitives */

/* (EXIT)
   Halt SCHEME, with no intention of restarting. */

DEFINE_PRIMITIVE ("EXIT", Prim_non_restartable_exit, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);

  Microcode_Termination (TERM_HALT);
}

/* (HALT)
   Halt Scheme in such a way that it can be restarted.
   Not all operating systems support this. */

DEFINE_PRIMITIVE ("HALT", Prim_restartable_exit, 0, 0, 0)
{
  extern Boolean Restartable_Exit();
  PRIMITIVE_HEADER (0);

  PRIMITIVE_RETURN (((Restartable_Exit ()) ? SHARP_T : NIL));
}

/* (SET-RUN-LIGHT! OBJECT)
   On the HP Pascal workstation system, it allows the character
   displayed in the lower right-hand part of the screen to be changed.
   In CScheme, rings the bell.
   Used by various things to indicate the state of the system. */

DEFINE_PRIMITIVE ("SET-RUN-LIGHT!", Prim_set_run_light, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

#ifdef RUN_LIGHT_IS_BEEP
  {
    extern void OS_tty_beep();

    OS_tty_beep();
    OS_Flush_Output_Buffer();
    PRIMITIVE_RETURN (SHARP_T);
  }
#else
  PRIMITIVE_RETURN (NIL);
#endif
}

DEFINE_PRIMITIVE ("UNDER-EMACS?", Prim_under_emacs_p, 0, 0, 0)
{
  extern Boolean OS_Under_Emacs();
  PRIMITIVE_HEADER (0);

  PRIMITIVE_RETURN (((OS_Under_Emacs ()) ? SHARP_T : NIL));
}

#define CONVERT_ADDRESS(address)					\
  (C_Integer_To_Scheme_Integer ((long) (C_To_Scheme (address))))

DEFINE_PRIMITIVE ("GC-SPACE-STATUS", Prim_gc_space_status, 0, 0, 0)
{
  Pointer * constant_low;
  Pointer * constant_free;
  Pointer * constant_high;
  Pointer * heap_low;
  Pointer * heap_free;
  Pointer * heap_limit;
  Pointer * heap_high;
#ifndef USE_STACKLETS
  Pointer * stack_low;
  Pointer * stack_free;
  Pointer * stack_limit;
  Pointer * stack_high;
#endif /* USE_STACKLETS */
  Pointer result;
  PRIMITIVE_HEADER (0);

  constant_low = Constant_Space;
  constant_free = Free_Constant;
  constant_high = Constant_Top;
  heap_low = Heap_Bottom;
  heap_free = Free;
  heap_limit = MemTop;
  heap_high = Heap_Top;
#ifndef USE_STACKLETS
  stack_low = Absolute_Stack_Base;
  stack_free = Stack_Pointer;
  stack_limit = Stack_Guard;
  stack_high = Stack_Top;
#endif /* USE_STACKLETS */

  result = (make_vector (12, NIL));
  User_Vector_Set (result, 0, (MAKE_UNSIGNED_FIXNUM (sizeof (Pointer))));
  User_Vector_Set (result, 1, (CONVERT_ADDRESS (constant_low)));
  User_Vector_Set (result, 2, (CONVERT_ADDRESS (constant_free)));
  User_Vector_Set (result, 3, (CONVERT_ADDRESS (constant_high)));
  User_Vector_Set (result, 4, (CONVERT_ADDRESS (heap_low)));
  User_Vector_Set (result, 5, (CONVERT_ADDRESS (heap_free)));
  User_Vector_Set (result, 6, (CONVERT_ADDRESS (heap_limit)));
  User_Vector_Set (result, 7, (CONVERT_ADDRESS (heap_high)));
#ifndef USE_STACKLETS
  User_Vector_Set (result, 8, (CONVERT_ADDRESS (stack_low)));
  User_Vector_Set (result, 9, (CONVERT_ADDRESS (stack_free)));
  User_Vector_Set (result, 10, (CONVERT_ADDRESS (stack_limit)));
  User_Vector_Set (result, 11, (CONVERT_ADDRESS (stack_high)));
#endif /* USE_STACKLETS */
  PRIMITIVE_RETURN (result);
}

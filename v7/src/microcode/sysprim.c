/* -*-C-*-

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
MIT in each case. */

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/sysprim.c,v 9.23 1987/10/08 17:04:49 jinx Exp $
 *
 * Random system primitives.  Most are implemented in terms of
 * utilities in os.c
 *
 */
#include "scheme.h"
#include "primitive.h"

/* Interrupt primitives */

Built_In_Primitive(Prim_Chk_And_Cln_Input_Channel, 2,
		 "CHECK-AND-CLEAN-UP-INPUT-CHANNEL", 0x107)
{
  extern Boolean OS_Clean_Interrupt_Channel();
  Primitive_2_Args();

  return (OS_Clean_Interrupt_Channel(Get_Integer(Arg1),
				     Get_Integer(Arg2)) ?
	  TRUTH : NIL);
}

Built_In_Primitive(Prim_Get_Next_Interrupt_Char, 0,
		   "GET-NEXT-INTERRUPT-CHARACTER", 0x106)
{
  int result;
  extern int OS_Get_Next_Interrupt_Character();
  Primitive_0_Args();

  result = OS_Get_Next_Interrupt_Character();
  if (result == -1)
  {
    Primitive_Error(ERR_EXTERNAL_RETURN);
    /*NOTREACHED*/
  }
  IntCode &= ~INT_Character;
  return Make_Unsigned_Fixnum(result);
}

/* Time primitives */

Built_In_Primitive(Prim_System_Clock, 0, "SYSTEM-CLOCK", 0x109)
{
  Primitive_0_Args();

  return Make_Unsigned_Fixnum(System_Clock());
}

Built_In_Primitive(Prim_Setup_Timer_Interrupt, 2,
		   "SETUP-TIMER-INTERRUPT", 0x153)
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
  IntCode &= ~INT_Timer;
  return NIL;
}

/* Date and current time primitives */

#define Date_Primitive(OS_Name)						\
{									\
  int result;								\
  extern int OS_Name();							\
  Primitive_0_Args();							\
									\
  result = OS_Name();							\
  if (result == -1)							\
    return NIL;								\
  return Make_Unsigned_Fixnum(result);					\
}

Built_In_Primitive(Prim_Current_Year, 0, "CURRENT-YEAR", 0x126)
Date_Primitive(OS_Current_Year)

Built_In_Primitive(Prim_Current_Month, 0, "CURRENT-MONTH", 0x127)
Date_Primitive(OS_Current_Month)

Built_In_Primitive(Prim_Current_Day, 0, "CURRENT-DAY", 0x128)
Date_Primitive(OS_Current_Day)

Built_In_Primitive(Prim_Current_Hour, 0, "CURRENT-HOUR", 0x129)
Date_Primitive(OS_Current_Hour)

Built_In_Primitive(Prim_Current_Minute, 0, "CURRENT-MINUTE", 0x12A)
Date_Primitive(OS_Current_Minute)

Built_In_Primitive(Prim_Current_Second, 0, "CURRENT-SECOND", 0x12B)
Date_Primitive(OS_Current_Second)

/* Pretty random primitives */

/* (EXIT)
   Halt SCHEME, with no intention of restarting.
*/

Built_In_Primitive(Prim_Non_Restartable_Exit, 0, "EXIT", 0x16)
{
  Primitive_0_Args();

  Microcode_Termination(TERM_HALT);
}

/* (HALT)
   Halt Scheme in such a way that it can be restarted.
   Not all operating systems support this.
*/
Built_In_Primitive(Prim_Restartable_Exit, 0, "HALT", 0x1A)
{
  extern Boolean Restartable_Exit();
  Primitive_0_Args();

  return ((Restartable_Exit() ? TRUTH : NIL));
}

/* (SET-RUN-LIGHT! OBJECT)
   On the HP Pascal workstation system, it allows the character
   displayed in the lower right-hand part of the screen to be changed.
   In CScheme, rings the bell.
   Used by various things to indicate the state of the system.
*/

Built_In_Primitive(Prim_Set_Run_Light, 1, "SET-RUN-LIGHT!", 0xC0)
{
  Primitive_1_Arg();
#ifdef RUN_LIGHT_IS_BEEP
  extern void OS_tty_beep();

  OS_tty_beep();
  OS_Flush_Output_Buffer();
  return TRUTH;
#else
  return NIL;
#endif
}

Built_In_Primitive( Prim_under_emacs_p, 0, "UNDER-EMACS?", 0x1A1)
{
  extern Boolean OS_Under_Emacs();
  Primitive_0_Args();

  return (OS_Under_Emacs() ? TRUTH : NIL);
}

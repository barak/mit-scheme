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

/* File: sysprim.c
 *
 * Random system primitives.  Most are implemented in terms of
 * utilities in os.c
 *
 */
#include "scheme.h"
#include "primitive.h"

/* Interrupt primitives */

Built_In_Primitive(Prim_Chk_And_Cln_Input_Channel, 2,
		 "CHECK-AND-CLEAN-UP-INPUT-CHANNEL")
{ extern Boolean OS_Clean_Interrupt_Channel();
  Primitive_2_Args();

  return (OS_Clean_Interrupt_Channel(Get_Integer(Arg1),
				     Get_Integer(Arg2)) ?
	  TRUTH : NIL);
}

Built_In_Primitive(Prim_Get_Next_Interrupt_Char, 0,
		   "GET-NEXT-INTERRUPT-CHARACTER")
{ int result;
  extern int OS_Get_Next_Interrupt_Character();
  Primitive_0_Args();

  result = OS_Get_Next_Interrupt_Character();
  if (result == -1)
  { Primitive_Error(ERR_EXTERNAL_RETURN);
    /*NOTREACHED*/
  }
  IntCode &= ~INT_Character;
  return Make_Unsigned_Fixnum(result);
}

/* Time primitives */

Built_In_Primitive(Prim_System_Clock, 0, "SYSTEM-CLOCK")
{ Primitive_0_Args();
  return FIXNUM_0 + System_Clock();
}

Built_In_Primitive(Prim_Setup_Timer_Interrupt, 2, "SETUP-TIMER-INTERRUPT")
{ Primitive_2_Args();
  if ((Arg1 == NIL) && (Arg2==NIL)) Clear_Int_Timer();
  else
  { long Days, Centi_Seconds;
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

#define Date_Primitive(Prim_Name, OS_Name, S_Name)	\
Built_In_Primitive(Prim_Name, 0, S_Name)		\
{ int result;						\
  extern int OS_Name();					\
							\
  result = OS_Name();					\
  if (result == -1) return NIL;				\
  return Make_Unsigned_Fixnum(result);			\
}

Date_Primitive(Prim_Current_Year, OS_Current_Year, "YEAR");
Date_Primitive(Prim_Current_Month, OS_Current_Month, "MONTH");
Date_Primitive(Prim_Current_Day, OS_Current_Day, "DAY");
Date_Primitive(Prim_Current_Hour, OS_Current_Hour, "HOUR");
Date_Primitive(Prim_Current_Minute, OS_Current_Minute, "MINUTE");
Date_Primitive(Prim_Current_Second, OS_Current_Second, "SECOND");

/* Truly random primitives */

/* (NON-RESTARTABLE-EXIT)
      [Primitive number 0x16]
      Halt SCHEME, with no intention of restarting.
*/

Built_In_Primitive(Prim_Non_Restartable_Exit, 0, "NON-RESTARTABLE-EXIT")
{ Primitive_0_Args();
  Microcode_Termination(TERM_HALT);
}

Built_In_Primitive(Prim_Restartable_Exit, 0, "RESTARTABLE-EXIT")
{ extern Boolean Restartable_Exit();
  Primitive_0_Args();

  Restartable_Exit();
  return (Restartable_Exit() ? TRUTH : NIL);
}

/* (SET_RUN_LIGHT OBJECT)
      [Primitive number 0xC0]
      On the HP9836, allows the character displayed in the lower
      right-hand part of the screen to be changed. In CScheme, rings
      the bell.  Used only by GC to indicate that it has started and
      ended.
*/
Built_In_Primitive(Prim_Set_Run_Light, 1, "SET-RUN-LIGHT!")
{ Primitive_1_Arg();
#ifdef RUN_LIGHT_IS_BEEP
  extern void OS_tty_beep();

  OS_tty_beep();
  OS_Flush_Output_Buffer();
  return TRUTH;
#else
  return NIL;
#endif
}

Built_In_Primitive( Prim_under_emacs_p, 0, "UNDER-EMACS?")
{ extern Boolean OS_Under_Emacs();
  Primitive_0_Args();

  return (OS_Under_Emacs() ? TRUTH : NIL);
}

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/fhooks.c,v 9.21 1987/01/22 14:24:45 jinx Exp $
 *
 * This file contains hooks and handles for the new fluid bindings
 * scheme for multiprocessors.
 */

#include "scheme.h"
#include "primitive.h"
#include "locks.h"

/* (SET-FLUID-BINDINGS! NEW-BINDINGS)
      Sets the microcode fluid-bindings variable.  Returns the previous value.
*/
Define_Primitive(Prim_Set_Fluid_Bindings, 1, "SET-FLUID-BINDINGS!")
{ Pointer Result;
  Primitive_1_Arg();
  if (Arg1 != NIL) Arg_1_Type(TC_LIST);
  Result = Fluid_Bindings;
  Fluid_Bindings = Arg1;
  return Result;
}

/* (GET-FLUID-BINDINGS NEW-BINDINGS)
      Gets the microcode fluid-bindings variable.
*/
Define_Primitive(Prim_Get_Fluid_Bindings, 0, "GET-FLUID-BINDINGS")
{ Primitive_0_Args();
  return Fluid_Bindings;
}

/* (WITH-SAVED-FLUID-BINDINGS THUNK)
      Executes THUNK, then restores the previous fluid bindings.
*/
Define_Primitive(Prim_With_Saved_Fluid_Bindings,1,"WITH-SAVED-FLUID-BINDINGS")
{ Primitive_1_Arg();
  Pop_Primitive_Frame(1);
  /* Save previous fluid bindings for later restore */
 Will_Push(CONTINUATION_SIZE + STACK_ENV_EXTRA_SLOTS + 1);
  Store_Expression(Fluid_Bindings);
  Store_Return(RC_RESTORE_FLUIDS);
  Save_Cont();
  Push(Arg1);
  Push(STACK_FRAME_HEADER);
 Pushed();
  longjmp(*Back_To_Eval, PRIM_APPLY);
}

/* (ADD-FLUID-BINDING!  ENVIRONMENT SYMBOL-OR-VARIABLE VALUE)
      Looks up symbol-or-variable in environment.  If it has not been
      fluidized, fluidizes it.  A fluid binding with the specified 
      value is created in this interpreter's fluid bindings.      
*/
Define_Primitive(Prim_Add_Fluid_Binding, 3, "ADD-FLUID-BINDING!")
{ Pointer Trap_Obj;
  int Temp_Obj;

  Primitive_3_Args();
  if (Arg1 != GLOBAL_ENV) Arg_1_Type(TC_ENVIRONMENT);
  switch (Type_Code(Arg2))
  { case TC_VARIABLE:
      Temp_Obj = Lookup_Slot(Arg2, Arg1);
      if (Temp_Obj == NO_SLOT || Temp_Obj == FOUND_UNBOUND)
	Primitive_Error(ERR_UNBOUND_VARIABLE);
      break;
    case TC_INTERNED_SYMBOL:
    case TC_UNINTERNED_SYMBOL:
      Temp_Obj = Symbol_Lookup_Slot(Arg1, Arg2);
      if (Temp_Obj == NO_SLOT || Temp_Obj == FOUND_UNBOUND)
	Primitive_Error(ERR_UNBOUND_VARIABLE);
      break;
    default:
      Primitive_Error(ERR_ARG_2_WRONG_TYPE);
    }  
  /* Lock region, check if the slot at Lookup_Base[Lookup_Offset] is
   a fluid already.  Return it if so, make a new fluid and store it
   there if not, unlock the region. */
  {
#ifdef COMPILE_FUTURES
    Lock_Handle Set_Serializer;
#endif
    Pointer Found_Val, Safe_Val;
    if (Lookup_Offset == HEAP_ENV_FUNCTION) Primitive_Error(ERR_BAD_SET);
#ifdef COMPILE_FUTURES
    Set_Serializer = Lock_Cell(Nth_Vector_Loc(Lookup_Base, Lookup_Offset));
#endif
    Found_Val = Fast_Vector_Ref(Lookup_Base, Lookup_Offset);
    Safe_Val = Found_Val & ~DANGER_BIT;
    if (Type_Code(Safe_Val) == TC_TRAP)	Trap_Obj = Found_Val;
    else
    { Primitive_GC_If_Needed(TRAP_SIZE);
      Trap_Obj = (Pointer) Free;
      *Free++ = NIL;		/* Tag for fluids */
      *Free++ = Safe_Val;
      *Free++ = Arg2;	        /* For debugging purposes */
      Store_Type_Code(Trap_Obj,
		      ((Found_Val==Safe_Val)?TC_TRAP:TC_TRAP|DANGER_TYPE));
      Fast_Vector_Set(Lookup_Base, Lookup_Offset, Trap_Obj);
    }
#ifdef COMPILE_FUTURES
    Unlock_Cell(Set_Serializer);
#endif
    Add_Fluid_Binding(Trap_Obj, Arg3);
    Val = NIL;
    return Val;
  }
}

/* (MAKE-FLUID-BINDING!  ENVIRONMENT SYMBOL-OR-VARIABLE VALUE)
      Looks up symbol-or-variable in environment.  If it has not been
      fluidized, fluidizes it.  A fluid binding with the specified 
      value is created in this interpreter's fluid bindings.  Unlike
      ADD-FLUID-BINDING!, it is not an error to discover no binding
      for this variable; a fluid binding will be made anyway.  This is
      simple in the global case, since there is always a value slot
      available in the symbol itself.  If the last frame searched
      in the environment chain is closed (does not have a parent
      and does not allow search of the global environment), an AUX
      binding must be established in the last frame.
*/
Define_Primitive(Prim_Make_Fluid_Binding, 3, "MAKE-FLUID-BINDING!")
{ Pointer Trap_Obj;

  Primitive_3_Args();
  if (Arg1 != GLOBAL_ENV) Arg_1_Type(TC_ENVIRONMENT);
  switch (Type_Code(Arg2))
  { /* Need to check for unbound in non-global env and build
       an AUX binding in that frame if so.  Do nothing in
       usual case, unbound in global env.
    */
    case TC_VARIABLE:
      Binding_Lookup_Slot(Arg2, Arg1);
      break;
    case TC_INTERNED_SYMBOL:
    case TC_UNINTERNED_SYMBOL:
      Symbol_Binding_Lookup_Slot(Arg1, Arg2);
      break;
    default:
      Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  }  
  /* Lock region, check if the slot at Lookup_Base[Lookup_Offset] is
   a fluid already.  Return it if so, make a new fluid and store it
   there if not, unlock the region. */
  {
#ifdef COMPILE_FUTURES
    Lock_Handle Set_Serializer;
#endif
    Pointer Found_Val, Safe_Val;
    if (Lookup_Offset == HEAP_ENV_FUNCTION) Primitive_Error(ERR_BAD_SET);
#ifdef COMPILE_FUTURES
    Set_Serializer = Lock_Cell(Nth_Vector_Loc(Lookup_Base, Lookup_Offset));
#endif
    Found_Val = Fast_Vector_Ref(Lookup_Base, Lookup_Offset);
    Safe_Val = Found_Val & ~DANGER_BIT;
    if (Type_Code(Safe_Val) == TC_TRAP)	Trap_Obj = Found_Val;
    else
    { Primitive_GC_If_Needed(TRAP_SIZE);
      Trap_Obj = (Pointer) Free;
      *Free++ = NIL;		/* Tag for fluids */
      /* Binding version always makes unbounds unassigned */
      *Free++ = (Safe_Val == UNBOUND_OBJECT) ? UNASSIGNED_OBJECT:Safe_Val;
      *Free++ = Arg2;	        /* For debugging purposes */
      Store_Type_Code(Trap_Obj,
		      ((Found_Val==Safe_Val)?TC_TRAP:TC_TRAP|DANGER_TYPE));
      Fast_Vector_Set(Lookup_Base, Lookup_Offset, Trap_Obj);
    }
#ifdef COMPILE_FUTURES
    Unlock_Cell(Set_Serializer);
#endif
    Add_Fluid_Binding(Trap_Obj, Arg3);
    Val = NIL;
    return Val;
  }
}

Add_Fluid_Binding(Key, Value)
Pointer Key, Value;
{ Pointer New_Fluids;
  
  Primitive_GC_If_Needed(2 + 2);
  New_Fluids = Make_Pointer(TC_LIST, Free);
  *Free = Make_Pointer(TC_LIST, &Free[2]);
  Free += 1;
  *Free++ = Fluid_Bindings;
  *Free++ = Key;
  *Free++ = Value;
  Fluid_Bindings = New_Fluids;
}

Symbol_Lookup_Slot(Frame, Symbol)
Pointer Frame, Symbol;
{ int result;
  Pointer *Variable = Free;
  Free += 3;
  Variable[VARIABLE_SYMBOL] = (Symbol);
  Variable[VARIABLE_COMPILED_TYPE] = UNCOMPILED_VARIABLE;
  result = Lookup_Slot(Make_Pointer(TC_VARIABLE, Variable), (Frame));
  if (Free == Variable+3) Free = Variable;
  return result;
}

Binding_Symbol_Lookup_Slot(Frame, Symbol)
Pointer Frame, Symbol;
{ int result;
  Pointer *Variable = Free;
  Free += 3;
  Variable[VARIABLE_SYMBOL] = (Symbol);
  Variable[VARIABLE_COMPILED_TYPE] = UNCOMPILED_VARIABLE;
  result = Binding_Lookup_Slot(Make_Pointer(TC_VARIABLE, Variable), (Frame));
  if (Free == Variable+3) Free = Variable;
  return result;
}

/* A version which creates a new binding if unbound in last frame */

Symbol_Binding_Lookup_Slot(Frame, Symbol)
Pointer Frame, Symbol;
{ int result;
  Pointer *Variable = Free;
  Free += 3;
  Variable[VARIABLE_SYMBOL] = (Symbol);
  Variable[VARIABLE_COMPILED_TYPE] = UNCOMPILED_VARIABLE;
  result = Binding_Lookup_Slot(Make_Pointer(TC_VARIABLE, Variable), (Frame));
  if (Free == Variable+3) Free = Variable;
  return result;
}



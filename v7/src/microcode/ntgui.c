/* -*-C-*-

$Id: ntgui.c,v 1.11 1993/09/11 02:45:55 gjr Exp $

Copyright (c) 1993 Massachusetts Institute of Technology

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

#include <string.h>
#include <stdarg.h>
#include "scheme.h"
#include "prims.h"
#include "os.h"
#include "nt.h"
#include "ntdialog.h"
#include "ntgui.h"
#include "ntscreen.h"

extern /*static*/ HANDLE  ghInstance = 0;

BOOL InitApplication(HANDLE);
BOOL InitInstance(HANDLE, int);

void *xmalloc(int);
void xfree(void*);

#ifdef GUI
int PASCAL
WinMain (HANDLE hInst, HANDLE hPrevInst, LPSTR lpCmdLine, int nCmdShow)
{
    int argc;
    char **argv;
    extern int main (int, char **);

    ghInstance = hInst;
    
    {
      int cmdlen = strlen(lpCmdLine);
      int maxargs = cmdlen/2+2;
      char *cmdline = malloc(cmdlen+1);
      char *s;

      argv = malloc(sizeof(char*) * maxargs);
      
      if (cmdline==0 || argv==0) {
	outf_fatal ("WinMain cant malloc");
	outf_flush_fatal ();
	return  FALSE;
      }	

      argc = 1;
      argv[0] = "scheme";
      
      s = strcpy (cmdline, lpCmdLine);
      
      while (*s) {
	if (*s==' ')
	  *s++ = 0;
	else {
	  argv[argc++] = s;
	  while (*s != 0 && *s != ' ') s++;
	}
      }
      argv[argc] = 0;
    }
    
    if (!hPrevInst)
      if (!InitApplication(ghInstance))
	return  FALSE;
    
    if (!InitInstance(ghInstance, nCmdShow))
      return  FALSE;
      
    return (main (argc, argv));
}
#endif


BOOL
DEFUN (InitApplication, (hInstance), HANDLE hInstance)
{
    // WNDCLASS wc;
    static BOOL done = FALSE;
    
    if (done) return  TRUE;
    done = TRUE;
    
    //wc.style         = CS_HREDRAW | CS_VREDRAW;
    //wc.lpfnWndProc   = TranscriptWndProc;
    //wc.cbClsExtra    = 0;
    //wc.cbWndExtra    = sizeof (Transcript*);
    //wc.hInstance     = hInstance;
    //wc.hIcon         = NULL;
    //wc.hCursor       = LoadCursor (NULL, IDC_ARROW);
    //wc.hbrBackground = GetStockObject (WHITE_BRUSH);
    //wc.lpszMenuName  = transcript_class_name;
    //wc.lpszClassName = transcript_class_name;

    //if (!RegisterClass(&wc))
    //  return  FALSE;
  
    return  Screen_InitApplication (hInstance);
    //return  TRUE;
}

static BOOL instance_initialized = FALSE;

BOOL
DEFUN (InitInstance, (hInstance, nCmdShow), HANDLE hInstance AND int nCmdShow)
{
    instance_initialized = TRUE;

    return  Screen_InitInstance (hInstance, nCmdShow);
    //return  TRUE;
}



//void
//DEFUN_VOID (nt_gui_default_poll)
//{
//   static int n = 0;
//#ifdef GUI
//   DWORD pending_types;
//   int events_processed = 0;
//
//   outf_console("\001");  outf_flush_console();
//   while (events_processed < 5 &&
//          (pending_types = GetQueueStatus(QS_INPUT)) >> 16) {
//     MSG msg;
//     //outf_console("GetQueueStatus() = 0x%08x\n", pending_types);
//     //outf_console("GetMessage()\n");
//   outf_console("\360");  outf_flush_console();
//     GetMessage (&msg, 0, 0, 0);
//     TranslateMessage(&msg);
//     DispatchMessage(&msg);
//   outf_console("\361");  outf_flush_console();
//     events_processed ++;
//   }
//   //outf_console("events_processed = %d\n", events_processed);
//   outf_console("\002");  outf_flush_console();
//#endif
//}


extern BOOL MIT_TranslateMessage (CONST MSG *);

void
DEFUN_VOID (nt_gui_default_poll)
{
#ifdef GUI
   MSG  msg;
   int events_processed = 0;

   while (//events_processed < 5 &&
          PeekMessage (&msg, 0, 0, 0, PM_REMOVE))
   {
     MIT_TranslateMessage(&msg);
     DispatchMessage(&msg);
     events_processed ++;
   }
#endif
}

extern HANDLE master_tty_window;
extern void catatonia_trigger (void);
extern unsigned long * winnt_catatonia_block;

void
catatonia_trigger (void)
{
  int mes_result;
  static BOOL already_exitting = FALSE;
  SCHEME_OBJECT saved = winnt_catatonia_block[CATATONIA_BLOCK_LIMIT];

  winnt_catatonia_block[CATATONIA_BLOCK_LIMIT] = 0;

  mes_result = (MessageBox (master_tty_window,
			    "Scheme appears to have become catatonic.\n"
			    "OK to kill it?",
			    "MIT Scheme",
			    (MB_ICONSTOP | MB_OKCANCEL)));

  winnt_catatonia_block[CATATONIA_BLOCK_COUNTER] = 0;
  winnt_catatonia_block[CATATONIA_BLOCK_LIMIT] = saved;

  if (mes_result != IDOK)
    return;
  else if (already_exitting)
    exit (1);
  else
  {
    extern void termination_normal (int);
    already_exitting = TRUE;
    termination_normal (0);
  }
}

static void
nt_gui_high_priority_poll (void)
{
  MSG close_msg;

  if (PeekMessage (&close_msg, master_tty_window,
		   WM_CATATONIC, (WM_CATATONIC + 1),
		   PM_REMOVE))
  {
    MIT_TranslateMessage (&close_msg);
    DispatchMessage (&close_msg);
  }
  return;
}

DEFINE_PRIMITIVE ("MICROCODE-POLL-INTERRUPT-HANDLER",
                  Prim_microcode_poll_interrupt_handler, 2, 2,
		  "NT High-priority timer interrupt handler for Windows I/O.")
{
#ifndef USE_WM_TIMER
  extern void low_level_timer_tick (void);
#endif

  PRIMITIVE_HEADER (2);
  if (((ARG_REF (1)) & (ARG_REF (2)) & INT_Global_GC) != 0)
  {
    nt_gui_high_priority_poll ();
    CLEAR_INTERRUPT (INT_Global_GC);
  }
  else
  {
    winnt_catatonia_block[CATATONIA_BLOCK_COUNTER] = 0;
    nt_gui_default_poll ();
#ifndef USE_WM_TIMER
    low_level_timer_tick ();
#endif
    CLEAR_INTERRUPT (INT_Global_1);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("NT-DEFAULT-POLL-GUI", Prim_nt_default_poll_gui, 2, 2,
"")
{
  PRIMITIVE_HEADER(2)
  {
    nt_gui_default_poll ();
    PRIMITIVE_RETURN  (UNSPECIFIC);
  }
}

extern void EXFUN (NT_gui_init, (void));

void
DEFUN_VOID (NT_gui_init)
{
   if (!instance_initialized) {
     if (!InitApplication (ghInstance))
       outf_console ("InitApplication failed\n");
     if (!InitInstance (ghInstance, SW_SHOWNORMAL))
       outf_console ("InitInstance failed\n");
   }
}

static long  
scheme_object_to_windows_object (SCHEME_OBJECT thing)
{
    if (INTEGER_P (thing))
      return  integer_to_long (thing);

    if (STRING_P (thing))
      return  (long) STRING_LOC (thing, 0);

    if (thing==SHARP_F)
      return  0;
    if (thing==SHARP_T)
      return  1;

    if (OBJECT_TYPE (thing) == TC_VECTOR_1B ||
        OBJECT_TYPE (thing) == TC_VECTOR_16B)
      return  (long) VECTOR_LOC (thing, 0);

    return  (long)thing;
}

/****************************************************************************/
/* first scheme window procedure requires every procedure to be purified    */
/****************************************************************************/

extern SCHEME_OBJECT C_call_scheme (SCHEME_OBJECT, long, SCHEME_OBJECT *);

static SCHEME_OBJECT
apply4 (SCHEME_OBJECT procedure, SCHEME_OBJECT arg1, SCHEME_OBJECT arg2,
                                 SCHEME_OBJECT arg3, SCHEME_OBJECT arg4)
{
    return  C_call_scheme (procedure, 4, &arg1);
}

LRESULT CALLBACK
C_to_Scheme_WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    SCHEME_OBJECT  thunk;
    SCHEME_OBJECT  result;
    
    if (message==WM_CREATE || message==WM_NCCREATE) {
      /*install thunk*/
      LPCREATESTRUCT lpcs = (LPCREATESTRUCT) lParam;
      SetWindowLong(hwnd, 0, (LONG)lpcs->lpCreateParams);
    }
    
    thunk = GetWindowLong (hwnd, 0);
    
    if (thunk==0)
      return  DefWindowProc (hwnd, message, wParam, lParam);
    
    result = apply4(thunk,
                ulong_to_integer(hwnd), ulong_to_integer(message),
		ulong_to_integer(wParam), ulong_to_integer(lParam));

    return  scheme_object_to_windows_object (result);
}


DEFINE_PRIMITIVE ("GET-SCHEME-WINDOW-PROCEDURE", Prim_get_scheme_window_procedure, 1, 1,
"")
{
  PRIMITIVE_HEADER(1);
  {
    HWND hWnd = (HWND)arg_integer (1);
    SCHEME_OBJECT  result;
    
    if (GetWindowLong(hWnd, GWL_WNDPROC) != (LONG) C_to_Scheme_WndProc)
      result = SHARP_F;
    else
      result = (SCHEME_OBJECT) GetWindowLong(hWnd, 0);

    PRIMITIVE_RETURN (result);
  }
}

/****************************************************************************/
/*
    Second version:  There is only one scheme wndproc, which is called
    to re-dispatch to the correct wndproc, indexing of the hwnd argument.
    The one scheme procedure is set with SET-GENERAL-SCHEME-WNDPROC.
    The procedure must be a purified first.
*/


static SCHEME_OBJECT general_scheme_wndproc = SHARP_F;

DEFINE_PRIMITIVE ("GET-GENERAL-SCHEME-WNDPROC",
Prim_get_general_scheme_wndproc, 0, 0, "")
{
  PRIMITIVE_HEADER(0);
  {
    PRIMITIVE_RETURN (general_scheme_wndproc);
  }
}

DEFINE_PRIMITIVE ("SET-GENERAL-SCHEME-WNDPROC",
Prim_set_general_scheme_wndproc, 1, 1, "")
{
  PRIMITIVE_HEADER(1);
  {
    SCHEME_OBJECT  wndproc = ARG_REF(1);
    if (! (ADDRESS_CONSTANT_P (OBJECT_ADDRESS (wndproc))))
      signal_error_from_primitive (ERR_ARG_1_WRONG_TYPE);
    general_scheme_wndproc = wndproc;
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}


LRESULT CALLBACK
C_to_Scheme_WndProc_2 (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    SCHEME_OBJECT  result;
    
    if (general_scheme_wndproc == SHARP_F)
      return  DefWindowProc (hwnd, message, wParam, lParam);
    
    result = apply4(general_scheme_wndproc,
                    ulong_to_integer(hwnd), ulong_to_integer(message),
		    ulong_to_integer(wParam), ulong_to_integer(lParam));

    return  scheme_object_to_windows_object (result);
}


/***************************************************************************/

void
failed_foreign_function (void)
{
  PRIMITIVE_ABORT (ERR_INAPPLICABLE_OBJECT);
}

DEFINE_PRIMITIVE ("GET-HANDLE", Prim_get_handle, 1, 1,
"(GET-HANDLE id)\n"
"Returns an otherwise hard to get global C variable\n"
"id	entity\n"
"0	instance handle\n"
"1	master tty handle\n"
"2	C to Scheme windows procedure address\n"
"3	C to Scheme windows procedure address (eta version)\n"
"4	failed-foreign-function address\n"
)
{
  PRIMITIVE_HEADER(1);
  {
    long  arg = arg_integer (1);
    long  result = 0;
    switch (arg) {
      case 0:	result = (long) ghInstance;			break;
      case 1:   result = (long) master_tty_window;		break;
      case 2:	result = (long) C_to_Scheme_WndProc;		break;
      case 3:	result = (long) C_to_Scheme_WndProc_2;		break;
      case 4:	result = (long) failed_foreign_function;	break;
      default:  error_bad_range_arg (1);
      }
    PRIMITIVE_RETURN (long_to_integer (result));
  }
}



static unsigned long
DEFUN (arg_ulong_default, (arg_number, def),
       int arg_number AND unsigned long def)
{
  fast SCHEME_OBJECT object = (ARG_REF (arg_number));
  if (object == SHARP_F)
    return  def;
  if (! (INTEGER_P (object)))
    error_wrong_type_arg (arg_number);
  return  integer_to_ulong (object);
}


DEFINE_PRIMITIVE ("WIN:CREATE-WINDOW", Prim_create_window, 10, 10,
"class-name\n"
"window-name\n"
"style\n"
"X\n"
"Y\n"
"width\n"
"height\n"
"parent\n"
"menu\n"
"(instance omitted)\n"
"lpParam: (lambda (hwnd message wparam lparam)). [think about MDI later]\n")
{
    LPSTR  class_name;
    LPSTR  window_name;
    DWORD  style;
    int    x, y, w, h;
    HWND   hWndParent;
    HMENU  hMenu;
    //HANDLE hInst;
    LPVOID lpvParam;
    HWND   result;
    
    CHECK_ARG (1, STRING_P);
    CHECK_ARG (2, STRING_P);
    class_name = STRING_LOC (ARG_REF (1), 0);
    window_name = STRING_LOC (ARG_REF (2), 0);
    style = integer_to_ulong (ARG_REF (3));
    x = arg_ulong_default (4, CW_USEDEFAULT);
    y = arg_ulong_default (5, CW_USEDEFAULT);
    w = arg_ulong_default (6, CW_USEDEFAULT);
    h = arg_ulong_default (7, CW_USEDEFAULT);
    hWndParent = (HWND) arg_ulong_default (8, 0);
    hMenu      =  (HMENU) arg_ulong_default (9, 0);
    lpvParam   = (LPVOID)  ARG_REF (10);
    
    result = CreateWindowEx (0, class_name, window_name, style, x, y, w, h,
			     hWndParent, hMenu, ghInstance, lpvParam);
		 
    return  ulong_to_integer (result);
}


DEFINE_PRIMITIVE ("WIN:DEF-WINDOW-PROC", Prim_def_window_proc, 4, 4, "")
{
    //outf_console ("\001");
    return
      long_to_integer
	(DefWindowProc
	 (((HWND) (scheme_object_to_windows_object (ARG_REF (1)))),
          ((UINT) (scheme_object_to_windows_object (ARG_REF (2)))),
	  ((WPARAM) (scheme_object_to_windows_object (ARG_REF (3)))),
	  ((LPARAM) (scheme_object_to_windows_object (ARG_REF (4))))));
}


DEFINE_PRIMITIVE ("REGISTER-CLASS",
                  Prim__register_class, 10, 10,
"(REGISTER-CLASS style wndproc clsExtra wndExtra hInstance hIcon hCursor\n"
"                hBackground menu-name class-name)\n\n"

"cursor     = 32512(arrow), 32513(ibeam), 32514(hourglass) 32515(cross), 32516(uparrow)\n"
"background = 0 (white_brush)\n"
)
{
    // should lift background and cursor
    WNDCLASS wc;
    BOOL  rc;
    PRIMITIVE_HEADER (10);
    CHECK_ARG (10, STRING_P);

    wc.style         = arg_integer (1);
    wc.lpfnWndProc   = ((WNDPROC) (arg_integer (2)));
    wc.cbClsExtra    = scheme_object_to_windows_object (ARG_REF(3));
    wc.cbWndExtra    = scheme_object_to_windows_object (ARG_REF(4));
    wc.hInstance     = (HANDLE)scheme_object_to_windows_object (ARG_REF(5));
    wc.hIcon         = (HANDLE)scheme_object_to_windows_object (ARG_REF(6));
    wc.hCursor       = LoadCursor (NULL, MAKEINTRESOURCE(arg_integer(7)));
    wc.hbrBackground = GetStockObject (arg_integer(8));
    wc.lpszMenuName  = (char*)scheme_object_to_windows_object (ARG_REF(9));
    wc.lpszClassName = (char*)scheme_object_to_windows_object (ARG_REF(10));

    rc = RegisterClass (&wc);
    PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT(rc));
}

DEFINE_PRIMITIVE ("APPLY_1", Prim_apply_1_xyz, 2, 2, "")
{
    SCHEME_OBJECT  proc, arg, result;
    PRIMITIVE_HEADER (2);
    
    proc = ARG_REF (1);
    arg  = ARG_REF (2);
    
    result = C_call_scheme (proc, 1, &arg);
    
    PRIMITIVE_RETURN (result);
}

/************************************************************************/
/* Primitive versions of library stuff					*/
/************************************************************************/


DEFINE_PRIMITIVE ("NT:GET-MODULE-HANDLE", Prim_get_module_handle, 1, 1,
"(GET-MODULE-HANDLE string) -> handle")
{
    HANDLE it;

    PRIMITIVE_HEADER (1);
    
    CHECK_ARG (1, STRING_P);
    it = GetModuleHandle (STRING_LOC (ARG_REF (1), 0));
    PRIMITIVE_RETURN (long_to_integer (it));
}

DEFINE_PRIMITIVE ("NT:LOAD-LIBRARY", Prim_nt_load_library, 1, 1,
"(LOAD-LIBRARY string) -> handle")
{
    HANDLE it;

    PRIMITIVE_HEADER (1);
    
    CHECK_ARG (1, STRING_P);
    it = LoadLibrary ((LPSTR)STRING_LOC (ARG_REF (1), 0));
    PRIMITIVE_RETURN (long_to_integer (it));
}

DEFINE_PRIMITIVE ("NT:FREE-LIBRARY", Prim_nt_free_library, 1, 1,
"(FREE-LIBRARY library-module-handle) -> bool")
{
    HANDLE handle;
    BOOL   result;
    
    PRIMITIVE_HEADER (1);
    
    handle = ((HANDLE) (arg_integer (1)));
    result = FreeLibrary (handle);
    PRIMITIVE_RETURN (result ? SHARP_T : SHARP_F);
}

DEFINE_PRIMITIVE ("NT:GET-PROC-ADDRESS", Prim_nt_get_proc_address, 2, 2,
"(GET-PROC-ADDRESS handle string/integer) -> address")
{
    HMODULE  module;
    LPSTR    function_name;
    FARPROC  it;
    SCHEME_OBJECT  function;
    
    PRIMITIVE_HEADER (2);
    
    module   = (HMODULE) arg_integer (1);
    function = ARG_REF (2);
    if (STRING_P (function))
      function_name = STRING_LOC (function, 0);
    else
      function_name = (LPSTR) arg_integer (2);

    it = GetProcAddress (module, function_name);

    PRIMITIVE_RETURN (it==NULL ? SHARP_F : long_to_integer (it));
}

DEFINE_PRIMITIVE ("NT:SEND-MESSAGE", Prim_send_message, 4, 4,
"(SEND-MESSAGE  handle  message  wparam  lparam)")
{
    HWND    hwnd;
    UINT    message;
    WPARAM  wParam;
    LPARAM  lParam;
    SCHEME_OBJECT  thing;
    PRIMITIVE_HEADER (4);
    
    hwnd    = (HWND) arg_integer (1);
    message = arg_integer (2);
    wParam  = arg_integer (3);
    thing = ARG_REF (4);
    if (STRING_P (thing))
      lParam = (LPARAM) STRING_LOC (thing, 0);
    else
      lParam = arg_integer (4);
    
    PRIMITIVE_RETURN (
      long_to_integer (SendMessage (hwnd, message, wParam, lParam)));
}

// Indirect calls to a __stdcall procedure are compiled as if to a _cdecl
// procedure.  The result is disaster.  The fudge_calls give us some
// protection because the procedure entry code pushes registers.  These
// get trampled, but with luck the values saved were not live.
//
//static long fudge_call_1 (long (* WINAPI f)(long), long a)
//{
//   return  f(a);
//}
//
//DEFINE_PRIMITIVE ("CALL-FF-1", Prim_call_ff_1, 2, 2,
//"")
//{
//    long  result;
//    long (* WINAPI f)(long);
//    PRIMITIVE_HEADER (2);
//    
//    f =  arg_integer (1);
//    result = fudge_call_1 (f, call_ff_arg (ARG_REF (2)));
//    
//    PRIMITIVE_RETURN (long_to_integer (result));
//}
//
//static long fudge_call_2 (long (* WINAPI f)(long,long), long a1, long a2)
//{
//    return  f(a1,a2);
//}
//
//DEFINE_PRIMITIVE ("CALL-FF-2", Prim_call_ff_2, 3, 3,
//"")
//{
//    long (* WINAPI f)(long,long);
//    
//    PRIMITIVE_HEADER (3);
//    
//    f =  arg_integer (1);
//    PRIMITIVE_RETURN (long_to_integer (fudge_call_2 (f, call_ff_arg (ARG_REF(2)), call_ff_arg (ARG_REF(3)))));
//}
//
//static long fudge_call_3 (long (* WINAPI f)(long,long,long),
//                          long a1, long a2, long a3)
//{
//    return  f(a1,a2,a3);
//}
//
//DEFINE_PRIMITIVE ("CALL-FF-3", Prim_call_ff_3, 4, 4,
//"")
//{
//    long (*f)(long,long,long);
//    long result;
//    
//    PRIMITIVE_HEADER (4);
//    
//    f =  arg_integer (1);
//    result = fudge_call_3 (f, call_ff_arg(ARG_REF(2)), call_ff_arg(ARG_REF(3)), call_ff_arg(ARG_REF(4)));
//    PRIMITIVE_RETURN (long_to_integer (result));
//}

static  SCHEME_OBJECT call_ff_really()
{

  {
    /*  use a struct for locals that live across the foreign function call
        so that their position in the stack is the right end of the stack
	frame with respect to the stacked C arguments */
    struct {
      long c_args[50];
      long old_esp;
    } local;
    
    long result;

    /*  We save the stack pointer and restore it because the called function
        may pop the arguments (pascal/__stdcall) or expect us to (__cdecl). */

    /*  The stack pointer is saved in a static variable so that we can find
        it if the compiler does SP-relative addressing with a broken SP */
    
    /*  The implication is that things will break if this gets overwritten.
        This will happen if the foreign function directly or indirectly 
	allows a Scheme interrupt to be processed (eg by calling as scheme
	function with interrupts enabled and that function gets rescheduled
	in the threads package. */
    
    static long saved_esp;
    
    long nargs = (LEXPR_N_ARGUMENTS ());
    if (nargs < 1)
      signal_error_from_primitive (ERR_WRONG_NUMBER_OF_ARGUMENTS);
    if (nargs > 30)
      signal_error_from_primitive (ERR_WRONG_NUMBER_OF_ARGUMENTS);
    {
      long *arg_sp = &local.c_args[10];
      SCHEME_OBJECT *argument_scan = ARG_LOC (2);
      SCHEME_OBJECT *argument_limit = ARG_LOC (nargs+1);

      long  function_address = arg_integer(1);

      while (argument_scan != argument_limit)
	*arg_sp++ = 
	  scheme_object_to_windows_object (STACK_LOCATIVE_POP(argument_scan));
      
      arg_sp = &local.c_args[10];
      local.old_esp = saved_esp;
      __asm
      {
	// Important: The order of these instructions guards against
	// stack pointer relative addressing.
	mov	eax, dword ptr [function_address]
	mov	dword ptr [saved_esp], esp
	mov	esp, dword ptr [arg_sp]
	call	eax
	mov	esp, dword ptr [saved_esp]
	mov	dword ptr [result], eax
      }
      saved_esp = local.old_esp;
      return  long_to_integer (result);
    }
  }
}

DEFINE_PRIMITIVE ("CALL-FF", Prim_call_ff, 0, LEXPR, 0)
{
    /* this indirection saves registers correctly in this stack frame
       rather than in a bad position in relation to the bogus C argument
       stack */
    PRIMITIVE_HEADER (LEXPR);
    PRIMITIVE_RETURN (call_ff_really());
}

//
// Primitives for hacking strings:
// to fetch and set signed and unsigned 32 and 16 bit values at byte offsets
//

DEFINE_PRIMITIVE ("INT32-OFFSET-REF", Prim_int32_offset_ref, 2, 2,
"(long-offset-ref mem-addr byte-offset)\n"
"Fetch 32 bit signed long from memory (a string)"
)
{
    PRIMITIVE_HEADER (2);
    {
      long *base;
      int  offset;
      CHECK_ARG (1, STRING_P);
      base = (long*) STRING_LOC (ARG_REF(1), 0);
      offset  = arg_integer (2);
      PRIMITIVE_RETURN ( long_to_integer(* (long*) (((char*)base)+offset) ) );
    }
}

DEFINE_PRIMITIVE ("INT32-OFFSET-SET!", Prim_int32_offset_set, 3, 3,
"(long-offset-sef mem-addr byte-offset 32-bit-value)\n"
"Set 32 bit signed long from memory (integer address or vector data)"
)
{
    PRIMITIVE_HEADER (3);
    {
      long *base;
      int  offset;
      long value;
      CHECK_ARG (1, STRING_P);
      base   = (long*) STRING_LOC (ARG_REF(1), 0);
      offset = arg_integer (2);
      value  = scheme_object_to_windows_object (ARG_REF (3));
      * (long*) (((char*)base)+offset)  =  value;
    }
    PRIMITIVE_RETURN (UNSPECIFIC);
}


DEFINE_PRIMITIVE ("UINT32-OFFSET-REF", Prim_uint32_offset_ref, 2, 2,
"(uint32-offset-ref mem-addr byte-offset)\n"
"Fetch 32 bit unsigned long from memory (a string)"
)
{
    PRIMITIVE_HEADER (2);
    {
      unsigned long *base;
      int  offset;
      CHECK_ARG (1, STRING_P);
      base = (unsigned long*) STRING_LOC (ARG_REF(1), 0);
      offset  = arg_integer (2);
      PRIMITIVE_RETURN ( ulong_to_integer(* (unsigned long*) (((char*)base)+offset) ) );
    }
}

DEFINE_PRIMITIVE ("UINT32-OFFSET-SET!", Prim_uint32_offset_set, 3, 3,
"(unit32-offset-sef mem-addr byte-offset 32-bit-value)\n"
"Set 32 bit unsigned long at offset from memory"
)
{
    PRIMITIVE_HEADER (3);
    {
      unsigned long *base;
      int  offset;
      unsigned long value;
      CHECK_ARG (1, STRING_P);
      base   = (unsigned long*) STRING_LOC (ARG_REF(1), 0);
      offset = arg_integer (2);
      value  = scheme_object_to_windows_object (ARG_REF (3));
      * (unsigned long*) (((char*)base)+offset)  =  value;
    }
    PRIMITIVE_RETURN (UNSPECIFIC);
}

static void *
xmalloc (int size)
{
    void *result = malloc(size);
    if (!result) {
      outf_fatal ("ntgui: xmalloc failed");
      outf_flush_fatal ();
      abort ();
    }
    return  result;
}

static void
xfree (void *p)
{
    free (p);
}

/* GUI utilities for debuggging .*/

#ifdef W32_TRAP_DEBUG

extern HANDLE ghInstance;
extern int TellUser (char *, ...);
extern int TellUserEx (int, char *, ...);
extern char * AskUser (char *, int);

int
TellUser (char * format, ...)
{
  va_list arg_ptr;
  char buffer[1024];
  
  va_start (arg_ptr, format);
  wvsprintf (&buffer[0], format, arg_ptr);
  va_end (arg_ptr);
  return (MessageBox (master_tty_window,
		      ((LPCSTR) &buffer[0]),
		      ((LPCSTR) "MIT Scheme Win32 Notification"),
		      (MB_TASKMODAL | MB_ICONINFORMATION
		       | MB_SETFOREGROUND | MB_OK)));
}

int
TellUserEx (int flags, char * format, ...)
{
  va_list arg_ptr;
  char buffer[1024];
  
  va_start (arg_ptr, format);
  wvsprintf (&buffer[0], format, arg_ptr);
  va_end (arg_ptr);
  return (MessageBox (master_tty_window,
		      ((LPCSTR) &buffer[0]),
		      ((LPCSTR) "MIT Scheme Win32 Notification"),
		      (MB_TASKMODAL | MB_ICONINFORMATION
		       | MB_SETFOREGROUND | flags)));
}

static char * askuserbuffer = ((char *) NULL);
static int askuserbufferlength = 0;

static BOOL APIENTRY
DEFUN (askuserdlgproc, (hwnddlg, message, wparam, lparam),
       HWND hwnddlg AND UINT message
       AND WPARAM wparam AND LPARAM lparam)
{
  switch (message)
  {
    case WM_CLOSE:
    done:
      GetDlgItemText (hwnddlg, SCHEME_INPUT_TEXT,
		      askuserbuffer,
		      askuserbufferlength);
      EndDialog (hwnddlg, 0);
      return (TRUE);

    case WM_COMMAND:
      switch (wparam)
      {
        case IDOK:
	  goto done;
	  
        case IDCANCEL:
	  EndDialog (hwnddlg, -1);
	  return (TRUE);

        default:
	  return (FALSE);
      }

    case WM_INITDIALOG:
      return (TRUE);

    default:
      return (FALSE);
  }
}

char *
DEFUN (AskUser, (buf, len), char * buf AND int len)
{
  char * result;

  askuserbuffer = buf;
  askuserbufferlength = len;
  result = (DialogBox (ghInstance,
		       SCHEME_INPUT,
		       master_tty_window,
		       askuserdlgproc));
  if (result == -1)
    return ((char *) NULL);

  askuserbuffer = ((char *) NULL);
  askuserbufferlength = 0;
  return (buf);
}

#endif /* W32_TRAP_DEBUG */

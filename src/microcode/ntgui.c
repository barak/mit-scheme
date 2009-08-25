/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

*/

#include "scheme.h"
#include "prims.h"
#include "os.h"
#include "nt.h"
#include "ntdialog.h"
#include "ntgui.h"
#include "ntscreen.h"

extern /*static*/ HANDLE  ghInstance = 0;
extern void scheme_main (int argc, const char ** argv);
extern void NT_preallocate_heap (void);
BOOL InitApplication(HANDLE);
BOOL InitInstance(HANDLE, int);

static SCHEME_OBJECT parse_event (SCREEN_EVENT *);

int WINAPI
WinMain (HANDLE hInst, HANDLE hPrevInst, LPSTR lpCmdLine, int nCmdShow)
{
    int argc;
    char **argv;
    extern int main (int, char **);

    NT_preallocate_heap ();
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

      while ((*s) != '\0')
	{
	  while ((*s) == ' ')
	    s += 1;
	  if ((*s) == '"')
	    {
	      s += 1;
	      (argv[argc++]) = s;
	      while (1)
		{
		  if ((*s) == '"')
		    {
		      (*s++) = '\0';
		      break;
		    }
		  if ((*s) == '\0')
		    {
		      outf_fatal ("WinMain: unterminated quoted argument.");
		      outf_flush_fatal ();
		      return (FALSE);
		    }
		  s += 1;
		}
	    }
	  else
	    {
	      (argv[argc++]) = s;
	      while (1)
		{
		  if ((*s) == ' ')
		    {
		      (*s++) = '\0';
		      break;
		    }
		  if ((*s) == '\0')
		    break;
		  s += 1;
		}
	    }
	}
      argv[argc] = 0;
    }

    if (!hPrevInst)
      if (!InitApplication(ghInstance))
	return  FALSE;

    if (!InitInstance(ghInstance, nCmdShow))
      return  FALSE;

    scheme_main (argc, ((const char **) argv));
    return (0);
}

BOOL
InitApplication (HANDLE hInstance)
{
    static BOOL done = FALSE;
    if (done) return (TRUE);
    done = TRUE;
    return (Screen_InitApplication (hInstance));
}

static BOOL instance_initialized = FALSE;

BOOL
InitInstance (HANDLE hInstance, int nCmdShow)
{
  instance_initialized = TRUE;
  return (Screen_InitInstance (hInstance, nCmdShow));
}

void
nt_gui_default_poll (void)
{
  MSG msg;
  int events_processed = 0;
  while (PeekMessage ((&msg), 0, 0, 0, PM_REMOVE))
    {
      DispatchMessage (&msg);
      events_processed += 1;
    }
}

extern HANDLE master_tty_window;
extern void catatonia_trigger (void);
extern unsigned long * win32_catatonia_block;

void
catatonia_trigger (void)
{
  int mes_result;
  static BOOL already_exitting = FALSE;
  SCHEME_OBJECT saved = win32_catatonia_block[CATATONIA_BLOCK_LIMIT];

  win32_catatonia_block[CATATONIA_BLOCK_LIMIT] = 0;

  mes_result = (MessageBox (master_tty_window,
			    "Scheme appears to have become catatonic.\n"
			    "OK to kill it?",
			    "MIT/GNU Scheme",
			    (MB_ICONSTOP | MB_OKCANCEL)));

  win32_catatonia_block[CATATONIA_BLOCK_COUNTER] = 0;
  win32_catatonia_block[CATATONIA_BLOCK_LIMIT] = saved;

  if (mes_result != IDOK)
    return;
  else if (already_exitting)
    exit (1);
  else
  {
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
    DispatchMessage (&close_msg);
}

DEFINE_PRIMITIVE ("MICROCODE-POLL-INTERRUPT-HANDLER", Prim_microcode_poll_interrupt_handler, 2, 2,
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
    win32_catatonia_block[CATATONIA_BLOCK_COUNTER] = 0;
    nt_gui_default_poll ();
#ifndef USE_WM_TIMER
    low_level_timer_tick ();
#endif
    CLEAR_INTERRUPT (INT_Global_1);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("NT-DEFAULT-POLL-GUI", Prim_nt_default_poll_gui, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);

  nt_gui_default_poll ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}

extern void NT_gui_init (void);

void
NT_gui_init (void)
{
  if (!instance_initialized)
    {
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
      return  (long) (STRING_POINTER (thing));

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
  SCHEME_OBJECT argvec [4];
  (argvec[0]) = arg1;
  (argvec[1]) = arg2;
  (argvec[2]) = arg3;
  (argvec[3]) = arg4;
  return (C_call_scheme (procedure, 4, argvec));
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

    result
      = (apply4 (thunk,
		 (ulong_to_integer ((unsigned long) hwnd)),
		 (ulong_to_integer (message)),
		 (ulong_to_integer (wParam)),
		 (ulong_to_integer (lParam))));

    return  scheme_object_to_windows_object (result);
}

DEFINE_PRIMITIVE ("GET-SCHEME-WINDOW-PROCEDURE", Prim_get_scheme_window_procedure, 1, 1, 0)
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

DEFINE_PRIMITIVE ("GET-GENERAL-SCHEME-WNDPROC", Prim_get_general_scheme_wndproc, 0, 0, 0)
{
  PRIMITIVE_HEADER(0);
  {
    PRIMITIVE_RETURN (general_scheme_wndproc);
  }
}

DEFINE_PRIMITIVE ("SET-GENERAL-SCHEME-WNDPROC", Prim_set_general_scheme_wndproc, 1, 1, 0)
{
  PRIMITIVE_HEADER(1);
  {
    SCHEME_OBJECT  wndproc = ARG_REF(1);
    if (!ADDRESS_IN_CONSTANT_P (OBJECT_ADDRESS (wndproc)))
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

    result
      = (apply4 (general_scheme_wndproc,
		 (ulong_to_integer ((unsigned long) hwnd)),
		 (ulong_to_integer (message)),
		 (ulong_to_integer (wParam)),
		 (ulong_to_integer (lParam))));

    return  scheme_object_to_windows_object (result);
}

/***************************************************************************/

void
failed_foreign_function (void)
{
  PRIMITIVE_ABORT (ERR_INAPPLICABLE_OBJECT);
}

DEFINE_PRIMITIVE ("GET-HANDLE", Prim_get_handle, 1, 1,
  "(id)\n"
  "Returns an otherwise hard to get global C variable\n"
  "id	entity\n"
  "0	instance handle\n"
  "1	master tty handle\n"
  "2	C to Scheme windows procedure address\n"
  "3	C to Scheme windows procedure address (eta version)\n"
  "4	failed-foreign-function address\n")
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
arg_ulong_default (int arg_number, unsigned long def)
{
  SCHEME_OBJECT object = (ARG_REF (arg_number));
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
    LPVOID lpvParam;
    HWND   result;

    CHECK_ARG (1, STRING_P);
    CHECK_ARG (2, STRING_P);
    class_name = (STRING_POINTER (ARG_REF (1)));
    window_name = (STRING_POINTER (ARG_REF (2)));
    style = integer_to_ulong (ARG_REF (3));
    x = (int) arg_ulong_default (4, ((unsigned long) CW_USEDEFAULT));
    y = (int) arg_ulong_default (5, ((unsigned long) CW_USEDEFAULT));
    w = (int) arg_ulong_default (6, ((unsigned long) CW_USEDEFAULT));
    h = (int) arg_ulong_default (7, ((unsigned long) CW_USEDEFAULT));
    hWndParent = (HWND) arg_ulong_default (8, 0);
    hMenu      =  (HMENU) arg_ulong_default (9, 0);
    lpvParam   = (LPVOID)  ARG_REF (10);

    result = CreateWindowEx (0, class_name, window_name, style, x, y, w, h,
			     hWndParent, hMenu, ghInstance, lpvParam);

    return  ulong_to_integer ((unsigned long) result);
}

DEFINE_PRIMITIVE ("WIN:DEF-WINDOW-PROC", Prim_def_window_proc, 4, 4, 0)
{
#if 0
    outf_console ("\001");
#endif
    return
      long_to_integer
	(DefWindowProc
	 (((HWND) (scheme_object_to_windows_object (ARG_REF (1)))),
          ((UINT) (scheme_object_to_windows_object (ARG_REF (2)))),
	  ((WPARAM) (scheme_object_to_windows_object (ARG_REF (3)))),
	  ((LPARAM) (scheme_object_to_windows_object (ARG_REF (4))))));
}

DEFINE_PRIMITIVE ("REGISTER-CLASS", Prim__register_class, 10, 10,
  "(style wndproc clsExtra wndExtra hInstance hIcon hCursor\n"
  "                hBackground menu-name class-name)\n"
  "\n"
  "cursor     = 32512(arrow), 32513(ibeam), 32514(hourglass),\n"
  "             32515(cross), 32516(uparrow)\n"
  "background = 0 (white_brush)\n")
{
    /* should lift background and cursor */
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

DEFINE_PRIMITIVE ("APPLY_1", Prim_apply_1_xyz, 2, 2, 0)
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
  "(string) -> handle")
{
    HANDLE it;

    PRIMITIVE_HEADER (1);

    CHECK_ARG (1, STRING_P);
    it = GetModuleHandle (STRING_POINTER (ARG_REF (1)));
    PRIMITIVE_RETURN (long_to_integer ((long) it));
}

DEFINE_PRIMITIVE ("NT:LOAD-LIBRARY", Prim_nt_load_library, 1, 1,
  "(string) -> handle")
{
    HANDLE it;

    PRIMITIVE_HEADER (1);

    CHECK_ARG (1, STRING_P);
    it = LoadLibrary ((LPSTR) (STRING_POINTER (ARG_REF (1))));
    PRIMITIVE_RETURN (long_to_integer ((long) it));
}

DEFINE_PRIMITIVE ("NT:FREE-LIBRARY", Prim_nt_free_library, 1, 1,
  "(library-module-handle) -> bool")
{
    HANDLE handle;
    BOOL   result;

    PRIMITIVE_HEADER (1);

    handle = ((HANDLE) (arg_integer (1)));
    result = FreeLibrary (handle);
    PRIMITIVE_RETURN (result ? SHARP_T : SHARP_F);
}

DEFINE_PRIMITIVE ("NT:GET-PROC-ADDRESS", Prim_nt_get_proc_address, 2, 2,
  "(handle string/integer) -> address")
{
    HMODULE  module;
    LPSTR    function_name;
    FARPROC  it;
    SCHEME_OBJECT  function;

    PRIMITIVE_HEADER (2);

    module   = (HMODULE) arg_integer (1);
    function = ARG_REF (2);
    if (STRING_P (function))
      function_name = (STRING_POINTER (function));
    else
      function_name = (LPSTR) arg_integer (2);

    it = GetProcAddress (module, function_name);

    PRIMITIVE_RETURN (it==NULL ? SHARP_F : long_to_integer ((long) it));
}

DEFINE_PRIMITIVE ("NT:SEND-MESSAGE", Prim_send_message, 4, 4,
  "(handle message wparam lparam)")
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
      lParam = (LPARAM) (STRING_POINTER (thing));
    else
      lParam = arg_integer (4);

    PRIMITIVE_RETURN (long_to_integer (SendMessage (hwnd, message, wParam, lParam)));
}

static SCHEME_OBJECT call_ff_really (void);

DEFINE_PRIMITIVE ("CALL-FF", Prim_call_ff, 0, LEXPR, 0)
{
  /* This indirection saves registers correctly in this stack frame
     rather than in a bad position in relation to the bogus C argument
     stack.  */
  PRIMITIVE_HEADER (LEXPR);
  PRIMITIVE_RETURN (call_ff_really ());
}

static SCHEME_OBJECT
call_ff_really (void)
{
  unsigned long function_address;
  SCHEME_OBJECT * argument_scan;
  SCHEME_OBJECT * argument_limit;
  long result = UNSPECIFIC;
  unsigned long nargs = GET_LEXPR_ACTUALS;
  if (nargs < 1)
    signal_error_from_primitive (ERR_WRONG_NUMBER_OF_ARGUMENTS);
  if (nargs > 30)
    signal_error_from_primitive (ERR_WRONG_NUMBER_OF_ARGUMENTS);

  function_address = (arg_ulong_integer (1));
  argument_scan = (ARG_LOC (nargs + 1));
  argument_limit = (ARG_LOC (2));
  while (argument_scan > argument_limit)
    {
      long arg
	= (scheme_object_to_windows_object
	   (STACK_LOCATIVE_PUSH (argument_scan)));
#ifdef CL386
      __asm push arg
#else /* not CL386 */
#ifdef __WATCOMC__
      {
	extern void call_ff_really_1 (void);
#pragma aux call_ff_really_1 = "push arg";
	call_ff_really_1 ();
      }
#endif /* __WATCOMC__ */
#endif /* not CL386 */
    }
#ifdef CL386
  __asm
  {
    mov eax, function_address
    call eax
    mov result, eax
  }
#else /* not CL386 */
#ifdef __WATCOMC__
  {
    extern void call_ff_really_2 (void);
#pragma aux call_ff_really_2 =						\
    "mov eax,function_address"						\
    "call eax"								\
    "mov result,eax"							\
    modify [eax edx ecx];
    call_ff_really_2 ();
  }
#endif /* __WATCOMC__ */
#endif /* not CL386 */
  return (long_to_integer (result));
}

/* Primitives for hacking strings, to fetch and set signed and
   unsigned 32 and 16 bit values at byte offsets.  */

DEFINE_PRIMITIVE ("INT32-OFFSET-REF", Prim_int32_offset_ref, 2, 2,
  "(mem-addr byte-offset)\n"
  "Fetch 32 bit signed long from memory (a string)")
{
    PRIMITIVE_HEADER (2);
    {
      long *base;
      int  offset;
      CHECK_ARG (1, STRING_P);
      base = (long*) (STRING_POINTER (ARG_REF (1)));
      offset  = arg_integer (2);
      PRIMITIVE_RETURN ( long_to_integer(* (long*) (((char*)base)+offset) ) );
    }
}

DEFINE_PRIMITIVE ("INT32-OFFSET-SET!", Prim_int32_offset_set, 3, 3,
  "(mem-addr byte-offset 32-bit-value)\n"
  "Set 32 bit signed long from memory (integer address or vector data)")
{
    PRIMITIVE_HEADER (3);
    {
      long *base;
      int  offset;
      long value;
      CHECK_ARG (1, STRING_P);
      base   = (long*) (STRING_POINTER (ARG_REF (1)));
      offset = arg_integer (2);
      value  = scheme_object_to_windows_object (ARG_REF (3));
      * (long*) (((char*)base)+offset)  =  value;
    }
    PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("UINT32-OFFSET-REF", Prim_uint32_offset_ref, 2, 2,
  "(mem-addr byte-offset)\n"
  "Fetch 32 bit unsigned long from memory (a string)")
{
    PRIMITIVE_HEADER (2);
    {
      unsigned long *base;
      int  offset;
      CHECK_ARG (1, STRING_P);
      base = (unsigned long*) (STRING_POINTER (ARG_REF (1)));
      offset  = arg_integer (2);
      PRIMITIVE_RETURN
	(ulong_to_integer(* (unsigned long*) (((char*)base)+offset)));
    }
}

DEFINE_PRIMITIVE ("UINT32-OFFSET-SET!", Prim_uint32_offset_set, 3, 3,
  "(mem-addr byte-offset 32-bit-value)\n"
  "Set 32 bit unsigned long at offset from memory")
{
    PRIMITIVE_HEADER (3);
    {
      unsigned long *base;
      int  offset;
      unsigned long value;
      CHECK_ARG (1, STRING_P);
      base   = (unsigned long*) (STRING_POINTER (ARG_REF (1)));
      offset = arg_integer (2);
      value  = scheme_object_to_windows_object (ARG_REF (3));
      * (unsigned long*) (((char*)base)+offset)  =  value;
    }
    PRIMITIVE_RETURN (UNSPECIFIC);
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
		      ((LPCSTR) "MIT/GNU Scheme Win32 Notification"),
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
		      ((LPCSTR) "MIT/GNU Scheme Win32 Notification"),
		      (MB_TASKMODAL | MB_ICONINFORMATION
		       | MB_SETFOREGROUND | flags)));
}

static char * askuserbuffer = ((char *) NULL);
static int askuserbufferlength = 0;

static BOOL APIENTRY
askuserdlgproc (HWND hwnddlg, UINT message,
       WPARAM wparam, LPARAM lparam)
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
AskUser (char * buf, int len)
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

/* Events */

/* Worst case consing for longs.
   This should really be available elsewhere.  */
#define LONG_TO_INTEGER_WORDS (4)
#define MAX_EVENT_STORAGE ((9 * (LONG_TO_INTEGER_WORDS + 1)) + 1)

DEFINE_PRIMITIVE ("WIN32-READ-EVENT", Prim_win32_read_event, 0, 0,
  "()\n\
Returns the next event from the event queue.\n\
The event is deleted from the queue.\n\
Returns #f if there are no events in the queue.")
{
  PRIMITIVE_HEADER (0);
  /* Ensure that the primitive is not restarted due to GC: */
  Primitive_GC_If_Needed (MAX_EVENT_STORAGE);
  {
    SCREEN_EVENT event;
    SCHEME_OBJECT sevent;
    while (1)
      {
	if (!Screen_read_event (&event))
	  PRIMITIVE_RETURN (SHARP_F);
	sevent = (parse_event (&event));
	if (sevent != SHARP_F)
	  PRIMITIVE_RETURN (sevent);
      }
  }
}

#define INIT_RESULT(n)							\
{									\
  result = (allocate_marked_vector (TC_VECTOR, ((n) + 2), 1));		\
  WRITE_UNSIGNED (event -> type);					\
  WRITE_UNSIGNED ((unsigned long) (event -> handle));			\
}

#define WRITE_RESULT(object) VECTOR_SET (result, (index++), (object))
#define WRITE_UNSIGNED(n) WRITE_RESULT (ulong_to_integer (n))
#define WRITE_SIGNED(n) WRITE_RESULT (long_to_integer (n))
#define WRITE_FLAG(n) WRITE_RESULT (((n) == 0) ? SHARP_F : SHARP_T)

static SCHEME_OBJECT
parse_event (SCREEN_EVENT * event)
{
  unsigned int index = 0;
  SCHEME_OBJECT result;
  switch (event -> type)
    {
    case SCREEN_EVENT_TYPE_RESIZE:
      INIT_RESULT (2);
      WRITE_UNSIGNED (event->event.resize.rows);
      WRITE_UNSIGNED (event->event.resize.columns);
      break;
    case SCREEN_EVENT_TYPE_KEY:
      INIT_RESULT (6);
      WRITE_UNSIGNED (event->event.key.repeat_count);
      WRITE_SIGNED   (event->event.key.virtual_keycode);
      WRITE_UNSIGNED (event->event.key.virtual_scancode);
      WRITE_UNSIGNED (event->event.key.control_key_state);
      WRITE_SIGNED   (event->event.key.ch);
      WRITE_FLAG     (event->event.key.key_down);
      break;
    case SCREEN_EVENT_TYPE_MOUSE:
      INIT_RESULT (7);
      WRITE_UNSIGNED (event->event.mouse.row);
      WRITE_UNSIGNED (event->event.mouse.column);
      WRITE_UNSIGNED (event->event.mouse.control_key_state);
      WRITE_UNSIGNED (event->event.mouse.button_state);
      WRITE_FLAG     (event->event.mouse.up);
      WRITE_FLAG     (event->event.mouse.mouse_moved);
      WRITE_FLAG     (event->event.mouse.double_click);
      break;
    case SCREEN_EVENT_TYPE_CLOSE:
      INIT_RESULT (0);
      break;
    case SCREEN_EVENT_TYPE_FOCUS:
      INIT_RESULT (1);
      WRITE_FLAG     (event->event.focus.gained_p);
      break;
    case SCREEN_EVENT_TYPE_VISIBILITY:
      INIT_RESULT (1);
      WRITE_FLAG     (event->event.visibility.show_p);
      break;
    default:
      result = SHARP_F;
      break;
    }
  return (result);
}

/* Primitives for Edwin Screens */
#define GETSCREEN(x) ((SCREEN) (GetWindowLong (x, 0)))

DEFINE_PRIMITIVE ("WIN32-SCREEN-CLEAR-RECTANGLE!", Prim_win32_screen_clear_rectangle, 6, 6,
  "(hwnd xl xh yl yh attribute)")
{
  PRIMITIVE_HEADER (6);
  {
    HWND  hwnd = (HWND) arg_integer (1);
    SCREEN  screen = GETSCREEN ((HWND) hwnd);

    Screen_SetAttributeDirect (screen, (SCREEN_ATTRIBUTE) arg_integer (6));
    clear_screen_rectangle (screen,
			    arg_integer(4), arg_integer(2),
			    arg_integer(5), arg_integer(3));
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

DEFINE_PRIMITIVE ("WIN32-SCREEN-INVALIDATE-RECT!", Prim_win32_screen_invalidate_rect, 5, 5, 0)
{
  PRIMITIVE_HEADER (5);
  {
    RECT rect;
    HWND  handle = (HWND) arg_integer (1);
    SCREEN screen = GETSCREEN (handle);

    Screen_CR_to_RECT (&rect, screen, arg_integer (4), arg_integer (2),
		       arg_integer (5), arg_integer (3));

    InvalidateRect (handle, &rect, FALSE);
    PRIMITIVE_RETURN(UNSPECIFIC);
  }
}

DEFINE_PRIMITIVE ("WIN32-SCREEN-VERTICAL-SCROLL!", Prim_win32_screen_vertical_scroll, 6, 6,
  "(handle xl xu yl yu amount)")
{
  PRIMITIVE_HEADER (6);
  {
    SCREEN screen = GETSCREEN ((HWND) arg_integer (1));
    int position = arg_integer (6);

    scroll_screen_vertically (screen, arg_integer (4), arg_integer (2),
			      arg_integer (5), arg_integer (3), position);

    PRIMITIVE_RETURN(UNSPECIFIC);
  }
}

DEFINE_PRIMITIVE ("WIN32-SCREEN-WRITE-CHAR!", Prim_win32_screen_write_char, 5, 5,
  "(handle x y char attribute)")
{
  PRIMITIVE_HEADER (5);
  {
    SCREEN screen = GETSCREEN ((HWND) arg_integer (1));

    if (!screen)
      error_bad_range_arg (1);

    Screen_SetAttributeDirect (screen, (SCREEN_ATTRIBUTE) arg_integer (5));
    Screen_SetPosition (screen, arg_integer (3), arg_integer (2));
    Screen_WriteCharUninterpreted (screen, (char) arg_integer (4), 0);
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

DEFINE_PRIMITIVE ("WIN32-SCREEN-WRITE-SUBSTRING!", Prim_win32_screen_write_substring, 7, 7,
 "(handle x y string start end attribute)")
{
  PRIMITIVE_HEADER (7);
  {
    SCREEN screen = GETSCREEN ((HWND) arg_integer (1));
    int  start = arg_nonnegative_integer (5);
    int  end   = arg_nonnegative_integer (6);

    if (!screen)
      error_bad_range_arg (1);
    CHECK_ARG (4, STRING_P);
    if (start > STRING_LENGTH (ARG_REF (4)))
      error_bad_range_arg (5);
    if (end > STRING_LENGTH (ARG_REF (4)))
      error_bad_range_arg (6);
    Screen_SetAttributeDirect (screen, (SCREEN_ATTRIBUTE) arg_integer (7));
    WriteScreenBlock_NoInvalidRect (screen,
				    arg_integer (3), arg_integer (2),
				    ((LPSTR) STRING_ARG (4))+start,
				    end-start);
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

DEFINE_PRIMITIVE ("WIN32-SCREEN-MOVE-CURSOR!", Prim_win32_screen_move_cursor, 3, 3,
  "(handle x y)")
{
  PRIMITIVE_HEADER (3);
  {
    SCREEN screen = GETSCREEN ((HWND) arg_integer (1));

    Screen_SetPosition (screen, arg_integer (3), arg_integer (2));

    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

DEFINE_PRIMITIVE ("WIN32-SCREEN-CHAR-DIMENSIONS",  Prim_win32_screen_char_dimensions, 1, 1,
  "(handle)\n\
Returns pair (width . height).")
{
  PRIMITIVE_HEADER (1);
  {
    HWND handle = ((HWND) (arg_integer (1)));
    int xchar;
    int ychar;
    screen_char_dimensions (handle, (&xchar), (&ychar));
    PRIMITIVE_RETURN
      (cons ((long_to_integer (xchar)), (long_to_integer (ychar))));
  }
}

DEFINE_PRIMITIVE ("WIN32-SCREEN-SIZE",  Prim_win32_screen_size, 1, 1,
  "(handle)\n\
Returns pair (width . height).")
{
  PRIMITIVE_HEADER (1);
  {
    HWND handle = (HWND) arg_integer (1);
    int width=0, height=0;
    Screen_GetSize (handle, &height, &width);
    PRIMITIVE_RETURN
      (cons (long_to_integer (width), long_to_integer (height)));
  }
}

DEFINE_PRIMITIVE ("WIN32-SET-SCREEN-SIZE",  Prim_win32_set_screen_size, 3, 3,
  "(handle width height)")
{
  PRIMITIVE_HEADER (3);
  {
    HWND handle = ((HWND) (arg_integer (1)));
    int xchar;
    int ychar;
    screen_char_dimensions (handle, (&xchar), (&ychar));
    PRIMITIVE_RETURN
      (cons ((long_to_integer (xchar)), (long_to_integer (ychar))));
  }
}

DEFINE_PRIMITIVE ("WIN32-SCREEN-CREATE!", Prim_win32_screen_create, 2, 2,
  "(parent-handle modes)")
{
  PRIMITIVE_HEADER (2);
  {
    HWND hwnd = Screen_Create ((HANDLE) arg_integer (1),
			       "Scheme Screen",
			       (int) SW_SHOWNA);

    if (hwnd != 0)
      SendMessage (hwnd, SCREEN_SETMODES,
		   (WPARAM) arg_integer (2), (LPARAM) 0);

    PRIMITIVE_RETURN (hwnd ? long_to_integer ((long) hwnd) : SHARP_F);
  }
}

DEFINE_PRIMITIVE ("WIN32-SCREEN-SHOW-CURSOR!", Prim_win32_screen_show_cursor, 2, 2,
  "(handle show?)")
{
  PRIMITIVE_HEADER (2);
  {
    SCREEN screen = GETSCREEN ((HWND) arg_integer (1));
    Enable_Cursor (screen, (ARG_REF (2) == SHARP_F) ? FALSE : TRUE);
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

DEFINE_PRIMITIVE ("WIN32-SCREEN-SET-ICON!", Prim_win32_screen_set_icon, 2, 2,
  "(screen-handle icon-handle)")
{
  PRIMITIVE_HEADER (2);
  {
    SCREEN screen = GETSCREEN ((HWND) arg_integer (1));
    HICON  result = ScreenSetIcon (screen, (HICON) arg_integer (2));
    PRIMITIVE_RETURN (ulong_to_integer((unsigned long) result));
  }
}

DEFINE_PRIMITIVE ("WIN32-SCREEN-CURRENT-FOCUS", Prim_win32_screen_current_focus, 0, 0,
  "() -> hwnd")
{
  PRIMITIVE_HEADER (0);
  {
    PRIMITIVE_RETURN (ulong_to_integer((unsigned long) ScreenCurrentFocus()));
  }
}

DEFINE_PRIMITIVE ("WIN32-SCREEN-SET-DEFAULT-FONT!", Prim_win32_screen_set_default_font, 1, 1,
  "(font-name)")
{
  PRIMITIVE_HEADER (1);
  {
    BOOL rc = ScreenSetDefaultFont (STRING_ARG (1));
    PRIMITIVE_RETURN ( rc ? SHARP_T : SHARP_F);
  }
}

DEFINE_PRIMITIVE ("WIN32-SCREEN-SET-FONT!", Prim_win32_screen_set_font, 2, 2,
  "(screen-handle font-name)")
{
  PRIMITIVE_HEADER (2);
  {
    SCREEN  screen = GETSCREEN ((HWND) arg_integer (1));
    if (!screen) error_bad_range_arg (1);
    PRIMITIVE_RETURN ( ScreenSetFont (screen, STRING_ARG (2))
		      ? SHARP_T : SHARP_F);
  }
}

DEFINE_PRIMITIVE ("WIN32-SCREEN-SET-FOREGROUND-COLOR!", Prim_win32_screen_set_foreground_color, 2, 2,
  "(screen-handle rgb)")
{
  PRIMITIVE_HEADER (2);
  {
    SCREEN  screen = GETSCREEN ((HWND) arg_integer (1));
    if (!screen) error_bad_range_arg (1);
    PRIMITIVE_RETURN ( ScreenSetForegroundColour (screen, arg_integer (2))
		      ? SHARP_T : SHARP_F);
  }
}

DEFINE_PRIMITIVE ("WIN32-SCREEN-SET-BACKGROUND-COLOR!", Prim_win32_screen_set_background_color, 2, 2,
  "(screen-handle rgb)")
{
  PRIMITIVE_HEADER (2);
  {
    SCREEN  screen = GETSCREEN ((HWND) arg_integer (1));
    if (!screen) error_bad_range_arg (1);
    PRIMITIVE_RETURN ( ScreenSetBackgroundColour (screen, arg_integer (2))
		      ? SHARP_T : SHARP_F);
  }
}

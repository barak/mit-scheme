/* -*-C-*-

$Id: ntscreen.c,v 1.50 2003/02/14 18:48:12 cph Exp $

Copyright 1993-2002 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

*/

/* #include <stdio.h> */
#include <stdlib.h>
#include "nt.h"
#include "ntscreen.h"
#include "ntgui.h"
#include <windowsx.h>

/* constant definitions */

#define GWL_SCREEN        0
#define SCREENEXTRABYTES        (sizeof(LONG))

#define MAXCOLS 180
#define MAXROWS 100

/* ascii definitions */

#define ASCII_BEL       (0x07)
#define ASCII_BS        (0x08)
#define ASCII_LF        (0x0A)
#define ASCII_FF        (0x0C)
#define ASCII_CR        (0x0D)
#define ASCII_ESC       (0x1B)
#define ASCII_DEL       (0x7F)

#define ASCII_CONTROLIFY(ascii) ((ascii) - '@')
#define ASCII_METAFY(ascii)     ((ascii) | 0200)

/* data structures */

#ifndef MAX_FREE_EVENTS
#define MAX_FREE_EVENTS 1024
#endif

typedef struct tagSCREEN_EVENT_LINK
{
  SCREEN_EVENT event;
  struct tagSCREEN_EVENT_LINK * next;
} SCREEN_EVENT_LINK;

#define MAX_COMMANDS 30

#define MAX_BINDINGS 10

#define MAX_LINEINPUT 1024

#define COMPUTE_SCROLL_LINES(height)    ((((height) * 2) + 4) / 5)

typedef struct tagSCREENINFO
{
   SCREEN  registry_link;

   HWND    hWnd;
   HICON   hIcon;

   char              * chars;
   SCREEN_ATTRIBUTE  * attrs;
   unsigned long mode_flags;	/* event types & modes */
   SCREEN_ATTRIBUTE  write_attribute;

   BOOL    cursor_visible;
   BOOL    has_focus;

   HFONT   hFont;
   LOGFONT lfFont;
   DWORD   rgbFGColour;
   DWORD   rgbBGColour;
   int     xSize, ySize;
   int     xScroll, yScroll;
   int     xOffset, yOffset; /* coords of top left corner of client area wrt */
                             /* character area */
   int     column, row;      /* caret position */
   int     xChar, yChar;     /* size of characters in pixels */
   int     width, height;    /* size of text ares in characters */

   int n_commands;
   struct
   {
     WORD wID;
     COMMAND_HANDLER thunk;
   } commands[MAX_COMMANDS];

   int n_bindings;
   struct
   {
     char   key;
     WORD   command;
   } bindings[MAX_BINDINGS];

   /* for line input */
   int n_chars;
   char * line_buffer;

   /* ANSI emulator overflow */
   int n_pending;
   LPSTR pending;

   HBRUSH bkgnd_brush;
   int scroll_lines;

} SCREEN_STRUCT;

/* #define WIDTH(screen) (screen->width) */
#define WIDTH(screen) MAXCOLS
#define HEIGHT(screen) MAXROWS
/* macros ( for easier readability ) */

#define GETSCREEN( x ) ((SCREEN) GetWindowLong( x, GWL_SCREEN ))
#define SETSCREEN( x, y ) SetWindowLong( x, GWL_SCREEN, (LONG) y )

/* CRT mappings to NT API */

#define _fmemset   memset
#define _fmemmove  memmove

static LRESULT CreateScreenInfo (HWND);
static VOID DestroyScreenInfo (HWND);
static BOOL ResetScreen (SCREEN);
extern BOOL KillScreenFocus (HWND);
static VOID PaintScreen (HWND);
/* static VOID EraseScreen (HWND, HDC); */
static BOOL SetScreenFocus (HWND);
static BOOL ScrollScreenHorz (HWND, WORD, WORD);
static BOOL ScrollScreenVert (HWND, WORD, WORD);
static BOOL SizeScreen (HWND, WORD, WORD);
static BOOL handle_window_pos_changing (HWND, LPWINDOWPOS);
static void reset_modifiers (void);
static void record_modifier_transition (WPARAM, LPARAM, BOOL);
static int process_keydown (HWND, UINT, WPARAM, LPARAM);
static void process_character (HWND, UINT, WPARAM, LPARAM);
static VOID ProcessMouseButton (HWND, UINT, UINT, LONG, BOOL);
static VOID ProcessCloseMessage (SCREEN);
static void process_focus_message (HWND, int);
static void process_show_message (HWND, int);
static BOOL WriteScreenBlock (HWND, LPSTR, int);
static int  ReadScreen (SCREEN, char*, int);
static VOID MoveScreenCursor (SCREEN);
extern UINT ScreenPeekOrRead
  (SCREEN, int count, SCREEN_EVENT* buffer, BOOL remove);
extern void flush_typeahead (SCREEN);
static COMMAND_HANDLER ScreenSetCommand
  (SCREEN, WORD cmd, COMMAND_HANDLER handler);
static WORD ScreenSetBinding (SCREEN, char key, WORD command);
static VOID GetMinMaxSizes(HWND,LPPOINT,LPPOINT);
static BOOL AdjustedSize (SCREEN,int*,int*);
extern VOID Screen_Clear (SCREEN,int);
static BOOL SelectScreenFont (SCREEN, HWND);
static BOOL SelectScreenBackColor (SCREEN, HWND);

static HFONT set_font_1 (char *, LOGFONT *);
static BOOL parse_logfont (char *, LOGFONT *);
static long points_to_logical_units (long);
static BOOL search_for_font (LOGFONT *);
static int CALLBACK search_for_font_proc
  (ENUMLOGFONT *, NEWTEXTMETRIC *, int, LPARAM);

extern LRESULT ScreenCommand_ChooseFont (HWND, WORD);
extern LRESULT ScreenCommand_ChooseBackColor (HWND, WORD);

static SCREEN_EVENT * allocate_event (SCREEN, SCREEN_EVENT_TYPE);
static int read_event (SCREEN, SCREEN_EVENT_TYPE, int, SCREEN_EVENT *);

/* void *xmalloc (int size); */
/* void xfree (void*); */
#define xfree free
#define xmalloc malloc

extern LRESULT FAR CALLBACK ScreenWndProc (HWND, UINT, WPARAM, LPARAM);

static VOID RegisterScreen (SCREEN);
static VOID UnregisterScreen (SCREEN);

static const char * translate_message_code (UINT);

/* FILE GLOBAL VARIABLES */

static  HANDLE  ghInstance;
static  HICON   ghDefaultIcon;

static  LOGFONT lfDefaultLogFont;

static unsigned int n_free_events;
static SCREEN_EVENT_LINK * free_events;
static SCREEN_EVENT_LINK * event_queue_head;
static SCREEN_EVENT_LINK * event_queue_tail;

FILE * win32_trace_file;
unsigned long win32_trace_level;

static long
screen_x_extra (SCREEN screen)
{
  return ((GetSystemMetrics (SM_CXFRAME)) * 2);
}

static long
screen_y_extra (SCREEN screen)
{
  return (((GetSystemMetrics (SM_CYFRAME)) * 2)
	  + (GetSystemMetrics (SM_CYCAPTION))
	  + ((GetMenu (screen -> hWnd)) ? (GetSystemMetrics (SM_CYMENU)) : 0)
#ifdef __WATCOMC__
	  /* Magic: when the combination of cyframe*2 and cycaption is
	     28, AdjustWindowRect indicates that it should be 27.  I
	     don't know why this only happens under Watcom.  */
	  - 1
#endif
	  );
}

static long
pixel_to_char_width (SCREEN screen, long pixel_width)
{
  return
    (((pixel_width - (screen_x_extra (screen))) + (screen -> xOffset))
     / (screen -> xChar));
}

static long
pixel_to_char_height (SCREEN screen, long pixel_height)
{
  return
    (((pixel_height - (screen_y_extra (screen))) + (screen -> yOffset))
     / (screen -> yChar));
}

static long
char_to_pixel_width (SCREEN screen, long char_width)
{
  return
    (((char_width * (screen -> xChar)) - (screen -> xOffset))
     + (screen_x_extra (screen)));
}

static long
char_to_pixel_height (SCREEN screen, long char_height)
{
  return
    (((char_height * (screen -> yChar)) - (screen -> yOffset))
     + (screen_y_extra (screen)));
}

static void
init_LOGFONT (LOGFONT *lf)
{
    lf->lfHeight =         0;
    lf->lfWidth =          0;
    lf->lfEscapement =     0;
    lf->lfOrientation =    0;
    lf->lfWeight =         FW_NORMAL;
    lf->lfItalic =         0;
    lf->lfUnderline =      0;
    lf->lfStrikeOut =      0;
    lf->lfCharSet =        ANSI_CHARSET;
    lf->lfOutPrecision =   OUT_RASTER_PRECIS;
    lf->lfClipPrecision =  CLIP_DEFAULT_PRECIS;
    lf->lfQuality =        PROOF_QUALITY;
    lf->lfPitchAndFamily = FIXED_PITCH | FF_DONTCARE;
    lstrcpy (lf->lfFaceName, "");
}

static BOOL
init_color (char *color_symbol, HWND hWnd, DWORD *color)
{
  HDC hdc;
  char * envvar = getenv (color_symbol);
  if (envvar == NULL)
    return  FALSE;
  /* Use GetNearestColor to ensure consistency with the background
     text color. */
  hdc = GetDC (hWnd);
  *color = GetNearestColor (hdc, strtoul (envvar, NULL, 0));
  ReleaseDC (hWnd, hdc);
  return  TRUE;
}

static BOOL
init_geometry (char *geom_symbol, int *params)
{
  int ctr;
  char * token;
  char * envvar = getenv (geom_symbol);
  char tempvar[100];

  if (envvar == NULL)
    return  FALSE;

  envvar = lstrcpy (tempvar, envvar);

  for (ctr = 0, token = (strtok (envvar, ",;*+ \t\n"));
       ((ctr < 4) && (token != ((char *) NULL)));
       ctr++, token = (strtok (((char *) NULL), ",;*+ \t\n")))
    params[ctr] = strtoul (token, NULL, 0);
  return  FALSE;
}

#ifdef WINDOWSLOSES

static BOOL
  MIT_trap_alt_tab = ((BOOL) 1),
  MIT_trap_alt_escape = ((BOOL) 1);

static VOID
init_flag (char *flag_symbol, BOOL *flag)
{
  extern int strcmp_ci (char *, char *);
  char *envvar = getenv (flag_symbol);
  if (envvar != NULL)
  {
    if ((strcmp_ci (envvar, "true")) || (strcmp_ci (envvar, "yes")))
      *flag = (BOOL) 1;
    else if ((strcmp_ci (envvar, "false")) || (strcmp_ci (envvar, "no")))
      *flag = (BOOL) 0;
  }
}

VOID
init_MIT_Keyboard (VOID)
{
  init_flag ("MITSCHEME_TRAP_ALT_TAB", (& MIT_trap_alt_tab));
  init_flag ("MITSCHEME_TRAP_ALT_ESCAPE", (& MIT_trap_alt_escape));
}
#endif /* WINDOWSLOSES */

/* BOOL Screen_InitApplication (HANDLE hInstance)

   Description:
     First time initialization stuff for screen class.
     This registers information such as window classes.

   Parameters:
     HANDLE hInstance
        Handle to this instance of the application.
*/

BOOL
Screen_InitApplication (HANDLE hInstance)
{
   WNDCLASSEX wndclass;
   char * font_name = getenv ("MITSCHEME_FONT");

   init_LOGFONT (&lfDefaultLogFont);
   if (font_name)
     ScreenSetDefaultFont (font_name);

   win32_trace_file = 0;
   win32_trace_level = 0;

#ifdef WINDOWSLOSES
   init_MIT_Keyboard ();
#endif /* WINDOWSLOSES */

   wndclass.cbSize =        (sizeof (wndclass));
   wndclass.style =         0;
   wndclass.lpfnWndProc =   ScreenWndProc;
   wndclass.cbClsExtra =    0;
   wndclass.cbWndExtra =    SCREENEXTRABYTES;
   wndclass.hInstance =     hInstance;
   wndclass.hIcon =         (LoadIcon (hInstance, "SHIELD3_ICON"));
   wndclass.hCursor =       (LoadCursor (NULL, IDC_ARROW));
   wndclass.hbrBackground = 0;
   wndclass.lpszMenuName =  0;
   wndclass.lpszClassName = "MIT-SCREEN";
   wndclass.hIconSm =       (wndclass . hIcon);

   n_free_events = 0;
   free_events = 0;
   event_queue_head = 0;
   event_queue_tail = 0;

   return (RegisterClassEx (&wndclass));
}

/* BOOL Screen_InitInstance (HANDLE hInstance, int nCmdShow )

   Description:
      Initializes instance specific information for the screen class.
      returns TRUE on success.

   Parameters:
      HANDLE hInstance
         Handle to instance

      int nCmdShow
         How do we show the window?
*/

BOOL
Screen_InitInstance (HANDLE hInstance, int nCmdShow )
{
  ghInstance = hInstance;
  ghDefaultIcon = LoadIcon (hInstance, "SHIELD2_ICON");
  return  TRUE;
}

/* SCREEN  Screen_Create (HANDLE hParent, LPCSTR title, int nCmdShow)

   Description:
      Create a screen window with a given parent.

   Parameters:
      hParent
         Handle to parent window
*/

static int def_params[4] =
{
  CW_USEDEFAULT,                /* Left */
  CW_USEDEFAULT,                /* Top */
  CW_USEDEFAULT,                /* Width */
  CW_USEDEFAULT                 /* Height */
};

HANDLE
Screen_Create (HANDLE hParent, LPCSTR title, int nCmdShow)
{
  HWND hwnd;
  int ctr, params[4] = {-1, -1, -1, -1};

  if (hParent == ((HANDLE) NULL))
    init_geometry ("MITSCHEME_GEOMETRY", &params[0]);

  for (ctr = 0; ctr < 4; ctr++)
    if (params[ctr] == -1)
      params[ctr] = def_params[ctr];

  hwnd = CreateWindow ("MIT-SCREEN", title,
		       WS_OVERLAPPEDWINDOW,
		       params[0], params[1],
		       params[2], params[3],
		       hParent, NULL, ghInstance,
		       ((LPVOID) nCmdShow));
  return  hwnd;
}

VOID
Screen_Destroy (BOOL root, HANDLE hwnd)
{
  DestroyWindow (hwnd);
}

/* Registry of screen handles */

static SCREEN registered_screens = 0;

static VOID
RegisterScreen (SCREEN screen)
{
  screen->registry_link = registered_screens;
  registered_screens = screen;
}

static SCREEN*
head_to_registered_screen (HWND hWnd)
{
  SCREEN *link = &registered_screens;
  while (*link)
    if ((*link)->hWnd == hWnd)
      return  link;
    else
      link = &((*link)->registry_link);
  return  0;
}

static VOID
UnregisterScreen (SCREEN screen)
{
  SCREEN *link = head_to_registered_screen (screen->hWnd);
  /* if (link) */
  *link = screen->registry_link;
}

BOOL
Screen_IsScreenHandle (HANDLE handle)
{
  return  head_to_registered_screen (handle) != 0;
}

/* LRESULT FAR CALLBACK ScreenWndProc (HWND hWnd, UINT uMsg,
                                       WPARAM wParam, LPARAM lParam )

   This is the TTY Window Proc.  This handles ALL messages to the tty
   window.
*/

LRESULT FAR CALLBACK
ScreenWndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
   SCREEN  screen = GETSCREEN (hWnd);

   /* Ignore common but uninteresting messages.  */
   if (win32_trace_level
       > (((uMsg == WM_SCHEME_INTERRUPT)
	   || (uMsg == WM_PAINT)
	   || (uMsg == WM_TIMER)
	   || (uMsg == WM_NCHITTEST)
	   || (uMsg == WM_SETCURSOR)
	   || (uMsg == WM_MOUSEMOVE))
	  ? 2
	  : 0))
     {
       const char * name = (translate_message_code (uMsg));
       fprintf (win32_trace_file, "ScreenWndProc: ");
       fprintf (win32_trace_file, "hWnd=0x%x, ", hWnd);
       if (name)
	 fprintf (win32_trace_file, "uMsg=%s, ", name);
       else
	 fprintf (win32_trace_file, "uMsg=0x%x, ", uMsg);
       fprintf (win32_trace_file, "wParam=0x%x, lParam=0x%x\n",
		wParam, lParam);
       fflush (win32_trace_file);
     }
   switch (uMsg)
     {
     case WM_CREATE:
       {
	 LRESULT result = CreateScreenInfo (hWnd);
	 ShowWindow (hWnd,
		     ((int) ((LPCREATESTRUCT) lParam) -> lpCreateParams));
	 UpdateWindow (hWnd);
	 return  result;
       }

     case WM_SCHEME_INTERRUPT:
       return (0);

     case SCREEN_SETPOSITION:
       return  (LRESULT)Screen_SetPosition
	 (screen, HIWORD(lParam), LOWORD(lParam));

     case SCREEN_GETPOSITION:
       return  MAKELRESULT(screen->column, screen->row);

     case SCREEN_SETATTRIBUTE:
       screen->write_attribute = (SCREEN_ATTRIBUTE) wParam;
       return  0;

     case SCREEN_GETATTRIBUTE:
       return  (LRESULT) screen->write_attribute;

     case SCREEN_SETMODES:
       (screen -> mode_flags) = ((unsigned long) wParam);
       return (0);

     case SCREEN_GETMODES:
       return  (LRESULT) screen->mode_flags;

     case SCREEN_SETCOMMAND:
       return  (LRESULT)
	 ScreenSetCommand(screen, LOWORD(wParam), (COMMAND_HANDLER)lParam);

     case SCREEN_GETCOMMAND:
       return  (LRESULT)
	 ScreenSetCommand(screen, LOWORD(wParam), (COMMAND_HANDLER)-1);

     case SCREEN_SETBINDING:
       return  (LRESULT)
	 ScreenSetBinding(screen, LOBYTE(wParam), (WORD)lParam);

     case SCREEN_GETBINDING:
       return  (LRESULT)
	 ScreenSetBinding(screen, LOBYTE(wParam), (WORD)-1);

     case SCREEN_PEEKEVENT:
       return  (LRESULT)
	 ScreenPeekOrRead(screen, (int)wParam, (SCREEN_EVENT*)lParam, FALSE);

     case SCREEN_READEVENT:
       return  (LRESULT)
	 ScreenPeekOrRead(screen, (int)wParam, (SCREEN_EVENT*)lParam, TRUE);

     case SCREEN_WRITE:
       return  (LRESULT)WriteScreenBlock (hWnd, (LPSTR)lParam, (int)wParam);

     case SCREEN_READ:
       return  (LRESULT)ReadScreen (screen, (LPSTR)lParam, (int)wParam);

     case SCREEN_SETMENU:
       Screen_SetMenu (hWnd, (HMENU)lParam);
       return  0L;

     case SCREEN_CLEAR:
       Screen_Clear (screen, (int)wParam);
       return  0L;

     case WM_MOUSEACTIVATE:
       if ((LOWORD (lParam)) == HTCLIENT)
	 return (MA_ACTIVATEANDEAT);
       break;

     case WM_LBUTTONDOWN:
     case WM_MBUTTONDOWN:
     case WM_RBUTTONDOWN:
       if (IsIconic (hWnd)) goto use_default;
       ProcessMouseButton (hWnd, uMsg, wParam, lParam, FALSE);
       break;

     case WM_LBUTTONUP:
     case WM_MBUTTONUP:
     case WM_RBUTTONUP:
       if (IsIconic (hWnd)) goto use_default;
       ProcessMouseButton (hWnd, uMsg, wParam, lParam, TRUE);
       break;

     case WM_COMMAND:
     case WM_SYSCOMMAND:
       {
	 WORD  wID = LOWORD (wParam);
	 int  i;
	 for (i=0;  i<screen->n_commands; i++)
	   if (screen->commands[i].wID == wID)
	     {
	       LRESULT intrpt = (screen->commands[i].thunk(hWnd, wID));

	       if (intrpt)
		 flush_typeahead (screen);
	       return  intrpt;
	     }
	 return  DefWindowProc (hWnd, uMsg, wParam, lParam);
       }
       break;

     case WM_GETMINMAXINFO:
       {
	 LPMINMAXINFO info = ((LPMINMAXINFO) lParam);
	 GetMinMaxSizes (hWnd, &info->ptMinTrackSize, &info->ptMaxTrackSize);
       }
       break;

     case WM_PAINT:
       PaintScreen (hWnd);
       break;

     case WM_ERASEBKGND:
       /* We now do this in PaintScreen as it reduces flicker after
	  resizing.  */
       break;

     case WM_QUERYDRAGICON:
       return (LRESULT) (screen->hIcon ? screen->hIcon : ghDefaultIcon);

     case WM_SIZE:
       if (wParam!=SIZE_MINIMIZED)
	 SizeScreen (hWnd, HIWORD(lParam), LOWORD(lParam));
       break;

     HANDLE_MSG (hWnd, WM_WINDOWPOSCHANGING, handle_window_pos_changing);

     case WM_HSCROLL:
       ScrollScreenHorz (hWnd, LOWORD(wParam), HIWORD(wParam));
       break;

     case WM_VSCROLL:
       ScrollScreenVert (hWnd, LOWORD(wParam), HIWORD(wParam));
       break;

     case WM_SYSKEYDOWN:
     case WM_KEYDOWN:
       record_modifier_transition (wParam, lParam, 1);
       if ((IsIconic (hWnd))
	   || (!process_keydown (hWnd, uMsg, wParam, lParam)))
	 goto use_default;
       break;

     case WM_SYSKEYUP:
     case WM_KEYUP:
       record_modifier_transition (wParam, lParam, 0);
       goto use_default;

     case WM_SYSCHAR:
     case WM_CHAR:
       if (IsIconic (hWnd)) goto use_default;
       process_character (hWnd, uMsg, wParam, lParam);
       break;

      case WM_NCACTIVATE:
       /* Windows doesn't send us focus messages when putting up and
	  taking down a system popup dialog as for Ctrl-Alt-Del on
	  Windows 95. The only indication we get that something
	  happened is receiving this message afterwards.  So this is a
	  good time to reset our keyboard modifiers' state. */
       reset_modifiers ();
       goto use_default;

     case WM_INPUTLANGCHANGE:
       /* Clear dead keys in the keyboard state; for simplicity only
	  preserve modifier key states.  */
       {
	 BYTE keystate [256];
	 GetKeyboardState (keystate);
	 {
	   unsigned int i;
	   for (i = 0; (i < 256); i += 1)
	     switch (i)
	       {
	       case VK_SHIFT:
	       case VK_LSHIFT:
	       case VK_RSHIFT:
	       case VK_CAPITAL:
	       case VK_NUMLOCK:
	       case VK_SCROLL:
	       case VK_CONTROL:
	       case VK_LCONTROL:
	       case VK_RCONTROL:
	       case VK_MENU:
	       case VK_LMENU:
	       case VK_RMENU:
	       case VK_LWIN:
	       case VK_RWIN:
		 (keystate[i]) = 0;
		 break;
	       }
	 }
	 SetKeyboardState (keystate);
       }
      goto use_default;

     case WM_SETFOCUS:
       SetScreenFocus (hWnd);
       reset_modifiers ();
       process_focus_message (hWnd, 1);
       goto use_default;

     case WM_KILLFOCUS:
       KillScreenFocus (hWnd);
       process_focus_message (hWnd, 0);
       goto use_default;

     case WM_SHOWWINDOW:
       process_show_message (hWnd, ((int) wParam));
       goto use_default;

     case WM_DESTROY:
       DestroyScreenInfo (hWnd);
       break;

     case WM_CATATONIC:
       {
	 extern void catatonia_trigger (void);
	 catatonia_trigger ();
       }
       break;

     case WM_CLOSE:
       {
	 extern HANDLE master_tty_window;

	 if (!(screen->mode_flags & SCREEN_EVENT_TYPE_CLOSE))
	   {
	     if (IDOK !=
		 MessageBox (hWnd,
			     hWnd==(HWND)master_tty_window
			     ? ("Closing this window will terminate Scheme.\n"
				"Changes to Edwin buffers might be lost.\n"
				"\n"
				"Really Exit Scheme?")
			     : "OK to close this window?",
			     "MIT/GNU Scheme",
			     (MB_ICONQUESTION | MB_OKCANCEL)))
	       break;
	   }
	 else
	   {
	     ProcessCloseMessage (screen);
	     break;
	   }

	 if (hWnd == ((HWND) master_tty_window))
	   termination_normal (0);
	 goto use_default;
       }

#ifdef USE_WM_TIMER
     case WM_TIMER:
       {
	 extern void TimerProc (HWND, UINT, UINT, DWORD);
	 TimerProc (hWnd, uMsg, wParam, lParam);
       }
       break;
#endif /* USE_WM_TIMER */

     case WM_HOTKEY:
       {
	 extern int signal_keyboard_character_interrupt (int);
	 signal_keyboard_character_interrupt (-2);
       }

     use_default:
     default:
       return (DefWindowProc (hWnd, uMsg, wParam, lParam));
     }
   return (0L);
}

static VOID
ClearScreen_internal (SCREEN screen)
{
  screen->row                   = 0;
  screen->column                = 0;
  _fmemset (screen->chars, ' ', MAXROWS * MAXCOLS);
  _fmemset (screen->attrs, screen->write_attribute,
	    MAXROWS * MAXCOLS * sizeof(SCREEN_ATTRIBUTE));
}

/* LRESULT CreateScreenInfo (HWND hWnd)

   Description:
      Creates the tty information structure and sets
      menu option availability.  Returns -1 if unsuccessful.

   Parameters:
      HWND  hWnd
*/

static LRESULT
CreateScreenInfo (HWND hWnd)
{
   HMENU   hMenu;
   SCREEN  screen;

   if (NULL == (screen =
		(SCREEN) LocalAlloc (LPTR, sizeof(SCREEN_STRUCT) )))
      return  (LRESULT) -1;

   screen->hWnd                 = hWnd;
   screen->hIcon                =
     LoadIcon ((HINSTANCE) GetWindowLong(hWnd,GWL_HINSTANCE), "SHIELD3_ICON");
   screen->chars                = NULL;
   screen->attrs                = NULL;
   screen->write_attribute      = 0;
   screen->cursor_visible       = TRUE;
   screen->has_focus            = TRUE;
#if 0
   screen->mode_flags           = (SCREEN_EVENT_TYPE_MASK
#endif
   screen->mode_flags           = (SCREEN_EVENT_TYPE_KEY
				   | SCREEN_MODE_AUTOWRAP
				   | SCREEN_MODE_ECHO
#if 0
				   | SCREEN_MODE_CR_NEWLINES
#endif
				   | SCREEN_MODE_LINE_INPUT
				   | SCREEN_MODE_PROCESS_OUTPUT
				   | SCREEN_MODE_EAGER_UPDATE
				   | SCREEN_MODE_NEWLINE_CRS);
   screen->xSize                = 0;
   screen->ySize                = 0;
   screen->xScroll              = 0;
   screen->yScroll              = 0;
   screen->xOffset              = 0;
   screen->yOffset              = 0;
   screen->hFont                = NULL;
   if (! (init_color ("MITSCHEME_FOREGROUND", hWnd, &screen->rgbFGColour)))
     screen->rgbFGColour        = GetSysColor (COLOR_WINDOWTEXT);
   if (! (init_color ("MITSCHEME_BACKGROUND", hWnd, &screen->rgbBGColour)))
     screen->rgbBGColour        = GetSysColor (COLOR_WINDOW);
   screen->width                = 0;
   screen->height               = 0;
   screen->scroll_lines         = 1;

   screen->chars = xmalloc (MAXROWS * MAXCOLS);
   screen->attrs = xmalloc (MAXROWS * MAXCOLS * sizeof(SCREEN_ATTRIBUTE));

   /* clear screen space */
   ClearScreen_internal (screen);

   /* setup default font information */
   screen->lfFont = lfDefaultLogFont;

   /* set handle before any further message processing. */
   SETSCREEN (hWnd, screen);
   RegisterScreen (screen);

   screen->n_commands = 0;
   screen->n_bindings = 0;
   /* reset the character information, etc. */

   screen->bkgnd_brush = NULL;
   ResetScreen (screen);

   hMenu = GetSystemMenu (hWnd, FALSE);
   AppendMenu (hMenu, MF_SEPARATOR, 0, 0);
/* AppendMenu (hMenu, MF_STRING, IDM_SETTINGS, "&Settings..."); */
   AppendMenu (hMenu, MF_STRING, SCREEN_COMMAND_CHOOSEFONT, "&Font...");
   AppendMenu (hMenu, MF_STRING, SCREEN_COMMAND_CHOOSEBACKCOLOR,
	       "&Background...");

   SendMessage (hWnd, SCREEN_SETCOMMAND,
		SCREEN_COMMAND_CHOOSEFONT, (LPARAM)ScreenCommand_ChooseFont);
/* SendMessage (hWnd, SCREEN_SETBINDING, 6, SCREEN_COMMAND_CHOOSEFONT); */
   SendMessage (hWnd, SCREEN_SETCOMMAND, SCREEN_COMMAND_CHOOSEBACKCOLOR,
		(LPARAM)ScreenCommand_ChooseBackColor);
/* SendMessage (hWnd, SCREEN_SETBINDING, 7, SCREEN_COMMAND_CHOOSEBACKCOLOR); */

   screen->n_chars = 0;
   screen->line_buffer = xmalloc (MAX_LINEINPUT + 1);

   screen->n_pending = 0;
   screen->pending = ((LPSTR) NULL);
   return  (LRESULT) TRUE;
}

/* VOID DestroyScreenInfo (HWND hWnd )

   Description:
      Destroys block associated with TTY window handle.

   Parameters:
      HWND hWnd
         handle to TTY window
*/

static VOID
DestroyScreenInfo (HWND hWnd)
{
   SCREEN screen = GETSCREEN (hWnd);

   if (NULL == screen)
     return;

   /* KillScreenFocus (hWnd); */
   UnregisterScreen (screen);
   DeleteObject (screen->hFont);

   if (screen->chars)
     xfree (screen->chars);
   if (screen->attrs)
     xfree (screen->attrs);

   LocalFree (screen);
}

/* COMMAND_HANDLER  ScreenSetCommand (SCREEN, WORD cmd, COMMAND_HANDLER h) */

static COMMAND_HANDLER
ScreenSetCommand (SCREEN screen, WORD cmd, COMMAND_HANDLER thunk)
{
    int i;
    for (i = 0; i < screen->n_commands; i++)
      if (screen->commands[i].wID == cmd)
      {
	COMMAND_HANDLER  result = screen->commands[i].thunk;
	if (thunk == 0)
	{
	  /* remove by overwriting with last in list */
	  screen->commands[i] = screen->commands[screen->n_commands-1];
	  screen->n_commands--;
	}
	else if (thunk == ((COMMAND_HANDLER) -1))
	{
	  /* just leave it alone */
	}
	else
	  /* redefine */
	  screen->commands[i].thunk = thunk;
	return  result;
      }

    /* didnt find it */
    if ((thunk == 0) || (thunk == ((COMMAND_HANDLER) -1)))
      return  0;
    /* add new command */
    if (screen->n_commands == MAX_COMMANDS)
      return ((COMMAND_HANDLER) - 1);

    screen->commands[screen->n_commands].wID   = cmd;
    screen->commands[screen->n_commands].thunk = thunk;
    screen->n_commands++;

    return  0;
}

/* WORD  ScreenSetBinding (SCREEN, char key, WORD command) */

static WORD
ScreenSetBinding (SCREEN screen, char key, WORD command)
{
    int i;
    for (i=0; i < screen->n_bindings; i++)
      if (screen->bindings[i].key == key)
      {
	WORD  result = screen->bindings[i].command;
	if (command == 0)
	{
	  /* remove by blatting with last in list */
	  screen->bindings[i] = screen->bindings[screen->n_bindings-1];
	  screen->n_bindings--;
	}
	else if (command == ((WORD) -1))
	{
	  /* let it be */
	}
	else
	  /* redefine */
	  screen->bindings[i].command = command;
	return  result;
      }

    /* no existing binding for key */
    if ((command == 0) || (command == ((WORD) -1)))
      return  0;
    /* add new binding */
    if (screen->n_bindings == MAX_BINDINGS)
      return ((WORD) - 1);

    screen->bindings[screen->n_bindings].key     = key;
    screen->bindings[screen->n_bindings].command = command;
    screen->n_bindings++;

    return  0;
}

/* Standard commands */

LRESULT
ScreenCommand_ChooseFont (HWND hWnd, WORD command)
{
  SCREEN  screen = GETSCREEN (hWnd);
  if (screen == 0)
    return  1L;
  SelectScreenFont (screen, hWnd);
  return  0L;
}

LRESULT
ScreenCommand_ChooseBackColor (HWND hWnd, WORD command)
{
  SCREEN  screen = GETSCREEN (hWnd);
  if (screen == 0)
    return  1L;
  SelectScreenBackColor (screen, hWnd);
  return  0L;
}

VOID
Screen_SetMenu (SCREEN screen, HMENU hMenu)
{
  HMENU hOld = GetMenu (screen->hWnd);
  SetMenu (screen->hWnd, hMenu);
  if (hOld)
    DestroyMenu (hOld);
}

/* BOOL AdjustedSize
    make sure that proposed width & height of screen are ok.
    return TRUE if adjusted, FALSE if OK.  */
static BOOL
AdjustedSize (SCREEN screen, int *width, int *height)
{
  POINT minsz, maxsz;
  GetMinMaxSizes (screen->hWnd, &minsz, &maxsz);
  if (*width<minsz.x || *width>maxsz.x || *height<minsz.y || *height>maxsz.y)
    {
      *width  = min (maxsz.x, max(minsz.x, *width));
      *height = min (maxsz.y, max(minsz.y, *height));
      return (TRUE);
    }
  return (FALSE);
}

/* BOOL ResetScreen (SCREEN  screen)
   Description:
      Resets the SCREEN character information and causes the
      screen to resize to update the scroll information.  */

static BOOL
ResetScreen (SCREEN screen)
{
   HWND        hWnd;
   HDC         hDC;
   TEXTMETRIC  tm;
   RECT        rcWindow;

   if (NULL == screen)
      return  FALSE;

   hWnd = screen->hWnd;

   if (screen->hFont)
     DeleteObject (screen->hFont);

   screen->hFont = CreateFontIndirect (&screen->lfFont);

   hDC = GetDC (hWnd);
   SelectObject (hDC, screen->hFont);
   GetTextMetrics (hDC, &tm);
   ReleaseDC (hWnd, hDC);

   screen->xChar = tm.tmAveCharWidth;
   screen->yChar = tm.tmHeight /* + tm.tmExternalLeading */;

   /* a slimy hack to make the caret the correct size: un- and re- focus */
   if (screen->cursor_visible) {
     KillScreenFocus (hWnd);
     SetScreenFocus (hWnd);
   }

   if (screen->bkgnd_brush != NULL)
     DeleteObject (screen->bkgnd_brush);
   screen->bkgnd_brush = CreateSolidBrush (screen->rgbBGColour);

   /* a slimy hack to force the scroll position, region to */
   /* be recalculated based on the new character sizes ???? */

   /* Veto screens that are too small or too large */
   {
     int width, height;
     GetWindowRect (hWnd, &rcWindow);
     width  = (rcWindow.right - rcWindow.left - (screen_x_extra (screen)));
     height = (rcWindow.bottom - rcWindow.top - (screen_y_extra (screen)));
     if (AdjustedSize (screen, &width, &height))
       MoveWindow (hWnd, rcWindow.left, rcWindow.top, width, height, TRUE);
     else
       SendMessage (hWnd, WM_SIZE, SIZENORMAL,
		    ((LPARAM) (MAKELONG (width,height))));
   }
   return  TRUE;
}

static VOID
Do_PaintScreen (HWND hWnd, SCREEN screen, HDC hDC, PAINTSTRUCT * ps)
{
  RECT          rect;

  int         nRow, nCol, nEndRow, nEndCol, nCount;
  int         nHorzPos, nVertPos, bias;
  HFONT       hOldFont;

  hOldFont = SelectObject (hDC, screen->hFont);
  rect = ps->rcPaint;

  { /* paint the background on the area surrounding the character grid */
    /* left strip */
    if (rect.left < - screen->xOffset) {
      RECT r = rect;
      r.right = -screen->xOffset;
      FillRect (hDC, &r, screen->bkgnd_brush);
    }
    /* right strip */
    if (rect.right > (screen->width * screen->xChar + screen->xOffset)) {
      RECT r = rect;
      r.left = (screen->width * screen->xChar + screen->xOffset);
      FillRect (hDC, &r, screen->bkgnd_brush);
    }
    /* top strip */
    if (rect.top < - screen->yOffset) {
      RECT r = rect;
      r.bottom =  - screen->yOffset;
      FillRect (hDC, &r, screen->bkgnd_brush);
    }
    /* bottom strip */
    if (rect.bottom > (screen->height * screen->yChar + screen->yOffset)) {
      RECT r = rect;
      r.top = (screen->height * screen->yChar + screen->yOffset);
      FillRect (hDC, &r, screen->bkgnd_brush);
    }
  }

  nRow =
    min (screen->height - 1,
	 max (0, (rect.top + screen->yOffset) / screen->yChar));
  nEndRow =
    min (screen->height - 1,
	 max (0, (rect.bottom + screen->yOffset - 1) / screen->yChar));
  nCol =
    min (screen->width - 1,
	 max (0, (rect.left + screen->xOffset) / screen->xChar));
  nEndCol =
    min (screen->width - 1,
	 max (0, (rect.right + screen->xOffset - 1) / screen->xChar));
  nCount = ((nEndCol - nCol) + 1);
  SetBkMode (hDC, OPAQUE);
  SetTextColor (hDC, screen->rgbFGColour);
  SetBkColor (hDC, screen->rgbBGColour);

  for (bias = ((nRow * MAXCOLS) + nCol),
       nVertPos = ((nRow * screen->yChar) - screen->yOffset);
       nRow <= nEndRow;
       nRow++, bias += MAXCOLS, nVertPos += screen->yChar)
  {
    int pos = 0;
    while (pos < nCount)
    {
      /* find consistent run of attributes */
      SCREEN_ATTRIBUTE  *attribp = &screen->attrs[bias + pos];
      SCREEN_ATTRIBUTE  attrib   = *attribp;
      int  nposn = (pos + 1);
      int  run_length;

      while ((nposn < nCount) && (*++attribp == attrib))
	nposn++;

      run_length = (nposn - pos);
      nHorzPos = (((nCol + pos) * screen->xChar) - screen->xOffset);
      rect.top    = nVertPos;
      rect.bottom = nVertPos + screen->yChar;
      rect.left   = nHorzPos;
      rect.right  = nHorzPos + (screen->xChar * run_length);
      if (attrib&1)
      {
	SetTextColor (hDC, screen->rgbBGColour);
	SetBkColor (hDC, screen->rgbFGColour);
      }
      ExtTextOut (hDC, nHorzPos, nVertPos, (ETO_OPAQUE | ETO_CLIPPED),
		  &rect, &screen->chars[bias + pos],
		  run_length, NULL);
#if 0
      if (attrib&2)  /* Bolden by horizontal 1-pixel smear */
        ExtTextOut (hDC, nHorzPos+1, nVertPos, (ETO_CLIPPED),
		    &rect, &screen->chars[bias + pos],
		    run_length, NULL);
#endif
      if (attrib&1)
      {
	SetTextColor (hDC, screen->rgbFGColour);
	SetBkColor (hDC, screen->rgbBGColour);
      }
      pos = nposn;
    }
  }
  SelectObject (hDC, hOldFont);
}

/* VOID PaintScreen (HWND hWnd )

   Description:
      Paints the rectangle determined by the paint struct of
      the DC.

   Parameters:
      HWND hWnd
         handle to TTY window (as always)
*/

static VOID
PaintScreen (HWND hWnd)
{
  SCREEN        screen = GETSCREEN (hWnd);
  HDC           hDC;
  PAINTSTRUCT   ps;

  if (NULL == screen)
    return;

  hDC =  BeginPaint (hWnd, &ps);
  if (IsIconic (hWnd)) {
    DefWindowProc (hWnd, WM_ICONERASEBKGND, (WPARAM) hDC, 0L);
    DrawIcon (hDC, 0, 0, screen->hIcon ? screen->hIcon : ghDefaultIcon);
  } else {
    Do_PaintScreen (hWnd, screen, hDC, &ps);
  }
  EndPaint (hWnd, &ps);
  MoveScreenCursor (screen);
}

#if 0
static VOID
EraseScreen (HWND hWnd, HDC hDC)
{
  SCREEN    screen = GETSCREEN (hWnd);
  RECT      rect;

  if (NULL == screen)
    return;

  if (! (IsIconic (hWnd))) {
    GetClientRect (hWnd, &rect);
    FillRect (hDC, &rect, screen->bkgnd_brush);
  }
}
#endif

static VOID _fastcall
SetCells (SCREEN screen, int row, int col, int count,
	  char ch, SCREEN_ATTRIBUTE attr)
{
  int  address1 = row * MAXCOLS + col;
  int  address2 = address1 + count;
  int  i;
  for (i = address1;  i<address2;  i++)
  {
    screen->chars[i] = ch;
    screen->attrs[i] = attr;
  }
}

static VOID
ScrollScreenBufferUp (SCREEN  screen,  int count)
{
  /* int  total_rows = MAXROWS; */
  int  total_rows = screen->height;
  int  rows_copied = max (total_rows - count, 0);
  count = min (count, total_rows);

  _fmemmove ((LPSTR) (screen->chars),
	     (LPSTR) (screen->chars + count * MAXCOLS),
	     rows_copied * MAXCOLS);
  _fmemmove ((LPSTR) (screen->attrs),
	     (LPSTR) (screen->attrs + count * MAXCOLS),
	     rows_copied * MAXCOLS);
  _fmemset ((LPSTR)(screen->chars + rows_copied * MAXCOLS),
	    ' ', count*MAXCOLS);
  _fmemset ((LPSTR)(screen->attrs + rows_copied * MAXCOLS),
	    screen->write_attribute, count*MAXCOLS);
}

/* BOOL SizeScreen (HWND hWnd, WORD wVertSize, WORD wHorzSize )
   Description:
      Set SCREEN size.  */

static BOOL
SizeScreen (HWND hWnd, WORD wVertSize, WORD wHorzSize )
{
   SCREEN screen = GETSCREEN (hWnd);
   int old_width, old_height;
   unsigned int new_width;
   unsigned int new_height;

   if (NULL == screen)
      return  FALSE;

   if (IsIconic(hWnd)) {
     /* This entire section is a crock to ensure a reasonably sized window
        when Scheme is started minimized.

        Since we protect this procedure against minimizing in the WndProc, we
        can get here only when window is launched in a minimized state.  We
        get here because of the SendMessage in ScreenReset.  Our duty is to
	fake a normal position and size.
        Luckily none of the scrolling business happens because all the cursor
        etc are at zero.  (Hopefully it would be clipped).
     */
     WINDOWPLACEMENT pl;
     int width, height, params[4] = {-1, -1, 0, 0};  /* left,top,width,height*/
     init_geometry ("MITSCHEME_GEOMETRY", &params[0]);
     width   = min (params[2] ? params[2] : 80*screen->xChar,
		    GetSystemMetrics(SM_CXSCREEN));
     height  = min (params[3] ? params[3] : 40*screen->yChar,
		    GetSystemMetrics(SM_CYSCREEN));
     pl.length = (sizeof (pl));
     GetWindowPlacement (hWnd, &pl);
     AdjustedSize (screen, &width, &height);
     pl.rcNormalPosition.left = params[0]==-1 ? 0 : params[0];
     pl.rcNormalPosition.top  = params[1]==-1 ? 0 : params[1];
     pl.rcNormalPosition.bottom = pl.rcNormalPosition.top + height;
     pl.rcNormalPosition.right  = pl.rcNormalPosition.left + width;
     SetWindowPlacement (hWnd, &pl);
     wVertSize = height;
     wHorzSize = width;
   }

   /* if (GetMenu(hWnd)) wVertSize -= GetSystemMetrics(SM_CYMENU); */
   old_width  = screen->width;
   old_height = screen->height;

   new_width  =
     max (1, min ((wHorzSize + screen->xOffset) / screen->xChar, MAXCOLS));
   new_height =
     max (1, min ((wVertSize + screen->yOffset) / screen->yChar, MAXROWS));

   if (new_width > old_width)
   {
     /* Clear out revealed character cells */
     int  row, rows = min (old_height, new_height);
     for (row = 0; row < rows; row++)
       SetCells (screen, row, old_width, new_width-old_width, ' ', 0);
   }

   if (new_height > old_height)
   {
     /* Clear out revealed character cells */
     int  row;
     for (row = old_height; row < new_height; row++)
       SetCells (screen, row, 0, new_width, ' ', 0);
   }
   else if (screen->row >= new_height)
   {
     ScrollScreenBufferUp (screen, ((screen->row - new_height) + 1));
     screen->row = (new_height - 1);
   }

   screen->width  = new_width;
   screen->height = new_height;

   /* scroll window to fit in cursor */
   if (screen->column >= new_width)
   {
     screen->column = 0;
     screen->row += 1;
   }
   if (screen->row >= new_height)
   {
     ScrollScreenBufferUp (screen, 1);
     screen->row = (new_height - 1);
   }
   MoveScreenCursor (screen);

   screen->ySize = (int) wVertSize;
   screen->xSize = (int) wHorzSize;
   screen->yScroll = 0;
   screen->xScroll = 0;

   if ((screen->mode_flags & SCREEN_MODE_EDWIN) == 0)
     screen->scroll_lines = (COMPUTE_SCROLL_LINES (new_height));
   else if (screen->mode_flags & SCREEN_EVENT_TYPE_RESIZE)
     {
       /* queue RESIZE event */
       SCREEN_EVENT * event
	 = (allocate_event (screen, SCREEN_EVENT_TYPE_RESIZE));
       if (event)
	 {
	   event->event.resize.rows = new_height;
	   event->event.resize.columns = new_width;
	 }
     }
   else
     {
       /* Queue a character based resize event */
       SCREEN_EVENT * event = (allocate_event (screen, SCREEN_EVENT_TYPE_KEY));
       if (event)
	 {
	   event->event.key.repeat_count = 1;
	   event->event.key.virtual_keycode = 0;
	   event->event.key.virtual_scancode = 0;
	   event->event.key.ch = SCREEN_EDWIN_RESIZE_COMMAND;
	   event->event.key.control_key_state = 0;
	 }
     }
   /* Cause screen to be redrawn, but if we are under Edwin, don't
      bother as Edwin has to calculate the redisplay anyway.  Well, we
      do bother otherwise we would have to clear the part of the
      screen that is not in a character box.  */
#if 0
   if ((screen->mode_flags & SCREEN_MODE_EDWIN) == 0)
#endif
     {
       InvalidateRect (hWnd, NULL, TRUE);
     }

   return  TRUE;

}

static BOOL
handle_window_pos_changing (HWND hwnd, LPWINDOWPOS wp)
{
  BOOL result = (FORWARD_WM_WINDOWPOSCHANGING (hwnd, wp, DefWindowProc));
  if ((wp -> flags) & SWP_NOSIZE)
    return (result);
  {
    SCREEN screen = (GETSCREEN (hwnd));
    (wp -> cx)
      = (char_to_pixel_width (screen,
			      (pixel_to_char_width (screen, (wp -> cx)))));
    (wp -> cy)
      = (char_to_pixel_height (screen,
			       (pixel_to_char_height (screen, (wp -> cy)))));
  }
  return (0);
}

void
screen_char_dimensions (HWND hwnd, int * xchar, int * ychar)
{
  SCREEN screen = (GETSCREEN (hwnd));
  (*xchar) = (screen -> xChar);
  (*ychar) = (screen -> yChar);
}

/* BOOL ScrollScreenVert (HWND hWnd, WORD wScrollCmd, WORD wScrollPos )

   Description:
      Scrolls TTY window vertically.

   Parameters:
      HWND hWnd
         handle to TTY window

      WORD wScrollCmd
         type of scrolling we're doing

      WORD wScrollPos
         scroll position
*/

static BOOL
ScrollScreenVert (HWND hWnd, WORD wScrollCmd, WORD wScrollPos)
{
   int        nScrollAmt;
   SCREEN     screen = GETSCREEN (hWnd);

   if (NULL == screen)
      return  FALSE;

   switch (wScrollCmd)
   {
      case SB_TOP:
	 nScrollAmt = -screen->yOffset;
	 break;

      case SB_BOTTOM:
	 nScrollAmt = screen->yScroll - screen->yOffset;
	 break;

      case SB_PAGEUP:
	 nScrollAmt = -screen->ySize;
	 break;

      case SB_PAGEDOWN:
	 nScrollAmt = screen->ySize;
	 break;

      case SB_LINEUP:
	 nScrollAmt = -screen->yChar;
	 break;

      case SB_LINEDOWN:
	 nScrollAmt = screen->yChar;
	 break;

      case SB_THUMBPOSITION:
	 nScrollAmt = wScrollPos - screen->yOffset;
	 break;

      default:
	 return  FALSE;
   }
   if ((screen->yOffset + nScrollAmt) > screen->yScroll)
      nScrollAmt = screen->yScroll - screen->yOffset;
   if ((screen->yOffset + nScrollAmt) < 0)
      nScrollAmt = -screen->yOffset;
   ScrollWindow (hWnd, 0, -nScrollAmt, NULL, NULL);
   screen->yOffset = screen->yOffset + nScrollAmt;
   SetScrollPos (hWnd, SB_VERT, screen->yOffset, TRUE);

   return  TRUE;
}

/* BOOL ScrollScreenHorz (HWND hWnd, WORD wScrollCmd, WORD wScrollPos )

   Description:
      Scrolls TTY window horizontally.

   Parameters:
      HWND hWnd
         handle to TTY window

      WORD wScrollCmd
         type of scrolling we're doing

      WORD wScrollPos
         scroll position
*/

static BOOL
ScrollScreenHorz (HWND hWnd, WORD wScrollCmd, WORD wScrollPos)
{
   int        nScrollAmt;
   SCREEN     screen = GETSCREEN (hWnd);

   if (NULL == screen)
      return  FALSE;

   switch (wScrollCmd)
   {
      case SB_TOP:
	 nScrollAmt = -screen->xOffset;
	 break;

      case SB_BOTTOM:
	 nScrollAmt = screen->xScroll - screen->xOffset;
	 break;

      case SB_PAGEUP:
	 nScrollAmt = -screen->xSize;
	 break;

      case SB_PAGEDOWN:
	 nScrollAmt = screen->xSize;
	 break;

      case SB_LINEUP:
	 nScrollAmt = -screen->xChar;
	 break;

      case SB_LINEDOWN:
	 nScrollAmt = screen->xChar;
	 break;

      case SB_THUMBPOSITION:
	 nScrollAmt = wScrollPos - screen->xOffset;
	 break;

      default:
	 return  FALSE;
   }
   if ((screen->xOffset + nScrollAmt) > screen->xScroll)
      nScrollAmt = screen->xScroll - screen->xOffset;
   if ((screen->xOffset + nScrollAmt) < 0)
      nScrollAmt = -screen->xOffset;
   ScrollWindow (hWnd, -nScrollAmt, 0, NULL, NULL);
   screen->xOffset = screen->xOffset + nScrollAmt;
   SetScrollPos (hWnd, SB_HORZ, screen->xOffset, TRUE);

   return  TRUE;
}

static SCREEN screen_focus = NULL;

HWND
ScreenCurrentFocus (void)
{
  SCREEN *link;
  HWND   hWnd;

  if (screen_focus)
    return  screen_focus->hWnd;
  hWnd = GetFocus();
  if (NULL == hWnd)
    return  NULL;
  link = head_to_registered_screen (hWnd);
  if (NULL == link)
    return  NULL;
  screen_focus = (*link);
  return  hWnd;
}

/* BOOL SetScreenFocus (HWND hWnd )

   Description:
      Sets the focus to the TTY window also creates caret.

   Parameters:
      HWND hWnd
         handle to TTY window
*/

static BOOL
SetScreenFocus (HWND hWnd)
{
   SCREEN  screen = GETSCREEN (hWnd);

   if (NULL == screen)  return  FALSE;

   screen_focus = screen;
   CreateCaret (hWnd, NULL, screen->xChar, screen->yChar);
   if (screen->cursor_visible)
      ShowCaret (hWnd);

   MoveScreenCursor (screen);
   return  TRUE;
}

/* BOOL KillScreenFocus (HWND hWnd )

   Description:
      Kills TTY focus and destroys the caret.

   Parameters:
      HWND hWnd
         handle to TTY window
*/

BOOL
KillScreenFocus (HWND hWnd )
{
   SCREEN  screen = GETSCREEN (hWnd);

   if (NULL == screen)  return  FALSE;

   screen_focus = NULL;
#if 0
   if (screen->cursor_visible)
     HideCaret (hWnd);
#endif
   DestroyCaret ();
   return  TRUE;
}

/* VOID MoveScreenCursor (SCREEN screen)

   Description:
      Moves caret to current position.
*/

static VOID
MoveScreenCursor (SCREEN screen)
{
  ScreenCurrentFocus();
  if (screen == screen_focus)
#if 0
    SetCaretPos (screen->column * screen->xChar - screen->xOffset
		 /* This ensures visiblity at the far left: */
		 + ((screen->column == screen->width) ? -2 : 0),
		 screen->row * screen->yChar - screen->yOffset);
#endif
  SetCaretPos (/* This ensures visiblity at the far left: */
	       (screen->column >= screen->width)
	       ? (screen->width * screen->xChar - screen->xOffset - 2)
	       : (screen->column * screen->xChar - screen->xOffset),
	       screen->row * screen->yChar - screen->yOffset);
}

BOOL
Screen_SetPosition (SCREEN screen, int row, int column)
{
  if ((row < 0) || (row >= screen->height))
    return (FALSE);
#if 0
  if ((column < 0) || (column > screen->width))         /* may be == */
#endif
  if ((column < 0) || (column >= MAXCOLS))
    return (FALSE);
  screen->row    = row;
  screen->column = column;
  MoveScreenCursor (screen);
  return  TRUE;
}

/* UINT ScreenPeekOrRead (SCREEN, n_to_read, SCREEN_EVENT* buffer, BOOL remove)

   Copy events into buffer.  Return number of events processed.
   If remove=TRUE, remove events from screen queue (i.e. Read)
   If remove=FALSE, leave events in queue (i.e. Peek)
   If buffer=NULL, process without copying.
   If n_to_read<0, process all events.
    .  n_to_read=-1, buffer=NULL, remove=FALSE -> count of pending events
    .  n_to_read=-1, buffer=NULL, remove=TRUE  -> flush queue
    .  n_to_read=n,  buffer=NULL, remove=TRUE  -> discard n events

   NB: if (n_to_read < 0), buffer is ignored.
*/

UINT
ScreenPeekOrRead (SCREEN screen, int n_to_read, SCREEN_EVENT * buffer,
		  BOOL remove)
{
  SCREEN_EVENT * scan_buffer = ((n_to_read < 0) ? 0 : buffer);
  SCREEN_EVENT event;
  int n_read = 0;

  if (remove)
    while (((n_read < n_to_read) || (n_to_read < 0))
	   && (read_event (screen, 0, 1, (&event))))
      {
	if (scan_buffer)
	  (*scan_buffer++) = event;
	n_read += 1;
      }
  else
    {
      SCREEN_EVENT_LINK * scan_queue = event_queue_head;
      while (((n_read < n_to_read) || (n_to_read < 0))
	     && (scan_queue != 0))
	if (((scan_queue -> event) . handle) == (screen -> hWnd))
	  {
	    if (scan_buffer)
	      (*scan_buffer++) = (scan_queue -> event);
	    scan_queue = (scan_queue -> next);
	    n_read += 1;
	  }
    }
  return (n_read);
}

void
flush_typeahead (SCREEN screen)
{
  while (read_event (screen, SCREEN_EVENT_TYPE_KEY, 1, 0))
    ;
  (screen -> n_chars) = 0;
}

/* The following handling of the keyboard is taken with only minor
   changes from Emacs 20.5.  */

#define LP_REPEAT(lparam)     ((lparam) & 0x0000ffff)
#define LP_SCAN_CODE(lparam) (((lparam) & 0x00ff0000) >> 16)
#define LP_EXTENDED(lparam)  (((lparam) & 0x01000000) != 0)

/* GetKeyState and MapVirtualKey on Windows 95 do not actually distinguish
   between left and right keys as advertised.  We test for this
   support dynamically, and set a flag when the support is absent.  If
   absent, we keep track of the left and right control and alt keys
   ourselves.  This is particularly necessary on keyboards that rely
   upon the AltGr key, which is represented as having the left control
   and right alt keys pressed.  For these keyboards, we need to know
   when the left alt key has been pressed in addition to the AltGr key
   so that we can properly support M-AltGr-key sequences (such as M-@
   on Swedish keyboards).  */

#define MOD_LCONTROL 0
#define MOD_RCONTROL 1
#define MOD_LMENU 2
#define MOD_RMENU 3

static int modifiers [4] = { 0, 0, 0, 0 };
static int record_modifiers_p;

static void
record_modifier_transition (WPARAM wparam, LPARAM lparam, int down_p)
{
  static int modifier_key_support_tested = 0;
  if (down_p && (!modifier_key_support_tested))
    {
      if (wparam == VK_CONTROL)
	{
	  record_modifiers_p
	    = (((GetKeyState (VK_LCONTROL) & 0x8000) == 0)
	       && ((GetKeyState (VK_RCONTROL) & 0x8000) == 0));
	  modifier_key_support_tested = 1;
	}
      else if (wparam == VK_MENU)
	{
	  record_modifiers_p
	    = (((GetKeyState (VK_LMENU) & 0x8000) == 0)
	       && ((GetKeyState (VK_RMENU) & 0x8000) == 0));
	  modifier_key_support_tested = 1;
	}
    }
  if (record_modifiers_p)
    {
      if (down_p)
	{
	  /* Synchronize modifier state with what is reported with the
	     current keystroke.  Even if we cannot distinguish between
	     left and right modifier keys, we know that, if no
	     modifiers are set, then neither the left nor right
	     modifier should be set.  */
	  if ((GetKeyState (VK_CONTROL) & 0x8000) == 0)
	    {
	      (modifiers[MOD_RCONTROL]) = 0;
	      (modifiers[MOD_LCONTROL]) = 0;
	    }
	  if ((GetKeyState (VK_MENU) & 0x8000) == 0)
	    {
	      (modifiers[MOD_RMENU]) = 0;
	      (modifiers[MOD_LMENU]) = 0;
	    }
	}
      if (wparam == VK_CONTROL)
	(modifiers [(LP_EXTENDED (lparam)) ? MOD_RCONTROL : MOD_LCONTROL])
	  = down_p;
      else if (wparam == VK_MENU)
	(modifiers [(LP_EXTENDED (lparam)) ? MOD_RMENU : MOD_LMENU]) = down_p;
    }
}

/* We can lose focus while a modifier key has been pressed.  When
   we regain focus, be conservative and clear all modifiers since 
   we cannot reconstruct the left and right modifier state.  */

static void
copy_current_state (unsigned int vk, BYTE * keystate)
{
  (keystate[vk]) = ((GetAsyncKeyState (vk) & 0x8000) >> 8);
}

static void
reset_modifiers (void)
{
  if ((GetFocus ()) != 0)
    {
      if (((GetAsyncKeyState (VK_CONTROL)) & 0x08000) == 0)
	{
	  /* Clear any recorded control modifier state.  */
	  (modifiers[MOD_RCONTROL]) = 0;
	  (modifiers[MOD_LCONTROL]) = 0;
	}
      if (((GetAsyncKeyState (VK_MENU)) & 0x08000) == 0)
	{
	  /* Clear any recorded alt modifier state.  */
	  (modifiers[MOD_RMENU]) = 0;
	  (modifiers[MOD_LMENU]) = 0;
	}
      /* Update the state of all modifier keys, because modifiers used in
	 hot-key combinations can get stuck on if Scheme loses focus as a
	 result of a hot-key being pressed.  */
      {
	BYTE keystate [256];
	GetKeyboardState (keystate);
	copy_current_state (VK_SHIFT, keystate);
	copy_current_state (VK_CONTROL, keystate);
	copy_current_state (VK_LCONTROL, keystate);
	copy_current_state (VK_RCONTROL, keystate);
	copy_current_state (VK_MENU, keystate);
	copy_current_state (VK_LMENU, keystate);
	copy_current_state (VK_RMENU, keystate);
	copy_current_state (VK_LWIN, keystate);
	copy_current_state (VK_RWIN, keystate);
	copy_current_state (VK_APPS, keystate);
	SetKeyboardState (keystate);
      }
    }
}

static int
modifier_set_p (int vk)
{
  if (record_modifiers_p)
    switch (vk)
      {
      case VK_LCONTROL:
	return (modifiers[MOD_LCONTROL]);
      case VK_RCONTROL:
	return (modifiers[MOD_RCONTROL]);
      case VK_LMENU:
	return (modifiers[MOD_LMENU]);
      case VK_RMENU:
	return (modifiers[MOD_RMENU]);
      }
  /* For toggled modifiers, test the low-order bit; otherwise the
     high-order bit.  */
  return
    (((GetKeyState (vk))
      & (((vk == VK_CAPITAL) || (vk == VK_NUMLOCK) || (vk == VK_SCROLL))
	 ? 0x0001
	 : 0x8000))
     != 0);
}

static unsigned int
get_modifiers (void)
{
  unsigned int mods = 0;

  if (modifier_set_p (VK_SHIFT))    mods |= SCREEN_SHIFT_PRESSED;
  if (modifier_set_p (VK_CAPITAL))  mods |= SCREEN_CAPSLOCK_ON;
  if (modifier_set_p (VK_NUMLOCK))  mods |= SCREEN_NUMLOCK_ON;
  if (modifier_set_p (VK_SCROLL))   mods |= SCREEN_SCROLLLOCK_ON;

  if (modifier_set_p (VK_RCONTROL))
    mods |= (SCREEN_RIGHT_CONTROL_PRESSED | SCREEN_CONTROL_PRESSED);
  if (modifier_set_p (VK_LMENU))
    mods |= (SCREEN_LEFT_ALT_PRESSED | SCREEN_ALT_PRESSED);

  /* Slight complication to handle case where AltGr is pressed.  */
  if ((modifier_set_p (VK_LCONTROL)) && (!modifier_set_p (VK_RMENU)))
    mods |= (SCREEN_LEFT_CONTROL_PRESSED | SCREEN_CONTROL_PRESSED);
  if ((modifier_set_p (VK_RMENU)) && (!modifier_set_p (VK_LCONTROL)))
    mods |= (SCREEN_RIGHT_ALT_PRESSED | SCREEN_ALT_PRESSED);

  return (mods);
}

static void
use_translate_message (HWND handle, UINT message, WPARAM wparam, LPARAM lparam)
{
  MSG msg;
  (msg . hwnd) = handle;
  (msg . message) = message;
  (msg . wParam) = wparam;
  (msg . lParam) = lparam;
  (msg . time) = (GetMessageTime ());
  (msg . pt . x) = 0;
  (msg . pt . y) = 0;
  TranslateMessage (&msg);
}

static void
make_key_event (HWND handle, WPARAM wparam, LPARAM lparam, int ch)
{
  SCREEN screen = (GETSCREEN (handle));
  SCREEN_EVENT * event;
  unsigned int modifiers = (get_modifiers ());

  /* Translate the Backspace key to the Delete character.  */
  if ((ch == 0x08) && (wparam == VK_BACK))
    ch = ASCII_DEL;

  /* If the unmodified key is bound to a command, send the command.
     Extra hair here is due to need to handle control characters.  */
  if (((modifiers & SCREEN_ALT_PRESSED) == 0)
      && (((modifiers & SCREEN_CONTROL_PRESSED) == 0)
	  || (('A' <= ch) && (ch <= 'Z'))
	  || (('a' <= ch) && (ch <= 'z'))
	  || (ch == '@') || (ch == '[') || (ch == '\\')
	  || (ch == ']') || (ch == '^') || (ch == '_')))
    {
      int ch2 = ch;
      unsigned int i;
      if ((modifiers & SCREEN_CONTROL_PRESSED) != 0)
	{
	  if (('a' <= ch) && (ch <= 'z'))
	    ch2 -= ('a' - 'A');
	  ch2 -= '@';
	}
      for (i = 0; (i < (screen -> n_bindings)); i += 1)
	if ((((screen -> bindings) [i]) . key) == ch2)
	  {
	    if (SendMessage
		(handle,
		 WM_COMMAND,
		 (MAKEWPARAM ((((screen -> bindings) [i]) . command), 0)),
		 0))
	      return;
	    else
	      break;
	  }
    }

  if ((ch == (-1)) && (((screen -> mode_flags) & SCREEN_MODE_VK_KEYS) == 0))
    return;

  event = (allocate_event (screen, SCREEN_EVENT_TYPE_KEY));
  if (!event) return;
  ((event -> event.key) . repeat_count) = (LP_REPEAT (lparam));
  ((event -> event.key) . virtual_keycode) = wparam;
  ((event -> event.key) . virtual_scancode) = (LP_SCAN_CODE (lparam));
  ((event -> event.key) . ch) = ch;
  ((event -> event.key) . control_key_state) = modifiers;

  if (win32_trace_level > 0)
    {
      fprintf (win32_trace_file, "make_key_event: ");
      fprintf
	(win32_trace_file,
	 "handle=0x%x keycode=0x%x scancode=0x%x ch=0x%x modifiers=0x%x\n",
	 handle, wparam, (LP_SCAN_CODE (lparam)), ch, modifiers);
      fflush (win32_trace_file);
    }
}

/* Process WM_KEYDOWN and WM_SYSKEYDOWN.  Return 1 to indicate that
   the key was handled, and that the message proc should return;
   return 0 to indicate that the default action should take place.  */

static int
process_keydown (HWND handle, UINT message, WPARAM wparam, LPARAM lparam)
{
  switch (wparam)
    {
    case VK_MENU:
      /* Prevent Windows from activating the menu bar if an Alt key is
	 pressed and released by itself.  */
      return (1);

    case VK_LWIN:
    case VK_RWIN:
    case VK_APPS:
    case VK_NUMLOCK:
    case VK_SCROLL:
    case VK_CAPITAL:
    case VK_CONTROL: 
    case VK_SHIFT:
      /* Let Windows handle the modifier keys.  */
      use_translate_message (handle, message, wparam, lparam);
      return (0);
    }

  /* Always let Windows handle AltGr key chords; for some reason,
     ToAscii doesn't always process AltGr chords correctly.  */
  if ((modifier_set_p (VK_LCONTROL)) && (modifier_set_p (VK_RMENU)))
    {
      use_translate_message (handle, message, wparam, lparam);
      return (0);
    }

  /* Edwin handles some of the special keys directly, so provide
     symbols for those keys rather than translating them.  */
  if ((((GETSCREEN (handle)) -> mode_flags) & SCREEN_MODE_VK_KEYS)
      && ((wparam == VK_LEFT)
	  || (wparam == VK_RIGHT)
	  || (wparam == VK_UP)
	  || (wparam == VK_DOWN)
	  || (wparam == VK_HOME)
	  || (wparam == VK_END)
	  || (wparam == VK_PRIOR)
	  || (wparam == VK_NEXT)
	  || (wparam == VK_INSERT)
	  || (wparam == VK_DELETE)
	  || ((wparam >= VK_F1) && (wparam <= VK_F24))))
    {
      make_key_event (handle, wparam, lparam, (-1));
      return (1);
    }

  /* Let TranslateMessage handle anything not involving Alt or Ctrl.  */
  if (((get_modifiers ())
       & (SCREEN_ALT_PRESSED | SCREEN_CONTROL_PRESSED))
      == 0)
    {
      use_translate_message (handle, message, wparam, lparam);
      return (0);
    }

  /* Otherwise, handle translation directly, as otherwise Windows
     will do the wrong thing.  */

  /* Don't translate modified alphabetic keystrokes, so the user
     doesn't need to constantly switch layout to type control or meta
     keystrokes when the normal layout translates alphabetic
     characters to non-ascii characters.  */
  if (('A' <= wparam) && (wparam <= 'Z'))
    {
      make_key_event
	(handle, wparam, lparam,
	 (((modifier_set_p (VK_SHIFT)) || (modifier_set_p (VK_CAPITAL)))
	  ? wparam
	  : (wparam + ('a' - 'A'))));
      return (1);
    }

  /* OK, here's the real hair.  Translate the unmodified keystroke to
     the corresponding character(s), then add the modifiers back in.  */
  {
    BYTE keystate [256];
    BYTE ansi_code [4];
    int n_chars;
    int i;

    memset (keystate, 0, (sizeof (keystate)));
    (keystate[wparam]) = 0x80;
    if (modifier_set_p (VK_SHIFT))
      (keystate[VK_SHIFT]) = 0x80;
    if (modifier_set_p (VK_CAPITAL))
      (keystate[VK_CAPITAL]) = 0x01;

    /* On NT, call ToUnicode instead and then convert to the current
       locale's default codepage.  */
    if (NT_windows_type == wintype_nt)
      {
	WCHAR buffer [128];
	char code_page [20];
	int code_page_number;

	n_chars
	  = (ToUnicode (wparam, (LP_SCAN_CODE (lparam)), keystate,
			buffer, 128, 0));
	if (n_chars <= 0)
	  return (1);
	GetLocaleInfo ((GetThreadLocale ()), LOCALE_IDEFAULTANSICODEPAGE,
		       code_page, (sizeof (code_page)));
	code_page_number = (atoi (code_page));
	n_chars
	  = (WideCharToMultiByte (code_page_number, 0, buffer, n_chars,
				  ansi_code, (sizeof (ansi_code)), 0, 0));
      }
    else
      n_chars
	= (ToAscii (wparam, (LP_SCAN_CODE (lparam)), keystate,
		    ((LPWORD) ansi_code), 0));
    for (i = 0; (i < n_chars); i += 1)
      make_key_event (handle, wparam, lparam, (ansi_code[i]));
    return (1);
  }
}

static void
process_character (HWND handle, UINT message, WPARAM wparam, LPARAM lparam)
{
  make_key_event
    (handle, (MapVirtualKey ((LP_SCAN_CODE (lparam)), 1)), lparam, wparam);
}

static void
process_focus_message (HWND handle, int gained_p)
{
  SCREEN screen = (GETSCREEN (handle));
  SCREEN_EVENT * event = (allocate_event (screen, SCREEN_EVENT_TYPE_FOCUS));
  if (event)
    (event->event.focus.gained_p) = gained_p;
}

static void
process_show_message (HWND handle, int show_p)
{
  SCREEN screen = (GETSCREEN (handle));
  SCREEN_EVENT * event
    = (allocate_event (screen, SCREEN_EVENT_TYPE_VISIBILITY));
  if (event)
    (event->event.visibility.show_p) = show_p;
}

static VOID
ProcessCloseMessage (SCREEN screen)
{
  (void) allocate_event (screen, SCREEN_EVENT_TYPE_CLOSE);
}

static VOID
ProcessMouseButton (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam,
                    BOOL up)
{
  SCREEN screen = GETSCREEN (hWnd);
  SCREEN_EVENT * event;
  unsigned int control = 0;
  unsigned int button = 0;

  if (NULL == screen)
    return;

  if (uMsg & MK_CONTROL)
    control |= SCREEN_CONTROL_PRESSED;
  if (uMsg & MK_SHIFT)
    control |= SCREEN_SHIFT_PRESSED;

  switch (uMsg)
    {
    case WM_LBUTTONDOWN:
    case WM_LBUTTONUP:
      button = SCREEN_MOUSE_EVENT_LEFT_PRESSED;
      break;

    case WM_MBUTTONDOWN:
    case WM_MBUTTONUP:
      button = SCREEN_MOUSE_EVENT_MIDDLE_PRESSED;
      break;

    case WM_RBUTTONDOWN:
    case WM_RBUTTONUP:
      button = SCREEN_MOUSE_EVENT_RIGHT_PRESSED;
      break;
    }
  event = (allocate_event (screen, SCREEN_EVENT_TYPE_MOUSE));
  if (event)
    {
      event->event.mouse.row = ((HIWORD (lParam)) / (screen -> yChar));
      event->event.mouse.column = ((LOWORD (lParam)) / (screen -> xChar));
      event->event.mouse.control_key_state = control;
      event->event.mouse.button_state = button;
      event->event.mouse.up = up;
      event->event.mouse.mouse_moved = 0;
      event->event.mouse.double_click = 0;
    }
}

/* Utilities for WriteScreenBlock */

static VOID _fastcall
Screen_BS (SCREEN screen)
{
  if (screen->column > 0)
    screen->column --;
  else if (screen->row > 0)
  {
    screen->row--;
    screen->column = (screen->width - 1);
  }
  if (screen->mode_flags & SCREEN_MODE_EAGER_UPDATE)
    MoveScreenCursor (screen);
}

static VOID _fastcall
Screen_LF (SCREEN screen)
{
  if ((screen->row++) >= (screen->height - 1))
  {
    ScrollScreenBufferUp (screen, screen->scroll_lines);
    ScrollWindow (screen->hWnd, 0,
		  (-screen->yChar * screen->scroll_lines),
		  NULL,
		  NULL);
#if 0
    InvalidateRect (hWnd, NULL, FALSE);
    screen->row--;
#endif
    screen->row = (screen->height - screen->scroll_lines);
  }
  if (screen->mode_flags & SCREEN_MODE_EAGER_UPDATE)
  {
    MoveScreenCursor (screen);
    UpdateWindow (screen->hWnd);
  }
}

static VOID _fastcall
Screen_CR (SCREEN screen)
{
  screen->column = 0;
  if (screen->mode_flags & SCREEN_MODE_EAGER_UPDATE)
    MoveScreenCursor (screen);
}

static VOID _fastcall
Screen_CRLF (SCREEN screen)
{
  Screen_CR (screen);
  Screen_LF (screen);
}

VOID _fastcall
clear_screen_rectangle (SCREEN screen,
			int lo_row, int lo_col,
			int hi_row, int hi_col)
{
  RECT rect;
  int row, delta_col;
  char * screen_chars;
  SCREEN_ATTRIBUTE * screen_attrs;

  delta_col = (hi_col - lo_col);

  for (row = lo_row,
       screen_chars = &screen->chars[lo_row * MAXCOLS + lo_col],
       screen_attrs = &screen->attrs[lo_row * MAXCOLS + lo_col];
       row < hi_row;
       row++,
       screen_chars += MAXCOLS,
       screen_attrs += MAXCOLS)
  {
    _fmemset (screen_chars, ' ', delta_col);
    _fmemset (screen_attrs,
	      screen->write_attribute,
	      (delta_col * (sizeof (SCREEN_ATTRIBUTE))));
  }

  rect.left   = ((lo_col * screen->xChar) - screen->xOffset);
  rect.right  = ((hi_col * screen->xChar) - screen->xOffset);
  rect.top    = ((lo_row * screen->yChar) - screen->yOffset);
  rect.bottom = ((hi_row * screen->yChar) - screen->yOffset);
  InvalidateRect (screen->hWnd, &rect, FALSE);
}

#define INIT_SCREEN_WRITE_CHAR_STATE(state) state.row = -1

static VOID _fastcall
Finish_ScreenWriteChar (SCREEN screen, struct screen_write_char_s * rectp)
{
  if (rectp->row != -1)
    InvalidateRect (screen->hWnd, &rectp->rect, FALSE);
  if ((screen->column >= screen->width)
      && ((screen->mode_flags & SCREEN_MODE_AUTOWRAP) != 0))
    Screen_CRLF (screen);
  rectp->row = -1;
}

VOID _fastcall
Screen_WriteCharUninterpreted (SCREEN screen, int ch,
			       struct screen_write_char_s * rectp)
{
  /* Line wrap/overwrite the last position */

  if (screen->column >= screen->width)
  {
    if ((screen->mode_flags & SCREEN_MODE_AUTOWRAP) != 0)
    {
      if ((rectp != ((struct screen_write_char_s *) NULL))
	  && (rectp->row != -1))
      {
	InvalidateRect (screen->hWnd, &rectp->rect, FALSE);
	rectp->row = -1;
      }
      Screen_CRLF (screen);
    }
    else
    {
      screen->column -= 1;
      if (rectp != ((struct screen_write_char_s *) NULL))
      {
	rectp->col -= 1;
	rectp->rect.right -= screen->xChar;
      }
    }
  }
  if (screen->row >= MAXROWS)
    screen->row = (MAXROWS - 1);

  if (screen->column >= MAXCOLS)
    screen->column = (MAXCOLS - 1);

  if (screen->row < 0)
    screen->row = 0;

  if (screen->column < 0)
    screen->column = 0;

  screen->chars[screen->row * MAXCOLS + screen->column] = ch;
  screen->attrs[screen->row * MAXCOLS + screen->column] =
    screen->write_attribute;
  if (rectp == ((struct screen_write_char_s *) NULL))
  {
    RECT       rect;

    rect.left   = ((screen->column * screen->xChar) - screen->xOffset);
    rect.right  = rect.left + screen->xChar;
    rect.top    = ((screen->row * screen->yChar) - screen->yOffset);
    rect.bottom = rect.top + screen->yChar;
    InvalidateRect (screen->hWnd, &rect, FALSE);
  }
  else if ((rectp->row == screen->row) && (rectp->col == screen->column))
  {
    rectp->col += 1;
    rectp->rect.right += screen->xChar;
  }
  else
  {
    if (rectp->row != -1)
      InvalidateRect (screen->hWnd, &rectp->rect, FALSE);

    rectp->rect.left   = ((screen->column * screen->xChar) - screen->xOffset);
    rectp->rect.right  = rectp->rect.left + screen->xChar;
    rectp->rect.top    = ((screen->row * screen->yChar) - screen->yOffset);
    rectp->rect.bottom = rectp->rect.top + screen->yChar;
    rectp->col = (screen->column + 1);
    rectp->row = screen->row;
  }
  screen->column += 1;
}

static VOID _fastcall
Screen_TAB (SCREEN screen, struct screen_write_char_s * rectp)
{
  do
    Screen_WriteCharUninterpreted (screen, ' ', rectp);
  while ((screen->column % 8) != 0);
}

static VOID _fastcall
relocate_cursor (SCREEN screen, int row, int col)
{
  screen->row = ((row < 0)
		 ? 0
		 : ((row >= screen->height)
		    ? (screen->height - 1)
		    : row));
  screen->column = ((col < 0)
		    ? 0
		    : ((col >= screen->width)
		       ? (screen->width - 1)
		       : col));
  if (screen->mode_flags & SCREEN_MODE_EAGER_UPDATE)
    MoveScreenCursor (screen);
}

static VOID _fastcall
cursor_right (SCREEN screen, int delta)
{
  int new_col = (screen->column + delta);

  if (new_col < screen->width)
    screen->column = new_col;
  else if ((screen->mode_flags & SCREEN_MODE_AUTOWRAP) == 0)
    screen->column = (screen->width - 1);
  else
  {
    while (new_col >= screen->width)
    {
      Screen_CRLF (screen);
      new_col -= screen->width;
    }
    screen->column = new_col;
  }
}

VOID _fastcall
scroll_screen_vertically (SCREEN screen,
			  int lo_row_from, int lo_col,
			  int hi_row_from, int hi_col,
			  int lo_row_to)
{
  RECT rect;
  int row, delta_col, hi_row_to;
  char * chars_from, * chars_to;
  SCREEN_ATTRIBUTE * attrs_from, * attrs_to;

  if (lo_row_to < 0)
    lo_row_to = 0;
  if (lo_row_to > MAXCOLS)
    lo_row_to = MAXCOLS;

  delta_col = (hi_col - lo_col);
  hi_row_to = (lo_row_to + (hi_row_from - lo_row_from));

  if (lo_row_from > lo_row_to)          /* Scrolling up. */
    for (row = lo_row_from,
	 chars_from = &screen->chars[lo_row_from * MAXCOLS + lo_col],
	 attrs_from = &screen->attrs[lo_row_from * MAXCOLS + lo_col],
	 chars_to = &screen->chars[lo_row_to * MAXCOLS + lo_col],
	 attrs_to = &screen->attrs[lo_row_to * MAXCOLS + lo_col];
	 row < hi_row_from;
	 row++,
	 chars_from += MAXCOLS,
	 attrs_from += MAXCOLS,
	 chars_to += MAXCOLS,
	 attrs_to += MAXCOLS)
    {
      _fmemmove (((LPSTR) chars_to), ((LPSTR) chars_from), delta_col);
      _fmemmove (((LPSTR) attrs_to), ((LPSTR) attrs_from), delta_col);
    }
  else                                   /* Scrolling down. */
    for (row = (hi_row_from - 1),
	 chars_from =  &screen->chars[(hi_row_from - 1) * MAXCOLS + lo_col],
	 attrs_from =  &screen->attrs[(hi_row_from - 1) * MAXCOLS + lo_col],
	 chars_to =  &screen->chars[(hi_row_to - 1) * MAXCOLS + lo_col],
	 attrs_to =  &screen->attrs[(hi_row_to - 1) * MAXCOLS + lo_col];
	 row >= lo_row_from;
	 row--,
	 chars_from -= MAXCOLS,
	 attrs_from -= MAXCOLS,
	 chars_to -= MAXCOLS,
	 attrs_to -= MAXCOLS)
    {
      _fmemmove (((LPSTR) chars_to), ((LPSTR) chars_from), delta_col);
      _fmemmove (((LPSTR) attrs_to), ((LPSTR) attrs_from), delta_col);
    }

  rect.left   = ((lo_col * screen->xChar) - screen->xOffset);
  rect.right  = ((hi_col * screen->xChar) - screen->xOffset);
  rect.top    = ((lo_row_to * screen->yChar) - screen->yOffset);
  rect.bottom = ((hi_row_to * screen->yChar) - screen->yOffset);
  InvalidateRect (screen->hWnd, &rect, FALSE);
}

static VOID _fastcall
scroll_screen_line_horizontally (SCREEN screen, int row,
				 int lo_col_from, int hi_col_from,
				 int lo_col_to)
{
  RECT rect;
  int delta_col = (hi_col_from - lo_col_from);
  int hi_col_to = (lo_col_to + delta_col);

  _fmemmove (((LPSTR) &screen->chars[(row * MAXCOLS) + lo_col_to]),
	     ((LPSTR) &screen->chars[(row * MAXCOLS) + lo_col_from]),
	     delta_col);
  _fmemmove (((LPSTR) &screen->attrs[(row * MAXCOLS) + lo_col_to]),
	     ((LPSTR) &screen->attrs[(row * MAXCOLS) + lo_col_from]),
	     delta_col);

  rect.left   = ((lo_col_to * screen->xChar) - screen->xOffset);
  rect.right  = ((hi_col_to * screen->xChar) - screen->xOffset);
  rect.top    = ((row * screen->yChar) - screen->yOffset);
  rect.bottom = (((row + 1) * screen->yChar) - screen->yOffset);
  InvalidateRect (screen->hWnd, &rect, FALSE);
}

static int _fastcall
read_decimal (LPSTR str, int lo, int len, int * hi)
{
  int ctr, result;

  for (result = 0, ctr = lo;
       ctr < len;
       result = ((result * 10) + ((str[ctr]) - '0')), ctr++)
    if ((str[ctr] < '0') || (str[ctr] > '9'))
      break;

  * hi = ctr;
  return (result);
}

#ifdef PRETTY_PRINT_CHARS
static VOID _fastcall
screen_write_octal (SCREEN screen, unsigned char the_char,
		    struct screen_write_char_s * rectp)
{
  Screen_WriteCharUninterpreted (screen, '\\', rectp);
  Screen_WriteCharUninterpreted (screen, ((the_char / 0100) + '0'), rectp);
  Screen_WriteCharUninterpreted (screen, (((the_char % 0100) / 010) + '0'),
				 rectp);
  Screen_WriteCharUninterpreted (screen, ((the_char % 010) + '0'), rectp);
}
#endif /* PRETTY_PRINT_CHARS */

static VOID
WriteScreenBlock_suspend (SCREEN screen, LPSTR lpBlock, int i, int nLength)
{
  screen->n_pending = (nLength - i);
  screen->pending = ((LPSTR) (LocalAlloc (NONZEROLPTR, screen->n_pending)));
  if (screen->pending != ((LPSTR) NULL))
    strncpy (screen->pending, (lpBlock + i), screen->n_pending);
  else
  {
    screen->n_pending = 0;
    MessageBeep (0);
  }
}

static VOID
WriteScreenBlock_continue (SCREEN screen,
			   LPSTR lpBlock_in, int nLength_in,
			   LPSTR * lpBlock, int * nLength)
{
  (* nLength) = (screen->n_pending + nLength_in);
  (* lpBlock) = ((LPSTR) (LocalAlloc (NONZEROLPTR, (* nLength))));
  if ((* lpBlock) == ((LPSTR) NULL))
  {
    MessageBeep (0);
    (* nLength) = nLength_in;
    (* lpBlock) = lpBlock_in;
  }
  else
  {
    strncpy ((* lpBlock), screen->pending, screen->n_pending);
    strncpy (((* lpBlock) + screen->n_pending), lpBlock_in, nLength_in);
  }
  LocalFree (screen->pending);
  screen->n_pending = 0;
}

/* BOOL WriteScreenBlock (HWND hWnd, LPSTR lpBlock_in, int nLength_in )

   Description:
      Writes block of characters to TTY screen.  Interprets lots of ANSI
      sequences.
*/

BOOL
WriteScreenBlock (HWND hWnd, LPSTR lpBlock_in, int nLength_in)
{
  int i;
  LPSTR lpBlock;
  int nLength;
  WORD saved_mode_flags;
  SCREEN screen = (GETSCREEN (hWnd));
  struct screen_write_char_s state;

  if (NULL == screen)
    return (FALSE);

  INIT_SCREEN_WRITE_CHAR_STATE (state);
  saved_mode_flags = (screen->mode_flags & SCREEN_MODE_EAGER_UPDATE);
  screen->mode_flags &= (~ (SCREEN_MODE_EAGER_UPDATE));

  if (screen->n_pending != 0)
    WriteScreenBlock_continue (screen,
			       lpBlock_in, nLength_in,
			       &lpBlock, &nLength);
  else
  {
    nLength = nLength_in;
    lpBlock = lpBlock_in;
  }

  if ((screen->mode_flags & SCREEN_MODE_PROCESS_OUTPUT) == 0)
    for (i = 0; i < nLength; i++)
      Screen_WriteCharUninterpreted (screen, (lpBlock[i]), &state);
  else for (i = 0; i < nLength; i++)
  {
    unsigned char the_char = ((unsigned char) (lpBlock[i]));

    switch (the_char)
    {
    case ASCII_BEL:
      MessageBeep (0);
      break;

    case ASCII_BS:
      Screen_BS (screen);
      break;

    case '\t':
      Screen_TAB (screen, &state);
      break;

    case ASCII_LF:
      if (screen->mode_flags & SCREEN_MODE_NEWLINE_CRS)
	Screen_CR (screen);
      Finish_ScreenWriteChar (screen, &state);
      Screen_LF (screen);
      break;

    case ASCII_CR:
      Screen_CR (screen);
      if (screen->mode_flags & SCREEN_MODE_CR_NEWLINES)
      {
	Finish_ScreenWriteChar (screen, &state);
	Screen_LF (screen);
      }
      break;

    case ASCII_FF:
      Finish_ScreenWriteChar (screen, &state);
      Screen_Clear (screen, 0);
      break;

    default:
    char_default:
#ifdef PRETTY_PRINT_CHARS
      if (the_char < ' ')
      {
	Screen_WriteCharUninterpreted (screen, '^', &state);
	Screen_WriteCharUninterpreted (screen, (the_char + '@'), &state);
      }
      else if (the_char < ASCII_DEL)
	Screen_WriteCharUninterpreted (screen, the_char, &state);
      else if (the_char == ASCII_DEL)
      {
	Screen_WriteCharUninterpreted (screen, '^', &state);
	Screen_WriteCharUninterpreted (screen, '?', &state);
      }
      else
	screen_write_octal (screen, ((unsigned char) the_char), &state);
#else /* not PRETTY_PRINT_CHARS */
      Screen_WriteCharUninterpreted (screen, the_char, &state);
#endif /* PRETTY_PRINT_CHARS */
      break;

    case ASCII_ESC:
    {
      char dispatch;

      Finish_ScreenWriteChar (screen, &state);
      if ((i + 2) >= nLength)
      {
	WriteScreenBlock_suspend (screen, lpBlock, i, nLength);
	i = (nLength - 1);      /* 1 added in for loop */
	break;
      }

      if (lpBlock[i + 1] != '[')
	goto char_default;

      dispatch = (lpBlock[i + 2]);
      switch (dispatch)
      {
      case 'K':
	/* Clear Line */
	clear_screen_rectangle (screen,
				screen->row, screen->column,
				(screen->row + 1), screen->width);
	i += 2;         /* 1 added in for loop */
	continue;

      case 'J':
	/* Clear to bottom */
	if (screen->column == 0)
	  clear_screen_rectangle (screen, screen->row, 0,
				  screen->height, screen->width);
	else
	{
	  clear_screen_rectangle (screen,
				  screen->row, screen->column,
				  (screen->row + 1), screen->width);
	  clear_screen_rectangle (screen, (screen->row + 1), 0,
				  screen->height, screen->width);
	}
	i += 2;         /* 1 added in for loop */
	continue;

      case 'H':
	/* Cursor home */
	relocate_cursor (screen, 0, 0);
	i += 2;         /* 1 added in for loop */
	continue;

      case 'A':
	/* Cursor up */
	relocate_cursor (screen, (screen->row - 1), screen->column);
	i += 2;         /* 1 added in for loop */
	continue;

      case 'C':
	/* Cursor right */
	cursor_right (screen, 1);
	i += 2;         /* 1 added in for loop */
	continue;

      case 'L':
	/* Insert line */
	scroll_screen_vertically (screen,
				  screen->row, screen->column,
				  (screen->height - 1), screen->width,
				  (screen->row + 1));
	clear_screen_rectangle (screen,
				screen->row, screen->column,
				(screen->row + 1), screen->width);
	i += 2;         /* 1 added in for loop */
	continue;

      case 'M':
	/* Delete line */
	scroll_screen_vertically (screen,
				  (screen->row + 1), screen->column,
				  screen->height, screen->width,
				  screen->row);
	clear_screen_rectangle (screen,
				(screen->height - 1), screen->column,
				screen->height, screen->width);
	i += 2;         /* 1 added in for loop */
	continue;

      case 'P':
	/* Delete char */
	scroll_screen_line_horizontally (screen, screen->row,
					 (screen->column + 1), screen->width,
					 screen->column);
	i += 2;
	continue;

      case '@':
	/* Insert char */
	scroll_screen_line_horizontally (screen, screen->row,
					 screen->column, (screen->width - 1),
					 (screen->column + 1));
	i += 2;
	continue;

      default:
	if ((dispatch >= '0') && (dispatch <= '9'))
	{
	  int j, x_value;

	  x_value = (read_decimal (&lpBlock[0], (i + 2), nLength, &j));
	  if (j >= nLength)
	  {
	    WriteScreenBlock_suspend (screen, lpBlock, i, nLength);
	    i = (j - 1); /* 1 added in for loop */
	    continue;
	  }
	  else switch (lpBlock[j])
	  {
	    case ';':
	    {
	      int k, y_value;

	      y_value = (read_decimal (&lpBlock[0], (j + 1), nLength, &k));
	      if ((k < nLength) && (lpBlock[k] == 'H'))
		/* Direct cursor motion */
		relocate_cursor (screen, (x_value - 1), (y_value - 1));
	      else if (k < nLength)
		MessageBeep (0);
	      else
		WriteScreenBlock_suspend (screen, lpBlock, i, nLength);
	      i = k;    /* 1 added in for loop */
	      continue;
	    }

	    case 'A':
	      /* Multi cursor up */
	      relocate_cursor (screen, (screen->row - x_value),
			       screen->column);
	      i = j; /* 1 added in for loop */
	      continue;

	    case 'C':
	      /* Multi cursor right */
	      cursor_right (screen, x_value);
	      i = j; /* 1 added in for loop */
	      continue;

	    case 'L':
	      /* Multi insert line */
	      scroll_screen_vertically (screen,
					screen->row, screen->column,
					(screen->height - 1), screen->width,
					(screen->row + x_value));
	      clear_screen_rectangle (screen,
				      screen->row, screen->column,
				      (screen->row + x_value), screen->width);
	      i = j; /* 1 added in for loop */
	      continue;

	    case 'M':
	      /* Multi delete line */
	      scroll_screen_vertically (screen,
					(screen->row + x_value),
					screen->column,
					screen->height, screen->width,
					screen->row);
	      clear_screen_rectangle (screen,
				      (screen->height - x_value),
				      screen->column,
				      screen->height, screen->width);
	      i = j; /* 1 added in for loop */
	      continue;

	    case 'P':
	      /* Multi delete char */
	      scroll_screen_line_horizontally (screen, screen->row,
					       (screen->column + x_value),
					       screen->width,
					       screen->column);
	      i = j; /* 1 added in for loop */
	      continue;

	    case '@':
	      /* Multi insert char */
	      scroll_screen_line_horizontally (screen, screen->row,
					       screen->column,
					       (screen->width - x_value),
					       (screen->column + x_value));
	      i = j; /* 1 added in for loop */
	      continue;

	    case 'm':
	      if ((j == (i + 3)) && ((x_value == 0) || (x_value == 7)))
	      {
		/* Enter stdout (7) or exit stdout (0) */
		screen->write_attribute = (x_value == 7);
		i = j;  /* 1 added in for loop */
		continue;
	      }
	      goto use_default;

	    case 'p':
	      /* Not a real ANSI escape.  Modelled after aaa. */
	      if ((j == (i + 3)) && (x_value < 2))
	      {
		/* Enter edwin/emacs (1) mode or exit edwin/emacs (0) mode. */
		if (x_value == 1)
		{
		  screen->mode_flags |= SCREEN_MODE_EDWIN;
		  screen->mode_flags &= (~ SCREEN_MODE_NEWLINE_CRS);
		  screen->scroll_lines = 1;
		  SetWindowText (screen->hWnd, "Edwin");
		}
		else
		{
		  screen->mode_flags &= (~ SCREEN_MODE_EDWIN);
		  screen->mode_flags |= SCREEN_MODE_NEWLINE_CRS;
		  screen->scroll_lines
		    = (COMPUTE_SCROLL_LINES (screen->height));
		  SetWindowText (screen->hWnd, "MIT/GNU Scheme");
		}
		i = j;  /* 1 added in for loop */
		continue;
	      }
	      goto use_default;

	    default:
	    use_default:
	      MessageBeep (0);
	      i = j;    /* 1 added in for loop */
	      continue;
	  }
	}
	break;
      }
    }
    }
  }

  if (lpBlock != lpBlock_in)
    LocalFree (lpBlock);

  Finish_ScreenWriteChar (screen, &state);
  if (saved_mode_flags != 0)
  {
    UpdateWindow (screen->hWnd);
    MoveScreenCursor (screen);
    screen->mode_flags |= saved_mode_flags;
  }
  return  TRUE;
}

/* A fast raw write to the screen memory. */
/* Client is responsible for invalidating the correct region */

VOID
WriteScreenBlock_NoInvalidRect (SCREEN screen, int row, int column,
				LPSTR lpBlock, int nLength)
{
  int i, limit, start;

  if (row < 0  ||  row >= MAXROWS)
    return;

  if (column < 0) {
    lpBlock += (- column);
    nLength += column;
    column = 0;
  }

  if (column + nLength >= MAXCOLS)
    limit = MAXCOLS - column;
  else
    limit = nLength;

  start = row * MAXCOLS + column;
  for (i = 0; i < limit; i++) {
    int place = start + i;
    screen->chars[place] = lpBlock[i];
    screen->attrs[place] = screen->write_attribute;
  }
}

/* Utilities for line-buffered input. */

static VOID
key_buffer_insert_self (SCREEN screen, int ch)
{
  if (screen->n_chars < MAX_LINEINPUT)
  {
    screen->line_buffer[screen->n_chars++] = ch;
    if (screen->mode_flags & SCREEN_MODE_ECHO)
    {
      if (ch == '\n')
	Screen_CRLF (screen);
      else
      {
	char c = ((char) ch);
#if 0
	Screen_WriteCharUninterpreted (screen, ch, NULL);
#endif
	WriteScreenBlock (screen->hWnd, &c, 1);
      }
    }
  }
}

static VOID
key_buffer_erase_character (SCREEN screen)
{
  if (screen->n_chars > 0)
  {
    screen->n_chars -= 1;
    if (screen->mode_flags & SCREEN_MODE_ECHO)
    {
      Screen_BS (screen);
      Screen_WriteCharUninterpreted (screen, ' ', NULL);
      Screen_BS (screen);
    }
  }
}

static VOID
buffered_key_command (SCREEN screen,  int ch)
{
  switch (ch)
  {
    case '\n':
    case '\r':
      key_buffer_insert_self (screen, '\n');
      break;

    case '\b':
    case ASCII_DEL:
      key_buffer_erase_character (screen);
      break;

    default:
      key_buffer_insert_self (screen, ch);
      break;
  }
  if (screen->mode_flags & SCREEN_MODE_EAGER_UPDATE)
    UpdateWindow (screen->hWnd);
}

/* Line-buffered input. */

static int
ReadScreen_line_input (SCREEN screen, LPSTR buffer, int buflen)
{
  SCREEN_EVENT event;
  int result = (-1);			/* No EOL seen yet. */

  while (read_event (screen, SCREEN_EVENT_TYPE_KEY, 1, (&event)))
    {
      int ch = event.event.key.ch;
      if ((event.event.key.control_key_state & SCREEN_ANY_ALT_KEY_MASK) != 0)
	ch |= 0200;

      if (ch != 0)
	buffered_key_command (screen, ch);

      if ((ch == '\n') || (ch == '\r'))
	{
	  int i, count = (min (screen->n_chars, buflen));

	  for (i = 0; i < count; i++)
	    buffer[i] = screen->line_buffer[i];
	  screen->n_chars -= count;
	  if (screen->n_chars > 0)
	    _fmemmove (&screen->line_buffer[0],
		       &screen->line_buffer[count],
		       screen->n_chars);
	  result = (count);
	  break;
	}
    }
  return (result);
}

/* Untranslated/unbuffered input */

static int
ReadScreen_raw (SCREEN screen, LPSTR buffer, int buflen)
{
  SCREEN_EVENT event;
  int position = 0;

  while ((position < buflen)
	 && (read_event (screen, SCREEN_EVENT_TYPE_KEY, 1, (&event))))
    {
      int ch = event.event.key.ch;
      if ((event.event.key.control_key_state & SCREEN_ANY_ALT_KEY_MASK) != 0)
	ch |= 0200;

      /* Store the character */
      buffer[position++] = ch;
      if (screen->mode_flags & SCREEN_MODE_ECHO)
	{
	  char c = ((char) ch);
	  WriteScreenBlock (screen->hWnd, &c, 1);
	}
    }
  return ((position == 0) ? -1 : position);
}

/* int  ReadScreen (SCREEN screen, LPSTR buffer, int buflen)

   Read characters into buffer.
   If in line mode, collect characters into the line buffer.
   Return the number of characters read.
   In raw mode, return -1 if there are no characters.
   If in line mode and not yet at end of line, return -1 (i.e. this
     is a non-blocking read
*/

static int
ReadScreen (SCREEN screen, LPSTR buffer, int buflen)
{
  if (screen->mode_flags & SCREEN_MODE_LINE_INPUT)
    return (ReadScreen_line_input (screen, buffer, buflen));
  else
    return (ReadScreen_raw (screen, buffer, buflen));
}

VOID
Screen_Clear (SCREEN screen, int kind)
{
  if (kind == 0)
  {
    /* clear whole screen */
      ClearScreen_internal(screen);
    InvalidateRect (screen->hWnd, NULL, TRUE);
    return;
  }
  if (kind == 1)
    /* clear to eol */
    return;
}

/* VOID GetMinMaxSizes (HWND hWnd, LPPOINT min_size, LPPOINT max_size)

   Description:
      determine the minimum and maxinum sizes for a screen window.
*/

static VOID
GetMinMaxSizes (HWND hWnd, LPPOINT min_size, LPPOINT max_size)
{
    SCREEN  screen = GETSCREEN (hWnd);
    int  extra_width, extra_height;

    if (screen==0) return;
    extra_width  = 2*GetSystemMetrics(SM_CXFRAME);
    extra_height = 2*GetSystemMetrics(SM_CYFRAME)
		 + GetSystemMetrics(SM_CYCAPTION)
		 + (GetMenu(hWnd) ? GetSystemMetrics(SM_CYMENU) : 0);
    /* The min size sould be configurable so Edwin can configure it */
    /* to prevent the window being shrunk so far that the displayed */
    /* buffers wont fit. */
    min_size->x = screen->xChar * 5  +  extra_width;
    min_size->y = screen->yChar * 3  +  extra_height;
    max_size->x = screen->xChar * MAXCOLS  +  extra_width;
    max_size->y = screen->yChar * MAXROWS  +  extra_height;
}

static BOOL
SelectScreenFont (SCREEN  screen,  HWND owner)
{
   CHOOSEFONT  cfTTYFont;

   if (NULL == screen)  return  FALSE;

   cfTTYFont.lStructSize    = sizeof (CHOOSEFONT);
   cfTTYFont.hwndOwner      = owner;
   cfTTYFont.hDC            = NULL;
   cfTTYFont.rgbColors      = screen->rgbFGColour;
   cfTTYFont.lpLogFont      = &screen->lfFont;
   cfTTYFont.Flags          = (
			         CF_FIXEDPITCHONLY
			       | CF_SCREENFONTS
			       | CF_EFFECTS
			       | CF_INITTOLOGFONTSTRUCT
			       );
   cfTTYFont.lCustData      = 0;
   cfTTYFont.lpfnHook       = NULL;
   cfTTYFont.lpTemplateName = NULL;
   cfTTYFont.hInstance      = (HINSTANCE) GetWindowLong(owner, GWL_HINSTANCE);

   if (ChooseFont (&cfTTYFont))
   {
     screen->rgbFGColour = cfTTYFont.rgbColors;
     ResetScreen (screen);
   }
   return  TRUE;
}

/* BOOL SelectScreenBackColor (SCREEN screen, HWND owner)

   Description:
      Selects the background color for the TTY screen.
      Uses the Common Dialog ChooseColor() API.
*/

static BOOL
SelectScreenBackColor (SCREEN  screen,  HWND owner)
{
   static DWORD custcolors[16] = {
      RGB(0x00,0x00,0x00)
     ,RGB(0x80,0x00,0x00)
     ,RGB(0x00,0x80,0x00)
     ,RGB(0x80,0x80,0x00)
     ,RGB(0x00,0x00,0x80)
     ,RGB(0x80,0x00,0x80)
     ,RGB(0x00,0x80,0x80)
     ,RGB(0xC0,0xC0,0xC0)
     ,RGB(0xC0,0xDC,0xC0)
     ,RGB(0xA6,0xCA,0xF0)
     ,RGB(0xFF,0xFB,0xF0)
     ,RGB(0xA0,0xA0,0xA4)
     ,RGB(0x80,0x80,0x80)
     /* ,RGB(0xFF,0x00,0x00) */
     ,RGB(0x00,0xFF,0x00)
     ,RGB(0xFF,0xFF,0x00)
     /* ,RGB(0x00,0x00,0xFF) */
     /* ,RGB(0xFF,0x00,0xFF) */
     /* ,RGB(0x00,0xFF,0xFF) */
     ,RGB(0xFF,0xFF,0xFF)
     };

   CHOOSECOLOR backcolor;

   if (NULL == screen)
     return  FALSE;

   backcolor.lStructSize    = sizeof (CHOOSECOLOR);
   backcolor.hwndOwner      = owner;
   backcolor.hInstance      = (HINSTANCE) GetWindowLong(owner, GWL_HINSTANCE);

   backcolor.rgbResult      = screen->rgbBGColour;
   backcolor.lpCustColors   = &custcolors[0];
   backcolor.Flags          = (CC_RGBINIT);

   backcolor.lCustData      = 0;
   backcolor.lpfnHook       = NULL;
   backcolor.lpTemplateName = NULL;

   if (ChooseColor (&backcolor))
   {
     HDC hdc = GetDC (owner);

     /* Use GetNearestColor to ensure consistency with the background */
     /* text color. */
     screen->rgbBGColour = GetNearestColor (hdc, (backcolor.rgbResult));
     if (screen->bkgnd_brush != NULL)
       DeleteObject (screen->bkgnd_brush);
     screen->bkgnd_brush = CreateSolidBrush (screen->rgbBGColour);
     InvalidateRect (owner, NULL, TRUE);
     ReleaseDC (owner, hdc);
   }
   return  TRUE;
}

/* Event Queue */

static SCREEN_EVENT *
allocate_event (SCREEN screen, SCREEN_EVENT_TYPE type)
{
  SCREEN_EVENT_LINK * new_event;
  if (((screen -> mode_flags) & type) == 0)
    return (0);
  if (free_events == 0)
    new_event = (xmalloc (sizeof (SCREEN_EVENT_LINK)));
  else
    {
      new_event = free_events;
      free_events = (free_events -> next);
      n_free_events -= 1;
    }
  (new_event -> next) = 0;
  if (event_queue_tail == 0)
    event_queue_head = new_event;
  else
    (event_queue_tail -> next) = new_event;
  event_queue_tail = new_event;
  ((new_event -> event) . handle) = (screen -> hWnd);
  ((new_event -> event) . type) = type;
  return (& (new_event -> event));
}

/* read_event (screen, type, delete_p, event)
   Reads the next matching event out of the queue.
   Returns non-zero iff a matching event is found.
   If screen is non-zero, only events for that screen are considered.
   If type is non-zero, only events of that type are considered.
   If delete_p is non-zero, the event is deleted from the queue.  */

static int
read_event (SCREEN screen, SCREEN_EVENT_TYPE type, int delete_p,
	    SCREEN_EVENT * event)
{
  SCREEN_EVENT_LINK * scan_queue = event_queue_head;
  SCREEN_EVENT_LINK * prev_queue = 0;
  while (scan_queue != 0)
    {
      if (((screen == 0)
	   || (((scan_queue -> event) . handle) == (screen -> hWnd)))
	  && ((type == 0) || (((scan_queue -> event) . type) == type)))
	{
	  if (event != 0)
	    (*event) = (scan_queue -> event);
	  if (delete_p)
	    {
	      if (prev_queue == 0)
		{
		  event_queue_head = (event_queue_head -> next);
		  if (event_queue_head == 0)
		    event_queue_tail = 0;
		}
	      else
		{
		  (prev_queue -> next) = (scan_queue -> next);
		  if (event_queue_tail == scan_queue)
		    event_queue_tail = prev_queue;
		}
	      ((scan_queue -> event) . handle) = INVALID_HANDLE_VALUE;
	      if (n_free_events < MAX_FREE_EVENTS)
		{
		  (scan_queue -> next) = free_events;
		  free_events = scan_queue;
		  n_free_events += 1;
		}
	      else
		free (scan_queue);
	    }
	  return (1);
	}
      else
	{
	  prev_queue = scan_queue;
	  scan_queue = (scan_queue -> next);
	}
    }
  return (0);
}

int
Screen_read_event (SCREEN_EVENT * event)
{
  int result = (read_event (0, 0, 1, event));
  if (win32_trace_level > 1)
    {
      fprintf (win32_trace_file, "Screen_read_event: result=%d\n", result);
      fflush (win32_trace_file);
    }
  return (result);
}

int
Screen_pending_events_p (void)
{
  return (read_event (0, 0, 0, 0));
}

VOID
Screen_SetAttribute (HANDLE screen, SCREEN_ATTRIBUTE sa)
{
  SendMessage (screen, SCREEN_SETATTRIBUTE, (WPARAM)sa, 0);
}

VOID _fastcall
Screen_SetAttributeDirect (SCREEN screen, SCREEN_ATTRIBUTE sa)
{
  screen->write_attribute = sa;
}

VOID
Screen_WriteChar (HANDLE screen, char ch)
{
  SendMessage (screen, SCREEN_WRITE, 1, (LPARAM)(LPSTR) &ch);
}

VOID
Screen_WriteText (HANDLE screen, char *string)
{
  SendMessage (screen, SCREEN_WRITE, strlen(string), (LPARAM)(LPSTR)string);
}

VOID
Screen_SetCursorPosition (HANDLE screen, int line, int column)
{
  SendMessage (screen, SCREEN_SETPOSITION, 0, MAKELPARAM(column,line));
}

VOID
Screen_SetMode (HANDLE screen, int mode)
{
  SendMessage (screen, SCREEN_SETMODES, ((WPARAM) mode), 0);
}

int
Screen_GetMode (HANDLE screen)
{
  return  SendMessage (screen, SCREEN_GETMODES, 0, 0);
}

#define SCREEN_MODE_COOKED (SCREEN_MODE_LINE_INPUT | SCREEN_MODE_ECHO)

int
Screen_Read (HANDLE hWnd, BOOL buffered_p, char * buffer, int buflen)
{
  int result;
  WORD input_flags;
  SCREEN screen = (GETSCREEN (hWnd));

  if (screen != NULL)
  {
    input_flags = (screen->mode_flags & SCREEN_MODE_COOKED);
    screen->mode_flags &= (~ SCREEN_MODE_COOKED);
    if (buffered_p)
      screen->mode_flags |= SCREEN_MODE_COOKED;
  }

  result = (SendMessage (hWnd, SCREEN_READ,
			 ((WPARAM) buflen), ((LPARAM) buffer)));

  if (screen != NULL)
  {
    screen->mode_flags &= (~ SCREEN_MODE_COOKED);
    screen->mode_flags |= input_flags;
  }

  return  result;
}

VOID
Screen_GetSize (HWND hWnd, int *rows, int *columns)
{
  SCREEN  screen = GETSCREEN (hWnd);
  if (screen == 0)
    return;
  *rows    = screen->height;
  *columns = screen->width;
}

VOID
Screen_CR_to_RECT (RECT * rect, SCREEN screen,
		   int lo_row, int lo_col,
		   int hi_row, int hi_col)
{
  rect->left   = ((lo_col * screen->xChar) - screen->xOffset);
  rect->right  = ((hi_col * screen->xChar) - screen->xOffset);
  rect->top    = ((lo_row * screen->yChar) - screen->yOffset);
  rect->bottom = ((hi_row * screen->yChar) - screen->yOffset);
}

VOID
Enable_Cursor (SCREEN screen, BOOL show)
{
  ScreenCurrentFocus();
  if (show) {
    if (!screen->cursor_visible) {
      screen->cursor_visible = TRUE;
      if (screen && screen == screen_focus)
	ShowCaret (screen->hWnd);
    }
  } else {
    if (screen->cursor_visible) {
      screen->cursor_visible = FALSE;
      if (screen && screen == screen_focus)
	HideCaret (screen->hWnd);
    }
  }
}

HICON
ScreenSetIcon(SCREEN screen, HICON hIcon)
{
  HICON  result = screen->hIcon;
  screen->hIcon = hIcon;
  return  result;
}

BOOL
ScreenSetDefaultFont (char *description)
{
  LOGFONT lf;
  HFONT hfont;

  /* modify default name & size, but undo characteristics */
  lf = lfDefaultLogFont;
  (lf . lfWeight) = FW_NORMAL;
  (lf . lfItalic) = FALSE;
  (lf . lfUnderline) = FALSE;
  (lf . lfStrikeOut) = FALSE;
  hfont = (set_font_1 (description, (&lf)));
  if (hfont == NULL)
    return (FALSE);
  else
    {
      DeleteObject (hfont);
      lfDefaultLogFont = lf;
      return (TRUE);
    }
}

BOOL
ScreenSetFont (SCREEN screen, char *description)
{
  LOGFONT lf;
  HFONT hfont;

  init_LOGFONT (&lf);
  hfont = (set_font_1 (description, (&lf)));
  if (hfont == NULL)
    return (FALSE);
  else
    {
      (screen -> hFont) = hfont;
      (screen -> lfFont) = lf;
      ResetScreen (screen);
      return (TRUE);
    }
}

static HFONT
set_font_1 (char * description, LOGFONT * lf)
{
  HFONT hfont = NULL;
  if (parse_logfont (description, lf))
    {
      (void) search_for_font (lf);
      hfont = (CreateFontIndirect (lf));
    }
  return (hfont);
}

static BOOL
parse_logfont (char * name, LOGFONT * lf)
{
  int i = 0;
  int name_ended = 0;
  int number_p;
  int len;
  char * start = name;
  char * end = name;
  char * scan;

  while (1)
    {
      while ((*start) == ' ')
	start += 1;
      if ((*start) == '\0')
	return (TRUE);
      end = start;
      while (((*end) != ' ') && ((*end) != '\0'))
	end += 1;
      len = (end - start);
      scan = start;
      number_p = 0;
      if (scan < end)
	while (1)
	  {
	    if (scan == end)
	      {
		number_p = 1;
		break;
	      }
	    if (! (((*scan) >= '0') && ((*scan) <= '9')))
	      {
		number_p = 0;
		break;
	      }
	    scan += 1;
	  }
      if (number_p)
	{
	  long points = (atol (start));
	  (lf -> lfHeight) = (- (points_to_logical_units (points)));
	  name_ended = 1;
	}
      else if ((len == 4) && ((_strnicmp (start, "bold", len)) == 0))
	{
	  (lf -> lfWeight) = FW_BOLD;
	  name_ended = 1;
	}
      else if ((len == 6) && ((_strnicmp (start, "italic", len)) == 0))
	{
	  (lf -> lfItalic) = TRUE;
	  name_ended = 1;
	}
      else if ((len == 7) && ((_strnicmp (start, "regular", len)) == 0))
	{
	  (lf -> lfWeight) = FW_NORMAL;
	  (lf -> lfItalic) = FALSE;
	  name_ended = 1;
	}
      else if ((len == 9) && ((_strnicmp (start, "underline", len)) == 0))
	{
	  (lf -> lfUnderline) = TRUE;
	  name_ended = 1;
	}
      else if ((len == 9) && ((_strnicmp (start, "strikeout", len)) == 0))
	{
	  (lf -> lfStrikeOut) = TRUE;
	  name_ended = 1;
	}
      else if ((len < (LF_FACESIZE - i)) && (!name_ended))
	{
	  if (i > 0)
	    ((lf -> lfFaceName) [i++]) = ' ';
	  while (start < end)
	    ((lf -> lfFaceName) [i++]) = (*start++);
	  ((lf -> lfFaceName) [i]) = '\0';
	}
      else
	return (FALSE);
      start = end;
    }
}

static long
points_to_logical_units (long points)
{
  HDC hdc = (CreateDC ("DISPLAY", NULL, NULL, NULL));
  float pixels_per_inch_y = ((float) (GetDeviceCaps (hdc, LOGPIXELSY)));
  float pixels = ((((float) points) / 72.0) * pixels_per_inch_y);
  POINT pt;
  (pt . x) = 0;
  (pt . y) = ((int) (pixels * 10.0));
  DPtoLP (hdc, (&pt), 1);
  DeleteDC (hdc);
  return ((pt . y) / 10);
}

struct enum_font_args
{
  LOGFONT * lf;
  ENUMLOGFONT elf;
  BOOL foundp;
};

static BOOL
search_for_font (LOGFONT * lf)
{
  HDC hdc = (CreateDC ("DISPLAY", NULL, NULL, NULL));
  struct enum_font_args args;
  (args . lf) = lf;
  (args . foundp) = FALSE;
  (void) EnumFontFamilies (hdc,
			   (lf -> lfFaceName),
			   search_for_font_proc,
			   ((LPARAM) (&args)));
  if (args . foundp)
    (*lf) = (args . elf . elfLogFont);
  DeleteDC (hdc);
  return (args . foundp);
}

static int CALLBACK
search_for_font_proc (ENUMLOGFONT * elf, NEWTEXTMETRIC * ntm, int type,
		      LPARAM a)
{
  struct enum_font_args * args = ((struct enum_font_args *) a);
  if ((((elf -> elfLogFont) . lfHeight) == (- (args -> lf -> lfHeight)))
      && (((elf -> elfLogFont) . lfWeight) == (args -> lf -> lfWeight))
      && (((elf -> elfLogFont) . lfItalic) == (args -> lf -> lfItalic))
      && (((elf -> elfLogFont) . lfUnderline) == (args -> lf -> lfUnderline))
      && (((elf -> elfLogFont) . lfStrikeOut) == (args -> lf -> lfStrikeOut)))
    {
      (args -> elf) = (*elf);
      (args -> foundp) = TRUE;
      return (0);
    }
  else
    return (1);
}

static BOOL
change_colour (SCREEN screen, DWORD requested_colour, DWORD *colour_slot)
{
  HWND hWnd = screen->hWnd;
  HDC hdc = GetDC (hWnd);
  COLORREF actual_colour = GetNearestColor (hdc, requested_colour);
  if (actual_colour == CLR_INVALID) {
    ReleaseDC (hWnd, hdc);
    return  FALSE;
  }
  *colour_slot = actual_colour;

  /* Redraw screen with new colours */
  if (screen->bkgnd_brush != NULL)
    DeleteObject (screen->bkgnd_brush);
  screen->bkgnd_brush = CreateSolidBrush (screen->rgbBGColour);
  InvalidateRect (hWnd, NULL, TRUE);
  ReleaseDC (hWnd, hdc);

  return  TRUE;
}

BOOL
ScreenSetForegroundColour (SCREEN screen, DWORD colour)
{
  return  change_colour (screen, colour, &screen->rgbFGColour);
}

BOOL
ScreenSetBackgroundColour (SCREEN screen, DWORD colour)
{
  return  change_colour (screen, colour, &screen->rgbBGColour);
}

static const char *
translate_message_code (UINT uMsg)
{
  switch (uMsg)
    {
    case WM_NULL: return ("WM_NULL");
    case WM_CREATE: return ("WM_CREATE");
    case WM_DESTROY: return ("WM_DESTROY");
    case WM_MOVE: return ("WM_MOVE");
    case WM_SIZE: return ("WM_SIZE");
    case WM_ACTIVATE: return ("WM_ACTIVATE");
    case WM_SETFOCUS: return ("WM_SETFOCUS");
    case WM_KILLFOCUS: return ("WM_KILLFOCUS");
    case WM_ENABLE: return ("WM_ENABLE");
    case WM_SETREDRAW: return ("WM_SETREDRAW");
    case WM_SETTEXT: return ("WM_SETTEXT");
    case WM_GETTEXT: return ("WM_GETTEXT");
    case WM_GETTEXTLENGTH: return ("WM_GETTEXTLENGTH");
    case WM_PAINT: return ("WM_PAINT");
    case WM_CLOSE: return ("WM_CLOSE");
    case WM_QUERYENDSESSION: return ("WM_QUERYENDSESSION");
    case WM_QUIT: return ("WM_QUIT");
    case WM_QUERYOPEN: return ("WM_QUERYOPEN");
    case WM_ERASEBKGND: return ("WM_ERASEBKGND");
    case WM_SYSCOLORCHANGE: return ("WM_SYSCOLORCHANGE");
    case WM_ENDSESSION: return ("WM_ENDSESSION");
    case WM_SHOWWINDOW: return ("WM_SHOWWINDOW");
    case WM_WININICHANGE: return ("WM_WININICHANGE");
    case WM_DEVMODECHANGE: return ("WM_DEVMODECHANGE");
    case WM_ACTIVATEAPP: return ("WM_ACTIVATEAPP");
    case WM_FONTCHANGE: return ("WM_FONTCHANGE");
    case WM_TIMECHANGE: return ("WM_TIMECHANGE");
    case WM_CANCELMODE: return ("WM_CANCELMODE");
    case WM_SETCURSOR: return ("WM_SETCURSOR");
    case WM_MOUSEACTIVATE: return ("WM_MOUSEACTIVATE");
    case WM_CHILDACTIVATE: return ("WM_CHILDACTIVATE");
    case WM_QUEUESYNC: return ("WM_QUEUESYNC");
    case WM_GETMINMAXINFO: return ("WM_GETMINMAXINFO");
    case WM_PAINTICON: return ("WM_PAINTICON");
    case WM_ICONERASEBKGND: return ("WM_ICONERASEBKGND");
    case WM_NEXTDLGCTL: return ("WM_NEXTDLGCTL");
    case WM_SPOOLERSTATUS: return ("WM_SPOOLERSTATUS");
    case WM_DRAWITEM: return ("WM_DRAWITEM");
    case WM_MEASUREITEM: return ("WM_MEASUREITEM");
    case WM_DELETEITEM: return ("WM_DELETEITEM");
    case WM_VKEYTOITEM: return ("WM_VKEYTOITEM");
    case WM_CHARTOITEM: return ("WM_CHARTOITEM");
    case WM_SETFONT: return ("WM_SETFONT");
    case WM_GETFONT: return ("WM_GETFONT");
    case WM_SETHOTKEY: return ("WM_SETHOTKEY");
    case WM_GETHOTKEY: return ("WM_GETHOTKEY");
    case WM_QUERYDRAGICON: return ("WM_QUERYDRAGICON");
    case WM_COMPAREITEM: return ("WM_COMPAREITEM");
    case WM_COMPACTING: return ("WM_COMPACTING");
    case WM_COMMNOTIFY: return ("WM_COMMNOTIFY");
    case WM_WINDOWPOSCHANGING: return ("WM_WINDOWPOSCHANGING");
    case WM_WINDOWPOSCHANGED: return ("WM_WINDOWPOSCHANGED");
    case WM_POWER: return ("WM_POWER");
    case WM_COPYDATA: return ("WM_COPYDATA");
    case WM_CANCELJOURNAL: return ("WM_CANCELJOURNAL");
    case WM_NCCREATE: return ("WM_NCCREATE");
    case WM_NCDESTROY: return ("WM_NCDESTROY");
    case WM_NCCALCSIZE: return ("WM_NCCALCSIZE");
    case WM_NCHITTEST: return ("WM_NCHITTEST");
    case WM_NCPAINT: return ("WM_NCPAINT");
    case WM_NCACTIVATE: return ("WM_NCACTIVATE");
    case WM_GETDLGCODE: return ("WM_GETDLGCODE");
    case WM_NCMOUSEMOVE: return ("WM_NCMOUSEMOVE");
    case WM_NCLBUTTONDOWN: return ("WM_NCLBUTTONDOWN");
    case WM_NCLBUTTONUP: return ("WM_NCLBUTTONUP");
    case WM_NCLBUTTONDBLCLK: return ("WM_NCLBUTTONDBLCLK");
    case WM_NCRBUTTONDOWN: return ("WM_NCRBUTTONDOWN");
    case WM_NCRBUTTONUP: return ("WM_NCRBUTTONUP");
    case WM_NCRBUTTONDBLCLK: return ("WM_NCRBUTTONDBLCLK");
    case WM_NCMBUTTONDOWN: return ("WM_NCMBUTTONDOWN");
    case WM_NCMBUTTONUP: return ("WM_NCMBUTTONUP");
    case WM_NCMBUTTONDBLCLK: return ("WM_NCMBUTTONDBLCLK");
    case WM_KEYDOWN: return ("WM_KEYDOWN");
    case WM_KEYUP: return ("WM_KEYUP");
    case WM_CHAR: return ("WM_CHAR");
    case WM_DEADCHAR: return ("WM_DEADCHAR");
    case WM_SYSKEYDOWN: return ("WM_SYSKEYDOWN");
    case WM_SYSKEYUP: return ("WM_SYSKEYUP");
    case WM_SYSCHAR: return ("WM_SYSCHAR");
    case WM_SYSDEADCHAR: return ("WM_SYSDEADCHAR");
    case WM_KEYLAST: return ("WM_KEYLAST");
    case WM_INITDIALOG: return ("WM_INITDIALOG");
    case WM_COMMAND: return ("WM_COMMAND");
    case WM_SYSCOMMAND: return ("WM_SYSCOMMAND");
    case WM_TIMER: return ("WM_TIMER");
    case WM_HSCROLL: return ("WM_HSCROLL");
    case WM_VSCROLL: return ("WM_VSCROLL");
    case WM_INITMENU: return ("WM_INITMENU");
    case WM_INITMENUPOPUP: return ("WM_INITMENUPOPUP");
    case WM_MENUSELECT: return ("WM_MENUSELECT");
    case WM_MENUCHAR: return ("WM_MENUCHAR");
    case WM_ENTERIDLE: return ("WM_ENTERIDLE");
    case WM_CTLCOLORMSGBOX: return ("WM_CTLCOLORMSGBOX");
    case WM_CTLCOLOREDIT: return ("WM_CTLCOLOREDIT");
    case WM_CTLCOLORLISTBOX: return ("WM_CTLCOLORLISTBOX");
    case WM_CTLCOLORBTN: return ("WM_CTLCOLORBTN");
    case WM_CTLCOLORDLG: return ("WM_CTLCOLORDLG");
    case WM_CTLCOLORSCROLLBAR: return ("WM_CTLCOLORSCROLLBAR");
    case WM_CTLCOLORSTATIC: return ("WM_CTLCOLORSTATIC");
    case WM_MOUSEMOVE: return ("WM_MOUSEMOVE");
    case WM_LBUTTONDOWN: return ("WM_LBUTTONDOWN");
    case WM_LBUTTONUP: return ("WM_LBUTTONUP");
    case WM_LBUTTONDBLCLK: return ("WM_LBUTTONDBLCLK");
    case WM_RBUTTONDOWN: return ("WM_RBUTTONDOWN");
    case WM_RBUTTONUP: return ("WM_RBUTTONUP");
    case WM_RBUTTONDBLCLK: return ("WM_RBUTTONDBLCLK");
    case WM_MBUTTONDOWN: return ("WM_MBUTTONDOWN");
    case WM_MBUTTONUP: return ("WM_MBUTTONUP");
    case WM_MBUTTONDBLCLK: return ("WM_MBUTTONDBLCLK");
    case WM_PARENTNOTIFY: return ("WM_PARENTNOTIFY");
    case WM_ENTERMENULOOP: return ("WM_ENTERMENULOOP");
    case WM_EXITMENULOOP: return ("WM_EXITMENULOOP");
    case WM_MDICREATE: return ("WM_MDICREATE");
    case WM_MDIDESTROY: return ("WM_MDIDESTROY");
    case WM_MDIACTIVATE: return ("WM_MDIACTIVATE");
    case WM_MDIRESTORE: return ("WM_MDIRESTORE");
    case WM_MDINEXT: return ("WM_MDINEXT");
    case WM_MDIMAXIMIZE: return ("WM_MDIMAXIMIZE");
    case WM_MDITILE: return ("WM_MDITILE");
    case WM_MDICASCADE: return ("WM_MDICASCADE");
    case WM_MDIICONARRANGE: return ("WM_MDIICONARRANGE");
    case WM_MDIGETACTIVE: return ("WM_MDIGETACTIVE");
    case WM_MDISETMENU: return ("WM_MDISETMENU");
    case WM_ENTERSIZEMOVE: return ("WM_ENTERSIZEMOVE");
    case WM_EXITSIZEMOVE: return ("WM_EXITSIZEMOVE");
    case WM_DROPFILES: return ("WM_DROPFILES");
    case WM_MDIREFRESHMENU: return ("WM_MDIREFRESHMENU");
    case WM_CUT: return ("WM_CUT");
    case WM_COPY: return ("WM_COPY");
    case WM_PASTE: return ("WM_PASTE");
    case WM_CLEAR: return ("WM_CLEAR");
    case WM_UNDO: return ("WM_UNDO");
    case WM_RENDERFORMAT: return ("WM_RENDERFORMAT");
    case WM_RENDERALLFORMATS: return ("WM_RENDERALLFORMATS");
    case WM_DESTROYCLIPBOARD: return ("WM_DESTROYCLIPBOARD");
    case WM_DRAWCLIPBOARD: return ("WM_DRAWCLIPBOARD");
    case WM_PAINTCLIPBOARD: return ("WM_PAINTCLIPBOARD");
    case WM_VSCROLLCLIPBOARD: return ("WM_VSCROLLCLIPBOARD");
    case WM_SIZECLIPBOARD: return ("WM_SIZECLIPBOARD");
    case WM_ASKCBFORMATNAME: return ("WM_ASKCBFORMATNAME");
    case WM_CHANGECBCHAIN: return ("WM_CHANGECBCHAIN");
    case WM_HSCROLLCLIPBOARD: return ("WM_HSCROLLCLIPBOARD");
    case WM_QUERYNEWPALETTE: return ("WM_QUERYNEWPALETTE");
    case WM_PALETTEISCHANGING: return ("WM_PALETTEISCHANGING");
    case WM_PALETTECHANGED: return ("WM_PALETTECHANGED");
    case WM_HOTKEY: return ("WM_HOTKEY");
    case WM_PENWINFIRST: return ("WM_PENWINFIRST");
    case WM_PENWINLAST: return ("WM_PENWINLAST");

#if(WINVER >= 0x0400)
    case WM_NOTIFY: return ("WM_NOTIFY");
    case WM_INPUTLANGCHANGEREQUEST: return ("WM_INPUTLANGCHANGEREQUEST");
    case WM_INPUTLANGCHANGE: return ("WM_INPUTLANGCHANGE");
    case WM_TCARD: return ("WM_TCARD");
    case WM_HELP: return ("WM_HELP");
    case WM_USERCHANGED: return ("WM_USERCHANGED");
    case WM_NOTIFYFORMAT: return ("WM_NOTIFYFORMAT");
    case WM_CONTEXTMENU: return ("WM_CONTEXTMENU");
    case WM_STYLECHANGING: return ("WM_STYLECHANGING");
    case WM_STYLECHANGED: return ("WM_STYLECHANGED");
    case WM_DISPLAYCHANGE: return ("WM_DISPLAYCHANGE");
    case WM_GETICON: return ("WM_GETICON");
    case WM_SETICON: return ("WM_SETICON");
    case WM_IME_STARTCOMPOSITION: return ("WM_IME_STARTCOMPOSITION");
    case WM_IME_ENDCOMPOSITION: return ("WM_IME_ENDCOMPOSITION");
    case WM_IME_COMPOSITION: return ("WM_IME_COMPOSITION");
    case WM_NEXTMENU: return ("WM_NEXTMENU");
    case WM_SIZING: return ("WM_SIZING");
    case WM_CAPTURECHANGED: return ("WM_CAPTURECHANGED");
    case WM_MOVING: return ("WM_MOVING");
    case WM_POWERBROADCAST: return ("WM_POWERBROADCAST");
    case WM_DEVICECHANGE: return ("WM_DEVICECHANGE");
    case WM_IME_SETCONTEXT: return ("WM_IME_SETCONTEXT");
    case WM_IME_NOTIFY: return ("WM_IME_NOTIFY");
    case WM_IME_CONTROL: return ("WM_IME_CONTROL");
    case WM_IME_COMPOSITIONFULL: return ("WM_IME_COMPOSITIONFULL");
    case WM_IME_SELECT: return ("WM_IME_SELECT");
    case WM_IME_CHAR: return ("WM_IME_CHAR");
    case WM_IME_KEYDOWN: return ("WM_IME_KEYDOWN");
    case WM_IME_KEYUP: return ("WM_IME_KEYUP");
    case WM_PRINT: return ("WM_PRINT");
    case WM_PRINTCLIENT: return ("WM_PRINTCLIENT");
    case WM_HANDHELDFIRST: return ("WM_HANDHELDFIRST");
    case WM_HANDHELDLAST: return ("WM_HANDHELDLAST");
    case WM_AFXFIRST: return ("WM_AFXFIRST");
    case WM_AFXLAST: return ("WM_AFXLAST");
    case WM_APP: return ("WM_APP");
#endif /* WINVER >= 0x0400 */
    case SCREEN_WRITE: return ("SCREEN_WRITE");
    case SCREEN_SETPOSITION: return ("SCREEN_SETPOSITION");
    case SCREEN_GETPOSITION: return ("SCREEN_GETPOSITION");
    case SCREEN_SETATTRIBUTE: return ("SCREEN_SETATTRIBUTE");
    case SCREEN_GETATTRIBUTE: return ("SCREEN_GETATTRIBUTE");
    case SCREEN_PEEKEVENT: return ("SCREEN_PEEKEVENT");
    case SCREEN_READEVENT: return ("SCREEN_READEVENT");
    case SCREEN_SETMODES: return ("SCREEN_SETMODES");
    case SCREEN_GETMODES: return ("SCREEN_GETMODES");
    case SCREEN_SETCOMMAND: return ("SCREEN_SETCOMMAND");
    case SCREEN_GETCOMMAND: return ("SCREEN_GETCOMMAND");
    case SCREEN_SETBINDING: return ("SCREEN_SETBINDING");
    case SCREEN_GETBINDING: return ("SCREEN_GETBINDING");
    case SCREEN_SETMENU: return ("SCREEN_SETMENU");
    case SCREEN_READ: return ("SCREEN_READ");
    case SCREEN_CLEAR: return ("SCREEN_CLEAR");
    case WM_CATATONIC: return ("WM_CATATONIC");
    case WM_SCHEME_INTERRUPT: return ("WM_SCHEME_INTERRUPT");
    default: return (0);
    }
}

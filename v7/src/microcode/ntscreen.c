/* -*-C-*-

$Id: ntscreen.c,v 1.21 1994/10/25 15:36:18 adams Exp $

Copyright (c) 1993-1994 Massachusetts Institute of Technology

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

//#include <stdio.h>
#include <stdlib.h>
#include "nt.h"
#include "ntscreen.h"
#include "ntgui.h"
//#include "screen.rh"
/* Allow conditionalization for underlying OS. */
extern BOOL win32_under_win32s_p (void);

// constant definitions

#define GWL_SCREEN        0
#define SCREENEXTRABYTES        (sizeof(LONG))

//#define ATOM_TTYINFO       0x100

#define MAXCOLS 180
#define MAXROWS 100

// cursor states

#define CS_HIDE         0x00
#define CS_SHOW         0x01

// Flow control flags

// ascii definitions

#define ASCII_BEL       0x07
#define ASCII_BS        0x08
#define ASCII_LF        0x0A
#define ASCII_FF        0x0C
#define ASCII_CR        0x0D
#define ASCII_ESC       0x1B
#define ASCII_DEL       0x7F

// data structures

#define MAX_EVENTS 500

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
   
   char              * chars;
   SCREEN_ATTRIBUTE  * attrs;
   WORD              mode_flags;        //events & modes
   SCREEN_ATTRIBUTE  write_attribute;
   
   WORD    CursorState;

   HFONT   hFont;
   LOGFONT lfFont;
   DWORD   rgbFGColour;
   DWORD   rgbBGColour;
   int     xSize, ySize, xScroll, yScroll, xOffset, yOffset;
   int     column, row, xChar, yChar;
   int     width, height;  //size in characters

   int n_events;
   SCREEN_EVENT_LINK * events;
   SCREEN_EVENT_LINK * free_events;
   SCREEN_EVENT_LINK * queue_head, * queue_tail;

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

   // for line input
   int n_chars;
   char * line_buffer;

   /* ANSI emulator overflow */
   int n_pending;
   LPSTR pending;
   
   HBRUSH bkgnd_brush;
   int scroll_lines;

} SCREEN_STRUCT;

//#define WIDTH(screen) (screen->width)
#define WIDTH(screen) MAXCOLS
#define HEIGHT(screen) MAXROWS
// macros ( for easier readability )


#define GETSCREEN( x ) ((SCREEN) GetWindowLong( x, GWL_SCREEN ))
#define SETSCREEN( x, y ) SetWindowLong( x, GWL_SCREEN, (LONG) y )


// CRT mappings to NT API

#define _fmemset   memset
#define _fmemmove  memmove


LRESULT CreateScreenInfo (HWND);
VOID DestroyScreenInfo (HWND);
BOOL ResetScreen (SCREEN);
BOOL KillScreenFocus (HWND);
VOID PaintScreen (HWND);
VOID EraseScreen (HWND, HDC);
BOOL SetScreenFocus (HWND);
BOOL ScrollScreenHorz (HWND, WORD, WORD);
BOOL ScrollScreenVert (HWND, WORD, WORD);
BOOL SizeScreen (HWND, WORD, WORD);
VOID ProcessScreenCharacter (HWND, int, DWORD);
BOOL Process_KeyDown (HWND, UINT, WPARAM, LPARAM);
VOID ProcessMouseButton (HWND, UINT, UINT, LONG, BOOL);
VOID ProcessCloseMessage (SCREEN);
BOOL WriteScreenBlock (HWND, LPSTR, int);
int  ReadScreen (SCREEN, char*, int);
VOID MoveScreenCursor (SCREEN);
UINT ScreenPeekOrRead (SCREEN, int count, SCREEN_EVENT* buffer, BOOL remove);
COMMAND_HANDLER ScreenSetCommand (SCREEN, WORD cmd, COMMAND_HANDLER handler);
WORD ScreenSetBinding (SCREEN, char key, WORD command);
VOID GetMinMaxSizes(HWND,LPPOINT,LPPOINT);
VOID Screen_Clear (SCREEN,int);
BOOL SelectScreenFont (SCREEN, HWND);
BOOL SelectScreenBackColor (SCREEN, HWND);


/*Put here for a lack of a better place to put it */
LONG init_font_height (char * font_size_symbol);

LRESULT ScreenCommand_ChooseFont (HWND, WORD);
LRESULT ScreenCommand_ChooseBackColor (HWND, WORD);

SCREEN_EVENT  *alloc_event (SCREEN, SCREEN_EVENT_TYPE); //may return NULL
int  GetControlKeyState(void);

//void *xmalloc (int size);
//void xfree (void*);
#define xfree free
#define xmalloc malloc

LRESULT FAR PASCAL ScreenWndProc (HWND, UINT, WPARAM, LPARAM);

VOID RegisterScreen (SCREEN);
VOID UnregisterScreen (SCREEN);

BOOL
init_color (char *color_symbol, HWND hWnd, DWORD *color)
{
  HDC hdc;
  char * envvar = getenv (color_symbol);
  if (envvar == NULL)
    return  FALSE;
  // Use GetNearestColor to ensure consistency with the background text color.
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

static LONG
init_font_height (char *font_size_symbol)
{
  char *envvar = getenv (font_size_symbol);
  
  if (envvar == NULL)
    return  9; //default font height is 9
  
  return ((long) strtoul (envvar, NULL, 0));
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

//---------------------------------------------------------------------------
//  BOOL Screen_InitApplication (HANDLE hInstance )
//
//  Description:
//     First time initialization stuff for screen class.
//     This registers information such as window classes.
//
//  Parameters:
//     HANDLE hInstance
//        Handle to this instance of the application.
//
//---------------------------------------------------------------------------

BOOL 
Screen_InitApplication (HANDLE hInstance)
{
   WNDCLASS  wndclass ;

#ifdef WINDOWSLOSES
   init_MIT_Keyboard ();
#endif /* WINDOWSLOSES */

   wndclass.style =         0;
   wndclass.lpfnWndProc =   ScreenWndProc ;
   wndclass.cbClsExtra =    0;
   wndclass.cbWndExtra =    SCREENEXTRABYTES ;
   wndclass.hInstance =     hInstance ;
   wndclass.hIcon =         LoadIcon (hInstance, "SHIELD3_ICON");
   wndclass.hCursor =       LoadCursor (NULL, IDC_ARROW);
   wndclass.hbrBackground = NULL;
   wndclass.lpszMenuName =  0;
   wndclass.lpszClassName = "MIT-SCREEN";

   return  RegisterClass (&wndclass);
}

//---------------------------------------------------------------------------
//  BOOL Screen_InitInstance (HANDLE hInstance, int nCmdShow )
//
//  Description:
//     Initializes instance specific information for the screen class.
//     returns TRUE on success.
//
//  Parameters:
//     HANDLE hInstance
//        Handle to instance
//
//     int nCmdShow
//        How do we show the window?
//---------------------------------------------------------------------------

static  HANDLE  ghInstance;

BOOL 
Screen_InitInstance (HANDLE hInstance, int nCmdShow )
{
  ghInstance = hInstance;
  return  TRUE;
}

//---------------------------------------------------------------------------
//  SCREEN  Screen_Create (HANDLE hParent, LPCSTR title, int nCmdShow)
//
//  Description:
//     Create a screen window with a given parent.
//
//  Parameters:
//     hParent
//        Handle to parent window
//---------------------------------------------------------------------------

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
  int ctr, params[4];

  for (ctr = 0; ctr < 4; ctr++)
    params[ctr] = -1;

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

//---------------------------------------------------------------------------
//  Registry of screen handles
//---------------------------------------------------------------------------

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
  //  if (link)
  *link = screen->registry_link;
}

BOOL
Screen_IsScreenHandle (HANDLE handle)
{
  return  head_to_registered_screen (handle) != 0;
}

//---------------------------------------------------------------------------
//  LRESULT FAR PASCAL ScreenWndProc (HWND hWnd, UINT uMsg,
//                                 WPARAM wParam, LPARAM lParam )
//
//  This is the TTY Window Proc.  This handles ALL messages to the tty
//  window.
//
//---------------------------------------------------------------------------

LRESULT FAR PASCAL 
ScreenWndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
   SCREEN  screen = GETSCREEN (hWnd);

   switch (uMsg)
   {
      case WM_CREATE:
      {
	 LRESULT result = CreateScreenInfo (hWnd);
	 ShowWindow (hWnd, ((int) ((LPCREATESTRUCT) lParam) -> lpCreateParams));
	 UpdateWindow (hWnd);
	 return  result;
      }

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
	 screen->mode_flags = (WORD) wParam;
	 return  0;
	 
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

      case WM_LBUTTONDOWN:
      case WM_MBUTTONDOWN:
      case WM_RBUTTONDOWN:
	 ProcessMouseButton (hWnd, uMsg, wParam, lParam, FALSE);
	 break;
      
      case WM_LBUTTONUP:
      case WM_MBUTTONUP:
      case WM_RBUTTONUP:
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
	     void flush_typeahead (SCREEN);
	     LRESULT intrpt = (screen->commands[i].thunk(hWnd, wID));

	     if (intrpt)
	       flush_typeahead (screen);
	     return  intrpt;
	   }
	 return  DefWindowProc (hWnd, uMsg, wParam, lParam);
	 //return  DefWindowProc (hWnd, wID>=0xf000?WM_SYSCOMMAND:WM_COMMAND,
	 //                       wParam, lParam);
      }
      break ;

      case WM_GETMINMAXINFO:
      {
	 LPMINMAXINFO info = (LPMINMAXINFO) lParam;
	 GetMinMaxSizes (hWnd, &info->ptMinTrackSize, &info->ptMaxTrackSize);
	 return  0;
      }
      
      case WM_PAINT:
         PaintScreen (hWnd);
         break ;

      case WM_ERASEBKGND:
	 EraseScreen (hWnd, (HDC) wParam);
	 return (1L);

      case WM_SIZE:
	if (wParam!=SIZE_MINIMIZED)
	  SizeScreen (hWnd, HIWORD(lParam), LOWORD(lParam));
	break ;

      case WM_HSCROLL:
	 ScrollScreenHorz (hWnd, LOWORD(wParam), HIWORD(wParam));
	 break ;

      case WM_VSCROLL:
	 ScrollScreenVert (hWnd, LOWORD(wParam), HIWORD(wParam));
	 break ;

      case WM_SYSKEYDOWN:
      case WM_KEYDOWN:
         if (Process_KeyDown (hWnd, uMsg, wParam, lParam))
	   return  DefWindowProc (hWnd, uMsg, wParam, lParam);
         else
	   return  0L;

#ifdef WINDOWSLOSES
	 if (((wParam == VK_ESCAPE) && MIT_trap_alt_escape)
	     || ((wParam == VK_TAB) && MIT_trap_alt_tab))
	   return (0L);
	 else
	   return  DefWindowProc (hWnd, uMsg, wParam, lParam);
#endif /* WINDOWSLOSES */

      case WM_SYSKEYUP:
      case WM_KEYUP:
#if 1
	 return (1L);
#ifdef WINDOWSLOSES
	 if (((wParam == VK_ESCAPE && MIT_trap_alt_escape))
	     || ((wParam == VK_TAB) && MIT_trap_alt_escape))
	   return (0L);
	 else
#endif /* WINDOWSLOSES */
	 return  DefWindowProc (hWnd, uMsg, wParam, lParam);
	 
#endif

      case WM_SYSDEADCHAR:
      case WM_DEADCHAR:
#if 1
	 return  DefWindowProc (hWnd, uMsg, wParam, lParam);
#else
	 return (1L);
#endif
	 
      case WM_SYSCHAR:
      case WM_CHAR:
	 ProcessScreenCharacter (hWnd, LOBYTE (LOWORD(wParam)), (DWORD) lParam);
	 break ;

      case WM_SETFOCUS:
	SetScreenFocus (hWnd);
	break ;

      case WM_KILLFOCUS:
	 KillScreenFocus (hWnd);
	 break ;

      case WM_DESTROY:
	 DestroyScreenInfo (hWnd);
	 break ;

      case WM_CATATONIC:
      {
	extern void catatonia_trigger (void);
	catatonia_trigger ();
	break;
      }

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
			    "MIT Scheme",
			    (MB_ICONQUESTION | MB_OKCANCEL)))
	      break ;
	  }
	else
	{
	  ProcessCloseMessage (screen);
	  break ;
	}

	if (hWnd == ((HWND) master_tty_window))
	{
	  // This is bad.  We should post a quit message like nice people
	  extern void termination_normal (int);
	  termination_normal (0);
	}
	goto use_default;
      }

#ifdef USE_WM_TIMER
      case WM_TIMER:
      {
	extern VOID TimerProc (HWND, UINT, UINT, DWORD);

	TimerProc (hWnd, uMsg, wParam, lParam);
	return (0L);
      }
#endif /* USE_WM_TIMER */

      case WM_HOTKEY:
      {
	extern int signal_keyboard_character_interrupt (int);
	signal_keyboard_character_interrupt (-2);
      }

      use_default:
      default:
	 return  DefWindowProc (hWnd, uMsg, wParam, lParam);
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

static void
screen_reset_events (SCREEN screen)
{
  int i;

  screen->n_events = 0;
  screen->queue_head = ((SCREEN_EVENT_LINK *) NULL);
  screen->queue_tail = ((SCREEN_EVENT_LINK *) NULL);
  screen->free_events = &screen->events[0];
  for (i = 0; i < MAX_EVENTS; i++)
    screen->events[i].next = &screen->events[i + 1];
  screen->events[MAX_EVENTS - 1].next = ((SCREEN_EVENT_LINK *) NULL);
}

//---------------------------------------------------------------------------
//  LRESULT CreateScreenInfo (HWND hWnd)
//
//  Description:
//     Creates the tty information structure and sets
//     menu option availability.  Returns -1 if unsuccessful.
//
//  Parameters:
//     HWND  hWnd
//
//---------------------------------------------------------------------------
   
static LRESULT
CreateScreenInfo (HWND hWnd)
{
   HMENU   hMenu;
   SCREEN  screen;
   char * font_name = getenv ("MITSCHEME_FONT");
   
   if (NULL == (screen =
		(SCREEN) LocalAlloc (LPTR, sizeof(SCREEN_STRUCT) )))
      return  (LRESULT) -1;

   screen->hWnd                 = hWnd;
   screen->chars                = NULL;
   screen->attrs                = NULL;
   screen->write_attribute      = 0;
   screen->CursorState          = CS_HIDE ;
   screen->mode_flags           = SCREEN_EVENT_TYPE_KEY
			    //	| SCREEN_EVENT_TYPE_MOUSE
			    //  | SCREEN_EVENT_TYPE_RESIZE
			    //  | SCREEN_EVENT_TYPE_CLOSE
				| SCREEN_MODE_ECHO
			    //  | SCREEN_MODE_CR_NEWLINES
				| SCREEN_MODE_NEWLINE_CRS
				| SCREEN_MODE_AUTOWRAP
				| SCREEN_MODE_PROCESS_OUTPUT
				| SCREEN_MODE_LINE_INPUT
				| SCREEN_MODE_EAGER_UPDATE;
   screen->xSize                = 0;
   screen->ySize                = 0 ;
   screen->xScroll              = 0 ;
   screen->yScroll              = 0 ;
   screen->xOffset              = 0 ;
   screen->yOffset              = 0 ;
   screen->hFont                = NULL;
   if (! (init_color ("MITSCHEME_FOREGROUND", hWnd, &screen->rgbFGColour)))
     screen->rgbFGColour        = RGB(0,0,0);
   if (! (init_color ("MITSCHEME_BACKGROUND", hWnd, &screen->rgbBGColour)))
     screen->rgbBGColour        = GetSysColor (COLOR_WINDOW);
   screen->width                = 0;
   screen->height               = 0;
   screen->scroll_lines         = 1;
      
   screen->chars = xmalloc (MAXROWS * MAXCOLS);
   screen->attrs = xmalloc (MAXROWS * MAXCOLS * sizeof(SCREEN_ATTRIBUTE));

   ClearScreen_internal (screen);
   // clear screen space

   // setup default font information

   screen->lfFont.lfHeight =         init_font_height("MITSCHEME_FONT_SIZE");
   screen->lfFont.lfWidth =          0 ;
   screen->lfFont.lfEscapement =     0 ;
   screen->lfFont.lfOrientation =    0 ;
   screen->lfFont.lfWeight =         400 ;
   screen->lfFont.lfItalic =         0 ;
   screen->lfFont.lfUnderline =      0 ;
   screen->lfFont.lfStrikeOut =      0 ;
   screen->lfFont.lfCharSet =        ANSI_CHARSET ;
   screen->lfFont.lfOutPrecision =   OUT_CHARACTER_PRECIS ;
   screen->lfFont.lfClipPrecision =  CLIP_CHARACTER_PRECIS ;
   screen->lfFont.lfQuality =        DRAFT_QUALITY ;
   screen->lfFont.lfPitchAndFamily = FIXED_PITCH | FF_MODERN ;
   if (font_name == NULL)
     lstrcpy (screen->lfFont.lfFaceName, "FixedSys");
   else   
     lstrcpy (screen->lfFont.lfFaceName, font_name);
   
   // set handle before any further message processing.
   SETSCREEN (hWnd, screen);
   RegisterScreen (screen);

   screen->events = xmalloc ((sizeof (SCREEN_EVENT_LINK)) * MAX_EVENTS);
   screen_reset_events (screen);
 
   screen->n_commands = 0;
   screen->n_bindings = 0;
   // reset the character information, etc.

   screen->bkgnd_brush = NULL;
   ResetScreen (screen);

   hMenu = GetSystemMenu (hWnd, FALSE);
   AppendMenu (hMenu, MF_SEPARATOR, 0, 0);
// AppendMenu (hMenu, MF_STRING, IDM_SETTINGS, "&Settings...");
   AppendMenu (hMenu, MF_STRING, SCREEN_COMMAND_CHOOSEFONT, "&Font...");
   AppendMenu (hMenu, MF_STRING, SCREEN_COMMAND_CHOOSEBACKCOLOR, "&Background...");
    
   SendMessage (hWnd, SCREEN_SETCOMMAND,
		SCREEN_COMMAND_CHOOSEFONT, (LPARAM)ScreenCommand_ChooseFont);
// SendMessage (hWnd, SCREEN_SETBINDING, 6, SCREEN_COMMAND_CHOOSEFONT);
   SendMessage (hWnd, SCREEN_SETCOMMAND,
		SCREEN_COMMAND_CHOOSEBACKCOLOR, (LPARAM)ScreenCommand_ChooseBackColor);
// SendMessage (hWnd, SCREEN_SETBINDING, 7, SCREEN_COMMAND_CHOOSEBACKCOLOR);
		  
   screen->n_chars = 0;
   screen->line_buffer = xmalloc (MAX_LINEINPUT + 1);
     
   screen->n_pending = 0;
   screen->pending = ((LPSTR) NULL);
   return  (LRESULT) TRUE;
}

//---------------------------------------------------------------------------
//  VOID DestroyScreenInfo (HWND hWnd )
//
//  Description:
//     Destroys block associated with TTY window handle.
//
//  Parameters:
//     HWND hWnd
//        handle to TTY window
//
//---------------------------------------------------------------------------

static VOID
DestroyScreenInfo (HWND hWnd)
{
   SCREEN screen = GETSCREEN (hWnd);

   if (NULL == screen)
     return;

   KillScreenFocus (hWnd);
   UnregisterScreen (screen);
   DeleteObject (screen->hFont);

   if (screen->chars)
     xfree (screen->chars);
   if (screen->attrs)
     xfree (screen->attrs);
   if (screen->events)
     xfree (screen->events);

   LocalFree (screen);
}

//---------------------------------------------------------------------------
//  COMMAND_HANDLER  ScreenSetCommand (SCREEN, WORD cmd, COMMAND_HANDLER h)
//---------------------------------------------------------------------------

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
	  // remove by overwriting with last in list
	  screen->commands[i] = screen->commands[screen->n_commands-1];
	  screen->n_commands--;
	}
	else if (thunk == ((COMMAND_HANDLER) -1))
	{
	  // just leave it alone
	}
	else
	  // redefine
	  screen->commands[i].thunk = thunk;
	return  result;
      }
      
    // didnt find it
    if ((thunk == 0) || (thunk == ((COMMAND_HANDLER) -1)))
      return  0;
    // add new command
    if (screen->n_commands == MAX_COMMANDS)
      return ((COMMAND_HANDLER) - 1);

    screen->commands[screen->n_commands].wID   = cmd;
    screen->commands[screen->n_commands].thunk = thunk;
    screen->n_commands++;
    
    return  0;
}

//---------------------------------------------------------------------------
//  WORD  ScreenSetBinding (SCREEN, char key, WORD command)
//---------------------------------------------------------------------------

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
	  // remove by blatting with last in list
	  screen->bindings[i] = screen->bindings[screen->n_bindings-1];
	  screen->n_bindings--;
	}
	else if (command == ((WORD) -1))
	{
	  // let it be
	}
	else
	  // redefine
	  screen->bindings[i].command = command;
	return  result;
      }
      
    // no existing binding for key
    if ((command == 0) || (command == ((WORD) -1)))
      return  0;
    // add new binding
    if (screen->n_bindings == MAX_BINDINGS)
      return ((WORD) - 1);

    screen->bindings[screen->n_bindings].key     = key;
    screen->bindings[screen->n_bindings].command = command;
    screen->n_bindings++;
    
    return  0;
}

//===========================================================================
//  Standard commands
//===========================================================================

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

//---------------------------------------------------------------------------
//  Screen_SetMenu (SCREEN, HMENU)
//---------------------------------------------------------------------------

VOID 
Screen_SetMenu (SCREEN screen, HMENU hMenu)
{
  HMENU hOld = GetMenu (screen->hWnd);
  SetMenu (screen->hWnd, hMenu);
  if (hOld)
    DestroyMenu (hOld);
}

//---------------------------------------------------------------------------
//  BOOL ResetScreen (SCREEN  screen)
//
//  Description:
//     Resets the TTY character information and causes the
//     screen to resize to update the scroll information.
//
//  Parameters:
//     NPTTYINFO  npTTYInfo
//        pointer to TTY info structure
//
//---------------------------------------------------------------------------

static BOOL
ResetScreen (SCREEN screen)
{
   HWND        hWnd;
   HDC         hDC ;
   TEXTMETRIC  tm ;
   RECT        rcWindow ;
   
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

   screen->xChar = tm.tmAveCharWidth  ;
   screen->yChar = tm.tmHeight + tm.tmExternalLeading ;

   // a slimy hack to make the caret the correct size, un- and re- focus
   if (screen->CursorState == CS_SHOW) {
     KillScreenFocus (hWnd);
     SetScreenFocus (hWnd);
   }

   if (screen->bkgnd_brush != NULL)
     DeleteObject (screen->bkgnd_brush);
   screen->bkgnd_brush = CreateSolidBrush (screen->rgbBGColour);

   // a slimy hack to force the scroll position, region to
   // be recalculated based on the new character sizes
   {
     int width, height;
     POINT minsz, maxsz;
     GetWindowRect (hWnd, &rcWindow);
     GetMinMaxSizes (hWnd, &minsz, &maxsz);
     width  = rcWindow.right - rcWindow.left;
     height = (rcWindow.bottom - rcWindow.top
	       - GetSystemMetrics(SM_CYCAPTION)
	       - GetSystemMetrics(SM_CYFRAME)
	       - (GetMenu(hWnd) ? GetSystemMetrics(SM_CYMENU) : 0));
     if (width<minsz.x || width>maxsz.x || height<minsz.y || height>maxsz.y)
       MoveWindow (hWnd, rcWindow.left, rcWindow.top,
		   min(maxsz.x,max(minsz.x,width)),
		   min(maxsz.y,max(minsz.y,height)), TRUE);
     else
       PostMessage (hWnd, WM_SIZE, SIZENORMAL,
		    ((LPARAM) (MAKELONG (width,height))));
   }
   return  TRUE;
}

static VOID
Do_PaintScreen (HWND hWnd, SCREEN screen, HDC hDC, PAINTSTRUCT * ps)
{
  RECT          rect ;

  if (! (IsIconic (hWnd)))
  {
    int         nRow, nCol, nEndRow, nEndCol, nCount;
    int         nHorzPos, nVertPos, bias;
    HFONT       hOldFont;

    hOldFont = SelectObject (hDC, screen->hFont);
    rect = ps->rcPaint;

    nRow =
      min (screen->height - 1,
	   max (0, (rect.top + screen->yOffset) / screen->yChar));
    nEndRow =
      min (screen->height - 1,
	   ((rect.bottom + screen->yOffset - 1) / screen->yChar));
    nCol =
      min (screen->width - 1,
	   max (0, (rect.left + screen->xOffset) / screen->xChar));
    nEndCol =
      min (screen->width - 1,
	   ((rect.right + screen->xOffset - 1) / screen->xChar));
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
	// find consistent run of attributes
	SCREEN_ATTRIBUTE  *attribp = &screen->attrs[bias + pos];
	SCREEN_ATTRIBUTE  attrib   = *attribp;
	int  nposn = (pos + 1);
	int  run_length;

	while ((nposn < nCount) && (*++attribp == attrib))
	  nposn++;

	run_length = (nposn - pos);
	nHorzPos = (((nCol + pos) * screen->xChar) - screen->xOffset);
	rect.top    = nVertPos;
	rect.bottom = (nVertPos + screen->yChar);
	rect.left   = nHorzPos;
	rect.right  = (nHorzPos + (screen->xChar * run_length));
	if (attrib&1)
	{
	  SetTextColor (hDC, screen->rgbBGColour);
	  SetBkColor (hDC, screen->rgbFGColour);
	}
	ExtTextOut (hDC, nHorzPos, nVertPos, (ETO_OPAQUE | ETO_CLIPPED),
		    &rect, &screen->chars[bias + pos],
		    run_length, NULL);
	//if (attrib&2)  // Bolden by horizontal 1-pixel smear
	//  ExtTextOut (hDC, nHorzPos+1, nVertPos, (ETO_CLIPPED),
	//	      &rect, &screen->chars[bias + pos],
	//	      run_length, NULL);
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
}

//---------------------------------------------------------------------------
//  VOID PaintScreen (HWND hWnd )
//
//  Description:
//     Paints the rectangle determined by the paint struct of
//     the DC.
//
//  Parameters:
//     HWND hWnd
//        handle to TTY window (as always)
//
//---------------------------------------------------------------------------

static VOID
PaintScreen (HWND hWnd)
{
  SCREEN        screen = GETSCREEN (hWnd);
  HDC           hDC ;
  PAINTSTRUCT   ps ;

  if (NULL == screen)
    return;

  hDC =  BeginPaint (hWnd, &ps);
  Do_PaintScreen (hWnd, screen, hDC, &ps);
  EndPaint (hWnd, &ps);
  MoveScreenCursor (screen);
} 

//---------------------------------------------------------------------------
//  VOID EraseScreen (HWND hWnd, HDC hDC)
//---------------------------------------------------------------------------

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

//---------------------------------------------------------------------------
//  void SetCells (screen,r,col,count,char,attr)
//---------------------------------------------------------------------------

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

//---------------------------------------------------------------------------
//  void ScrollScreenBufferUp (SCREEN  screen,  int count)
//---------------------------------------------------------------------------

static VOID 
ScrollScreenBufferUp (SCREEN  screen,  int count)
{
  //int  total_rows = MAXROWS;
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

//---------------------------------------------------------------------------
//  BOOL SizeScreen (HWND hWnd, WORD wVertSize, WORD wHorzSize )
//
//  Description:
//     Sizes TTY and sets up scrolling regions.
//
//---------------------------------------------------------------------------

static BOOL
SizeScreen (HWND hWnd, WORD wVertSize, WORD wHorzSize )
{
   //int        nScrollAmt ;
   SCREEN     screen = GETSCREEN (hWnd);
   int old_width, old_height;
   int new_width, new_height;
   
   if (NULL == screen)
      return  FALSE;

//   if (GetMenu(hWnd)) wVertSize -= GetSystemMetrics(SM_CYMENU);
   old_width  = screen->width;
   old_height = screen->height;
   new_width  = min (wHorzSize / screen->xChar, MAXCOLS);
   new_height = min (wVertSize / screen->yChar, MAXROWS);

   if (new_width > old_width)
   {
     // Clear out revealed character cells
     int  row, rows = min (old_height, new_height);
     for (row = 0; row < rows; row++)
       SetCells (screen, row, old_width, new_width-old_width, ' ', 0);
   }

   if (new_height > old_height)
   {
     // Clear out revealed character cells
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

   // scroll window to fit in cursor
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
   
   screen->ySize = (int) wVertSize ;
//   screen->yScroll = max (0, (MAXROWS * screen->yChar) - screen->ySize);

   screen->yScroll = 0;
//   nScrollAmt = min (screen->yScroll, screen->yOffset ) -
//                     screen->yOffset;
//   ScrollWindow (hWnd, 0, -nScrollAmt, NULL, NULL);
//   screen->yOffset = screen->yOffset + nScrollAmt ;
//   SetScrollPos (hWnd, SB_VERT, screen->yOffset, FALSE);
//   SetScrollRange (hWnd, SB_VERT, 0, screen->yScroll, TRUE);

   screen->xSize = (int) wHorzSize ;
//   screen->xScroll = max (0, (MAXCOLS * screen->xChar) - screen->xSize);
   screen->xScroll = 0;                      
//   nScrollAmt = min (screen->xScroll, screen->xOffset) -
//                     screen->xOffset;
//   ScrollWindow (hWnd, 0, -nScrollAmt, NULL, NULL);
//   screen->xOffset = screen->xOffset + nScrollAmt ;
//   SetScrollPos (hWnd, SB_HORZ, screen->xOffset, FALSE);
//   SetScrollRange (hWnd, SB_HORZ, 0, screen->xScroll, TRUE);

   if ((screen->mode_flags & SCREEN_MODE_EDWIN) == 0)
     screen->scroll_lines = (COMPUTE_SCROLL_LINES (new_height));
   else if (screen->mode_flags & SCREEN_EVENT_TYPE_RESIZE) {
     // queue RESIZE event
     SCREEN_EVENT  *event = alloc_event (screen, SCREEN_EVENT_TYPE_RESIZE);
     if (event)
     {
       event->event.resize.rows    = new_height;
       event->event.resize.columns = new_width;
     }
   } else {
     // Queue a character based resize event
     SCREEN_EVENT * event = alloc_event (screen, SCREEN_EVENT_TYPE_KEY);
     if (event)
     {
       event->event.key.repeat_count = 1;
       event->event.key.virtual_keycode = 0;
       event->event.key.virtual_scancode = 0;
       event->event.key.ch = SCREEN_EDWIN_RESIZE_COMMAND;
       event->event.key.control_key_state = 0;
     }
   }

   // Cause screen to be redrawn, but if we are under Edwin, dont bother as
   // Edwin has to calculate the redisplay anyway.
   // Well, we do bother otherwise we would have to clear the part of the screen
   // that is not in a character box.
   // if ((screen->mode_flags & SCREEN_MODE_EDWIN) == 0)
   {
     /* The SendMessage stuff works fine under NT, but wedges Win 3.1.
	The only solution I've found is to redraw the whole screen.
      */
     //if (win32_under_win32s_p ())
     //  InvalidateRect (NULL, NULL, TRUE);
     //else
       InvalidateRect (hWnd, NULL, TRUE);
   }

   return  TRUE;

} // end of SizeScreen

//---------------------------------------------------------------------------
//  BOOL ScrollScreenVert (HWND hWnd, WORD wScrollCmd, WORD wScrollPos )
//
//  Description:
//     Scrolls TTY window vertically.
//
//  Parameters:
//     HWND hWnd
//        handle to TTY window
//
//     WORD wScrollCmd
//        type of scrolling we're doing
//
//     WORD wScrollPos
//        scroll position
//
//---------------------------------------------------------------------------

static BOOL 
ScrollScreenVert (HWND hWnd, WORD wScrollCmd, WORD wScrollPos)
{
   int        nScrollAmt ;
   SCREEN     screen = GETSCREEN (hWnd);

   if (NULL == screen)
      return  FALSE;

   switch (wScrollCmd)
   {
      case SB_TOP:
	 nScrollAmt = -screen->yOffset;
	 break ;

      case SB_BOTTOM:
	 nScrollAmt = screen->yScroll - screen->yOffset;
	 break ;

      case SB_PAGEUP:
	 nScrollAmt = -screen->ySize;
	 break ;

      case SB_PAGEDOWN:
	 nScrollAmt = screen->ySize;
	 break ;

      case SB_LINEUP:
	 nScrollAmt = -screen->yChar;
	 break ;

      case SB_LINEDOWN:
	 nScrollAmt = screen->yChar;
	 break ;

      case SB_THUMBPOSITION:
	 nScrollAmt = wScrollPos - screen->yOffset;
	 break ;

      default:
	 return  FALSE;
   }
   if ((screen->yOffset + nScrollAmt) > screen->yScroll)
      nScrollAmt = screen->yScroll - screen->yOffset;
   if ((screen->yOffset + nScrollAmt) < 0)
      nScrollAmt = -screen->yOffset;
   ScrollWindow (hWnd, 0, -nScrollAmt, NULL, NULL);
   screen->yOffset = screen->yOffset + nScrollAmt ;
   SetScrollPos (hWnd, SB_VERT, screen->yOffset, TRUE);

   return  TRUE;
}

//---------------------------------------------------------------------------
//  BOOL ScrollScreenHorz (HWND hWnd, WORD wScrollCmd, WORD wScrollPos )
//
//  Description:
//     Scrolls TTY window horizontally.
//
//  Parameters:
//     HWND hWnd
//        handle to TTY window
//
//     WORD wScrollCmd
//        type of scrolling we're doing
//
//     WORD wScrollPos
//        scroll position
//
//---------------------------------------------------------------------------

static BOOL 
ScrollScreenHorz (HWND hWnd, WORD wScrollCmd, WORD wScrollPos)
{
   int        nScrollAmt ;
   SCREEN     screen = GETSCREEN (hWnd);

   if (NULL == screen)
      return  FALSE;

   switch (wScrollCmd)
   {
      case SB_TOP:
	 nScrollAmt = -screen->xOffset;
	 break ;

      case SB_BOTTOM:
	 nScrollAmt = screen->xScroll - screen->xOffset;
	 break ;

      case SB_PAGEUP:
	 nScrollAmt = -screen->xSize;
	 break ;

      case SB_PAGEDOWN:
	 nScrollAmt = screen->xSize;
	 break ;

      case SB_LINEUP:
	 nScrollAmt = -screen->xChar;
	 break ;

      case SB_LINEDOWN:
	 nScrollAmt = screen->xChar;
	 break ;

      case SB_THUMBPOSITION:
	 nScrollAmt = wScrollPos - screen->xOffset;
	 break ;

      default:
	 return  FALSE;
   }
   if ((screen->xOffset + nScrollAmt) > screen->xScroll)
      nScrollAmt = screen->xScroll - screen->xOffset;
   if ((screen->xOffset + nScrollAmt) < 0)
      nScrollAmt = -screen->xOffset;
   ScrollWindow (hWnd, -nScrollAmt, 0, NULL, NULL);
   screen->xOffset = screen->xOffset + nScrollAmt ;
   SetScrollPos (hWnd, SB_HORZ, screen->xOffset, TRUE);

   return  TRUE;
}

//---------------------------------------------------------------------------
//  BOOL SetScreenFocus (HWND hWnd )
//
//  Description:
//     Sets the focus to the TTY window also creates caret.
//
//  Parameters:
//     HWND hWnd
//        handle to TTY window
//
//---------------------------------------------------------------------------

static BOOL 
SetScreenFocus (HWND hWnd)
{
   SCREEN  screen = GETSCREEN (hWnd);

   if (NULL == screen)  return  FALSE;

   //if (screen->CursorState != CS_SHOW)
   {
      CreateCaret (hWnd, NULL, max(screen->xChar/4,2), screen->yChar);
      ShowCaret (hWnd);
      screen->CursorState = CS_SHOW ;
   }
   MoveScreenCursor (screen);
   return  TRUE;
}

//---------------------------------------------------------------------------
//  BOOL KillScreenFocus (HWND hWnd )
//
//  Description:
//     Kills TTY focus and destroys the caret.
//
//  Parameters:
//     HWND hWnd
//        handle to TTY window
//
//---------------------------------------------------------------------------

BOOL 
KillScreenFocus (HWND hWnd )
{
   SCREEN  screen = GETSCREEN (hWnd);

   if (NULL == screen)  return  FALSE;

   if (screen->CursorState != CS_HIDE)
   {
      HideCaret (hWnd);
      DestroyCaret ();
      screen->CursorState = CS_HIDE;
   }
   return  TRUE;
}

//---------------------------------------------------------------------------
//  VOID MoveScreenCursor (SCREEN screen)
//
//  Description:
//     Moves caret to current position.
//---------------------------------------------------------------------------

static VOID 
MoveScreenCursor (SCREEN screen)
{
   if (screen->CursorState & CS_SHOW)
     SetCaretPos (screen->column * screen->xChar  -  screen->xOffset
		  // This ensures visiblity at the far left:
		  + ((screen->column == screen->width) ? -2 : 0),
		  screen->row    * screen->yChar  -  screen->yOffset);
}

//---------------------------------------------------------------------------
//  BOOL Screen_SetPosition (SCREEN, int row, int column);
//---------------------------------------------------------------------------

BOOL 
Screen_SetPosition (SCREEN screen, int row, int column)
{
  if ((row < 0) || (row >= screen->height))
    return (FALSE);
  if ((column < 0) || (column > screen->width))         // may be ==
    return (FALSE);
  screen->row    = row;
  screen->column = column;
  MoveScreenCursor (screen);
  return  TRUE;
}

//---------------------------------------------------------------------------
//  UINT ScreenPeekOrRead (SCREEN, count, SCREEN_EVENT* buffer, BOOL remove)
//
//  Copy events into buffer.  Return number of events processed.
//  If remove=TRUE, remove events from screen queue (i.e. Read)
//  If remove=FALSE, leave events in queue (i.e. Peek)
//  If buffer=NULL, process without copying.
//  If count<0, process all events.
//   .  count=-1, buffer=NULL, remove=FALSE -> count of pending events
//   .  count=-1, buffer=NULL, remove=TRUE  -> flush queue
//   .  count=n,  buffer=NULL, remove=TRUE  -> discard n events
//---------------------------------------------------------------------------

UINT
ScreenPeekOrRead (SCREEN screen, int count, SCREEN_EVENT * buffer, BOOL remove)
{
  int start_count;
  SCREEN_EVENT_LINK ** next_loc;
  SCREEN_EVENT * entry = buffer;
    
  if (count < 0)
    count = MAX_EVENTS;
    
  start_count = count;
  next_loc = & screen->queue_head;

  while ((count > 0) && ((* next_loc) != ((SCREEN_EVENT_LINK *) NULL)))
  {
    SCREEN_EVENT_LINK * current = (* next_loc);

    if (entry)
      *entry++ = current->event;
    if (remove)
    {
      (* next_loc) = current->next;
      current->next = screen->free_events;
      screen->free_events = current;
      screen->n_events -= 1;
    }
    count -= 1;
  }
  if (screen->queue_head == ((SCREEN_EVENT_LINK *) NULL))
    screen->queue_tail = ((SCREEN_EVENT_LINK *) NULL);

  return (start_count - count);
}

void
flush_typeahead (SCREEN screen)
{
  SCREEN_EVENT_LINK ** next_loc, * last;

  next_loc = & screen->queue_head;
  last = ((SCREEN_EVENT_LINK *) NULL);

  while ((* next_loc) != ((SCREEN_EVENT_LINK *) NULL))
  {
    SCREEN_EVENT_LINK * current = (* next_loc);
    
    if (current->event.type != SCREEN_EVENT_TYPE_KEY)
    {
      last = current;
      next_loc = &current->next;
    }
    else
    {
      (* next_loc) = current->next;
      current->next = screen->free_events;
      screen->free_events = current;
      screen->n_events -= 1;
    }
  }
  screen->queue_tail = last;
  screen->n_chars = 0;
}

//---------------------------------------------------------------------------
//
//---------------------------------------------------------------------------

#define KEYDATA_ALT_BIT 0x20000000

static int
GetControlKeyState(void)
{
  return
    (  (((GetKeyState (VK_MENU)) < 0)     ?     SCREEN_ALT_PRESSED   : 0)
     | (((GetKeyState (VK_CONTROL)) < 0)  ?     SCREEN_CTRL_PRESSED  : 0)
     | (((GetKeyState (VK_SHIFT)) < 0)    ?     SCREEN_SHIFT_PRESSED : 0)
     | (((GetKeyState (VK_NUMLOCK)) & 1)  ?     SCREEN_NUMLOCK_ON    : 0)
     | (((GetKeyState (VK_SCROLL)) & 1)   ?     SCREEN_SCROLLLOCK_ON : 0)
     | (((GetKeyState (VK_CAPITAL)) & 1)  ?     SCREEN_CAPSLOCK_ON   : 0));
}

static VOID _fastcall
make_key_event (SCREEN screen, int ch, int vk_code, DWORD lKeyData)
{
  SCREEN_EVENT  *event = alloc_event (screen, SCREEN_EVENT_TYPE_KEY);
  if (event) {
    event->event.key.repeat_count = lKeyData & 0xffff;
    event->event.key.virtual_keycode = vk_code;
    event->event.key.virtual_scancode = (lKeyData >> 16) & 0xff;
    event->event.key.ch = ch;
    event->event.key.control_key_state = GetControlKeyState();
  }
}

// We learn which scan codes are for keys that we process ourselves and
// hence do not want to accept as WM_CHARS from Transalate Message

BOOL scan_codes_to_avoid[256] = {0};

//---------------------------------------------------------------------------
//  VOID ProcessScreenCharacter (HWND hWnd, int ch, DWORD lKeyData)
//---------------------------------------------------------------------------

static VOID
ProcessScreenCharacter (HWND hWnd, int ch, DWORD lKeyData)
{
  // Process a WM_CHAR or WM_SYSCHAR character
   SCREEN  screen = GETSCREEN (hWnd);
   
   if (NULL == screen)
      return;
    
   // check for bindings:
   {
     int  i;
     for (i=0; i<screen->n_bindings; i++)
       if (screen->bindings[i].key == ch)
       {
	 if (SendMessage (screen->hWnd,
			  WM_COMMAND,
			  MAKEWPARAM(screen->bindings[i].command, 0),
			  0))
	   return;
	 else
	   break;
       }
   } 

   // exclude characters that TranslateMessage produces but we handle
   // differently in order to get all those wonderful control-shift-meta
   // things

   if (scan_codes_to_avoid[(lKeyData >> 16) & 0xff])
     return;

   switch (ch) {
     case -1:
     return;
   }

   make_key_event (screen, ch, 0, lKeyData);
}


static BOOL
Process_KeyDown (HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
  // processing for non-ascii VKs
  // Assume msg = WM_KEYDOWN or WM_SYSKEYDOWN
  // returns flag to say if DefWindowProc should be called.
  SCREEN  screen = GETSCREEN (hWnd);
  BOOL  default_ok = TRUE;
  int   scan_code = (lParam >> 16) & 0xff;

  switch (wParam) {
    case VK_BACK:
      make_key_event (screen, ASCII_DEL, 0, lParam);
      scan_codes_to_avoid[scan_code] = TRUE;
      return  TRUE;

    case VK_SPACE:
      make_key_event (screen, ' ', 0, lParam);
      scan_codes_to_avoid[scan_code] = TRUE;
      return  TRUE;

    case '0':  case '1':  case '2':  case '3':  case '4':
    case '5':  case '6':  case '7':  case '8':  case '9':
      // This is very naughty.  We should figure out how to allow the
      // keyboard to produce the correct  keys according to the keyboard
      // layout.
      if (GetKeyState(VK_SHIFT)<0)
	make_key_event (screen, ")!@#$%^&*("[wParam-'0'], 0, lParam);
      else
	make_key_event (screen, wParam, 0, lParam);
      scan_codes_to_avoid[scan_code] = TRUE;
      return  TRUE;

    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
    case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
    case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
    case 'V': case 'W': case 'X': case 'Y': case 'Z':
    {
      // This is very naughty.  We should figure out how to allow the
      // keyboard to produce the correct  keys according to the keyboard
      // layout.
      int  shift = ((GetKeyState(VK_SHIFT)>>15) ^ GetKeyState(VK_CAPITAL)) & 1;
      int  control = GetKeyState(VK_CONTROL) < 0;
      int  alt = GetKeyState(VK_MENU) < 0;

      if (control)
	make_key_event (screen, wParam-64, 0, lParam);
      else if (shift)
	make_key_event (screen, wParam,    0, lParam);
      else
	make_key_event (screen, wParam+32, 0, lParam);
      scan_codes_to_avoid[scan_code] = TRUE;
      return  TRUE;
    }

    case VK_F1:
    case VK_F2:
    case VK_F3:
    case VK_F4:
    case VK_F5:
    case VK_F6:
    case VK_F7:
    case VK_F8:
    case VK_F9:
    case VK_F10:
    case VK_F11:
    case VK_F12:
    case VK_UP:
    case VK_DOWN:
    case VK_LEFT:
    case VK_RIGHT:
    case VK_INSERT:
    case VK_DELETE:
    case VK_HOME:
    case VK_END:
    case VK_PRIOR:
    case VK_NEXT:
      make_key_event (screen, -1, wParam, lParam);
      return  TRUE;

    default:
      return  TRUE;
    }
}

static VOID
ProcessCloseMessage (SCREEN screen)
{
    SCREEN_EVENT * event ;

    event = alloc_event (screen, SCREEN_EVENT_TYPE_CLOSE);
}

static VOID
ProcessMouseButton (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL up)
{
  SCREEN screen = GETSCREEN (hWnd) ;
  SCREEN_EVENT * event ;
  int row, column ;
  int control = 0 ;
  int button = 0 ;

  if (NULL == screen)
    return;
  
  if (uMsg & MK_CONTROL)
    control=1;
  if (uMsg & MK_SHIFT)
    control|=2;

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
    
  column = LOWORD(lParam) / screen->xChar;
  row    = HIWORD(lParam) / screen->yChar;

  event = alloc_event (screen, SCREEN_EVENT_TYPE_MOUSE);
  if (event)
  {
    event->event.mouse.row = row;
    event->event.mouse.column = column;
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
    screen->column -- ;
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
    //InvalidateRect (hWnd, NULL, FALSE);
    //screen->row-- ;
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
  screen->column = 0 ;
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
    RECT       rect ;

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
				 int lo_col_from, int hi_col_from, int lo_col_to)
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
  Screen_WriteCharUninterpreted (screen, (((the_char % 0100) / 010) + '0'), rectp);
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

//---------------------------------------------------------------------------
//  BOOL WriteScreenBlock (HWND hWnd, LPSTR lpBlock_in, int nLength_in )
//
//  Description:
//     Writes block of characters to TTY screen.  Interprets lots of ANSI
//     sequences.
//
//---------------------------------------------------------------------------

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
    for (i = 0 ; i < nLength; i++)
      Screen_WriteCharUninterpreted (screen, (lpBlock[i]), &state);
  else for (i = 0 ; i < nLength; i++)
  {
    unsigned char the_char = ((unsigned char) (lpBlock[i]));

    switch (the_char)
    {
    case ASCII_BEL:
      MessageBeep (0);
      break ;

    case ASCII_BS:
      Screen_BS (screen);
      break ;

    case '\t':
      Screen_TAB (screen, &state);
      break;

    case ASCII_LF:
      if (screen->mode_flags & SCREEN_MODE_NEWLINE_CRS)
	Screen_CR (screen);
      Finish_ScreenWriteChar (screen, &state);
      Screen_LF (screen);
      break ;

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
	      relocate_cursor (screen, (screen->row - x_value), screen->column);
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
					(screen->row + x_value), screen->column,
					screen->height, screen->width,
					screen->row);
	      clear_screen_rectangle (screen,
				      (screen->height - x_value), screen->column,
				      screen->height, screen->width);
	      i = j; /* 1 added in for loop */
	      continue;

	    case 'P':
	      /* Multi delete char */
	      scroll_screen_line_horizontally (screen, screen->row,
					       (screen->column + x_value), screen->width,
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
		  screen->scroll_lines = (COMPUTE_SCROLL_LINES (screen->height));
		  SetWindowText (screen->hWnd, "MIT Scheme");
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

//
// A fast raw write to the screen memory.
// Client is responsible for invalidating the correct region
//
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
  for (i = 0 ; i < limit; i++) {
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
	// Screen_WriteCharUninterpreted (screen, ch, NULL);
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
  int result;
  SCREEN_EVENT_LINK ** next_loc, * last;

  result = -1;                  /* No EOL seen yet. */
  next_loc = & screen->queue_head;
  last = ((SCREEN_EVENT_LINK *) NULL);

  while ((* next_loc) != ((SCREEN_EVENT_LINK *) NULL))
  {
    SCREEN_EVENT_LINK * current = (* next_loc);
    
    if (current->event.type != SCREEN_EVENT_TYPE_KEY)
    {
      last = current;
      next_loc = &current->next;
    }
    else
    {
      int ch = current->event.event.key.ch;
      if ((current->event.event.key.control_key_state
	   & SCREEN_ANY_ALT_KEY_MASK)
	  != 0)
	ch |= 0200;         
	
      if (ch != 0)
	buffered_key_command (screen, ch);
	  
      /* dequeue */

      (* next_loc) = current->next;
      current->next = screen->free_events;
      screen->free_events = current;
      screen->n_events -= 1;

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
  }
  if ((* next_loc) == ((SCREEN_EVENT_LINK *) NULL))
    screen->queue_tail = last;
  return (result);
}

/* Untranslated/unbuffered input */

static int
ReadScreen_raw (SCREEN screen, LPSTR buffer, int buflen)
{
  int position;
  SCREEN_EVENT_LINK ** next_loc, * last;

  last = ((SCREEN_EVENT_LINK *) NULL);

  for (position = 0, next_loc = & screen->queue_head;
       ((position < buflen) && ((* next_loc) != ((SCREEN_EVENT_LINK *) NULL)));
       )
  {
    SCREEN_EVENT_LINK * current = (* next_loc);
    
    if (current->event.type != SCREEN_EVENT_TYPE_KEY)
    {
      last = current;
      next_loc = &current->next;
    }
    else
    {
      int ch = current->event.event.key.ch;
      if ((current->event.event.key.control_key_state
	   & SCREEN_ANY_ALT_KEY_MASK)
	  != 0)
	ch |= 0200;         
	
      /* Store the character */

      buffer[position++] = ch;
      if (screen->mode_flags & SCREEN_MODE_ECHO)
      {
	char c = ((char) ch);
	WriteScreenBlock (screen->hWnd, &c, 1);
      }
	  
      /* dequeue */

      (* next_loc) = current->next;
      current->next = screen->free_events;
      screen->free_events = current;
      screen->n_events -= 1;
    }
  }
  if ((* next_loc) == ((SCREEN_EVENT_LINK *) NULL))
    screen->queue_tail = last;
  return ((position == 0) ? -1 : position);
}

//---------------------------------------------------------------------------
//  int  ReadScreen (SCREEN screen, LPSTR buffer, int buflen)
//
//  Read characters into buffer.
//  If in line mode, collect characters into the line buffer.
//  Return the number of characters read.
//  In raw mode, return -1 if there are no characters.
//  If in line mode and not yet at end of line, return -1 (i.e. this
//    is a non-blocking read
//
//---------------------------------------------------------------------------

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
    // clear whole screen
      ClearScreen_internal(screen);
    InvalidateRect (screen->hWnd, NULL, TRUE);
    return;
  }
  if (kind == 1)
    // clear to eol
    return;
}

//---------------------------------------------------------------------------
//  VOID GetMinMaxSizes (HWND hWnd, LPPOINT min_size, LPPOINT max_size)
//
//  Description:
//     determine the minimum and maxinum sizes for a screen window.//
//---------------------------------------------------------------------------

static VOID
GetMinMaxSizes (HWND hWnd, LPPOINT min_size, LPPOINT max_size)
{
    SCREEN  screen = GETSCREEN (hWnd);
    int  extra_width, extra_height;

    if (screen==0) return;
    extra_width  = 2*GetSystemMetrics(SM_CXFRAME);
    extra_height = 2*GetSystemMetrics(SM_CYFRAME)
		 + GetSystemMetrics(SM_CYCAPTION)
		 + (GetMenu(hWnd) ? GetSystemMetrics(SM_CYMENU) : 0)
		 ;
    min_size->x = screen->xChar + extra_width;
    min_size->y = screen->yChar + extra_height;
    max_size->x = screen->xChar * MAXCOLS  +  extra_width;
    max_size->y = screen->yChar * MAXROWS  +  extra_height;
}

//---------------------------------------------------------------------------
//  BOOL SelectScreenFont (SCREEN screen, HWND owner)
//
//  Description:
//     Selects the current font for the TTY screen.
//     Uses the Common Dialog ChooseFont() API.
//
//  Parameters:
//     HWND hDlg
//
//---------------------------------------------------------------------------

static BOOL
SelectScreenFont (SCREEN  screen,  HWND owner)
{
   CHOOSEFONT  cfTTYFont ;

   if (NULL == screen)  return  FALSE;

   cfTTYFont.lStructSize    = sizeof (CHOOSEFONT);
   cfTTYFont.hwndOwner      = owner ;
   cfTTYFont.hDC            = NULL ;
   cfTTYFont.rgbColors      = screen->rgbFGColour;
   cfTTYFont.lpLogFont      = &screen->lfFont;
   cfTTYFont.Flags          = (
			         CF_FIXEDPITCHONLY
			       | CF_SCREENFONTS
			       | CF_EFFECTS
			       | CF_INITTOLOGFONTSTRUCT
			       );
   cfTTYFont.lCustData      = 0 ;
   cfTTYFont.lpfnHook       = NULL ;
   cfTTYFont.lpTemplateName = NULL ;
   cfTTYFont.hInstance      = (HINSTANCE) GetWindowLong(owner, GWL_HINSTANCE);

   if (ChooseFont (&cfTTYFont))
   {
     screen->rgbFGColour = cfTTYFont.rgbColors;
     ResetScreen (screen);
   }
   return  TRUE;
}

//---------------------------------------------------------------------------
//  BOOL SelectScreenBackColor (SCREEN screen, HWND owner)
//
//  Description:
//     Selects the background color for the TTY screen.
//     Uses the Common Dialog ChooseColor() API.
//
//  Parameters:
//     HWND hDlg
//
//---------------------------------------------------------------------------

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
     //,RGB(0xFF,0x00,0x00)
     ,RGB(0x00,0xFF,0x00)
     ,RGB(0xFF,0xFF,0x00)
     //,RGB(0x00,0x00,0xFF)
     //,RGB(0xFF,0x00,0xFF)
     //,RGB(0x00,0xFF,0xFF)
     ,RGB(0xFF,0xFF,0xFF)
     };

   CHOOSECOLOR backcolor;

   if (NULL == screen)
     return  FALSE;

   backcolor.lStructSize    = sizeof (CHOOSECOLOR);
   backcolor.hwndOwner      = owner ;
   backcolor.hInstance      = (HINSTANCE) GetWindowLong(owner, GWL_HINSTANCE);

   backcolor.rgbResult      = screen->rgbBGColour;
   backcolor.lpCustColors   = &custcolors[0];
   backcolor.Flags          = (CC_RGBINIT);

   backcolor.lCustData      = 0 ;
   backcolor.lpfnHook       = NULL ;
   backcolor.lpTemplateName = NULL ;

   if (ChooseColor (&backcolor))
   {
     HDC hdc = GetDC (owner);

     /* Use GetNearestColor to ensure consistency with the background
	text color.
      */
     screen->rgbBGColour = GetNearestColor (hdc, (backcolor.rgbResult));
     if (screen->bkgnd_brush != NULL)
       DeleteObject (screen->bkgnd_brush);
     screen->bkgnd_brush = CreateSolidBrush (screen->rgbBGColour);
     InvalidateRect (owner, NULL, TRUE);
     //SendMessage (owner, WM_ERASEBKGND, ((WPARAM) hdc), ((LPARAM) 0));
     ReleaseDC (owner, hdc);
   }
   return  TRUE;
}

//---------------------------------------------------------------------------

static void
alloc_event_failure (SCREEN screen)
{
  if ((MessageBox
       (screen->hWnd,
	"Scheme has leaked some keyboard event storage.\n"
	"OK to reset and clear all pending events?",
	"MIT Scheme",
	(MB_ICONSTOP | MB_OKCANCEL)))
      == IDOK)
    screen_reset_events (screen);
}

static SCREEN_EVENT *
alloc_event (SCREEN screen, SCREEN_EVENT_TYPE type)
{
  SCREEN_EVENT_LINK * new_event;
  if ((screen->mode_flags & type) == 0)
    return ((SCREEN_EVENT *) NULL);

  if (screen->free_events == ((SCREEN_EVENT_LINK *) NULL))
  {
    if (screen->n_events == MAX_EVENTS)
    {
      MessageBeep (0xFFFFFFFFUL);
      return ((SCREEN_EVENT *) NULL);
    }
    alloc_event_failure (screen);
    if (screen->free_events == ((SCREEN_EVENT_LINK *) NULL))
      return ((SCREEN_EVENT *) NULL);
  }
    
  new_event = screen->free_events;
  screen->free_events = new_event->next;
  new_event->event.type = type;
  new_event->next = ((SCREEN_EVENT_LINK *) NULL);
  if (screen->queue_tail == ((SCREEN_EVENT_LINK *) NULL))
    screen->queue_head = new_event;
  else
    screen->queue_tail->next = new_event;
  screen->queue_tail = new_event;
  screen->n_events += 1;

  return  &new_event->event;
}

BOOL
Screen_GetEvent (HANDLE hwnd, SCREEN_EVENT * event)
{
  SCREEN_EVENT_LINK * new_event;
  SCREEN screen = GETSCREEN (hwnd);

  if ((screen == ((SCREEN) NULL)) || (screen->n_events == 0))
    return  FALSE;
  screen->n_events -= 1;
  new_event = screen->queue_head;
  *event = new_event->event;
  screen->queue_head = new_event->next;
  if (screen->queue_head == ((SCREEN_EVENT_LINK *) NULL))
    screen->queue_tail = ((SCREEN_EVENT_LINK *) NULL);
  new_event->next = screen->free_events;
  screen->free_events = new_event;
  return  TRUE;
}

BOOL
Screen_PeekEvent (HANDLE hwnd, SCREEN_EVENT * event)
{
  SCREEN screen = (GETSCREEN (hwnd));

  if ((screen == ((SCREEN) NULL)) || (screen->n_events == 0))
    return  FALSE;
  if (event != ((SCREEN_EVENT *) NULL))
    *event = screen->queue_head->event;
  return  TRUE;
}

//---------------------------------------------------------------------------

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
  SendMessage (screen, SCREEN_SETMODES, (LPARAM)mode, 0);
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
Screen_GetSize (HANDLE hWnd, int *rows, int *columns)
{
  SCREEN screen = (GETSCREEN (hWnd));
  if (screen == 0)
    return;
  *rows    = screen->height;
  *columns = screen->width;
}

///* Utilities for MIT character translation */
//
//static BOOL _fastcall
//MIT_post_char_message (CONST MSG * lpmsg, WPARAM the_char)
//{
//  return  PostMessage (lpmsg->hwnd,
//		       ((lpmsg->message == WM_KEYDOWN)
//			? WM_CHAR
//			: WM_SYSCHAR),
//		       the_char,
//		       lpmsg->lParam));
//}
//
//#ifndef VK_A
//#  define VK_A 'A'
//#  define VK_Z 'Z'
//#endif /* VK_A */
//
///* US IBM-PC keyboard */
//
//#define VK_ATSIGN       '2'
//#define VK_CARET        '6'
//#define VK_LSQB         219
//#define VK_RSQB         221
//#define VK_BACKSLASH    220
//#define VK_UNDERSCORE   189
//
//#define ASCII_CONTROLIFY(ascii) ((ascii) - '@')
//#define ASCII_METAFY(ascii)     ((ascii) | 0200)
//
//static BOOL _fastcall
//MIT_controlify (WPARAM virtual_key, WPARAM * control_char)
//{
//  BOOL result = ((BOOL) 1);
//
//  if ((virtual_key >= VK_A) && (virtual_key <= VK_Z))
//    * control_char = (ASCII_CONTROLIFY ('A' + (virtual_key - VK_A)));
//  else if (virtual_key == VK_ATSIGN)
//    * control_char = (ASCII_CONTROLIFY ('@'));
//  else if (virtual_key == VK_CARET)
//    * control_char = (ASCII_CONTROLIFY ('^'));
//  else if (virtual_key == VK_LSQB)
//    * control_char = (ASCII_CONTROLIFY ('['));
//  else if (virtual_key == VK_RSQB)
//    * control_char = (ASCII_CONTROLIFY (']'));
//  else if (virtual_key == VK_BACKSLASH)
//    * control_char = (ASCII_CONTROLIFY ('\\'));
//  else if (virtual_key == VK_UNDERSCORE)
//    * control_char = (ASCII_CONTROLIFY ('_'));
//  else
//    result = ((BOOL) 0);
//
//  return (result);
//}

//BOOL
//MIT_TranslateMessage (CONST MSG * lpmsg)
//{
//  UINT message = (lpmsg->message);
//
//  switch (message)
//  {
//    case WM_KEYDOWN:
//    case WM_SYSKEYDOWN:
//    {
//      WPARAM virtual_key = (lpmsg->wParam);
//
//      switch (virtual_key)
//      {
//	case VK_LEFT:
//	  return (MIT_post_char_message (lpmsg, (ASCII_CONTROLIFY ('B'))));
//
//	case VK_RIGHT:
//	  return (MIT_post_char_message (lpmsg, (ASCII_CONTROLIFY ('F'))));
//
//	case VK_UP:
//	  return (MIT_post_char_message (lpmsg, (ASCII_CONTROLIFY ('P'))));
//
//	case VK_DOWN:
//	  return (MIT_post_char_message (lpmsg, (ASCII_CONTROLIFY ('N'))));
//
//	case VK_HOME:
//	  return (MIT_post_char_message (lpmsg, (ASCII_CONTROLIFY ('A'))));
//
//	case VK_END:
//	  return (MIT_post_char_message (lpmsg, (ASCII_CONTROLIFY ('E'))));
//
//	case VK_BACK:
//	  return (MIT_post_char_message (lpmsg, ASCII_DEL));
//
//	case VK_DELETE:
//	  return (MIT_post_char_message (lpmsg, (ASCII_CONTROLIFY ('D'))));
//	  
//	case VK_PRIOR:
//	  return (MIT_post_char_message (lpmsg, (ASCII_METAFY ('v'))));
//
//	case VK_NEXT:
//	  return (MIT_post_char_message (lpmsg, (ASCII_CONTROLIFY ('V'))));
//
//	case VK_INSERT:
//	  return (MIT_post_char_message (lpmsg, (ASCII_CONTROLIFY ('O'))));
//
//	case VK_SPACE:
//	  if ((((DWORD) (GetKeyState (VK_CONTROL))) & 0x8000) != 0)
//	    return (MIT_post_char_message (lpmsg, ((WPARAM) '\0')));
//	  break;
//
//#ifdef WINDOWSLOSES
//	case VK_TAB:
//	  if (MIT_trap_alt_tab)
//	    return ((BOOL) 0);
//	  break;
//
//	case VK_ESCAPE:
//	  if ((message == WM_SYSKEYDOWN) && MIT_trap_alt_escape)
//	    return ((BOOL) 0);
//	  break;
//#endif /* WINDOWSLOSES */
//
//	default:
//	{
//	  WPARAM control_char;
//
//	  if (((message == WM_SYSKEYDOWN) || (lpmsg->lParam & KEYDATA_ALT_BIT))
//	      && ((((DWORD) (GetKeyState (VK_CONTROL))) & 0x8000) != 0)
//	      && (MIT_controlify (virtual_key, &control_char)))
//	    return (MIT_post_char_message (lpmsg, control_char));
//	  break;
//	}
//      }
//      break;
//    }
//
//#ifdef WINDOWSLOSES
//    case WM_KEYUP:
//    case WM_SYSKEYUP:
//    {
//      WPARAM virtual_key = (lpmsg->wParam);
//
//      switch (virtual_key)
//      {
//	case VK_TAB:
//	  if (MIT_trap_alt_tab)
//	    return (MIT_post_char_message (lpmsg, ((WPARAM) '\t')));
//	  break;
//
//	case VK_ESCAPE:
//	  if (MIT_trap_alt_escape)
//	    return (MIT_post_char_message (lpmsg, ((WPARAM) ASCII_ESC)));
//	  break;
//
//	default:
//	  break;
//      }
//      break;
//    }
//#endif /* WINDOWSLOSES */
//
//    default:
//      break;
//  }
//  return  TranslateMessage (lpmsg);
//}
//

int
Screen_Width (SCREEN screen)
{
  return  screen->width;
}

int
Screen_Height (SCREEN screen)
{
  return  screen->height;
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
  if (show)
    screen->CursorState = CS_SHOW;
  else
    screen->CursorState = CS_HIDE;
}

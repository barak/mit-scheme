/* -*-C-*-

$Id: ntscreen.c,v 1.8 1993/08/11 20:28:55 adams Exp $

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

#include <stdlib.h>
#include "ntscreen.h"
//#include "screen.rh"

// constant definitions

#define GWL_SCREEN        0
#define SCREENEXTRABYTES        sizeof( LONG )

#define ATOM_TTYINFO       0x100

#if 0
#define MAXCOLS 80
#define MAXROWS 40
#else
#define MAXCOLS 132
#define MAXROWS 80
#endif

// cursor states

#define CS_HIDE         0x00
#define CS_SHOW         0x01

// Flow control flags

// ascii definitions

#define ASCII_BEL       0x07
#define ASCII_BS        0x08
#define ASCII_LF        0x0A
#define ASCII_FF	0x0C
#define ASCII_CR        0x0D
#define ASCII_ESC	0x1B
#define ASCII_DEL	0x7F

// data structures

#define MAX_EVENTS 1000

typedef struct tagSCREEN_EVENT_LINK {
  SCREEN_EVENT event;
  struct tagSCREEN_EVENT_LINK *next;
} SCREEN_EVENT_LINK;

#define MAX_COMMANDS 30

#define MAX_BINDINGS 10 

#define MAX_LINEINPUT 1024

typedef struct tagSCREENINFO
{
   SCREEN  registry_link;
   
   HWND	   hWnd;
   
   char              *chars;
   SCREEN_ATTRIBUTE  *attrs;
   WORD              mode_flags;	//events & modes
   SCREEN_ATTRIBUTE  write_attribute;
   
   WORD    CursorState ;

   HFONT   hFont ;
   LOGFONT lfFont ;
   DWORD   rgbFGColour ;
   int     xSize, ySize, xScroll, yScroll, xOffset, yOffset;
   int     column, row, xChar, yChar ;
   int     width, height;  //size in characters

   int	n_events;
   SCREEN_EVENT_LINK  *events;
   SCREEN_EVENT_LINK  *free_events;
   SCREEN_EVENT_LINK  *queue_head,  *queue_tail;

   int n_commands;
   struct {
     WORD wID;
     COMMAND_HANDLER  thunk;
   } commands[MAX_COMMANDS];
   
   int n_bindings;
   struct {
     char   key;
     WORD   command;
   } bindings[MAX_BINDINGS];

   
   // for line input
   int n_chars;
   char  *line_buffer;
} SCREEN_STRUCT;

//#define WIDTH(screen) (screen->width)
#define WIDTH(screen) MAXCOLS
#define HEIGHT(screen) MAXROWS
// macros ( for easier readability )

#define GETHINST( x )  ((HINSTANCE) GetWindowLong( x, GWL_HINSTANCE ))
#define GETSCREEN( x ) ((SCREEN) GetWindowLong( x, GWL_SCREEN ))
#define SETSCREEN( x, y ) SetWindowLong( x, GWL_SCREEN, (LONG) y )


#define SET_PROP( x, y, z )  SetProp( x, MAKEINTATOM( y ), z )
#define GET_PROP( x, y )     GetProp( x, MAKEINTATOM( y ) )
#define REMOVE_PROP( x, y )  RemoveProp( x, MAKEINTATOM( y ) )

// CRT mappings to NT API

#define _fmemset   memset
#define _fmemmove  memmove

// function prototypes (private)

LRESULT CreateScreenInfo (HWND);
BOOL DestroyScreenInfo (HWND);
BOOL ResetScreen (SCREEN);
BOOL KillScreenFocus (HWND);
BOOL PaintScreen (HWND);
BOOL SetScreenFocus (HWND);
BOOL ScrollScreenHorz (HWND, WORD, WORD);
BOOL ScrollScreenVert (HWND, WORD, WORD);
BOOL SizeScreen (HWND, WORD, WORD);
BOOL ProcessScreenCharacter (HWND, int, int, DWORD);
BOOL WriteScreenBlock (HWND, LPSTR, int);
int  ReadScreen (SCREEN, char*, int);
BOOL MoveScreenCursor (SCREEN);
BOOL Screen_SetPosition (SCREEN, int, int);
UINT ScreenPeekOrRead (SCREEN, int count, SCREEN_EVENT* buffer, BOOL remove);
COMMAND_HANDLER ScreenSetCommand (SCREEN, WORD cmd, COMMAND_HANDLER handler);
WORD ScreenSetBinding (SCREEN, char key, WORD command);
VOID GetMinMaxSizes(HWND,LPPOINT,LPPOINT);
VOID Screen_Clear (SCREEN,int);
VOID GoModalDialogBoxParam (HINSTANCE, LPCSTR, HWND, DLGPROC, LPARAM);
VOID FillComboBox (HINSTANCE, HWND, int, DWORD *, WORD, DWORD);
BOOL SelectScreenFont (SCREEN, HWND);
BOOL SettingsDlgInit (HWND);
BOOL SettingsDlgTerm (HWND);

LRESULT ScreenCommand_ChooseFont (HWND, WORD);

VOID SetDebuggingTitle (SCREEN);

SCREEN_EVENT  *alloc_event (SCREEN, SCREEN_EVENT_TYPE); //may return NULL
int  GetControlKeyState(DWORD lKeyData);
//void *xmalloc (int size);
//void xfree (void*);
#define xfree free
#define xmalloc malloc

LRESULT FAR PASCAL ScreenWndProc (HWND, UINT, WPARAM, LPARAM);
BOOL FAR PASCAL SettingsDlgProc (HWND, UINT, WPARAM, LPARAM ) ;

VOID RegisterScreen (SCREEN);
VOID UnregisterScreen (SCREEN);

#ifdef WINDOWSLOSES

static BOOL
  MIT_trap_alt_tab = ((BOOL) 0),
  MIT_trap_alt_escape = ((BOOL) 1);

static VOID
init_flag (char * flag_symbol, BOOL * flag)
{
  extern int strcmp_ci (char *, char *);
  char * envvar = (getenv (flag_symbol));
  if (envvar != NULL)
  {
    if ((strcmp_ci (envvar, "true")) || (strcmp_ci (envvar, "yes")))
      * flag = ((BOOL) 1);
    else if ((strcmp_ci (envvar, "false")) || (strcmp_ci (envvar, "no")))
      * flag = ((BOOL) 0);
  }
  return;
}

VOID
init_MIT_Keyboard (VOID)
{
  init_flag ("MITSCHEME_TRAP_ALT_TAB", (& MIT_trap_alt_tab));
  init_flag ("MITSCHEME_TRAP_ALT_ESCAPE", (& MIT_trap_alt_escape));
  return;
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
   wndclass.hbrBackground = (HBRUSH) (COLOR_WINDOW + 1) ;
   wndclass.lpszMenuName =  0;
   wndclass.lpszClassName = "SCREEN";

   return  (RegisterClass (&wndclass));
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
  return (TRUE);
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

HANDLE
Screen_Create (HANDLE hParent, LPCSTR title, int nCmdShow)
{
  HWND  hWnd = (CreateWindow ("SCREEN", title,
			      WS_OVERLAPPEDWINDOW,
			      CW_USEDEFAULT, CW_USEDEFAULT,
			      CW_USEDEFAULT, CW_USEDEFAULT,
			      hParent, NULL, ghInstance,
			      ((LPVOID) nCmdShow)));

  if (hWnd == NULL)
    return (NULL);
  return (hWnd);
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
  return;
}

static SCREEN 
* head_to_registered_screen (HWND hWnd)
{
  SCREEN *link = &registered_screens;
  while (*link)
    if ((*link)->hWnd == hWnd)
      return link;
    else
      link = &((*link)->registry_link);
  return (0);
}

static VOID
UnregisterScreen (SCREEN screen)
{
  SCREEN *link = head_to_registered_screen (screen->hWnd);
  //  if (link)
    *link = screen->registry_link;
  return;
}

BOOL
Screen_IsScreenHandle (HANDLE handle)
{
  return ((head_to_registered_screen (handle)) != 0);
}

//---------------------------------------------------------------------------
//  LRESULT FAR PASCAL ScreenWndProc (HWND hWnd, UINT uMsg,
//                                 WPARAM wParam, LPARAM lParam )
//
//  Description:
//     This is the TTY Window Proc.  This handles ALL messages
//     to the tty window.
//
//  Parameters:
//     As documented for Window procedures.
//
//  Win-32 Porting Issues:
//     - WM_HSCROLL and WM_VSCROLL packing is different under Win-32.
//     - Needed LOWORD() of wParam for WM_CHAR messages.
//
//---------------------------------------------------------------------------

LRESULT FAR PASCAL 
ScreenWndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
   SCREEN  screen = GETSCREEN (hWnd);

   static  BOOL		vk_pending = FALSE;
   static  int		vk_code;
   static  LPARAM	vk_lparam;
   
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
	 
      case WM_COMMAND:
      case WM_SYSCOMMAND:
      {
	 WORD  wID = LOWORD (wParam);
	 int  i;
	 for (i=0;  i<screen->n_commands; i++)
	   if (screen->commands[i].wID == wID)
	     return  screen->commands[i].thunk(hWnd, wID);
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
	 if (vk_pending)
	   ProcessScreenCharacter (hWnd, vk_code, -1, vk_lparam);
	 vk_pending = TRUE;
	 vk_code    = wParam;
	 vk_lparam  = lParam;
#ifdef WINDOWSLOSES
         if (((wParam == VK_ESCAPE) && MIT_trap_alt_escape)
	     || ((wParam == VK_TAB) && MIT_trap_alt_tab))
           return (0L);
         else
#endif /* WINDOWSLOSES */
	   return (DefWindowProc (hWnd, uMsg, wParam, lParam));

      case WM_SYSKEYUP:
      case WM_KEYUP:
	 if (vk_pending)
	   ProcessScreenCharacter (hWnd, vk_code, -1, vk_lparam);
	 vk_pending = FALSE;
#ifdef WINDOWSLOSES
         if (((wParam == VK_ESCAPE) && MIT_trap_alt_escape)
	     || ((wParam == VK_TAB) && MIT_trap_alt_tab))
           return (0L);
         else
#endif /* WINDOWSLOSES */
	   return (DefWindowProc (hWnd, uMsg, wParam, lParam));

      case WM_SYSDEADCHAR:
      case WM_DEADCHAR:
	 vk_pending = FALSE;
	 return (DefWindowProc (hWnd, uMsg, wParam, lParam));
	 
      case WM_SYSCHAR:
      case WM_CHAR:
         ProcessScreenCharacter (hWnd, vk_code,
				 (LOBYTE (LOWORD(wParam))),
				 ((DWORD) lParam));
         vk_pending = FALSE;
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

      case WM_CLOSE:
         if (IDOK != (MessageBox (hWnd, "OK to close screen window?",
				  "Screen Sample",
				  (MB_ICONQUESTION | MB_OKCANCEL))))
            break ;

         // fall through

      default:
         return (DefWindowProc (hWnd, uMsg, wParam, lParam));
   }
   return (0L);
}

static VOID
ClearScreen_internal (SCREEN screen)
{
  screen->row			= 0;
  screen->column		= 0;
  _fmemset (screen->chars, ' ', MAXROWS * MAXCOLS);
  _fmemset (screen->attrs, screen->write_attribute,
	    MAXROWS * MAXCOLS * sizeof(SCREEN_ATTRIBUTE));
  return;
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
   int     i;
   SCREEN  screen;

   if (NULL == (screen =
                   (SCREEN) LocalAlloc (LPTR, sizeof(SCREEN_STRUCT) )))
      return  (LRESULT) -1;

   screen->hWnd			= hWnd;
   screen->chars		= NULL;
   screen->attrs		= NULL;
   screen->write_attribute	= 0;
   screen->CursorState		= CS_HIDE ;
   screen->mode_flags		= SCREEN_EVENT_TYPE_KEY
                                | SCREEN_EVENT_TYPE_MOUSE
				| SCREEN_EVENT_TYPE_RESIZE
                                | SCREEN_MODE_ECHO
			     // | SCREEN_MODE_NEWLINE
			        | SCREEN_MODE_AUTOWRAP
				| SCREEN_MODE_PROCESS_OUTPUT
				| SCREEN_MODE_LINE_INPUT
				| SCREEN_MODE_EAGER_UPDATE;
   screen->xSize		= 0;
   screen->ySize		= 0 ;
   screen->xScroll		= 0 ;
   screen->yScroll		= 0 ;
   screen->xOffset		= 0 ;
   screen->yOffset		= 0 ;
   screen->hFont		= NULL;
   screen->rgbFGColour		= RGB(0,0,0);
   screen->width		= 0;
   screen->height		= 0;
      
   screen->chars = xmalloc (MAXROWS * MAXCOLS);
   screen->attrs = xmalloc (MAXROWS * MAXCOLS * sizeof(SCREEN_ATTRIBUTE));

   ClearScreen_internal (screen);
   // clear screen space

   // setup default font information

   screen->lfFont.lfHeight =         9 ;
   screen->lfFont.lfWidth =          0 ;
   screen->lfFont.lfEscapement =     0 ;
   screen->lfFont.lfOrientation =    0 ;
   screen->lfFont.lfWeight =         0 ;
   screen->lfFont.lfItalic =         0 ;
   screen->lfFont.lfUnderline =      0 ;
   screen->lfFont.lfStrikeOut =      0 ;
   screen->lfFont.lfCharSet =        OEM_CHARSET ;
   screen->lfFont.lfOutPrecision =   OUT_DEFAULT_PRECIS ;
   screen->lfFont.lfClipPrecision =  CLIP_DEFAULT_PRECIS ;
   screen->lfFont.lfQuality =        DEFAULT_QUALITY ;
   screen->lfFont.lfPitchAndFamily = FIXED_PITCH | FF_MODERN ;
   lstrcpy (screen->lfFont.lfFaceName, "FixedSys");

   // set handle before any further message processing.

   SETSCREEN (hWnd, screen);
   RegisterScreen (screen);

   screen->n_events = 0;
   screen->queue_head = screen->queue_tail = 0;
   screen->events = xmalloc (sizeof(SCREEN_EVENT_LINK) * MAX_EVENTS);
   screen->free_events = &screen->events[0];
   for (i = 0; i<MAX_EVENTS; i++)
     screen->events[i].next = &screen->events[i+1];
   screen->events[MAX_EVENTS-1].next = 0;
 
   screen->n_commands = 0;
   screen->n_bindings = 0;
   // reset the character information, etc.

   ResetScreen (screen);

   hMenu = GetSystemMenu (hWnd, FALSE);
   AppendMenu (hMenu, MF_SEPARATOR, 0, 0);
// AppendMenu (hMenu, MF_STRING, IDM_SETTINGS, "&Settings...");
   AppendMenu (hMenu, MF_STRING, SCREEN_COMMAND_CHOOSEFONT,     "&Font...");
    
   SendMessage (hWnd, SCREEN_SETCOMMAND,
                SCREEN_COMMAND_CHOOSEFONT, (LPARAM)ScreenCommand_ChooseFont);
   SendMessage (hWnd, SCREEN_SETBINDING, 6, SCREEN_COMMAND_CHOOSEFONT);
		  
   screen->n_chars = 0;
   screen->line_buffer = xmalloc (MAX_LINEINPUT + 1);
     
   return  (LRESULT) TRUE;
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

VOID
SetDebuggingTitle (SCREEN screen)
{
#if 1
  return;
#else				/* not 1 */
  char buf[80];

  wsprintf (buf, "%d@%d  q=%d  c=%d  b=%d",
	    screen->row, screen->column,
	    screen->n_events,
	    screen->n_commands,
	    screen->n_bindings,
	    0);
  SendMessage (screen->hWnd, WM_SETTEXT, 0, ((LPARAM) buf));
  return;
#endif				/* 1 */
}

//---------------------------------------------------------------------------
//  BOOL DestroyScreenInfo (HWND hWnd )
//
//  Description:
//     Destroys block associated with TTY window handle.
//
//  Parameters:
//     HWND hWnd
//        handle to TTY window
//
//---------------------------------------------------------------------------

static BOOL
DestroyScreenInfo (HWND hWnd)
{
   SCREEN screen = (GETSCREEN (hWnd));

   if (NULL == screen)
      return (FALSE);

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
   return (TRUE);
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
	return (result);
      }
      
    // didnt find it
    if ((thunk == 0) || (thunk == ((COMMAND_HANDLER) -1)))
      return (0);
    // add new command
    if (screen->n_commands == MAX_COMMANDS)
      return ((COMMAND_HANDLER) - 1);

    screen->commands[screen->n_commands].wID   = cmd;
    screen->commands[screen->n_commands].thunk = thunk;
    screen->n_commands++;
    
    return (0);
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
	return (result);
      }
      
    // no existing binding for key
    if ((command == 0) || (command == ((WORD) -1)))
      return (0);
    // add new binding
    if (screen->n_bindings == MAX_BINDINGS)
      return ((WORD) - 1);

    screen->bindings[screen->n_bindings].key     = key;
    screen->bindings[screen->n_bindings].command = command;
    screen->n_bindings++;
    
    return (0);
}

//===========================================================================
//  Standard commands
//===========================================================================

LRESULT
ScreenCommand_ChooseFont (HWND hWnd, WORD command)
{
  SCREEN  screen = GETSCREEN (hWnd);
  if (screen == 0)
    return (1L);
  SelectScreenFont (screen, hWnd);
  return (0L);
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
  return;
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
   HWND	       hWnd;
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
     SetScreenFocus(hWnd);
   }

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
       SendMessage (hWnd, WM_SIZE, SIZENORMAL,
		    ((LPARAM) (MAKELONG (width,height))));
   }

   return (TRUE);
}

//---------------------------------------------------------------------------
//  BOOL PaintScreen (HWND hWnd )
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

static BOOL 
PaintScreen (HWND hWnd)
{
  SCREEN	screen = (GETSCREEN (hWnd));
  HDC		hDC ;
  PAINTSTRUCT	ps ;
  RECT		rect ;

  if (NULL == screen)
    return (FALSE);

  hDC = (BeginPaint (hWnd, &ps));

  if (IsIconic (hWnd))
  {
//      // draw a minature version of the window
//      int  ICONSIZE = 36;
//      int  row, col;
//      HPEN hOldPen, hPen, hPen2;
//
//      hPen = CreatePen (PS_SOLID, 0,
//	                /* average of two RGB colours */
//	                ((GetSysColor (COLOR_ACTIVEBORDER)&0xfefefeL) >> 1) +
//			((GetSysColor (COLOR_WINDOWFRAME)&0xfefefeL)  >> 1));
//      hOldPen = SelectObject (hDC, hPen);
//      MoveToEx (hDC, 0, 0, NULL);
//      LineTo (hDC,0,ICONSIZE-1); LineTo (hDC,ICONSIZE-1,ICONSIZE-1);
//      LineTo (hDC,ICONSIZE-1,0); LineTo (hDC,0,0);
//      //Rectangle (hDC, 0, 0, ICONSIZE, ICONSIZE);
//      hPen2 = CreatePen (PS_SOLID, 0, GetSysColor (COLOR_ACTIVECAPTION));
//      SelectObject (hDC, hPen2);
//      DeleteObject (hPen);
//      MoveToEx (hDC, 1, 1, NULL);  LineTo (hDC, ICONSIZE-1, 1);
//      MoveToEx (hDC, 1, 2, NULL);  LineTo (hDC, ICONSIZE-1, 2);
//      MoveToEx (hDC, 1, 3, NULL);  LineTo (hDC, ICONSIZE-1, 3);
//
//      hPen = CreatePen (PS_SOLID, 0, screen->rgbFGColour);
//      SelectObject (hDC, hPen);
//      DeleteObject (hPen2);
//      SetBkColor (hDC, GetSysColor (COLOR_WINDOW));
//      rect = ps.rcPaint ;
//      for (row = 0; row < (ICONSIZE-6)/2; row++) {
//	if (row >= screen->height) break;
//	for (col = 0; col < ICONSIZE-4; col++) {
//	  int  run_length = 0;
//	  char *s = & screen->chars[row*WIDTH(screen) + col];
//	  while (col+run_length < ICONSIZE-4 && s[run_length] != ' ')
//	    run_length++;
//	  if (run_length>0) {
//	     int x = col+2;
//	     int y = row*2+5;
//	     MoveToEx (hDC, x, y, NULL);
//	     LineTo (hDC, x+run_length, y);
//	  }
//	}
//      }
//      SelectObject (hDC, hOldPen);
//      DeleteObject (hPen);
  }

  else
  {
    int		nRow, nCol, nEndRow, nEndCol, nCount;
    int		nHorzPos, nVertPos, bias;
    HFONT	hOldFont;
    DWORD	bgcolor;

    hOldFont = (SelectObject (hDC, screen->hFont));
    rect = ps.rcPaint;

//   nRow =
//      min (MAXROWS - 1,
//           max (0, (rect.top + screen->yOffset) / screen->yChar));
//   nEndRow =
//      min (MAXROWS - 1,
//           ((rect.bottom + screen->yOffset - 1) / screen->yChar));
    nRow =
      min (screen->height - 1,
	   max (0, (rect.top + screen->yOffset) / screen->yChar));
    nEndRow =
      min (screen->height - 1,
	   ((rect.bottom + screen->yOffset - 1) / screen->yChar));
//   nCol =
//      min (MAXCOLS - 1,
//           max (0, (rect.left + screen->xOffset) / screen->xChar));
//   nEndCol =
//      min (MAXCOLS - 1,
//           ((rect.right + screen->xOffset - 1) / screen->xChar));
    nCol =
      min (screen->width - 1,
	   max (0, (rect.left + screen->xOffset) / screen->xChar));
    nEndCol =
      min (screen->width - 1,
	   ((rect.right + screen->xOffset - 1) / screen->xChar));
    nCount = ((nEndCol - nCol) + 1);
    SetBkMode (hDC, OPAQUE);
    bgcolor = (GetSysColor (COLOR_WINDOW));
    SetTextColor (hDC, screen->rgbFGColour);
    SetBkColor (hDC, bgcolor);

    for (bias = ((nRow * MAXCOLS) + nCol),
	 nVertPos = ((nRow * screen->yChar) - screen->yOffset);	 
	 nRow <= nEndRow;
	 nRow++, bias += MAXCOLS, nVertPos += screen->yChar)
    {
      int pos = 0;
      while (pos < nCount)
      {
	/* find consistent run of attributes */
	SCREEN_ATTRIBUTE
	  * attribp = &screen->attrs[bias + pos],
	  attrib = *attribp;
	int
	  nposn = (pos + 1),
	  run_length;

	while ((nposn < nCount) && (*++attribp == attrib))
	  nposn++;

	run_length = (nposn - pos);
	nHorzPos = (((nCol + pos) * screen->xChar) - screen->xOffset);
	rect.top = nVertPos;
	rect.bottom = (nVertPos + screen->yChar);
	rect.left = nHorzPos;
	rect.right = (nHorzPos + (screen->xChar * run_length));
	if (attrib)
	{
	  SetTextColor (hDC, bgcolor);
	  SetBkColor (hDC, screen->rgbFGColour);
	}
	ExtTextOut (hDC, nHorzPos, nVertPos, (ETO_OPAQUE | ETO_CLIPPED),
		    &rect, &screen->chars[bias + pos],
		    run_length, NULL);
	if (attrib)
	{
	  SetTextColor (hDC, screen->rgbFGColour);
	  SetBkColor (hDC, bgcolor);
	}
	pos = nposn;
      }
    }
    SelectObject (hDC, hOldFont);
  }
  EndPaint (hWnd, &ps);
  MoveScreenCursor (screen);
  return (TRUE);
}

//---------------------------------------------------------------------------
//  void SetCells (screen,r,col,count,char,attr)
//
//---------------------------------------------------------------------------

static VOID
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
  return;
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
  return;
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

   screen->width  = new_width;
   screen->height = new_height;

   { // queue event
     SCREEN_EVENT  *event = alloc_event (screen, SCREEN_EVENT_TYPE_RESIZE);
//     char buf[80];
     if (event) {
       event->event.resize.rows    = new_height;
       event->event.resize.columns = new_width;
//       wsprintf (buf, "[Resize %dx%d]", new_height, new_width);
//       Screen_WriteText (screen->hWnd, buf);
     }
   }

   // Clear out revealed character cells
   if (new_width > old_width) {
     int  row, rows = min (old_height, new_height);
     for (row = 0; row < rows; row++)
       SetCells (screen, row, old_width, new_width-old_width, ' ', 0);
   }
   if (new_height > old_height) {
     int  row;
     for (row = old_height; row < new_height; row++)
       SetCells (screen, row, 0, new_width, ' ', 0);
   }

   // scroll window to fit in cursor
   if (screen->column >= new_width) {
     screen->column = 0;
     screen->row += 1;
   }
   if (screen->row >= new_height) {
     int  difference = screen->row - (new_height-1);
     ScrollScreenBufferUp (screen, difference);
     screen->row -= difference;
     MoveScreenCursor (screen);
   }
   
   screen->ySize = (int) wVertSize ;
   screen->yScroll = max (0, (MAXROWS * screen->yChar) -
                               screen->ySize);

   screen->yScroll = 0;
//   nScrollAmt = min (screen->yScroll, screen->yOffset ) -
//                     screen->yOffset;
//   ScrollWindow (hWnd, 0, -nScrollAmt, NULL, NULL);
//   screen->yOffset = screen->yOffset + nScrollAmt ;
//   SetScrollPos (hWnd, SB_VERT, screen->yOffset, FALSE);
   SetScrollRange (hWnd, SB_VERT, 0, screen->yScroll, TRUE);

   screen->xSize = (int) wHorzSize ;
   screen->xScroll = max (0, (MAXCOLS * screen->xChar) -
                                screen->xSize);
   screen->xScroll = 0;			     
//   nScrollAmt = min (screen->xScroll, screen->xOffset) -
//                     screen->xOffset;
//   ScrollWindow (hWnd, 0, -nScrollAmt, NULL, NULL);
//   screen->xOffset = screen->xOffset + nScrollAmt ;
//   SetScrollPos (hWnd, SB_HORZ, screen->xOffset, FALSE);
   SetScrollRange (hWnd, SB_HORZ, 0, screen->xScroll, TRUE);

   InvalidateRect (hWnd, NULL, TRUE);

   return  TRUE;

} // end of SizeTTY()

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
         return (FALSE);
   }
   if ((screen->xOffset + nScrollAmt) > screen->xScroll)
      nScrollAmt = screen->xScroll - screen->xOffset;
   if ((screen->xOffset + nScrollAmt) < 0)
      nScrollAmt = -screen->xOffset;
   ScrollWindow (hWnd, -nScrollAmt, 0, NULL, NULL);
   screen->xOffset = screen->xOffset + nScrollAmt ;
   SetScrollPos (hWnd, SB_HORZ, screen->xOffset, TRUE);

   return (TRUE);

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

   if (screen->CursorState != CS_SHOW)
   {
      CreateCaret (hWnd, NULL, max(screen->xChar/4,2), screen->yChar);
      ShowCaret (hWnd);
      screen->CursorState = CS_SHOW ;
   }
   MoveScreenCursor (screen);
   return (TRUE);
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
   return (TRUE);
}

//---------------------------------------------------------------------------
//  BOOL MoveScreenCursor (SCREEN screen)
//
//  Description:
//     Moves caret to current position.
//---------------------------------------------------------------------------

static BOOL 
MoveScreenCursor (SCREEN screen)
{
   if (screen->CursorState & CS_SHOW)
      SetCaretPos (screen->column * screen->xChar  -  screen->xOffset,
	           screen->row    * screen->yChar  -  screen->yOffset);

   SetDebuggingTitle (screen);		 
   return (TRUE);
}

//---------------------------------------------------------------------------
//  BOOL Screen_SetPosition (SCREEN, int row, int column);
//
//---------------------------------------------------------------------------

static BOOL 
Screen_SetPosition (SCREEN screen, int row, int column)
{
  if ((row < 0) || (row >= screen->height))
    return (FALSE);
  if ((column < 0) || (column > screen->width))		// may be ==
    return (FALSE);
  screen->row    = row;
  screen->column = column;
  return (MoveScreenCursor (screen));
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

static UINT
ScreenPeekOrRead (SCREEN screen, int count, SCREEN_EVENT * buffer, BOOL remove)
{
    UINT  processed = 0;
    SCREEN_EVENT_LINK  *current = screen->queue_head;
    SCREEN_EVENT* entry = buffer;
    
    if (count<0)
      count = MAX_EVENTS;
    
    while (count>0 && current)
    {
      if (entry)
	*entry++ = current->event;
      current = current->next;
      if (remove)
      {
	screen->queue_head->next = screen->events;
	screen->events     = screen->queue_head;
	screen->queue_head = current;
	screen->n_events--;
      }
    }
    return (processed);
}

//---------------------------------------------------------------------------
//  BOOL ProcessScreenCharacter (HWND hWnd, int vk_code, int ch, DWORD lKeyData)
//
//  Description:
//     This simply writes a character to the port and echos it
//     to the TTY screen if fLocalEcho is set.  Some minor
//     keyboard mapping could be performed here.
//
//  Parameters:
//     HWND hWnd
//        handle to TTY window
//
//     BYTE bOut
//        byte from keyboard
//
//  History:   Date       Author      Comment
//              5/11/91   BryanW      Wrote it.
//
//---------------------------------------------------------------------------

static BOOL
ProcessScreenCharacter (HWND hWnd, int vk_code, int bOut,
			DWORD lKeyData)
{
   SCREEN  screen = GETSCREEN (hWnd);
   SCREEN_EVENT *event;
   
   if (NULL == screen)
      return  FALSE;
    
   switch (vk_code)
   {
     case VK_SHIFT:
     case VK_CONTROL:
     case VK_CAPITAL:
     case VK_NUMLOCK:
     case VK_SCROLL:
       return  TRUE;
   }

   if (bOut == -1)
     return TRUE;

   // check for bindings:
   {
     int  i;
     for (i=0; i<screen->n_bindings; i++)
       if (screen->bindings[i].key == bOut)
       {
	 if (SendMessage (screen->hWnd,
			  WM_COMMAND,
			  MAKEWPARAM(screen->bindings[i].command, 0),
			  0))
	   return (TRUE);
	 else
	   break;
       }
   } 
   
   event = alloc_event (screen, SCREEN_EVENT_TYPE_KEY);
   if (event) {
//     char buf[80];
     event->event.key.repeat_count = lKeyData & 0xffff;
     event->event.key.virtual_keycode = vk_code;
     event->event.key.virtual_scancode = (lKeyData &0xff0000) >> 16;
     event->event.key.ch = bOut;
     event->event.key.control_key_state = GetControlKeyState(lKeyData);
//     wsprintf(buf,"[key %dof %d %d %d %02x]",
//       event->event.key.repeat_count,
//       event->event.key.virtual_keycode,
//       event->event.key.virtual_scancode,
//       event->event.key.ch,
//       event->event.key.control_key_state);
//     Screen_WriteText (screen, buf);     
   }
   
//   if (event && (screen->mode_flags & SCREEN_MODE_ECHO)) {
//     if (bOut)
//       WriteScreenBlock (hWnd, &bOut, 1);
//     else {
////       char name[20];
////       char buf[80];
////       GetKeyNameText(lKeyData, name, 20);
////       wsprintf (buf, "[%08x %d %d %s]", lKeyData, vk_code, bOut, name);
////       Screen_WriteText (screen, buf);
//     }
//   }

   return (TRUE);
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
  return;
}

static VOID _fastcall
Screen_LF (SCREEN screen)
{
  if ((screen->row++) >= (screen->height - 1))
  {
    ScrollScreenBufferUp (screen, 1);
    ScrollWindow (screen->hWnd, 0, -screen->yChar, NULL, NULL);
    //InvalidateRect (hWnd, NULL, FALSE);
    //screen->row-- ;
    screen->row = (screen->height - 1);
  }
  if (screen->mode_flags & SCREEN_MODE_EAGER_UPDATE)
  {
    MoveScreenCursor (screen);
    UpdateWindow (screen->hWnd);
  }
  return;
}

static VOID _fastcall 
Screen_CR (SCREEN screen)
{
  screen->column = 0 ;
  if (screen->mode_flags & SCREEN_MODE_NEWLINE)
    Screen_LF (screen);
  else if (screen->mode_flags & SCREEN_MODE_EAGER_UPDATE)
    MoveScreenCursor (screen);
  return;
}

struct screen_write_char_s
{
  RECT rect;
  int row;
  int col;
};

#define INIT_SCREEN_WRITE_CHAR_STATE(state) state.row = -1

static VOID _fastcall
Screen_WriteCharUninterpreted (SCREEN screen, int ch,
			       struct screen_write_char_s * rectp)
{
  /* Line wrap/overwrite the last position */
    
  if (screen->column >= screen->width)
  {
    if (screen->mode_flags & SCREEN_MODE_AUTOWRAP)
    {
      if ((rectp != ((struct screen_write_char_s *) NULL))
	  && (rectp->row != -1))
      {
	InvalidateRect (screen->hWnd, &rectp->rect, FALSE);
	rectp->row = -1;
      }
      Screen_CR (screen);
      if (! (screen->mode_flags & SCREEN_MODE_NEWLINE))
	Screen_LF (screen);
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

  screen->chars[screen->row * MAXCOLS + screen->column] = ch;
  screen->attrs[screen->row * MAXCOLS + screen->column] =
    screen->write_attribute;
  if (rectp == ((struct screen_write_char_s *) NULL))
  {
    RECT       rect ;

    rect.left = ((screen->column * screen->xChar) - screen->xOffset);
    rect.right = rect.left + screen->xChar;
    rect.top = ((screen->row * screen->yChar) - screen->yOffset);
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

    rectp->rect.left = ((screen->column * screen->xChar) - screen->xOffset);
    rectp->rect.right = rectp->rect.left + screen->xChar;
    rectp->rect.top = ((screen->row * screen->yChar) - screen->yOffset);
    rectp->rect.bottom = rectp->rect.top + screen->yChar;
    rectp->col = (screen->column + 1);
    rectp->row = screen->row;
  }
  screen->column += 1;
  return;
}

static VOID _fastcall
Finish_ScreenWriteChar (SCREEN screen, struct screen_write_char_s * rectp)
{
  if (rectp->row != -1)
    InvalidateRect (screen->hWnd, &rectp->rect, FALSE);
  return;
}

static VOID _fastcall
Screen_TAB (SCREEN screen, struct screen_write_char_s * rectp)
{
  do
    Screen_WriteCharUninterpreted (screen, ' ', rectp);
  while ((screen->column % 8) != 0);
  return;
}

static VOID _fastcall
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

  rect.left = ((lo_col * screen->xChar) - screen->xOffset);
  rect.right = ((hi_col * screen->xChar) - screen->xOffset);
  rect.top = ((lo_row * screen->yChar) - screen->yOffset);
  rect.bottom = ((hi_row * screen->yChar) - screen->yOffset);
  InvalidateRect (screen->hWnd, &rect, FALSE);

  return;
}

static VOID _fastcall
relocate_cursor (SCREEN screen, int row, int col)
{
  screen->row = ((row < 0)
		 ? 0
		 : ((row > screen->height)
		    ? (screen->height - 1)
		    : row));
  screen->column = ((col < 0)
		    ? 0
		    : ((col > screen->width)
		       ? (screen->width - 1)
		       : col));
  if (screen->mode_flags & SCREEN_MODE_EAGER_UPDATE)
    MoveScreenCursor (screen);
  return;
}

static VOID _fastcall
scroll_screen_vertically (SCREEN screen,
			  int lo_row_from, int lo_col,
			  int hi_row_from, int hi_col,
			  int lo_row_to)
{
  RECT rect;
  int row, delta_col, hi_row_to;
  char * chars_from, * chars_to;
  SCREEN_ATTRIBUTE * attrs_from, * attrs_to;

  delta_col = (hi_col - lo_col);
  hi_row_to = (lo_row_to + (hi_row_from - lo_row_from));

  if (lo_row_from > lo_row_to)		/* Scrolling up. */
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
  else					 /* Scrolling down. */
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

  rect.left = ((lo_col * screen->xChar) - screen->xOffset);
  rect.right = ((hi_col * screen->xChar) - screen->xOffset);
  rect.top = ((lo_row_to * screen->yChar) - screen->yOffset);
  rect.bottom = ((hi_row_to * screen->yChar) - screen->yOffset);
  InvalidateRect (screen->hWnd, &rect, FALSE);

  return;
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

  rect.left = ((lo_col_to * screen->xChar) - screen->xOffset);
  rect.right = ((hi_col_to * screen->xChar) - screen->xOffset);
  rect.top = ((row * screen->yChar) - screen->yOffset);
  rect.bottom = (((row + 1) * screen->yChar) - screen->yOffset);
  InvalidateRect (screen->hWnd, &rect, FALSE);

  return;
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

static VOID _fastcall
screen_write_octal (SCREEN screen, unsigned char the_char,
		    struct screen_write_char_s * rectp)
{
  Screen_WriteCharUninterpreted (screen, '\\', rectp);
  Screen_WriteCharUninterpreted (screen, ((the_char / 0100) + '0'), rectp);
  Screen_WriteCharUninterpreted (screen, (((the_char % 0100) / 010) + '0'), rectp);
  Screen_WriteCharUninterpreted (screen, ((the_char % 010) + '0'), rectp);
  return;
}

//---------------------------------------------------------------------------
//  BOOL WriteScreenBlock (HWND hWnd, LPSTR lpBlock, int nLength )
//
//  Description:
//     Writes block to TTY screen.  Nothing fancy - just
//     straight TTY.
//
//  Parameters:
//     HWND hWnd
//        handle to TTY window
//
//     LPSTR lpBlock
//        far pointer to block of data
//
//     int nLength
//        length of block
//
//
//---------------------------------------------------------------------------

static BOOL 
WriteScreenBlock (HWND hWnd, LPSTR lpBlock, int nLength )
{
   int i;
   WORD saved_mode_flags;
   SCREEN screen = (GETSCREEN (hWnd));
   struct screen_write_char_s state;

   if (NULL == screen)
      return (FALSE);

   INIT_SCREEN_WRITE_CHAR_STATE (state);
   saved_mode_flags = (screen->mode_flags & SCREEN_MODE_EAGER_UPDATE);
   screen->mode_flags &= (~ (SCREEN_MODE_EAGER_UPDATE));

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

#if 0
       case ASCII_LF:
	  Screen_CR (screen);
	  Screen_LF (screen);
	  break ;

       case ASCII_CR:
	  break;

#else /* not 0 */

       case ASCII_LF:
	  Screen_LF (screen);
	  break ;

       case ASCII_CR:
	  Screen_CR (screen);
	  break;
#endif /* 0 */

       case ASCII_FF:
	  Screen_Clear (screen, 0);
	  break;

       default:
#if 0
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
#else
	  Screen_WriteCharUninterpreted (screen, the_char, &state);
#endif
	  break;
	    
       case ASCII_ESC:
	{
	  char dispatch;

	  /* This emulates the subset of an ANSI terminal that
	     Edwin uses.  Temporary until we write a real screen
	     driver for Edwin.
	     This assumes the command is not split.
	   */
	  if ((i + 2) >= nLength)
	  {
	    MessageBeep (0);
	    break;
	  }

	  if (lpBlock[i+1] != '[')
	  {
	    i += 1;		/* 1 added in for loop */
	    MessageBeep (0);
	    break;
	  }

	  dispatch = (lpBlock[i + 2]);
	  switch (dispatch)
	  {
	    case 'K':
	      /* Clear Line */
	      clear_screen_rectangle (screen,
				      screen->row, screen->column,
				      (screen->row + 1), screen->width);
	      i += 2;		/* 1 added in for loop */
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
	      i += 2;		/* 1 added in for loop */
	      continue;

	    case 'H':
	      /* Cursor home */
	      relocate_cursor (screen, 0, 0);
	      i += 2;		/* 1 added in for loop */
	      continue;

	    case 'A':
	      /* Cursor up */
	      relocate_cursor (screen, (screen->row - 1), screen->column);
	      i += 2;		/* 1 added in for loop */
	      continue;

	    case 'C':
	      /* Cursor right */
	      relocate_cursor (screen, screen->row, (screen->column + 1));
	      i += 2;		/* 1 added in for loop */
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
	      i += 2;		/* 1 added in for loop */
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
	      i += 2;		/* 1 added in for loop */
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
#if 0
	      Screen_WriteCharUninterpreted (screen, ' ', &state);
#endif
	      i += 2;
	      continue;

	    default:
	      if ((dispatch >= '0') && (dispatch <= '9'))
	      {
		int j, x_value;

		x_value = (read_decimal (&lpBlock[0], (i + 2), nLength, &j));
		if (j >= nLength)
		{
		  MessageBeep (0);
		  i = (j - 1); /* 1 added in for loop */
		  continue;
		}
		else switch (lpBlock[j])
		{
		  case ';':
		  {
		    int k, y_value;

		    y_value = (read_decimal (&lpBlock[0], (j + 1),
					     nLength, &k));
		    if ((k < nLength) && (lpBlock[k] == 'H'))
		    {
		      /* Direct cursor motion */
		      relocate_cursor (screen, (x_value - 1), (y_value - 1));
		      i = k;	/* 1 added in for loop */
		      continue;
		    }
		    else
		    {
		      MessageBeep (0);
		      i = k; /* 1 added in for loop */
		      continue;
		    } 
		  }

		  case 'A':
		    /* Multi cursor up */
		    relocate_cursor (screen, (screen->row - x_value), screen->column);
		    i = j; /* 1 added in for loop */
		    continue;
		  
		  case 'C':
		    /* Multi cursor right */
		    relocate_cursor (screen, screen->row, (screen->column + x_value));
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
						     screen->column, (screen->width - x_value),
						     (screen->column + x_value));
#if 0
		    while (--x_value >= 0)
		      Screen_WriteCharUninterpreted (screen, ' ', &state);
#endif
		    i = j; /* 1 added in for loop */
		    continue;

		  case 'm':
		    if ((j == (i + 3)) && ((x_value == 0) || (x_value == 7)))
		    {
		      /* Enter stdout (7) or exit stdout (0) */
		      screen->write_attribute = (x_value == 7);
		      i = j;	/* 1 added in for loop */
		      continue;
		    }
		    /* fall through */

		  default:
		    MessageBeep (0);
		    i = j;	/* 1 added in for loop */
		    continue;
		}

	      }
	      break;
	  }
	}
      }
   }
   Finish_ScreenWriteChar (screen, &state);
   if (saved_mode_flags != 0)
   {
     UpdateWindow (screen->hWnd);
     MoveScreenCursor (screen);
     screen->mode_flags |= saved_mode_flags;
   }
   return (TRUE);
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
      {
	Screen_CR (screen);
	Screen_LF (screen);
      } else
      {
	char c = ((char) ch);
	//Screen_WriteCharUninterpreted (screen, ch, NULL);
	WriteScreenBlock (screen->hWnd, &c, 1);
      }
    }
  }
  return;
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
  return;
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
  return;
}

/* Line-buffered input. */

static int
ReadScreen_line_input (SCREEN screen, LPSTR buffer, int buflen)
{
    SCREEN_EVENT_LINK *current = screen->queue_head;
    SCREEN_EVENT_LINK *previous = 0;

    while (current)
    {
      if (current->event.type == SCREEN_EVENT_TYPE_KEY)
      {
	int  ch = current->event.event.key.ch;
	if ((current->event.event.key.control_key_state
	     & SCREEN_ANY_ALT_KEY_MASK)
	    != 0)
	  ch |= 0200;	    
	
	if (ch!=0)
	  buffered_key_command (screen, ch);
	  
        { // dequeue
	  SCREEN_EVENT_LINK  *next = current->next;
	  if (current == screen->queue_tail)
	    screen->queue_tail = previous;
	  if (previous)
	    previous->next = next;
	  else
	    screen->queue_head = next;
	  current->next = screen->free_events;
	  screen->free_events = current;
	  screen->n_events -= 1;
	  current = next;
	}
	  
	// If end of line then copy buffer and return
	if (ch == '\n' || ch == '\r')
	{
	  int  count = min (screen->n_chars, buflen);
	  int  i;
	  for (i = 0; i<count; i++)
	    buffer[i] = screen->line_buffer[i];
	  screen->n_chars = 0;
	  return  count;
	}

      }
      else /*not a key event*/
      {
	previous = current;
	current = current->next;	
      }	
    }
    //  We have copied all pending characters but there is no EOL yet
    return  -1;
}

/* Untranslated/unbuffered input */

static int
ReadScreen_raw (SCREEN screen, LPSTR buffer, int buflen)
{
    int  position = 0;
    SCREEN_EVENT_LINK *current = screen->queue_head;
    SCREEN_EVENT_LINK *previous = 0;

    while (current)
    {
      if (current->event.type == SCREEN_EVENT_TYPE_KEY)
      {
	int  ch = current->event.event.key.ch;
	if ((current->event.event.key.control_key_state
	     & SCREEN_ANY_ALT_KEY_MASK)
	    != 0)
	  ch |= 0200;	    
	
        // stash away the character
	if (position < buflen)
	  buffer[position++] = ch;
	if (screen->mode_flags & SCREEN_MODE_ECHO)
	{
	  char c = ((char) ch);
	  // Screen_WriteCharUninterpreted (screen, ch, NULL);
	  WriteScreenBlock (screen->hWnd, &c, 1);
	}
	
        { // dequeue
	  SCREEN_EVENT_LINK  *next = current->next;
	  if (current == screen->queue_tail)
	    screen->queue_tail = previous;
	  if (previous)
	    previous->next = next;
	  else
	    screen->queue_head = next;
	  current->next = screen->free_events;
	  screen->free_events = current;
	  screen->n_events -= 1;
	  current = next;
	}
	  
#if 0
	// If end of line or the buffer is full then  return
	if (ch == '\n' || ch == '\r' || position==buflen)
	  return  position;
#else
	// If end of line or the buffer is full then  return
	if (position == buflen)
	  return (position);
#endif

      }
      else /*not a key event*/
      {
	previous = current;
	current = current->next;	
      }	
    }
    //  We have copied all pending characters but there is no EOL yet
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

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
VOID
Screen_Clear (SCREEN screen, int kind)
{
    if (kind==0) {
      // clear whole screen
      ClearScreen_internal(screen);
      InvalidateRect (screen->hWnd, NULL, TRUE);
      return;
    }
    if (kind==1) {
      // clear to eol
      return;
    }
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
    return;
}

//---------------------------------------------------------------------------
//  VOID GoModalDialogBoxParam (HINSTANCE hInstance,
//                                   LPCSTR lpszTemplate, HWND hWnd,
//                                   DLGPROC lpDlgProc, LPARAM lParam )
//
//  Description:
//     It is a simple utility function that simply performs the
//     MPI and invokes the dialog box with a DWORD paramter.
//
//  Parameters:
//     similar to that of DialogBoxParam() with the exception
//     that the lpDlgProc is not a procedure instance
//
//---------------------------------------------------------------------------

static VOID
GoModalDialogBoxParam (HINSTANCE hInstance, LPCSTR lpszTemplate,
                                 HWND hWnd, DLGPROC lpDlgProc, LPARAM lParam )
{
   DLGPROC  lpProcInstance ;

   lpProcInstance = (DLGPROC) MakeProcInstance ((FARPROC) lpDlgProc,
                                                hInstance);
   DialogBoxParam (hInstance, lpszTemplate, hWnd, lpProcInstance, lParam);
   FreeProcInstance ((FARPROC) lpProcInstance);

}

//---------------------------------------------------------------------------
//  BOOL SettingsDlgInit (HWND hDlg )
//
//  Description:
//     Puts current settings into dialog box (via CheckRadioButton() etc.)
//
//  Parameters:
//     HWND hDlg
//        handle to dialog box
//
//  Win-32 Porting Issues:
//     - Constants require DWORD arrays for baud rate table, etc.
//     - There is no "MAXCOM" function in Win-32.  Number of COM ports
//       is assumed to be 4.
//
//---------------------------------------------------------------------------

static BOOL
SettingsDlgInit (HWND hDlg )
{
#if 0
   char       szBuffer[ MAXLEN_TEMPSTR ], szTemp[ MAXLEN_TEMPSTR ] ;
   NPTTYINFO  npTTYInfo ;
   WORD       wCount, wMaxCOM, wPosition ;

   if (NULL == (npTTYInfo = (screen) GET_PROP (hDlg, ATOM_TTYINFO )))
      return  FALSE;


   wMaxCOM = MAXPORTS ;

   // load the COM prefix from resources

   LoadString (GETHINST (hDlg ), IDS_COMPREFIX, szTemp, sizeof (szTemp ));

   // fill port combo box and make initial selection

   for (wCount = 0; wCount < wMaxCOM; wCount++)
   {
      wsprintf (szBuffer, "%s%d", (LPSTR) szTemp, wCount + 1);
      SendDlgItemMessage (hDlg, IDD_PORTCB, CB_ADDSTRING, 0,
                          (LPARAM) (LPSTR) szBuffer);
   }

   // disable COM port combo box if connection has already been
   // established (e.g. OpenComm() already successful)


   // other TTY settings

   CheckDlgButton (hDlg, IDD_AUTOWRAP, AUTOWRAP (screen));
   CheckDlgButton (hDlg, IDD_NEWLINE, NEWLINE (screen));
   CheckDlgButton (hDlg, IDD_LOCALECHO, LOCALECHO (screen));
#endif /* 0 */
   return (TRUE);
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
   cfTTYFont.Flags          = CF_SCREENFONTS | CF_FIXEDPITCHONLY |
                              CF_EFFECTS | CF_INITTOLOGFONTSTRUCT ;
   cfTTYFont.lCustData      = 0 ;
   cfTTYFont.lpfnHook       = NULL ;
   cfTTYFont.lpTemplateName = NULL ;
   cfTTYFont.hInstance      = GETHINST (owner);

   if (ChooseFont (&cfTTYFont ))
   {
     screen->rgbFGColour = cfTTYFont.rgbColors ;
     ResetScreen (screen);
   }
   return (TRUE);
}

//---------------------------------------------------------------------------
//  BOOL SettingsDlgTerm (HWND hDlg )
//
//  Description:
//     Puts dialog contents into TTY info structure.
//
//  Parameters:
//     HWND hDlg
//        handle to settings dialog
//
//---------------------------------------------------------------------------

static BOOL 
SettingsDlgTerm (HWND hDlg)
{
#if 0
   NPTTYINFO  npTTYInfo ;
   WORD       wSelection ;

   if (NULL == (npTTYInfo = (screen) GET_PROP (hDlg, ATOM_TTYINFO )))
      return  FALSE;

   // get other various settings

   AUTOWRAP (screen) = IsDlgButtonChecked (hDlg, IDD_AUTOWRAP);
   NEWLINE (screen) = IsDlgButtonChecked (hDlg, IDD_NEWLINE);
   LOCALECHO (screen) = IsDlgButtonChecked (hDlg, IDD_LOCALECHO);

   // control options
#endif /*0*/
   return  TRUE;
}

//---------------------------------------------------------------------------
//  BOOL FAR PASCAL SettingsDlgProc (HWND hDlg, UINT uMsg,
//                                   WPARAM wParam, LPARAM lParam )
//
//  Description:
//     This handles all of the user preference settings for
//     the TTY.
//
//  Parameters:
//     same as all dialog procedures
//
//  Win-32 Porting Issues:
//     - npTTYInfo is a DWORD in Win-32.
//
//  History:   Date       Author      Comment
//              5/10/91   BryanW      Wrote it.
//             10/20/91   BryanW      Now uses window properties to
//                                    store TTYInfo handle.  Also added
//                                    font selection.
//              6/15/92   BryanW      Ported to Win-32.
//
//---------------------------------------------------------------------------

BOOL FAR PASCAL
SettingsDlgProc (HWND hDlg, UINT uMsg,
		 WPARAM wParam, LPARAM lParam)
{
   switch (uMsg)
   {
      case WM_INITDIALOG:
      {
	 SCREEN  screen;

         // get & save pointer to TTY info structure

         screen = (SCREEN) lParam ;
         SET_PROP (hDlg, ATOM_TTYINFO, (HANDLE) screen);

         return  SettingsDlgInit (hDlg);
      }

      case WM_COMMAND:
         switch  (LOWORD(wParam))
         {
            case IDD_FONT:
	    {
	       SCREEN  screen = GET_PROP (hDlg, ATOM_TTYINFO);
               return  SelectScreenFont (screen, hDlg);
	    }

            case IDD_OK:
               // Copy stuff into structure
               SettingsDlgTerm (hDlg);
               EndDialog (hDlg, TRUE);
               return  TRUE;

            case IDD_CANCEL:
               // Just end
               EndDialog (hDlg, TRUE);
               return  TRUE;
         }
         break;

      case WM_DESTROY:
         REMOVE_PROP (hDlg, ATOM_TTYINFO);
         break ;
   }
   return (FALSE);
}

//---------------------------------------------------------------------------

#define KEYDATA_ALT_BIT 0x20000000

static int
GetControlKeyState(DWORD lKeyData)
{
  return
    (  (((GetKeyState (VK_RMENU)) < 0)    ?	SCREEN_RIGHT_ALT_PRESSED : 0)
     | (((GetKeyState (VK_LMENU)) < 0)    ?	SCREEN_LEFT_ALT_PRESSED : 0)
     | (((GetKeyState (VK_RCONTROL)) < 0) ?	SCREEN_RIGHT_CTRL_PRESSED : 0)
     | (((GetKeyState (VK_LCONTROL)) < 0) ?	SCREEN_LEFT_CTRL_PRESSED : 0)
     | (((GetKeyState (VK_SHIFT)) < 0)    ?	SCREEN_SHIFT_PRESSED : 0)
     | (((GetKeyState (VK_NUMLOCK)) & 1)  ?	SCREEN_NUMLOCK_ON : 0)
     | (((GetKeyState (VK_SCROLL)) & 1)   ?	SCREEN_SCROLLLOCK_ON : 0)
     | (((GetKeyState (VK_CAPITAL)) & 1)  ?	SCREEN_CAPSLOCK_ON : 0)
     | ((lKeyData & 0x01000000) 	  ?	SCREEN_ENHANCED_KEY : 0)
     | ((lKeyData & KEYDATA_ALT_BIT)      ?	SCREEN_ALT_KEY_PRESSED : 0));
}

static SCREEN_EVENT *
alloc_event (SCREEN screen,  SCREEN_EVENT_TYPE type)
{
    SCREEN_EVENT_LINK *link;
    if ((screen->mode_flags & type) == 0)
      return 0;

    if (screen->free_events==0) {
      MessageBeep (0xFFFFFFFFUL);
      Screen_WriteText (screen->hWnd, "[alloc_event=>0]");
      return  0;
    }
    
    link = screen->free_events;
    screen->free_events = screen->free_events->next;
    link->event.type = type;
    link->next = 0;
    if (screen->queue_head==0)
      screen->queue_head = screen->queue_tail = link;
    else
      screen->queue_tail = screen->queue_tail->next = link;
    screen->n_events += 1;

    SetDebuggingTitle (screen);
    
    return  &link->event;
}

BOOL
Screen_GetEvent (SCREEN screen, SCREEN_EVENT *event)
{
    SCREEN_EVENT_LINK *link;
    if (screen->n_events == 0)
      return  FALSE;
    screen->n_events -= 1;
    link = screen->queue_head;
    (*event) = link->event;
    screen->queue_head = link->next;
    link->next = screen->free_events;
    screen->free_events = link;
    return TRUE;
}

//---------------------------------------------------------------------------

VOID
Screen_SetAttribute (HANDLE screen, SCREEN_ATTRIBUTE sa)
{
  SendMessage (screen, SCREEN_SETATTRIBUTE, (WPARAM)sa, 0);
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
  SendMessage(screen, SCREEN_SETPOSITION, 0, MAKELPARAM(column,line));
}


VOID
Screen_SetMode (HANDLE screen, int mode)
{
  SendMessage (screen, SCREEN_SETMODES, (LPARAM)mode, 0);
}

int
Screen_GetMode (HANDLE screen)
{
  return (SendMessage (screen, SCREEN_GETMODES, 0, 0));
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

  return (result);
}

VOID
Screen_GetSize (HANDLE hWnd, int *rows, int *columns)
{
  SCREEN screen = (GETSCREEN (hWnd));
  if (screen == 0)
    return;
  *rows    = screen->height;
  *columns = screen->width;
  return;
}

/* Utilities for MIT character translation */

static BOOL _fastcall
MIT_post_char_message (CONST MSG * lpmsg, WPARAM the_char)
{
  return (PostMessage (lpmsg->hwnd,
		       ((lpmsg->message == WM_KEYDOWN)
			? WM_CHAR
			: WM_SYSCHAR),
		       the_char,
		       lpmsg->lParam));
}

#ifndef VK_A
#  define VK_A 'A'
#  define VK_Z 'Z'
#endif /* VK_A */

/* US IBM-PC keyboard */

#define VK_ATSIGN	'2'
#define VK_CARET	'6'
#define VK_LSQB		219
#define VK_RSQB		221
#define VK_BACKSLASH	220
#define VK_UNDERSCORE	189

#define ASCII_CONTROLIFY(ascii)	((ascii) - '@')

static BOOL _fastcall
MIT_controlify (WPARAM virtual_key, WPARAM * control_char)
{
  BOOL result = ((BOOL) 1);

  if ((virtual_key >= VK_A) && (virtual_key <= VK_Z))
    * control_char = (ASCII_CONTROLIFY ('A' + (virtual_key - VK_A)));
  else if (virtual_key == VK_ATSIGN)
    * control_char = (ASCII_CONTROLIFY ('@'));
  else if (virtual_key == VK_CARET)
    * control_char = (ASCII_CONTROLIFY ('^'));
  else if (virtual_key == VK_LSQB)
    * control_char = (ASCII_CONTROLIFY ('['));
  else if (virtual_key == VK_RSQB)
    * control_char = (ASCII_CONTROLIFY (']'));
  else if (virtual_key == VK_BACKSLASH)
    * control_char = (ASCII_CONTROLIFY ('\\'));
  else if (virtual_key == VK_UNDERSCORE)
    * control_char = (ASCII_CONTROLIFY ('_'));
  else
    result = ((BOOL) 0);

  return (result);
}

BOOL
MIT_TranslateMessage (CONST MSG * lpmsg)
{
  UINT message = (lpmsg->message);

  switch (message)
  {
    case WM_KEYDOWN:
    case WM_SYSKEYDOWN:
    {
      WPARAM virtual_key = (lpmsg->wParam);

      switch (virtual_key)
      {
        case VK_LEFT:
	  return (MIT_post_char_message (lpmsg, ((WPARAM) 'B'-64)));

        case VK_RIGHT:
	  return (MIT_post_char_message (lpmsg, ((WPARAM) 'F'-64)));

        case VK_UP:
	  return (MIT_post_char_message (lpmsg, ((WPARAM) 'P'-64)));

        case VK_DOWN:
	  return (MIT_post_char_message (lpmsg, ((WPARAM) 'N'-64)));

        case VK_HOME:
	  return (MIT_post_char_message (lpmsg, ((WPARAM) 'A'-64)));

        case VK_END:
	  return (MIT_post_char_message (lpmsg, ((WPARAM) 'E'-64)));

	case VK_BACK:
	  return (MIT_post_char_message (lpmsg, ((WPARAM) ASCII_DEL)));

	case VK_DELETE:
	  return (MIT_post_char_message (lpmsg, ((WPARAM) ASCII_BS)));
	  
	case VK_SPACE:
	  if (((GetKeyState (VK_RCONTROL)) < 0)
	      || ((GetKeyState (VK_LCONTROL)) < 0))
	    return (MIT_post_char_message (lpmsg, ((WPARAM) '\0')));
	  break;

#ifdef WINDOWSLOSES
	case VK_TAB:
	  if (MIT_trap_alt_tab)
	    return ((BOOL) 0);
	  break;

	case VK_ESCAPE:
	  if ((message == WM_SYSKEYDOWN) && MIT_trap_alt_escape)
	    return ((BOOL) 0);
	  break;
#endif /* WINDOWSLOSES */

	default:
	{
	  WPARAM control_char;

	  if (((message == WM_SYSKEYDOWN) || (lpmsg->lParam & KEYDATA_ALT_BIT))
	      && (((GetKeyState (VK_RCONTROL)) < 0)
		  || ((GetKeyState (VK_LCONTROL)) < 0))
	      && (MIT_controlify (virtual_key, &control_char)))
	    return (MIT_post_char_message (lpmsg, control_char));
	  break;
	}
      }
      break;
    }

#ifdef WINDOWSLOSES
    case WM_KEYUP:
    case WM_SYSKEYUP:
    {
      WPARAM virtual_key = (lpmsg->wParam);

      switch (virtual_key)
      {
	case VK_TAB:
	  if (MIT_trap_alt_tab)
	    return (MIT_post_char_message (lpmsg, ((WPARAM) '\t')));
	  break;

	case VK_ESCAPE:
	  if (MIT_trap_alt_escape)
	    return (MIT_post_char_message (lpmsg, ((WPARAM) ASCII_ESC)));
	  break;

	default:
	  break;
      }
      break;
    }
#endif /* WINDOWSLOSES */

    default:
      break;
  }
  return (TranslateMessage (lpmsg));
}

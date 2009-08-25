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

#ifndef SCM_NTSCREEN_H
#define SCM_NTSCREEN_H

#include <windows.h>
#include <commdlg.h>

typedef struct tagSCREENINFO *SCREEN;
typedef unsigned char SCREEN_ATTRIBUTE;

/* Events */

/* Mode flags: */

/* a) mask of interesting events */
#define	SCREEN_EVENT_TYPE_RESIZE	0x000001
#define	SCREEN_EVENT_TYPE_KEY		0x000002
#define	SCREEN_EVENT_TYPE_MOUSE		0x000004
#define	SCREEN_EVENT_TYPE_CLOSE		0x000008
#define	SCREEN_EVENT_TYPE_FOCUS		0x000010
#define	SCREEN_EVENT_TYPE_VISIBILITY	0x000020
#define SCREEN_EVENT_TYPE_MASK		0x00003F

/* b) flags for screen behaviour */
#define SCREEN_MODE_AUTOWRAP		0x001000
#define SCREEN_MODE_ECHO		0x002000
#define SCREEN_MODE_CR_NEWLINES		0x004000
#define SCREEN_MODE_LINE_INPUT		0x008000
#define SCREEN_MODE_PROCESS_OUTPUT	0x010000
#define SCREEN_MODE_EAGER_UPDATE	0x020000
#define SCREEN_MODE_EDWIN		0x040000
#define SCREEN_MODE_NEWLINE_CRS		0x080000
#define SCREEN_MODE_VK_KEYS		0x100000
#define SCREEN_MODE_MASK		0x1FF000

/* Kludge: */
#define SCREEN_EDWIN_RESIZE_COMMAND	0323		/* M-S */

typedef unsigned long SCREEN_EVENT_TYPE;

typedef struct
{
  unsigned int rows;
  unsigned int columns;
} SCREEN_RESIZE_EVENT_RECORD;

typedef struct
{
  unsigned int repeat_count;
  int virtual_keycode;
  unsigned int virtual_scancode;
  int ch;
  unsigned int control_key_state : 9;
  unsigned int key_down : 1;
} SCREEN_KEY_EVENT_RECORD;

typedef struct
{
  unsigned int row;
  unsigned int column;
  unsigned int control_key_state : 9;
  unsigned int button_state : 3;	/* the button being pressed */
  unsigned int up : 1;                  /* set for mouse *BUTTONUP messages */
  unsigned int mouse_moved : 1;		/* if neither then single click */
  unsigned int double_click : 1;
} SCREEN_MOUSE_EVENT_RECORD;

typedef struct
{
  unsigned int gained_p : 1;
} SCREEN_FOCUS_EVENT_RECORD;

typedef struct
{
  unsigned int show_p : 1;
} SCREEN_VISIBILITY_EVENT_RECORD;

typedef struct
{
  HWND handle;
  SCREEN_EVENT_TYPE type;
  union
    {
      SCREEN_KEY_EVENT_RECORD key;
      SCREEN_RESIZE_EVENT_RECORD resize;
      SCREEN_MOUSE_EVENT_RECORD mouse;
      SCREEN_FOCUS_EVENT_RECORD focus;
      SCREEN_VISIBILITY_EVENT_RECORD visibility;
    } event;
} SCREEN_EVENT;

/* control_key_state flags.  Only used for effective modifiers (i.e.
   not set when already incorporated into a character translation.  */

#define SCREEN_ALT_PRESSED            0x0001 /* An Alt key is pressed. */
#define SCREEN_CONTROL_PRESSED        0x0002 /* A Ctrl key is pressed. */
#define SCREEN_SHIFT_PRESSED          0x0004 /* A Shift key is pressed. */
#define SCREEN_CAPSLOCK_ON            0x0008
#define SCREEN_LEFT_CONTROL_PRESSED   0x0010
#define SCREEN_RIGHT_CONTROL_PRESSED  0x0020
#define SCREEN_LEFT_ALT_PRESSED       0x0040
#define SCREEN_RIGHT_ALT_PRESSED      0x0080
#define SCREEN_NUMLOCK_ON             0x0100
#define SCREEN_SCROLLLOCK_ON          0x0200
#define SCREEN_ANY_ALT_KEY_MASK	      SCREEN_ALT_PRESSED

/* button_state flags */
#define SCREEN_MOUSE_EVENT_LEFT_PRESSED   0x01
#define SCREEN_MOUSE_EVENT_RIGHT_PRESSED  0x02
#define SCREEN_MOUSE_EVENT_MIDDLE_PRESSED 0x04

/* Messages */

#ifndef SCREEN_COMMAND_FIRST
#define SCREEN_COMMAND_FIRST	(WM_USER + 10)
#endif

#define SCREEN_WRITE		(SCREEN_COMMAND_FIRST+0)
  /* text = (LPSTR)lParam */
  /* len  = (int)wParam */

#define SCREEN_SETPOSITION	(SCREEN_COMMAND_FIRST+1)
  /* column = LOWORD(lParam) */
  /* row    = HIWORD(lParam) */

#define SCREEN_GETPOSITION	(SCREEN_COMMAND_FIRST+2)
  /* return  column = LOWORD(retval) */
  /* return  row    = HIWORD(retval) */

#define SCREEN_SETATTRIBUTE	(SCREEN_COMMAND_FIRST+3)
  /* attribute = wParam */

#define SCREEN_GETATTRIBUTE	(SCREEN_COMMAND_FIRST+4)
  /* return  attribute = retval */

#define SCREEN_PEEKEVENT	(SCREEN_COMMAND_FIRST+5)
  /* count  = wParam */
  /* buffer = (SCREEN_EVENT*) lParam */
  /* returns #of events peeked */
  /* if buffer is NULL, can be used to count events pending */

#define SCREEN_READEVENT		(SCREEN_COMMAND_FIRST+6)
  /* count  = wParam */
  /* buffer = (SCREEN_EVENT*) lParam */
  /* returns #of events */
  /* if buffer is NULL, events are discarded */

#define SCREEN_SETMODES		(SCREEN_COMMAND_FIRST+7)
  /* modes = (WORD) wParam */

#define SCREEN_GETMODES		(SCREEN_COMMAND_FIRST+8)
  /* return  modes */

/* A window has commands, which may be bound to thunks.
   Control characters may be bound to commands.
   Thus commands may be invoked by keypress and by menu action.  */

typedef LRESULT (* COMMAND_HANDLER) (HWND, WORD);

#define SCREEN_SETCOMMAND	(SCREEN_COMMAND_FIRST+9)
  /* command = wParam */
  /* handler = COMMAND_HANDLER = lParam;  NULL=disable */
  /* returns old handler, or -1 on space error */

#define SCREEN_GETCOMMAND	(SCREEN_COMMAND_FIRST+10)
  /* command = wParam */
  /* return  handler for char */

#define SCREEN_SETBINDING	(SCREEN_COMMAND_FIRST+11)
  /* char = wParam */
  /* command = lParam; */

#define SCREEN_GETBINDING	(SCREEN_COMMAND_FIRST+12)
  /* char = wParam */
  /* return command */

#define SCREEN_SETMENU		(SCREEN_COMMAND_FIRST+13)
  /* hMenu = (HMENU)lParam */

#define SCREEN_READ		(SCREEN_COMMAND_FIRST+14)
  /* buffer = (LPSTR) lParam */
  /* length = wParam */
  /* return  characters read */
  /* (-1) if read would block in line-mode */

#define SCREEN_CLEAR		(SCREEN_COMMAND_FIRST+15)
  /* kind = wParam */
  /* kind=0  : whole screen */
  /* kind=1  : to eol */

/* Predefined commands for SCREEN_SETBINDING etc */

#define SCREEN_COMMAND_CHOOSEFONT	0x400
#define SCREEN_COMMAND_CLOSE		0x401
#define SCREEN_COMMAND_CHOOSEBACKCOLOR	0x402

struct screen_write_char_s
{
  RECT rect;
  unsigned int row;
  unsigned int col;
};

/* Do user-level timer interrupts by using WM_TIMER.  */
#define USE_WM_TIMER

extern FILE * win32_trace_file;
extern unsigned long win32_trace_level;
#ifndef WIN32_TRACE_FILENAME
#define WIN32_TRACE_FILENAME "w32trace.out"
#endif

#ifdef __WATCOMC__
#define _fastcall
#endif

extern VOID _fastcall clear_screen_rectangle (SCREEN, int, int, int, int);
extern VOID Screen_CR_to_RECT (RECT *, SCREEN, int, int, int, int);
extern VOID _fastcall scroll_screen_vertically
  (SCREEN, int, int, int, int, int);
extern VOID _fastcall Screen_WriteCharUninterpreted
  (SCREEN, int, struct screen_write_char_s *);
extern VOID _fastcall Screen_SetAttributeDirect (SCREEN, SCREEN_ATTRIBUTE);
extern VOID WriteScreenBlock_NoInvalidRect (SCREEN, int, int, LPSTR, int);
extern void Enable_Cursor (SCREEN, BOOL);
extern HICON ScreenSetIcon (SCREEN, HICON);
extern BOOL ScreenSetFont (SCREEN, char *);
extern BOOL ScreenSetForegroundColour (SCREEN, DWORD);
extern BOOL ScreenSetBackgroundColour (SCREEN, DWORD);
extern BOOL ScreenSetFont (SCREEN, char *);
extern BOOL ScreenSetDefaultFont (char *);

extern BOOL Screen_InitApplication (HANDLE hInstance);
extern BOOL Screen_InitInstance (HANDLE hInstance, int nCmdShow);

extern HANDLE Screen_Create (HANDLE, LPCSTR, int);
extern VOID Screen_Destroy (BOOL, HANDLE);
extern HWND ScreenCurrentFocus (void);
extern BOOL Screen_SetPosition (SCREEN, int, int);

extern void Screen_SetAttribute (HANDLE, SCREEN_ATTRIBUTE);
extern void Screen_WriteChar (HANDLE, char);
extern void Screen_WriteText (HANDLE, char*);
extern int  Screen_Read (HANDLE, BOOL, char *, int);
extern void Screen_SetCursorPosition (HANDLE, int line, int column);
extern void Screen_SetMenu (SCREEN, HMENU);
extern void Screen_SetMode (HANDLE, int);
extern int  Screen_GetMode (HANDLE);
extern VOID Screen_GetSize (HWND, int *rows, int *columns);
extern void screen_char_dimensions (HWND, int *, int *);

/* The following return zero iff no events */
extern int Screen_read_event (SCREEN_EVENT *);
extern int Screen_pending_events_p (void);

#endif /* SCM_NTSCREEN_H */

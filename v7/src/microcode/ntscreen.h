/* -*-C-*-

$Id: ntscreen.h,v 1.2 1993/06/24 02:07:42 gjr Exp $

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

#ifndef NTSCREEN_H
#define NTSCREEN_H

//---------------------------------------------------------------------------
//
//  Module: screen.h
//
//  Purpose:
//     This is the header file for the screen class
//
//---------------------------------------------------------------------------


//#undef NO_STRICT    // be bold!


//#ifndef WIN32
//#define WIN31        // this is a Windows 3.1 application
//#endif

#include <windows.h>
#include <commdlg.h>


// screen.rh
//  #defines for resources used in screen class
#define IDD_OK		IDOK
#define IDD_CANCEL	IDCANCEL
#define IDD_FONT	0x500
// end of screen.rh 


typedef struct tagSCREENINFO *SCREEN;
typedef unsigned char SCREEN_ATTRIBUTE;

// Mode flags:
//  a) mask of interesting events
#define	SCREEN_EVENT_TYPE_RESIZE	0x0001
#define	SCREEN_EVENT_TYPE_KEY		0x0002
#define	SCREEN_EVENT_TYPE_MOUSE		0x0004
#define SCREEN_EVENT_TYPE_ALL		0x0007
//  b) flags for screen behaviour
#define SCREEN_MODE_AUTOWRAP		0x0010
#define SCREEN_MODE_ECHO		0x0020
#define SCREEN_MODE_NEWLINE		0x0040
#define SCREEN_MODE_LINE_INPUT		0x0080
#define SCREEN_MODE_PROCESS_OUTPUT	0x0100
#define SCREEN_MODE_LAZY_UPDATE		0x0200

typedef WORD SCREEN_EVENT_TYPE;

typedef struct {
  int	repeat_count;
  int	virtual_keycode;
  int	virtual_scancode;
  int	control_key_state;
  char	ch;
  int	key_down : 1;
} SCREEN_KEY_EVENT_RECORD;

// control_key_state flags

#define SCREEN_RIGHT_ALT_PRESSED     0x0001 // the right alt key is pressed.
#define SCREEN_LEFT_ALT_PRESSED      0x0002 // the left alt key is pressed.
#define SCREEN_RIGHT_CTRL_PRESSED    0x0004 // the right ctrl key is pressed.
#define SCREEN_LEFT_CTRL_PRESSED     0x0008 // the left ctrl key is pressed.
#define SCREEN_SHIFT_PRESSED         0x0010 // the shift key is pressed.
#define SCREEN_NUMLOCK_ON            0x0020 // the numlock light is on.
#define SCREEN_SCROLLLOCK_ON         0x0040 // the scrolllock light is on.
#define SCREEN_CAPSLOCK_ON           0x0080 // the capslock light is on.
#define SCREEN_ENHANCED_KEY          0x0100 // the key is enhanced.

typedef struct {
  int	rows;
  int	columns;
} SCREEN_RESIZE_EVENT_RECORD;

typedef struct {
  int	row;
  int	column;
  int	control_key_state;
  int	button_state;
  int	mouse_moved  : 1;	// if neither then single click
  int	double_click : 1;
} SCREEN_MOUSE_EVENT_RECORD;

// button state flags
#define  SCREEN_MOUSE_EVENT_LEFT_PRESSED	0x01
#define  SCREEN_MOUSE_EVENT_RIGHT_PRESSED	0x02
#define  SCREEN_MOUSE_EVENT_MIDDLE_PRESSED	0x04

typedef struct {
  SCREEN_EVENT_TYPE  type;
  union {
    SCREEN_KEY_EVENT_RECORD	key;
    SCREEN_RESIZE_EVENT_RECORD	resize;
    SCREEN_MOUSE_EVENT_RECORD	mouse;
  } event;
} SCREEN_EVENT;

BOOL Screen_InitApplication (HANDLE hInstance);
BOOL Screen_InitInstance (HANDLE hInstance, int nCmdShow);


HANDLE  Screen_Create (HANDLE hParent, LPCSTR title, int nCmdShow);

void  Screen_Destroy (HANDLE);
void  Screen_SetAttribute (HANDLE, SCREEN_ATTRIBUTE);
void  Screen_WriteChar (HANDLE, char);
void  Screen_WriteText (HANDLE, char*);
int   Screen_Read (HANDLE, char*, int);
void  Screen_SetCursorPosition (HANDLE, int line, int column);
void  Screen_SetMenu (HANDLE, HMENU);
void  Screen_SetMode (HANDLE, int);
int   Screen_GetMode (HANDLE);
void  Screen_GetSize (HANDLE, int *rows, int *columns);

BOOL  Screen_GetEvent (HANDLE, SCREEN_EVENT*);	// return false on no events


//---------------------------------------------------------------------------
//  Messages
//---------------------------------------------------------------------------

#ifndef SCREEN_COMMAND_FIRST
#define SCREEN_COMMAND_FIRST	WM_USER
#endif

#define SCREEN_WRITE		(SCREEN_COMMAND_FIRST+0)
  // text = (LPSTR)lParam
  // len  = (int)wParam

#define SCREEN_SETPOSITION	(SCREEN_COMMAND_FIRST+1)
  // column = LOWORD(lParam)
  // row    = HIWORD(lParam)

#define SCREEN_GETPOSITION	(SCREEN_COMMAND_FIRST+2)
  // return  column = LOWORD(retval)
  // return  row    = HIWORD(retval)

#define SCREEN_SETATTRIBUTE	(SCREEN_COMMAND_FIRST+3)
  // attribute = wParam
    
#define SCREEN_GETATTRIBUTE	(SCREEN_COMMAND_FIRST+4)
  // return  attribute = retval
    
#define SCREEN_PEEKEVENT	(SCREEN_COMMAND_FIRST+5)
  // count  = wParam
  // buffer = (SCREEN_EVENT*) lParam
  // returns #of events peeked
  // if buffer is NULL, can be used to count events pending

#define SCREEN_READEVENT		(SCREEN_COMMAND_FIRST+6)
  // count  = wParam
  // buffer = (SCREEN_EVENT*) lParam
  // returns #of events
  // if buffer is NULL, events are discarded

#define SCREEN_SETMODES		(SCREEN_COMMAND_FIRST+7)
  // modes = (WORD) wParam

#define SCREEN_GETMODES		(SCREEN_COMMAND_FIRST+8)
  // return  modes

    
// A window has commands, which may be bound to thunks.
// Control characters may be bound to commands
// Thus commands may be invoked by keypress and by menu action

typedef LRESULT (*COMMAND_HANDLER)(HWND,WORD command);

#define SCREEN_SETCOMMAND	(SCREEN_COMMAND_FIRST+9)
  // command = wParam
  // handler = COMMAND_HANDLER = lParam;  NULL=disable
  // returns old handler, or -1 on space error

#define SCREEN_GETCOMMAND	(SCREEN_COMMAND_FIRST+10)
  // command = wParam
  // return  handler for char

#define SCREEN_SETBINDING	(SCREEN_COMMAND_FIRST+11)
  // char = wParam
  // command = lParam;

#define SCREEN_GETBINDING	(SCREEN_COMMAND_FIRST+12)
  // char = wParam
  // return command

#define SCREEN_SETMENU		(SCREEN_COMMAND_FIRST+13)
  // hMenu = (HMENU)lParam

#define SCREEN_READ		(SCREEN_COMMAND_FIRST+14)
  // buffer = (LPSTR) lParam
  // length = wParam
  // return  characters read
  //   (-1) if read would block in line-mode

#define SCREEN_CLEAR		(SCREEN_COMMAND_FIRST+15)
  // kind = wParam
  //    kind=0  : whole screen
  //    kind=1  : to eol
    
//---------------------------------------------------------------------------
//  Predefined commands for SCREEN_SETBINDING etc
//---------------------------------------------------------------------------

#define SCREEN_COMMAND_CHOOSEFONT	0x400
#define SCREEN_COMMAND_CLOSE		0x401

//---------------------------------------------------------------------------
//  End of File: screen.h
//---------------------------------------------------------------------------

#endif /* NTSCREEN_H */

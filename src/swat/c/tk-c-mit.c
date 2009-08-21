/* -*- C -*-
/* Uses tk-c.c - Support routines for Tk Widgets called from Scheme */

#include "scheme.h"
#include "prims.h"
#include "ansidecl.h"
#include "X11/Xlib.h"
#include "tk.h"
#include "tkInt.h"		/* For TkWindow */

DEFINE_PRIMITIVE ("%tclGlobalEval", Prim_tcl_eval, 2, 2, 0)
{ /* (%tclGlobalEval TK-main-window string) */
  Tcl_Interp *tclInterp;

  PRIMITIVE_HEADER(2);
  tclInterp = (((TkWindow *) arg_integer(1))->mainPtr)->interp;
  if (Tcl_GlobalEval(tclInterp, STRING_ARG(2)) != TCL_OK)
  { fprintf(stderr, "%tclGlobalEval: error '%s'\n",
	    tclInterp->result);
    error_external_return();
  }
  PRIMITIVE_RETURN (char_pointer_to_string
		    ((unsigned char *) tclInterp->result));
}

long TKEvent = true;
DEFINE_PRIMITIVE ("%tkCompletelyHandlesEvent?",
		  Prim_tk_completely_handles_event, 1, 1, 0)
{ /* (%tkCompletelyHandlesEvent? event) */
  XEvent *Event;

  PRIMITIVE_HEADER (1);

  /*  We return 0 if there is a bad argument rather than generating  */
  /* and error.  This avoids the need to put a                       */
  /*  dynamic wind around calls to this primitive.                   */
  /*  Error checking is                                              */
  /*  done at the next level up, in tk-completely-handles-event?     */

  if (!STRING_P(ARG_REF(1))) PRIMITIVE_RETURN(LONG_TO_UNSIGNED_FIXNUM(0));
  if (STRING_LENGTH(ARG_REF(1)) < sizeof(XEvent)) 
           PRIMITIVE_RETURN(LONG_TO_UNSIGNED_FIXNUM(0));


  Event = (XEvent *) STRING_ARG(1);
  TKEvent = true;
  Tk_HandleEvent(Event);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT(TKEvent));
}

void OurTopLevelGeometryProc(ClientData CallBackHash, XEvent *Event)
{ /* Based on the code for PackStructureProc in tkPack.c.  That code */
  /* handles four kinds of events: ConfigureNotify, DestroyNotify,   */
  /* MapNotify, and UnmapNotify.  Here, we consider only the         */
  /* ConfigureNotify case and reflect it back into Scheme.           */

  if (Event->type == ConfigureNotify)
  { 
#include <string.h>
    extern void
      AddSchemeCallBack(int argc, char **argv, long *countv);
    char *argv[2], CallBackNumber[50],
         EventChars[1+sizeof(XConfigureEvent)];
    long Counts[2];

    XConfigureEvent *E = (XConfigureEvent *) Event;
    Counts[0] = sprintf(CallBackNumber, "%d", (long) CallBackHash);
    argv[0] = CallBackNumber;
    Counts[1] = sizeof(XConfigureEvent);
    argv[1] = (char *) E;
    AddSchemeCallBack(2, argv, Counts);
  }
}

DEFINE_PRIMITIVE ("%tkCreateTopLevelWindow", Prim_tk_create_tl_window,
		  3, 3, 0)
{ /* (%tkCreateTopLevelWindow MainWindow Name CallBackHash) */
  Tk_Window Result;
  Tcl_Interp *tclInterp;
  
  PRIMITIVE_HEADER (3);
  tclInterp = (((TkWindow *) arg_integer(1))->mainPtr)->interp;
  Result =
    Tk_CreateWindow(tclInterp, (Tk_Window) arg_integer(1),
		    STRING_ARG(2), "");
  if (Result == NULL)
  { fprintf(stderr, "%tkCreateTopLevelWindow: error '%s'\n",
	    tclInterp->result);
    error_external_return();
  }
  Tk_SetWindowBackground(Result,
			 BlackPixelOfScreen(Tk_Screen(Result)));
  Tk_CreateEventHandler(Result,
			StructureNotifyMask,
			OurTopLevelGeometryProc,
			(ClientData) arg_integer(3));
  PRIMITIVE_RETURN (long_to_integer((long) Result));
}

char *TK_CallBack_List;
long NChars_In_TK_Callbacks = 0;

DEFINE_PRIMITIVE ("%tkDoEvents", Prim_tk_do_events, 0, 0, 0)
{ /* (%tkDoEvents) */
  extern void DoTkEvents ();
  PRIMITIVE_HEADER (0);
  DoTkEvents();
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%tkDrainCallBacks", Prim_tk_drain, 2, 2, 0)
{ /* (%tkDrainCallBacks nchar string)                                */
  /* Returns the number of characters available in the call back     */
  /* string if there is NOT enough room in the string to hold all of */
  /* the characters.  Otherwise, the characters are written into     */
  /* STRING, C variable is cleared, space freed and the primitive    */
  /* returns #F.                                                     */

  long NCharsInString;
  unsigned char *StringSpace;

  PRIMITIVE_HEADER (2);
  NCharsInString = arg_integer(1);
  StringSpace = (unsigned char *) STRING_ARG(2);
  if ((NChars_In_TK_Callbacks != 0) &&
      (NCharsInString >= NChars_In_TK_Callbacks))
  { fast unsigned char * scan_result = StringSpace;
    fast unsigned char * end_result = (scan_result + NChars_In_TK_Callbacks);
    fast unsigned char * data = (unsigned char *) TK_CallBack_List;
    while (scan_result < end_result)
      (*scan_result++) = (*data++);
    SET_STRING_LENGTH (ARG_REF(2), NChars_In_TK_Callbacks);
    /* free(TK_CallBack_List); */
    /* TK_CallBack_List = NULL; */
    NChars_In_TK_Callbacks = 0;
    PRIMITIVE_RETURN (SHARP_F);
  }
  else
  { PRIMITIVE_RETURN(long_to_integer(NChars_In_TK_Callbacks));
  }
}

void OurEventHandler(ClientData ignored_data, XEvent *ignored_event)
{ TKEvent = false;
}

DEFINE_PRIMITIVE ("%tkGenerateSchemeEvent",
		  Prim_tk_generate_scheme_event, 2, 2, 0)
{ /* (%tkGenerateSchemeEvent mask TkWindow) */
  PRIMITIVE_HEADER (2);
  if (arg_integer(1) == 0)
    Tk_DeleteEventHandler((Tk_Window) arg_integer(2),
			  arg_integer(1),
			  OurEventHandler,
			  (ClientData) 0);
  else Tk_CreateEventHandler((Tk_Window) arg_integer(2),
			     arg_integer(1),
			     OurEventHandler,
			     (ClientData) 0);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%tkInit", Prim_tk_init, 2, 2, 0)
{ /* (%tkInit display name) */
  extern long /*Tk_Window*/ InitTkApplication (long /*Display*/ *display,
					       char *Name);
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN((long_to_integer
		    ((long) InitTkApplication
		     ((long /*Display*/ *) arg_integer(1),
		      STRING_ARG(2)))));
}

typedef int (*cmdProc) (ClientData clientData, Tcl_Interp *interp,
			int argc, char **argv);

#define NTKCommands 14
cmdProc TkCommandTable[] =
{ Tk_AfterCmd, Tk_BindCmd, Tk_DestroyCmd, Tk_FocusCmd, Tk_GrabCmd,
  Tk_OptionCmd, Tk_PackCmd, Tk_PlaceCmd, Tk_SelectionCmd,
  Tk_TkCmd, Tk_TkwaitCmd, Tk_UpdateCmd, Tk_WinfoCmd, Tk_WmCmd
};

DEFINE_PRIMITIVE ("%tkInvokeCommand", Prim_tk_invoke, 2, LEXPR, 0)
{ /* (%tkInvokeCommand commandnumber tkmainwindow . argstrings) */
#include "tkInt.h"
  long WhichCommand, NArgsToPass, i, Result;
  char **Argv;
  SCHEME_OBJECT SchemeResult;
  Tcl_Interp *tclInterp;

  PRIMITIVE_HEADER(LEXPR);
  WhichCommand = arg_integer(1);
  tclInterp = (((TkWindow *) arg_integer(2))->mainPtr)->interp;
  if (WhichCommand > NTKCommands) error_bad_range_arg(1);
  NArgsToPass = LEXPR_N_ARGUMENTS() - 1;
  Argv = (char **) malloc((sizeof (char *)) * NArgsToPass);
  Argv[0] = "<InvokedFromScheme>";
  for (i=1; i < NArgsToPass; i++) Argv[i] = STRING_ARG(i+2);
  Result = (TkCommandTable[WhichCommand])((ClientData) arg_integer(2),
					  tclInterp,
					  NArgsToPass,
					  Argv);
  free(Argv);
  if (Result != TCL_OK)
  { fprintf(stderr, "tkInvokeCommand error: %s\n", tclInterp->result);
    error_external_return();
  }
  
  SchemeResult = (char_pointer_to_string
		  ((unsigned char *) tclInterp->result));
  Tcl_ResetResult(tclInterp);
  PRIMITIVE_RETURN(SchemeResult);
}

DEFINE_PRIMITIVE ("%tkKillApplication", Prim_tk_kill_app, 1, 1, 0)
{ /* (%tkKillApplication TKMainWindow) */
  Tk_Window TKWin;
  Tcl_Interp *Interp;
  
  PRIMITIVE_HEADER (1);
  TKWin = (Tk_Window) arg_integer(1);
  Interp = (((TkWindow *) TKWin)->mainPtr)->interp;
  Tk_DestroyWindow(TKWin);
  Tcl_DeleteInterp(Interp);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

void Our_Geometry_Manager(ClientData clientData, Tk_Window tkwin)
{ extern void AddSchemeCallBack(int argc, char **argv, long *countv);
  char *argv[1], CallBackNumber[50];
  long counts[1];

  counts[0] = sprintf(CallBackNumber, "%d", (long) clientData);
  argv[0] = CallBackNumber;
  AddSchemeCallBack(1, argv, counts);
}

DEFINE_PRIMITIVE ("%tkManageGeometry", Prim_tk_manage_geom, 2, 2, 0)
{ /* (%tkManageGeometry tkwin object-hash) */
  PRIMITIVE_HEADER (2);
  if (ARG_REF(2) == SHARP_F)
    Tk_ManageGeometry((Tk_Window) arg_integer(1), NULL, 0);
  else Tk_ManageGeometry((Tk_Window) arg_integer(1),
			 Our_Geometry_Manager,
			 (ClientData) arg_integer(2));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%tkMapWidget", Prim_tk_map_widget, 6, 6, 0)
{ extern char * tk_map_widget (long /*Button*/ *button,
			       long /*Tk_Window*/ tkMainWindow,
			       char *name,
			       long /*Window*/ xwindow,
			       int x, int y);
  PRIMITIVE_HEADER(6);
  PRIMITIVE_RETURN(char_pointer_to_string
		   ((unsigned char *)
		    tk_map_widget((long /*Button*/ *) arg_integer(1),
				  (long /*Tk_Window*/) arg_integer(2),
				  STRING_ARG(3),
				  (long /*Window*/) arg_integer(4),
				  arg_integer(5),
				  arg_integer(6))));

}

DEFINE_PRIMITIVE ("%tkMapWindow", Prim_tk_map_window, 1, 1, 0)
{ /* (%tkMapWindow TkWindow) returns X Window ID */
  Tk_Window tkwin;

  PRIMITIVE_HEADER(1);
  tkwin = (Tk_Window) arg_integer(1);
  Tk_MapWindow(tkwin);
  PRIMITIVE_RETURN(long_to_integer((long) Tk_WindowId(tkwin)));
}

DEFINE_PRIMITIVE ("%tkMoveWindow", Prim_tk_move, 3, 3, 0)
{ /* (%tkMoveWindow tkwin x y) */
  PRIMITIVE_HEADER (3);
  Tk_MoveWindow((Tk_Window) arg_integer(1),
		(int) arg_integer(2),
		(int) arg_integer(3));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%tkMoveResizeWindow", Prim_tk_move_resize, 5, 5, 0)
{ /* (%tkMoveResizeWindow tkwin x y width height) */
  PRIMITIVE_HEADER (5);
  Tk_MoveResizeWindow((Tk_Window) arg_integer(1),
		      (int) arg_integer(2), (int) arg_integer(3),
		      (unsigned int) arg_integer(4),
		      (unsigned int) arg_integer(5));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%tkNextWakeup", Prim_tk_next_wakeup, 0, 0, 0)
{ /* (%tkNextWakeup) */
  /* If the call back list isn't empty, wake up right away. */
  extern long tk_GetIntervalToNextEvent();
  long Result =
    (NChars_In_TK_Callbacks != 0) ? 0 : tk_GetIntervalToNextEvent();
  
  if (Result == -1)
    PRIMITIVE_RETURN(SHARP_F);
  else PRIMITIVE_RETURN(long_to_integer(Result));
}

DEFINE_PRIMITIVE ("%tkResizeWindow", Prim_tk_resize, 3, 3, 0)
{ /* (%tkResizeWindow tkwin width height) */
  PRIMITIVE_HEADER (3);
  Tk_ResizeWindow((Tk_Window) arg_integer(1),
		(int) arg_integer(2),
		(int) arg_integer(3));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%tkUnmapWindow", Prim_tk_unmap_window, 1, 1, 0)
{ /* (%tkUnmapWindow tk-win) */
  PRIMITIVE_HEADER (1);
  Tk_UnmapWindow((Tk_Window) arg_integer(1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%tkWinReqHeight", Prim_tk_win_req_height, 1, 1, 0)
{ /* (%tkwinReqHeight tk-win) */
  PRIMITIVE_HEADER(1);
  PRIMITIVE_RETURN (long_to_integer
		    ((long) Tk_ReqHeight (arg_integer(1))));
}

DEFINE_PRIMITIVE ("%tkWinReqWidth", Prim_tk_win_req_width, 1, 1, 0)
{ /* (%tkwinReqWidth tk-win) */
  PRIMITIVE_HEADER(1);
  PRIMITIVE_RETURN (long_to_integer
		    ((long) Tk_ReqWidth (arg_integer(1))));
}

DEFINE_PRIMITIVE ("%tkWidget.tkwin", Prim_tk_widget_get_tkwin, 1, 1, 0)
{ extern long /*Tk_Window*/ tk_tkwin_widget (long /*button*/ *button);
  PRIMITIVE_HEADER(1);
  PRIMITIVE_RETURN(long_to_integer
		   ((long) tk_tkwin_widget
		    ((long /*Button*/ *) arg_integer(1))));
}

DEFINE_PRIMITIVE ("%tkWinDisplay", Prim_tk_win_display, 1, 1, 0)
{ /* (%tkwinDisplay tk-win) */
  PRIMITIVE_HEADER(1);
  PRIMITIVE_RETURN (long_to_integer
		    ((long) Tk_Display ((Tk_Window) arg_integer(1))));
}

DEFINE_PRIMITIVE ("%tkWinIsMapped?", Prim_tk_win_is_mapped, 1, 1, 0)
{ /* (%tkwinismapped? tk-win) */
  PRIMITIVE_HEADER(1);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT
		    (Tk_IsMapped ((Tk_Window) arg_integer(1))));
}

DEFINE_PRIMITIVE ("%tkWinHeight", Prim_tk_win_height, 1, 1, 0)
{ /* (%tkwinHeight tk-win) */
  PRIMITIVE_HEADER(1);
  PRIMITIVE_RETURN (long_to_integer
		    ((long) Tk_Height ((Tk_Window) arg_integer(1))));
}

DEFINE_PRIMITIVE ("%tkWinWidth", Prim_tk_win_width, 1, 1, 0)
{ /* (%tkwinWidth tk-win) */
  PRIMITIVE_HEADER(1);
  PRIMITIVE_RETURN (long_to_integer
		    ((long) Tk_Width ((Tk_Window) arg_integer(1))));
}

DEFINE_PRIMITIVE ("%tkWinWindow", Prim_tk_win_window, 1, 1, 0)
{ /* (%tkwinWindow tk-win) */
  PRIMITIVE_HEADER(1);
  PRIMITIVE_RETURN (long_to_integer
		    ((long) Tk_WindowId ((Tk_Window) arg_integer(1))));
}

DEFINE_PRIMITIVE ("%tkWinX", Prim_tk_win_x, 1, 1, 0)
{ /* (%tkwinx tk-win) */
  PRIMITIVE_HEADER(1);
  PRIMITIVE_RETURN (long_to_integer
		    ((long) Tk_X ((Tk_Window) arg_integer(1))));
}

DEFINE_PRIMITIVE ("%tkWinY", Prim_tk_win_y, 1, 1, 0)
{ /* (%tkwiny tk-win) */
  PRIMITIVE_HEADER(1);
  PRIMITIVE_RETURN (long_to_integer
		    ((long) Tk_Y ((Tk_Window) arg_integer(1))));
}

DEFINE_PRIMITIVE ("%tkWinName", Prim_tk_win_name, 1, 1, 0)
{ /* (%tkwinname tk-win) */
  PRIMITIVE_HEADER(1);
  PRIMITIVE_RETURN (char_pointer_to_string
		    ((unsigned char *) Tk_Name ((Tk_Window) arg_integer(1))));
}

DEFINE_PRIMITIVE ("%tkWinPathName", Prim_tk_win_pathname, 1, 1, 0)
{ /* (%tkwinpathname tk-win) */
  PRIMITIVE_HEADER(1);
  PRIMITIVE_RETURN (char_pointer_to_string
		    ((unsigned char *) Tk_PathName ((Tk_Window) arg_integer(1))));
}


#include <stdio.h>
#include "tk.h"
#include "default.h"
#include "tkInt.h"

typedef struct
{ Tk_Window tkwin;
  Display *display;
  Tcl_Interp *interp;
} All_Widgets;


int *
MakeButton(Tk_Window tkMainWindow, char *name)
{ char      *argv [2];
  Tcl_Interp *tclInterp;

  argv [0] = "button";
  argv [1] = name;
  tclInterp = (((TkWindow *) arg_integer(1))->mainPtr)->interp;
  if (Tk_ButtonCmd ((ClientData) tkMainWindow,
		    tclInterp, 2, argv) != TCL_OK)
  { fprintf (stderr, tclInterp->result);
    exit (1);
  }
  return (int *) GetCmdClientData (tclInterp, name);
}

int *
MakeCanvas(Tk_Window tkMainWindow, char *name)
{ char      *argv [2];
  Tcl_Interp *tclInterp;

  argv [0] = "canvas";
  argv [1] = name;
  tclInterp = (((TkWindow *) arg_integer(1))->mainPtr)->interp;
  if (Tk_CanvasCmd ((ClientData) tkMainWindow,
		    tclInterp, 2, argv) != TCL_OK)
  { fprintf (stderr, tclInterp->result);
    exit (1);
  }
  return (int *) GetCmdClientData (tclInterp, name);
}

int *
MakeCheckButton(Tk_Window tkMainWindow, char *name)
{ char      *argv [2];
  Tcl_Interp *tclInterp;

  argv [0] = "checkbutton";
  argv [1] = name;
  tclInterp = (((TkWindow *) arg_integer(1))->mainPtr)->interp;
  if (Tk_ButtonCmd ((ClientData) tkMainWindow,
		    tclInterp, 2, argv) != TCL_OK)
  { fprintf (stderr, tclInterp->result);
    exit (1);
  }
  return (int *) GetCmdClientData (tclInterp, name);
}

int *
MakeEntry(Tk_Window tkMainWindow, char *name)
{ char      *argv [2];
  Tcl_Interp *tclInterp;

  argv [0] = "entry";
  argv [1] = name;
  tclInterp = (((TkWindow *) arg_integer(1))->mainPtr)->interp;
  if (Tk_EntryCmd ((ClientData) tkMainWindow,
		   tclInterp, 2, argv) != TCL_OK)
  { fprintf (stderr, tclInterp->result);
    exit (1);
  }
  return (int *) GetCmdClientData (tclInterp, name);
}

int *
MakeLabel(Tk_Window tkMainWindow, char *name)
{ char      *argv [2];
  Tcl_Interp *tclInterp;

  argv [0] = "label";
  argv [1] = name;
  tclInterp = (((TkWindow *) arg_integer(1))->mainPtr)->interp;
  if (Tk_ButtonCmd ((ClientData) tkMainWindow,
		   tclInterp, 2, argv) != TCL_OK)
  { fprintf (stderr, tclInterp->result);
    exit (1);
  }
  return (int *) GetCmdClientData (tclInterp, name);
}

int *
MakeListbox(Tk_Window tkMainWindow, char *name)
{ char      *argv [2];
  Tcl_Interp *tclInterp;

  argv [0] = "listbox";
  argv [1] = name;
  tclInterp = (((TkWindow *) arg_integer(1))->mainPtr)->interp;
  if (Tk_ListboxCmd ((ClientData) tkMainWindow,
		     tclInterp, 2, argv) != TCL_OK)
  { fprintf (stderr, tclInterp->result);
    exit (1);
  }
  return (int *) GetCmdClientData (tclInterp, name);
}

int *
MakeMenu(Tk_Window tkMainWindow, char *name)
{ char      *argv [2];
  Tcl_Interp *tclInterp;

  argv [0] = "menu";
  argv [1] = name;
  tclInterp = (((TkWindow *) arg_integer(1))->mainPtr)->interp;
  if (Tk_MenuCmd ((ClientData) tkMainWindow,
		   tclInterp, 2, argv) != TCL_OK)
  { fprintf (stderr, tclInterp->result);
    exit (1);
  }
  return (int *) GetCmdClientData (tclInterp, name);
}

int *
MakeMenuButton(Tk_Window tkMainWindow, char *name)
{ char      *argv [2];
  Tcl_Interp *tclInterp;

  argv [0] = "menubutton";
  argv [1] = name;
  tclInterp = (((TkWindow *) arg_integer(1))->mainPtr)->interp;
  if (Tk_MenubuttonCmd ((ClientData) tkMainWindow,
			tclInterp, 2, argv) != TCL_OK)
  { fprintf (stderr, tclInterp->result);
    exit (1);
  }
  return (int *) GetCmdClientData (tclInterp, name);
}

int *
MakeMessage(Tk_Window tkMainWindow, char *name)
{ char      *argv [2];
  Tcl_Interp *tclInterp;

  argv [0] = "message";
  argv [1] = name;
  tclInterp = (((TkWindow *) arg_integer(1))->mainPtr)->interp;
  if (Tk_MessageCmd ((ClientData) tkMainWindow,
		     tclInterp, 2, argv) != TCL_OK)
  { fprintf (stderr, tclInterp->result);
    exit (1);
  }
  return (int *) GetCmdClientData (tclInterp, name);
}

int *
MakeRadioButton(Tk_Window tkMainWindow, char *name)
{ char      *argv [2];
  Tcl_Interp *tclInterp;

  argv [0] = "radiobutton";
  argv [1] = name;
  tclInterp = (((TkWindow *) arg_integer(1))->mainPtr)->interp;
  if (Tk_ButtonCmd ((ClientData) tkMainWindow,
		    tclInterp, 2, argv) != TCL_OK)
  { fprintf (stderr, tclInterp->result);
    exit (1);
  }
  return (int *) GetCmdClientData (tclInterp, name);
}

int *
MakeScale(Tk_Window tkMainWindow, char *name)
{ char      *argv [2];
  Tcl_Interp *tclInterp;

  argv [0] = "scale";
  argv [1] = name;
  tclInterp = (((TkWindow *) arg_integer(1))->mainPtr)->interp;
  if (Tk_ScaleCmd ((ClientData) tkMainWindow,
		    tclInterp, 2, argv) != TCL_OK)
  { fprintf (stderr, tclInterp->result);
    exit (1);
  }
  
  return (int *) GetCmdClientData (tclInterp, name);
}

int *
MakeScrollBar(Tk_Window tkMainWindow, char *name)
{ char      *argv [2];
  Tcl_Interp *tclInterp;

  argv [0] = "scrollbar";
  argv [1] = name;
  tclInterp = (((TkWindow *) arg_integer(1))->mainPtr)->interp;
  if (Tk_ScrollbarCmd ((ClientData) tkMainWindow,
		       tclInterp, 2, argv) != TCL_OK)
  { fprintf (stderr, tclInterp->result);
    exit (1);
  }
  return (int *) GetCmdClientData (tclInterp, name);
}

int *
MakeText(Tk_Window tkMainWindow, char *name)
{ char      *argv [2];
  Tcl_Interp *tclInterp;

  argv [0] = "text";
  argv [1] = name;
  tclInterp = (((TkWindow *) arg_integer(1))->mainPtr)->interp;
  if (Tk_TextCmd ((ClientData) tkMainWindow,
		  tclInterp, 2, argv) != TCL_OK)
  { fprintf (stderr, tclInterp->result);
    exit (1);
  }
  return (int *) GetCmdClientData (tclInterp, name);
}

/* These ought to be in tk-c.c but need internal data structures to work */

char *
tk_map_widget (All_Widgets *Widget, Tk_Window tkMainWindow, char *name,
	       Window xwindow, int x, int y)
/* This better work for all TK widgets or there's trouble */
/* "name" is the name originally given to this widget. */
{ TkWindow *win;

  win = ((TkWindow *) (Widget->tkwin));
  if ((xwindow != (Window) NULL) &&
      (((win->parentPtr)->window) != xwindow))
  { extern int		NameWindow _ANSI_ARGS_((Tcl_Interp *interp,
						TkWindow *winPtr,
						TkWindow *parentPtr,
						char *name));
    char *argv [3];
    Tcl_Interp *tclInterp = (((TkWindow *) tkMainWindow)->mainPtr)->interp;
    Tk_Window InternalWindow =
      Tk_CreateWindow(tclInterp, tkMainWindow, name, (char *) NULL);

    ((TkWindow *) InternalWindow)->window = xwindow;
    if (((win->parentPtr)->window) != (Window) NULL)
      fprintf(stderr, "tk_map_widget: changing parent window!\n");
    NameWindow(tclInterp, (TkWindow *) win,
	       (TkWindow *) InternalWindow, name);
    argv[0] = "rename";
    argv[1] = name;
    argv[2] = Tk_PathName((Tk_Window) win);
    if (Tcl_RenameCmd((ClientData) 0, tclInterp, 3, argv) != TCL_OK)
    { fprintf(stderr, "Failed. %s\n", tclInterp->result);
    }
  }
  Tk_MoveWindow ((Tk_Window) win, x, y);
  if (xwindow == (Window) NULL) Tk_UnmapWindow((Tk_Window) win);
  else Tk_MapWindow ((Tk_Window) win);
  return Tk_PathName((Tk_Window) win);
}

Tk_Window
tk_tkwin_widget (All_Widgets *Widget)
/* This better work for all TK widgets or there's trouble */
{
    return Widget->tkwin;
}

void
tk_destroy_widget (All_Widgets *Widget)
{ /* This better work for all TK widgets or there's trouble */
  Tk_DestroyWindow(Widget->tkwin);
  return;
}

void
  tk_delete_display (Display *disp)
{ Tk_DestroyDisplayByNumber(disp);
  return;
}

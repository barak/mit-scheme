/**************************
  Changes on April 1, 1993 to support Scheme UITK:
  1) Added Tk_CreateMainWindow_from_display.
  2) Renamed GetScreen to GetScreenByName.  Added GetScreenByDisplay.
  3) Rewrote Tk_CreateMainWindow into Tk_CreateMainWindow_from_data,
     which takes both a lookup procedure for finding a Screen and the
     data necessary for the lookup.  Rewrite Tk_CreateMainWindow in
     terms of this.
  4) Changed the signature of CreateTopLevelWindow to accept a lookup
     procedure and data.
  5) Made NameWindow public for reparenting widgets when mapped by UITK
  6) Added external entry point Tk_DestroyDisplayByNumber.
**************************/

/* 
 * tkWindow.c --
 *
 *	This file provides basic window-manipulation procedures,
 *	which are equivalent to procedures in Xlib (and even
 *	invoke them) but also maintain the local Tk_Window
 *	structure.
 *
 * Copyright 1989-1992 Regents of the University of California.
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies.  The University of California
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */

#include "tkConfig.h"
#include "tkInt.h"

/*
 * Count of number of main windows currently open in this process.
 */

int tk_NumMainWindows;

/*
 * List of all displays currently in use.
 */

TkDisplay *tkDisplayList = NULL;

/*
 * Have statics in this module been initialized?
 */

static initialized = 0;

/*
 * Context information used to map from X window id's to
 * TkWindow structures (during event handling, for example):
 */

XContext tkWindowContext;

/*
 * The variables below hold several uid's that are used in many places
 * in the toolkit.
 */

Tk_Uid tkDisabledUid = NULL;
Tk_Uid tkActiveUid = NULL;
Tk_Uid tkNormalUid = NULL;

/*
 * Default values for "changes" and "atts" fields of TkWindows.  Note
 * that Tk always requests all events for all windows, except StructureNotify
 * events on internal windows:  these events are generated internally.
 */

static XWindowChanges defChanges = {
    0, 0, 1, 1, 0, 0, Above
};
#define ALL_EVENTS_MASK \
    KeyPressMask|KeyReleaseMask|ButtonPressMask|ButtonReleaseMask| \
    EnterWindowMask|LeaveWindowMask|PointerMotionMask|ExposureMask| \
    VisibilityChangeMask|SubstructureNotifyMask| \
    FocusChangeMask|PropertyChangeMask|ColormapChangeMask
static XSetWindowAttributes defAtts= {
    None,			/* background_pixmap */
    0,				/* background_pixel */
    CopyFromParent,		/* border_pixmap */
    0,				/* border_pixel */
    ForgetGravity,		/* bit_gravity */
    NorthWestGravity,		/* win_gravity */
    NotUseful,			/* backing_store */
    ~0,				/* backing_planes */
    0,				/* backing_pixel */
    False,			/* save_under */
    ALL_EVENTS_MASK,		/* event_mask */
    0,				/* do_not_propagate_mask */
    False,			/* override_redirect */
    CopyFromParent,		/* colormap */
    None			/* cursor */
};

/*
 * The following structure defines all of the commands supported by
 * Tk, and the C procedures that execute them.
 */

typedef struct {
    char *name;			/* Name of command. */
    int (*cmdProc) _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp,
	    int argc, char **argv));
				/* Command procedure. */
} TkCmd;

TkCmd commands[] = {
    /*
     * Commands that are part of the intrinsics:
     */

    {"after",		Tk_AfterCmd},
    {"bind",		Tk_BindCmd},
    {"destroy",		Tk_DestroyCmd},
    {"focus",		Tk_FocusCmd},
    {"grab",		Tk_GrabCmd},
    {"option",		Tk_OptionCmd},
    {"pack",		Tk_PackCmd},
    {"place",		Tk_PlaceCmd},
    {"selection",	Tk_SelectionCmd},
    {"tk",		Tk_TkCmd},
    {"tkwait",		Tk_TkwaitCmd},
    {"update",		Tk_UpdateCmd},
    {"winfo",		Tk_WinfoCmd},
    {"wm",		Tk_WmCmd},

    /*
     * Widget-creation commands.
     */
    {"button",		Tk_ButtonCmd},
    {"canvas",		Tk_CanvasCmd},
    {"checkbutton",	Tk_ButtonCmd},
    {"entry",		Tk_EntryCmd},
    {"frame",		Tk_FrameCmd},
    {"label",		Tk_ButtonCmd},
    {"listbox",		Tk_ListboxCmd},
    {"menu",		Tk_MenuCmd},
    {"menubutton",	Tk_MenubuttonCmd},
    {"message",		Tk_MessageCmd},
    {"radiobutton",	Tk_ButtonCmd},
    {"scale",		Tk_ScaleCmd},
    {"scrollbar",	Tk_ScrollbarCmd},
    {"text",		Tk_TextCmd},
    {"toplevel",	Tk_FrameCmd},
    {(char *) NULL,	(int (*)()) NULL}
};

/*
 * Forward declarations to procedures defined later in this file:
 */

static Tk_Window	CreateTopLevelWindow
  _ANSI_ARGS_((Tcl_Interp *interp,
	       Tk_Window parent,
	       char *name,
	       TkDisplay *LookupProcedure(Tcl_Interp *interp,
					  char *data,
					  int *screenPtr),
	       char *data));
static void		DoConfigureNotify _ANSI_ARGS_((TkWindow *winPtr));
static TkDisplay *	GetScreenByName _ANSI_ARGS_((Tcl_Interp *interp,
						     char *screenName,
						     int *screenPtr));
static TkDisplay *	GetScreenByDisplay _ANSI_ARGS_((Tcl_Interp *interp,
							char /*Display*/ *disp,
							int *screenPtr));
extern int		NameWindow _ANSI_ARGS_((Tcl_Interp *interp,
			    TkWindow *winPtr, TkWindow *parentPtr,
			    char *name));
static TkWindow	*	NewWindow _ANSI_ARGS_((TkDisplay *dispPtr,
			    int screenNum, TkWindow *parentPtr));

extern void             Tk_DestroyDisplayByNumber _ANSI_ARGS_ ((Display *disp));

/*
 *----------------------------------------------------------------------
 *
 * CreateTopLevelWindow --
 *
 *	Make a new window that will be at top-level (its parent will
 *	be the root window of a screen).
 *
 * Results:
 *	The return value is a token for the new window, or NULL if
 *	an error prevented the new window from being created.  If
 *	NULL is returned, an error message will be left in
 *	interp->result.
 *
 * Side effects:
 *	A new window structure is allocated locally.  An X
 *	window is NOT initially created, but will be created
 *	the first time the window is mapped.
 *
 *----------------------------------------------------------------------
 */

static Tk_Window
CreateTopLevelWindow(interp, parent, name, LookupProcedure, data)
    Tcl_Interp *interp;		/* Interpreter to use for error reporting. */
    Tk_Window parent;		/* Token for logical parent of new window
				 * (used for naming, options, etc.).  May
				 * be NULL. */
    char *name;			/* Name for new window;  if parent is
				 * non-NULL, must be unique among parent's
				 * children. */
    TkDisplay *(*LookupProcedure) _ANSI_ARGS_ ((Tcl_Interp *interp, char *data, int *screenPtr));
				/* Lookup a display structure, either */
				/* by name or by display connection */
    char *data;			/* Data supplied to LookupProcedure: */
				/* either a screen name or a display */
				/* connection */
{
    register TkWindow *winPtr;
    register TkDisplay *dispPtr;
    int screenId;

    if (!initialized) {
	initialized = 1;
	tkWindowContext = XUniqueContext();
	tkActiveUid = Tk_GetUid("active");
	tkDisabledUid = Tk_GetUid("disabled");
	tkNormalUid = Tk_GetUid("normal");
    }

    if ((LookupProcedure==GetScreenByName) && (parent != NULL) &&
	(data != NULL) && (data[0] == '\0'))
    { dispPtr = ((TkWindow *) parent)->dispPtr;
      screenId = Tk_ScreenNumber(parent);
    }
    else
    { dispPtr = LookupProcedure(interp, data, &screenId);
      if (dispPtr == NULL) return (Tk_Window) NULL;
    }

    winPtr = NewWindow(dispPtr, screenId, (TkWindow *) parent);

    /*
     * Internal windows don't normally ask for StructureNotify events,
     * since we can generate them internally.  However, for top-level
     * windows we need to as for the events because the window could
     * be manipulated externally.
     */

    winPtr->atts.event_mask |= StructureNotifyMask;

    /*
     * (Need to set the TK_TOP_LEVEL flag immediately here;  otherwise
     * Tk_DestroyWindow will core dump if it is called before the flag
     * has been set.)
     */

    winPtr->flags |= TK_TOP_LEVEL;
    if (parent != NULL) {
	if (NameWindow(interp, winPtr, (TkWindow *) parent, name) != TCL_OK) {
	    Tk_DestroyWindow((Tk_Window) winPtr);
	    return (Tk_Window) NULL;
	}
    }
    TkWmNewWindow(winPtr);
    return (Tk_Window) winPtr;
}

TkDisplay * MakeTkDisplay(display, NameLength, screenName)
     Display *display;
     int NameLength;
     char* screenName;
{
  register TkDisplay *dispPtr = (TkDisplay *) ckalloc(sizeof(TkDisplay));
  register long i;

  dispPtr->display = display;
  dispPtr->nextPtr = tkDisplayList;
  dispPtr->name = (char *) ckalloc((unsigned) (NameLength+1));
  dispPtr->lastEventTime = CurrentTime;
  strncpy(dispPtr->name, screenName, NameLength);
  dispPtr->focusTopLevelPtr = NULL;
  dispPtr->focussedOnEnter = 0;
  dispPtr->name[NameLength] = '\0';
  dispPtr->bindInfoStale = 1;
  dispPtr->errorPtr = NULL;
  dispPtr->deleteCount = 0;
  dispPtr->commWindow = NULL;
  dispPtr->selectionOwner = NULL;
  dispPtr->selectionSerial = 0;
  dispPtr->multipleAtom = None;
  dispPtr->atomInit = 0;
  dispPtr->cursorFont = None;
  dispPtr->grabWinPtr = NULL;
  dispPtr->eventualGrabWinPtr = NULL;
  dispPtr->buttonWinPtr = NULL;
  dispPtr->serverWinPtr = NULL;
  dispPtr->firstGrabEventPtr = NULL;
  dispPtr->lastGrabEventPtr = NULL;
  dispPtr->grabFlags = 0;
  dispPtr->colorModels =
    (Tk_ColorModel *) ckalloc((unsigned)
			      (ScreenCount(display)*
			       sizeof(Tk_ColorModel)));
  for (i = ScreenCount(display)-1; i >= 0; i--) {
    if (DisplayPlanes(display, i) <= 4) {
      dispPtr->colorModels[i] = TK_MONO;
    } else {
      dispPtr->colorModels[i] = TK_COLOR;
    }
  }
  tkDisplayList = dispPtr;
  Tk_CreateFileHandler(ConnectionNumber(display),
		       TK_READABLE, (void (*)()) NULL,
		       (ClientData) display);
  return dispPtr;
}

/*
 *----------------------------------------------------------------------
 *
 * GetScreenByName --
 *
 *	Given a string name for a display-plus-screen, find the
 *	TkDisplay structure for the display and return the screen
 *	number too.
 *
 * Results:
 *	The return value is a pointer to information about the display,
 *	or NULL if the display couldn't be opened.  In this case, an
 *	error message is left in interp->result.  The location at
 *	*screenPtr is overwritten with the screen number parsed from
 *	screenName.
 *
 * Side effects:
 *	A new connection is opened to the display if there is no
 *	connection already.  A new TkDisplay data structure is also
 *	setup, if necessary.
 *
 *----------------------------------------------------------------------
 */

static TkDisplay *
GetScreenByName(interp, screenName, screenPtr)
    Tcl_Interp *interp;		/* Place to leave error message. */
    char *screenName;		/* Name for screen.  NULL or empty means
				 * use DISPLAY envariable. */
    int *screenPtr;		/* Where to store screen number. */
{
    register TkDisplay *dispPtr;
    char *p;
    int length, screenId, i;

    /*
     * Separate the screen number from the rest of the display
     * name.  ScreenName is assumed to have the syntax
     * <display>.<screen> with the dot and the screen being
     * optional.
     */

    if ((screenName == NULL) || (screenName[0] == '\0')) {
	screenName = getenv("DISPLAY");
	if (screenName == NULL) {
	    interp->result =
		    "no display name and no $DISPLAY environment variable";
	    return (TkDisplay *) NULL;
	}
    }
    length = strlen(screenName);
    screenId = 0;
    p = screenName+length-1;
    while (isdigit(*p) && (p != screenName)) {
	p--;
    }
    if ((*p == '.') && (p[1] != '\0')) {
	length = p - screenName;
	screenId = strtoul(p+1, (char **) NULL, 10);
    }

    /*
     * See if we already have a connection to this display.  If not,
     * then open a new connection.
     */

    for (dispPtr = tkDisplayList; ; dispPtr = dispPtr->nextPtr)
    { if (dispPtr == NULL)
      { Display *display;
	/* block and unblock added by Hal  -- 7/22/95 in an attempt to fix a problem
           with making this work over PPP (i.e. slow) connections */
        block_signals ();
	display = XOpenDisplay(screenName);
        unblock_signals ();
	if (display == NULL)
	{ Tcl_AppendResult(interp, "couldn't connect to display \"",
			   screenName, "\"", (char *) NULL);
	  return (TkDisplay *) NULL;
	}
	dispPtr = MakeTkDisplay(display, length, screenName);
	break;
      }
      if ((strncmp(dispPtr->name, screenName, length) == 0)
	  && (dispPtr->name[length] == '\0'))
      {
	break;
      }
    }
    if (screenId >= ScreenCount(dispPtr->display)) {
	sprintf(interp->result, "bad screen number \"%d\"", screenId);
	return (TkDisplay *) NULL;
    }
    *screenPtr = screenId;
    return dispPtr;
}

static TkDisplay *GetScreenByDisplay(interp, disp, screenPtr)
     Tcl_Interp *interp;
     char *disp;
     int *screenPtr;
/* GetScreenByDisplay assumes screen 0! */
{ Display *Disp = (Display *) disp;
  register TkDisplay *dispPtr;

  for (dispPtr = tkDisplayList; ; dispPtr = dispPtr->nextPtr)
  { if (dispPtr == NULL)
    { dispPtr = MakeTkDisplay(Disp, 0, "");
      break;
    }
    if (Disp==(dispPtr->display)) break;
  }
  *screenPtr = 0;
  return dispPtr;
}

void Tk_DestroyDisplayByNumber(Disp)
     Display *Disp;
/* Tk_DestroyDisplayByNumber assumes screen 0! */
{ register TkDisplay *dispPtr, *Prev=(TkDisplay *) NULL;

  for (dispPtr = tkDisplayList; dispPtr != NULL;
       Prev=dispPtr, dispPtr = dispPtr->nextPtr)
  { if (Disp==(dispPtr->display))
    { if (Prev==(TkDisplay *) NULL)
	tkDisplayList = dispPtr->nextPtr;
      else Prev->nextPtr = dispPtr->nextPtr;
      ckfree(dispPtr->name);
      ckfree(dispPtr->colorModels);
      ckfree(dispPtr);
      Tk_DeleteFileHandler(ConnectionNumber(Disp));
      return;
    }
  }
  return;
}

/*
 *--------------------------------------------------------------
 *
 * NewWindow --
 *
 *	This procedure creates and initializes a TkWindow structure.
 *
 * Results:
 *	The return value is a pointer to the new window.
 *
 * Side effects:
 *	A new window structure is allocated and all its fields are
 *	initialized.
 *
 *--------------------------------------------------------------
 */

static TkWindow *
NewWindow(dispPtr, screenNum, parentPtr)
    TkDisplay *dispPtr;		/* Display associated with new window. */
    int screenNum;		/* Index of screen for new window. */
    TkWindow *parentPtr;	/* Parent from which this window should
				 * inherit visual inforamtion.  NULL means
				 * use screen defaults instead of
				 * inheriting. */
{
    register TkWindow *winPtr;

    winPtr = (TkWindow *) ckalloc(sizeof(TkWindow));
    winPtr->display = dispPtr->display;
    winPtr->dispPtr = dispPtr;
    winPtr->screenNum = screenNum;
    if (parentPtr != NULL) {
	winPtr->visual = parentPtr->visual;
	winPtr->depth = parentPtr->depth;
    } else {
	winPtr->visual = DefaultVisual(dispPtr->display, screenNum);
	winPtr->depth = DefaultDepth(dispPtr->display, screenNum);
    }
    winPtr->window = None;
    winPtr->childList = NULL;
    winPtr->parentPtr = NULL;
    winPtr->nextPtr = NULL;
    winPtr->mainPtr = NULL;
    winPtr->pathName = NULL;
    winPtr->nameUid = NULL;
    winPtr->classUid = NULL;
    winPtr->changes = defChanges;
    winPtr->dirtyChanges = CWX|CWY|CWWidth|CWHeight|CWBorderWidth;
    winPtr->atts = defAtts;
    if (parentPtr != NULL) {
	winPtr->atts.colormap = parentPtr->atts.colormap;
    } else {
	winPtr->atts.colormap = DefaultColormap(dispPtr->display, screenNum);
    }
    winPtr->dirtyAtts = CWEventMask|CWColormap;
    winPtr->flags = 0;
    winPtr->handlerList = NULL;
    winPtr->focusProc = NULL;
    winPtr->focusData = NULL;
    winPtr->optionLevel = -1;
    winPtr->selHandlerList = NULL;
    winPtr->selClearProc = NULL;
    winPtr->selClearData = NULL;
    winPtr->geomProc = NULL;
    winPtr->geomData = NULL;
    winPtr->reqWidth = winPtr->reqHeight = 0;
    winPtr->internalBorderWidth = 0;
    winPtr->wmInfoPtr = NULL;

    return winPtr;
}

/*
 *----------------------------------------------------------------------
 *
 * NameWindow --
 *
 *	This procedure is invoked to give a window a name and insert
 *	the window into the hierarchy associated with a particular
 *	application.
 *
 * Results:
 *	A standard Tcl return value.
 *
 * Side effects:
 *      See above.
 *
 *----------------------------------------------------------------------
 */

int
NameWindow(interp, winPtr, parentPtr, name)
    Tcl_Interp *interp;		/* Interpreter to use for error reporting. */
    register TkWindow *winPtr;	/* Window that is to be named and inserted. */
    TkWindow *parentPtr;	/* Pointer to logical parent for winPtr
				 * (used for naming, options, etc.). */
    char *name;			/* Name for winPtr;   must be unique among
				 * parentPtr's children. */
{
#define FIXED_SIZE 200
    char staticSpace[FIXED_SIZE];
    char *pathName;
    int new;
    Tcl_HashEntry *hPtr;
    int length1, length2;

    /*
     * Setup all the stuff except name right away, then do the name stuff
     * last.  This is so that if the name stuff fails, everything else
     * will be properly initialized (needed to destroy the window cleanly
     * after the naming failure).
     */
    winPtr->parentPtr = parentPtr;
    winPtr->nextPtr = parentPtr->childList;
    parentPtr->childList = winPtr;
    winPtr->mainPtr = parentPtr->mainPtr;
    winPtr->nameUid = Tk_GetUid(name);

    /*
     * Don't permit names that start with an upper-case letter:  this
     * will just cause confusion with class names in the option database.
     */

    if (isupper(name[0])) {
	Tcl_AppendResult(interp,
		"window name starts with an upper-case letter: \"",
		name, "\"", (char *) NULL);
	return TCL_ERROR;
    }

    /*
     * To permit names of arbitrary length, must be prepared to malloc
     * a buffer to hold the new path name.  To run fast in the common
     * case where names are short, use a fixed-size buffer on the
     * stack.
     */

    length1 = strlen(parentPtr->pathName);
    length2 = strlen(name);
    if ((length1+length2+2) <= FIXED_SIZE) {
	pathName = staticSpace;
    } else {
	pathName = (char *) ckalloc((unsigned) (length1+length2+2));
    }
    if (length1 == 1) {
	pathName[0] = '.';
	strcpy(pathName+1, name);
    } else {
	strcpy(pathName, parentPtr->pathName);
	pathName[length1] = '.';
	strcpy(pathName+length1+1, name);
    }
    hPtr = Tcl_CreateHashEntry(&parentPtr->mainPtr->nameTable, pathName, &new);
    if (pathName != staticSpace) {
	ckfree(pathName);
    }
    if (!new) {
	Tcl_AppendResult(interp, "window name \"", name,
		"\" already exists in parent", (char *) NULL);
	return TCL_ERROR;
    }
    Tcl_SetHashValue(hPtr, winPtr);
    winPtr->pathName = Tcl_GetHashKey(&parentPtr->mainPtr->nameTable, hPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Tk_CreateMainWindow --
 *
 *	Make a new main window.  A main window is a special kind of
 *	top-level window used as the outermost window in an
 *	application.
 *
 * Results:
 *	The return value is a token for the new window, or NULL if
 *	an error prevented the new window from being created.  If
 *	NULL is returned, an error message will be left in
 *	interp->result.
 *
 * Side effects:
 *	A new window structure is allocated locally;  "interp" is
 *	associated with the window and registered for "send" commands
 *	under "baseName".  BaseName may be extended with an instance
 *	number in the form "#2" if necessary to make it globally
 *	unique.  Tk-related commands are bound into interp.  An X
 *	window is NOT initially created, but will be created the
 *	first time the window is mapped.
 *
 *----------------------------------------------------------------------
 */

Tk_Window
Tk_CreateMainWindow_from_data(interp, baseName, LookupProcedure, data)
    Tcl_Interp *interp;		/* Interpreter to use for error reporting. */
    char *baseName;		/* Base name for application;  usually of the
				 * form "prog instance". */
    TkDisplay *(*LookupProcedure) _ANSI_ARGS_ ((Tcl_Interp *interp, char *data, int *screenPtr));
				/* Lookup a display structure, either */
				/* by name or by display connection */
    char *data;			/* Data supplied to LookupProcedure */
{
    Tk_Window tkwin;
    int result, dummy;
    Tcl_HashEntry *hPtr;
    register TkMainInfo *mainPtr;
    register TkWindow *winPtr;
    register TkCmd *cmdPtr;
    char *libDir;

    /*
     * Create the basic TkWindow structure.
     */

    tkwin = CreateTopLevelWindow(interp, (Tk_Window) NULL, baseName,
				 LookupProcedure, data);
    if (tkwin == NULL) {
	return NULL;
    }

    /*
     * Create the TkMainInfo structure for this application, and set
     * up name-related information for the new window.
     */

    winPtr = (TkWindow *) tkwin;
    mainPtr = (TkMainInfo *) ckalloc(sizeof(TkMainInfo));
    mainPtr->winPtr = winPtr;
    mainPtr->interp = interp;
    Tcl_InitHashTable(&mainPtr->nameTable, TCL_STRING_KEYS);
    mainPtr->bindingTable = Tk_CreateBindingTable(interp);
    mainPtr->focusPtr = winPtr;
    mainPtr->focusDefaultPtr = NULL;
    mainPtr->optionRootPtr = NULL;
    winPtr->mainPtr = mainPtr;
    hPtr = Tcl_CreateHashEntry(&mainPtr->nameTable, ".", &dummy);
    Tcl_SetHashValue(hPtr, winPtr);
    winPtr->pathName = Tcl_GetHashKey(&mainPtr->nameTable, hPtr);

    /*
     * Register the interpreter for "send" purposes.  If baseName isn't
     * already unique, find a unique suffix to add to it to make it
     * unique.  Change the window's name to contain the suffix.
     */

    result = Tk_RegisterInterp(interp, baseName, tkwin);
    if (result == TCL_OK) {
	winPtr->nameUid = Tk_GetUid(baseName);
    } else {
	char newName[110];
	int i;

	for (i = 2; ; i++) {
	    sprintf(newName, "%.100s #%d", baseName, i);
	    Tcl_SetResult(interp, (char *) NULL, TCL_STATIC);
	    result = Tk_RegisterInterp(interp, newName, tkwin);
	    if (result == TCL_OK) {
		break;
	    }
	    if (i >= 100) {
		Tcl_SetResult(interp,
			"couldn't generate unique name to register application",
			TCL_STATIC);
		Tk_DestroyWindow(tkwin);
	    }
	}
	winPtr->nameUid = Tk_GetUid(newName);
    }

    /*
     * Bind in Tk's commands.
     */

    for (cmdPtr = commands; cmdPtr->name != NULL; cmdPtr++) {
	Tcl_CreateCommand(interp, cmdPtr->name, cmdPtr->cmdProc,
		(ClientData) tkwin, (void (*)()) NULL);
    }

    /*
     * Set variables for the intepreter.
     */

    libDir = getenv("TK_LIBRARY");
    if (libDir == NULL) {
	libDir = TK_LIBRARY;
    }
    Tcl_SetVar(interp, "tk_library", libDir, TCL_GLOBAL_ONLY);
    Tcl_SetVar(interp, "tk_version", TK_VERSION, TCL_GLOBAL_ONLY);
    Tcl_SetVar(interp, "tkVersion", TK_VERSION, TCL_GLOBAL_ONLY);

    tk_NumMainWindows++;
    return tkwin;
}

Tk_Window
  Tk_CreateMainWindow(interp, screenName, baseName)
    Tcl_Interp *interp;		/* Interpreter to use for error reporting. */
    char *screenName;		/* Name of screen on which to create
				 * window.  Empty or NULL string means
				 * use DISPLAY environment variable. */
    char *baseName;		/* Base name for application;  usually of the
				 * form "prog instance". */
{ return Tk_CreateMainWindow_from_data(interp, baseName,
				       GetScreenByName, screenName);
}

Tk_Window
Tk_CreateMainWindow_from_display(interp, display, baseName)
    Tcl_Interp *interp;		/* Interpreter to use for error reporting. */
    Display *display;		/* X Display connection */
    char *baseName;		/* Base name for application;  usually of the
				 * form "prog instance". */
{ return Tk_CreateMainWindow_from_data(interp, baseName,
				       GetScreenByDisplay,
				       (char *) display);
}

/*
 *--------------------------------------------------------------
 *
 * Tk_CreateWindow --
 *
 *	Create a new internal or top-level window as a child of an
 *	existing window.
 *
 * Results:
 *	The return value is a token for the new window.  This
 *	is not the same as X's token for the window.  If an error
 *	occurred in creating the window (e.g. no such display or
 *	screen), then an error message is left in interp->result and
 *	NULL is returned.
 *
 * Side effects:
 *	A new window structure is allocated locally.  An X
 *	window is not initially created, but will be created
 *	the first time the window is mapped.
 *
 *--------------------------------------------------------------
 */

Tk_Window
Tk_CreateWindow(interp, parent, name, screenName)
    Tcl_Interp *interp;		/* Interpreter to use for error reporting.
				 * Interp->result is assumed to be
				 * initialized by the caller. */
    Tk_Window parent;		/* Token for parent of new window. */
    char *name;			/* Name for new window.  Must be unique
				 * among parent's children. */
    char *screenName;		/* If NULL, new window will be internal on
				 * same screen as its parent.  If non-NULL,
				 * gives name of screen on which to create
				 * new window;  window will be a top-level
				 * window. */
{
    TkWindow *parentPtr = (TkWindow *) parent;
    TkWindow *winPtr;

    if (screenName == NULL)
    { winPtr = NewWindow(parentPtr->dispPtr, parentPtr->screenNum,
			 parentPtr);
      if (NameWindow(interp, winPtr, parentPtr, name) != TCL_OK)
      { Tk_DestroyWindow((Tk_Window) winPtr);
	return NULL;
      }
      else
      { return (Tk_Window) winPtr;
      }
    }
    else
    { return CreateTopLevelWindow(interp, parent, name,
				  GetScreenByName, screenName);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Tk_CreateWindowFromPath --
 *
 *	This procedure is similar to Tk_CreateInternalWindow except
 *	that it uses a path name to create the window, rather than
 *	a parent and a child name.
 *
 * Results:
 *	The return value is a token for the new window.  This
 *	is not the same as X's token for the window.  If an error
 *	occurred in creating the window (e.g. no such display or
 *	screen), then an error message is left in interp->result and
 *	NULL is returned.
 *
 * Side effects:
 *	A new window structure is allocated locally.  An X
 *	window is not initially created, but will be created
 *	the first time the window is mapped.
 *
 *----------------------------------------------------------------------
 */

Tk_Window
Tk_CreateWindowFromPath(interp, tkwin, pathName, screenName)
    Tcl_Interp *interp;		/* Interpreter to use for error reporting.
				 * Interp->result is assumed to be
				 * initialized by the caller. */
    Tk_Window tkwin;		/* Token for any window in application
				 * that is to contain new window. */
    char *pathName;		/* Path name for new window within the
				 * application of tkwin.  The parent of
				 * this window must already exist, but
				 * the window itself must not exist. */
    char *screenName;		/* If NULL, new window will be on same
				 * screen as its parent.  If non-NULL,
				 * gives name of screen on which to create
				 * new window;  window will be a top-level
				 * window. */
{
#define FIXED_SPACE 5
    char fixedSpace[FIXED_SPACE+1];
    char *p;
    Tk_Window parent;
    int numChars;

    /*
     * Strip the parent's name out of pathName (it's everything up
     * to the last dot).  There are two tricky parts: (a) must
     * copy the parent's name somewhere else to avoid modifying
     * the pathName string (for large names, space for the copy
     * will have to be malloc'ed);  (b) must special-case the
     * situation where the parent is ".".
     */

    p = strrchr(pathName, '.');
    if (p == NULL) {
	Tcl_AppendResult(interp, "bad window path name \"", pathName,
		"\"", (char *) NULL);
	return NULL;
    }
    numChars = p-pathName;
    if (numChars > FIXED_SPACE) {
	p = (char *) ckalloc((unsigned) (numChars+1));
    } else {
	p = fixedSpace;
    }
    if (numChars == 0) {
	*p = '.';
	p[1] = '\0';
    } else {
	strncpy(p, pathName, numChars);
	p[numChars] = '\0';
    }

    /*
     * Find the parent window.
     */

    parent = Tk_NameToWindow(interp, p, tkwin);
    if (p != fixedSpace) {
	ckfree(p);
    }
    if (parent == NULL) {
	return NULL;
    }

    /*
     * Create the window.
     */

    if (screenName == NULL) {
	TkWindow *parentPtr = (TkWindow *) parent;
	TkWindow *winPtr;

	winPtr = NewWindow(parentPtr->dispPtr, parentPtr->screenNum,
			   parentPtr);
	if (NameWindow(interp, winPtr, parentPtr, pathName+numChars+1)
		!= TCL_OK) {
	    Tk_DestroyWindow((Tk_Window) winPtr);
	    return NULL;
	} else
	{ return (Tk_Window) winPtr;
	}
    } else {
	return CreateTopLevelWindow(interp, parent, pathName+numChars+1,
				    GetScreenByName, screenName);
    }
}

/*
 *--------------------------------------------------------------
 *
 * Tk_DestroyWindow --
 *
 *	Destroy an existing window.  After this call, the caller
 *	should never again use the token.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The window is deleted, along with all of its children.
 *	Relevant callback procedures are invoked.
 *
 *--------------------------------------------------------------
 */

void
Tk_DestroyWindow(tkwin)
    Tk_Window tkwin;		/* Window to destroy. */
{
    register TkWindow *winPtr = (TkWindow *) tkwin;
    XEvent event;

    if (winPtr->flags & TK_ALREADY_DEAD) {
	/*
	 * An destroy event binding caused the window to be destroyed
	 * again.  Ignore the request.
	 */

	return;
    }

    /*
     * Recursively destroy children.  The TK_RECURSIVE_DESTROY
     * flags means that the child's window needn't be explicitly
     * destroyed (the destroy of the parent already did it), nor
     * does it need to be removed from its parent's child list,
     * since the parent is being destroyed too.
     */

    while (winPtr->childList != NULL) {
	winPtr->childList->flags |= TK_RECURSIVE_DESTROY;
	Tk_DestroyWindow((Tk_Window) winPtr->childList);
    }

    /*
     * Generate a DestroyNotify event.  In order for the DestroyNotify
     * event to be processed correctly, need to make sure the window
     * exists.  This is a bit of a kludge, and may be unnecessarily
     * expensive, but without it no event handlers will get called for
     * windows that don't exist yet.
     */

    if (winPtr->window == None) {
	Tk_MakeWindowExist(tkwin);
    }
    winPtr->flags |= TK_ALREADY_DEAD;
    event.type = DestroyNotify;
    event.xdestroywindow.serial =
	    LastKnownRequestProcessed(winPtr->display);
    event.xdestroywindow.send_event = False;
    event.xdestroywindow.display = winPtr->display;
    event.xdestroywindow.event = winPtr->window;
    event.xdestroywindow.window = winPtr->window;
    Tk_HandleEvent(&event);

    /*
     * Cleanup the data structures associated with this window.
     * No need to destroy windows during recursive destroys, since
     * that will happen automatically when the parent window is
     * destroyed (not true for top-level windows:  must destroy
     * them explicitly).
     */

    if (winPtr->window != None) {
	if (!(winPtr->flags & TK_RECURSIVE_DESTROY)
		|| (winPtr->flags & TK_TOP_LEVEL)) {
	    XDestroyWindow(winPtr->display, winPtr->window);
	}
	XDeleteContext(winPtr->display, winPtr->window, tkWindowContext);
	winPtr->window = None;
    }
    if (winPtr->parentPtr != NULL) {
	if (winPtr->parentPtr->childList == winPtr) {
	    winPtr->parentPtr->childList = winPtr->nextPtr;
	} else {
	    register TkWindow *winPtr2;
    
	    for (winPtr2 = winPtr->parentPtr->childList; ;
		    winPtr2 = winPtr2->nextPtr) {
		if (winPtr2 == NULL) {
		    panic("Tk_DestroyWindow couldn't find child in parent (deleted twice?)");
		    break;
		}
		if (winPtr2->nextPtr == winPtr) {
		    winPtr2->nextPtr = winPtr->nextPtr;
		    break;
		}
	    }
	}
    }
    TkEventDeadWindow(winPtr);
    TkFocusDeadWindow(winPtr);
    TkOptionDeadWindow(winPtr);
    TkSelDeadWindow(winPtr);
    if (winPtr->flags & TK_TOP_LEVEL) {
	TkWmDeadWindow(winPtr);
    }
    TkGrabDeadWindow(winPtr);
    if (winPtr->mainPtr != NULL) {
	Tk_DeleteAllBindings(winPtr->mainPtr->bindingTable,
		(ClientData) winPtr->pathName);
	if (winPtr->pathName != NULL) {
	    Tcl_DeleteHashEntry(Tcl_FindHashEntry(&winPtr->mainPtr->nameTable,
		    winPtr->pathName));
	}
	if (winPtr->mainPtr->winPtr == winPtr) {
	    register TkCmd *cmdPtr;

	    /*
	     * Deleting a main window.  Delete the TkMainInfo structure too
	     * and replace all of Tk's commands with dummy commands that
	     * return errors.  Also delete the "send" command to unregister
	     * the interpreter.
	     */

	    for (cmdPtr = commands; cmdPtr->name != NULL; cmdPtr++) {
		Tcl_CreateCommand(winPtr->mainPtr->interp, cmdPtr->name,
			TkDeadAppCmd, (ClientData) NULL, (void (*)()) NULL);
	    }
	    Tcl_CreateCommand(winPtr->mainPtr->interp, "send",
		    TkDeadAppCmd, (ClientData) NULL, (void (*)()) NULL);
	    Tcl_DeleteHashTable(&winPtr->mainPtr->nameTable);
	    Tk_DeleteBindingTable(winPtr->mainPtr->bindingTable);
	    ckfree((char *) winPtr->mainPtr);
	    tk_NumMainWindows--;
	}
    }
    ckfree((char *) winPtr);
}

/*
 *--------------------------------------------------------------
 *
 * Tk_MapWindow --
 *
 *	Map a window within its parent.  This may require the
 *	window and/or its parents to actually be created.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The given window will be mapped.  Windows may also
 *	be created.
 *
 *--------------------------------------------------------------
 */

void
Tk_MapWindow(tkwin)
    Tk_Window tkwin;		/* Token for window to map. */
{
    register TkWindow *winPtr = (TkWindow *) tkwin;
    XEvent event;

    if (winPtr->flags & TK_MAPPED) {
	return;
    }
    if (winPtr->window == None) {
	Tk_MakeWindowExist(tkwin);
    }
    if (winPtr->flags & TK_TOP_LEVEL) {
	/*
	 * Lots of special processing has to be done for top-level
	 * windows.  Let tkWm.c handle everything itself.
	 */

	TkWmMapWindow(winPtr);
	return;
    }
    winPtr->flags |= TK_MAPPED;
    XMapWindow(winPtr->display, winPtr->window);
    event.type = MapNotify;
    event.xmap.serial = LastKnownRequestProcessed(winPtr->display);
    event.xmap.send_event = False;
    event.xmap.display = winPtr->display;
    event.xmap.event = winPtr->window;
    event.xmap.window = winPtr->window;
    event.xmap.override_redirect = winPtr->atts.override_redirect;
    Tk_HandleEvent(&event);
}

/*
 *--------------------------------------------------------------
 *
 * Tk_MakeWindowExist --
 *
 *	Ensure that a particular window actually exists.  This
 *	procedure shouldn't normally need to be invoked from
 *	outside the Tk package, but may be needed if someone
 *	wants to manipulate a window before mapping it.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	When the procedure returns, the X window associated with
 *	tkwin is guaranteed to exist.  This may require the
 *	window's ancestors to be created also.
 *
 *--------------------------------------------------------------
 */

void
Tk_MakeWindowExist(tkwin)
    Tk_Window tkwin;		/* Token for window. */
{
    register TkWindow *winPtr = (TkWindow *) tkwin;
    Window parent;

    if (winPtr->window != None) {
	return;
    }

    if (winPtr->flags & TK_TOP_LEVEL) {
	parent = XRootWindow(winPtr->display, winPtr->screenNum);
    } else {
	if (winPtr->parentPtr->window == None) {
	    Tk_MakeWindowExist((Tk_Window) winPtr->parentPtr);
	}
	parent = winPtr->parentPtr->window;
    }

    winPtr->window = XCreateWindow(winPtr->display, parent,
	    winPtr->changes.x, winPtr->changes.y,
	    winPtr->changes.width, winPtr->changes.height,
	    winPtr->changes.border_width, winPtr->depth,
	    InputOutput, winPtr->visual, winPtr->dirtyAtts,
	    &winPtr->atts);
    XSaveContext(winPtr->display, winPtr->window, tkWindowContext,
	    (caddr_t) winPtr);
    winPtr->dirtyAtts = 0;
    winPtr->dirtyChanges &= ~(CWX|CWY|CWWidth|CWHeight|CWBorderWidth);
    if (winPtr->dirtyChanges != 0) {
	XConfigureWindow(winPtr->display, winPtr->window,
		winPtr->dirtyChanges, &winPtr->changes);
	winPtr->dirtyChanges = 0;
    }

    /*
     * Issue a ConfigureNotify event if there were deferred configuration
     * changes.
     */

    if (winPtr->flags & TK_NEED_CONFIG_NOTIFY) {
	winPtr->flags &= ~TK_NEED_CONFIG_NOTIFY;
	DoConfigureNotify(winPtr);
    }
}

/*
 *--------------------------------------------------------------
 *
 * Tk_UnmapWindow, etc. --
 *
 *	There are several procedures under here, each of which
 *	mirrors an existing X procedure.  In addition to performing
 *	the functions of the corresponding procedure, each
 *	procedure also updates the local window structure and
 *	synthesizes an X event (if the window's structure is being
 *	managed internally).
 *
 * Results:
 *	See the manual entries.
 *
 * Side effects:
 *	See the manual entries.
 *
 *--------------------------------------------------------------
 */

void
Tk_UnmapWindow(tkwin)
    Tk_Window tkwin;		/* Token for window to unmap. */
{
    register TkWindow *winPtr = (TkWindow *) tkwin;

    if (!(winPtr->flags & TK_MAPPED)) {
	return;
    }
    winPtr->flags &= ~TK_MAPPED;
    XUnmapWindow(winPtr->display, winPtr->window);
    if (!(winPtr->flags & TK_TOP_LEVEL)) {
	XEvent event;

	event.type = UnmapNotify;
	event.xunmap.serial = LastKnownRequestProcessed(winPtr->display);
	event.xunmap.send_event = False;
	event.xunmap.display = winPtr->display;
	event.xunmap.event = winPtr->window;
	event.xunmap.window = winPtr->window;
	event.xunmap.from_configure = False;
	Tk_HandleEvent(&event);
    }
}

void
Tk_ConfigureWindow(tkwin, valueMask, valuePtr)
    Tk_Window tkwin;		/* Window to re-configure. */
    unsigned int valueMask;	/* Mask indicating which parts of
				 * *valuePtr are to be used. */
    XWindowChanges *valuePtr;	/* New values. */
{
    register TkWindow *winPtr = (TkWindow *) tkwin;

    if (valueMask & CWX) {
	winPtr->changes.x = valuePtr->x;
    }
    if (valueMask & CWY) {
	winPtr->changes.y = valuePtr->y;
    }
    if (valueMask & CWWidth) {
	winPtr->changes.width = valuePtr->width;
    }
    if (valueMask & CWHeight) {
	winPtr->changes.height = valuePtr->height;
    }
    if (valueMask & CWBorderWidth) {
	winPtr->changes.border_width = valuePtr->border_width;
    }
    if (valueMask & CWSibling) {
	winPtr->changes.sibling = valuePtr->sibling;
    }
    if (valueMask & CWStackMode) {
	winPtr->changes.stack_mode = valuePtr->stack_mode;
    }

    if (winPtr->window != None) {
	XConfigureWindow(winPtr->display, winPtr->window,
		valueMask, valuePtr);
	if (!(winPtr->flags & TK_TOP_LEVEL)) {
	    DoConfigureNotify(winPtr);
	}
    } else {
	winPtr->dirtyChanges |= valueMask;
	winPtr->flags |= TK_NEED_CONFIG_NOTIFY;
    }
}

void
Tk_MoveWindow(tkwin, x, y)
    Tk_Window tkwin;		/* Window to move. */
    int x, y;			/* New location for window (within
				 * parent). */
{
    register TkWindow *winPtr = (TkWindow *) tkwin;

    winPtr->changes.x = x;
    winPtr->changes.y = y;
    if (winPtr->window != None) {
	XMoveWindow(winPtr->display, winPtr->window, x, y);
	if (!(winPtr->flags & TK_TOP_LEVEL)) {
	    DoConfigureNotify(winPtr);
	}
    } else {
	winPtr->dirtyChanges |= CWX|CWY;
	winPtr->flags |= TK_NEED_CONFIG_NOTIFY;
    }
}

void
Tk_ResizeWindow(tkwin, width, height)
    Tk_Window tkwin;		/* Window to resize. */
    unsigned int width, height;	/* New dimensions for window. */
{
    register TkWindow *winPtr = (TkWindow *) tkwin;

    winPtr->changes.width = width;
    winPtr->changes.height = height;
    if (winPtr->window != None) {
	XResizeWindow(winPtr->display, winPtr->window, width, height);
	if (!(winPtr->flags & TK_TOP_LEVEL)) {
	    DoConfigureNotify(winPtr);
	}
    } else {
	winPtr->dirtyChanges |= CWWidth|CWHeight;
	winPtr->flags |= TK_NEED_CONFIG_NOTIFY;
    }
}

void
Tk_MoveResizeWindow(tkwin, x, y, width, height)
    Tk_Window tkwin;		/* Window to move and resize. */
    int x, y;			/* New location for window (within
				 * parent). */
    unsigned int width, height;	/* New dimensions for window. */
{
    register TkWindow *winPtr = (TkWindow *) tkwin;

    winPtr->changes.x = x;
    winPtr->changes.y = y;
    winPtr->changes.width = width;
    winPtr->changes.height = height;
    if (winPtr->window != None) {
	XMoveResizeWindow(winPtr->display, winPtr->window,
		x, y, width, height);
	if (!(winPtr->flags & TK_TOP_LEVEL)) {
	    DoConfigureNotify(winPtr);
	}
    } else {
	winPtr->dirtyChanges |= CWX|CWY|CWWidth|CWHeight;
	winPtr->flags |= TK_NEED_CONFIG_NOTIFY;
    }
}

void
Tk_SetWindowBorderWidth(tkwin, width)
    Tk_Window tkwin;		/* Window to modify. */
    int width;			/* New border width for window. */
{
    register TkWindow *winPtr = (TkWindow *) tkwin;

    winPtr->changes.border_width = width;
    if (winPtr->window != None) {
	XSetWindowBorderWidth(winPtr->display, winPtr->window, width);
	if (!(winPtr->flags & TK_TOP_LEVEL)) {
	    DoConfigureNotify(winPtr);
	}
    } else {
	winPtr->dirtyChanges |= CWBorderWidth;
	winPtr->flags |= TK_NEED_CONFIG_NOTIFY;
    }
}

void
Tk_ChangeWindowAttributes(tkwin, valueMask, attsPtr)
    Tk_Window tkwin;		/* Window to manipulate. */
    unsigned long valueMask;	/* OR'ed combination of bits,
				 * indicating which fields of
				 * *attsPtr are to be used. */
    register XSetWindowAttributes *attsPtr;
				/* New values for some attributes. */
{
    register TkWindow *winPtr = (TkWindow *) tkwin;

    if (valueMask & CWBackPixmap) {
	winPtr->atts.background_pixmap = attsPtr->background_pixmap;
    }
    if (valueMask & CWBackPixel) {
	winPtr->atts.background_pixel = attsPtr->background_pixel;
    }
    if (valueMask & CWBorderPixmap) {
	winPtr->atts.border_pixmap = attsPtr->border_pixmap;
    }
    if (valueMask & CWBorderPixel) {
	winPtr->atts.border_pixel = attsPtr->border_pixel;
    }
    if (valueMask & CWBitGravity) {
	winPtr->atts.bit_gravity = attsPtr->bit_gravity;
    }
    if (valueMask & CWWinGravity) {
	winPtr->atts.win_gravity = attsPtr->win_gravity;
    }
    if (valueMask & CWBackingStore) {
	winPtr->atts.backing_store = attsPtr->backing_store;
    }
    if (valueMask & CWBackingPlanes) {
	winPtr->atts.backing_planes = attsPtr->backing_planes;
    }
    if (valueMask & CWBackingPixel) {
	winPtr->atts.backing_pixel = attsPtr->backing_pixel;
    }
    if (valueMask & CWOverrideRedirect) {
	winPtr->atts.override_redirect = attsPtr->override_redirect;
    }
    if (valueMask & CWSaveUnder) {
	winPtr->atts.save_under = attsPtr->save_under;
    }
    if (valueMask & CWEventMask) {
	winPtr->atts.event_mask = attsPtr->event_mask;
    }
    if (valueMask & CWDontPropagate) {
	winPtr->atts.do_not_propagate_mask
		= attsPtr->do_not_propagate_mask;
    }
    if (valueMask & CWColormap) {
	winPtr->atts.colormap = attsPtr->colormap;
    }
    if (valueMask & CWCursor) {
	winPtr->atts.cursor = attsPtr->cursor;
    }

    if (winPtr->window != None) {
	XChangeWindowAttributes(winPtr->display, winPtr->window,
		valueMask, attsPtr);
    } else {
	winPtr->dirtyAtts |= valueMask;
    }
}

void
Tk_SetWindowBackground(tkwin, pixel)
    Tk_Window tkwin;		/* Window to manipulate. */
    unsigned long pixel;	/* Pixel value to use for
				 * window's background. */
{
    register TkWindow *winPtr = (TkWindow *) tkwin;

    winPtr->atts.background_pixel = pixel;

    if (winPtr->window != None) {
	XSetWindowBackground(winPtr->display, winPtr->window, pixel);
    } else {
	winPtr->dirtyAtts = (winPtr->dirtyAtts & ~CWBackPixmap)
		| CWBackPixel;
    }
}

void
Tk_SetWindowBackgroundPixmap(tkwin, pixmap)
    Tk_Window tkwin;		/* Window to manipulate. */
    Pixmap pixmap;		/* Pixmap to use for window's
				 * background. */
{
    register TkWindow *winPtr = (TkWindow *) tkwin;

    winPtr->atts.background_pixmap = pixmap;

    if (winPtr->window != None) {
	XSetWindowBackgroundPixmap(winPtr->display,
		winPtr->window, pixmap);
    } else {
	winPtr->dirtyAtts = (winPtr->dirtyAtts & ~CWBackPixel)
		| CWBackPixmap;
    }
}

void
Tk_SetWindowBorder(tkwin, pixel)
    Tk_Window tkwin;		/* Window to manipulate. */
    unsigned long pixel;	/* Pixel value to use for
				 * window's border. */
{
    register TkWindow *winPtr = (TkWindow *) tkwin;

    winPtr->atts.border_pixel = pixel;

    if (winPtr->window != None) {
	XSetWindowBorder(winPtr->display, winPtr->window, pixel);
    } else {
	winPtr->dirtyAtts = (winPtr->dirtyAtts & ~CWBorderPixmap)
		| CWBorderPixel;
    }
}

void
Tk_SetWindowBorderPixmap(tkwin, pixmap)
    Tk_Window tkwin;		/* Window to manipulate. */
    Pixmap pixmap;		/* Pixmap to use for window's
				 * border. */
{
    register TkWindow *winPtr = (TkWindow *) tkwin;

    winPtr->atts.border_pixmap = pixmap;

    if (winPtr->window != None) {
	XSetWindowBorderPixmap(winPtr->display,
		winPtr->window, pixmap);
    } else {
	winPtr->dirtyAtts = (winPtr->dirtyAtts & ~CWBorderPixel)
		| CWBorderPixmap;
    }
}

void
Tk_DefineCursor(tkwin, cursor)
    Tk_Window tkwin;		/* Window to manipulate. */
    Cursor cursor;		/* Cursor to use for window (may be None). */
{
    register TkWindow *winPtr = (TkWindow *) tkwin;

    winPtr->atts.cursor = cursor;

    if (winPtr->window != None) {
	XDefineCursor(winPtr->display, winPtr->window, cursor);
    } else {
	winPtr->dirtyAtts = winPtr->dirtyAtts | CWCursor;
    }
}

void
Tk_UndefineCursor(tkwin)
    Tk_Window tkwin;		/* Window to manipulate. */
{
    Tk_DefineCursor(tkwin, None);
}

void
Tk_SetWindowColormap(tkwin, colormap)
    Tk_Window tkwin;		/* Window to manipulate. */
    Colormap colormap;		/* Colormap to use for window. */
{
    register TkWindow *winPtr = (TkWindow *) tkwin;

    winPtr->atts.colormap = colormap;

    if (winPtr->window != None) {
	XSetWindowColormap(winPtr->display, winPtr->window, colormap);
    } else {
	winPtr->dirtyAtts |= CWColormap;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Tk_SetWindowVisual --
 *
 *	This procedure is called to specify a visual to be used
 *	for a Tk window when it is created.  This procedure, if
 *	called at all, must be called before the X window is created
 *	(i.e. before Tk_MakeWindowExist is called).
 *
 * Results:
 *	The return value is 1 if successful, or 0 if the X window has
 *	been already created.
 *
 * Side effects:
 *	The information given is stored for when the window is created.
 *
 *----------------------------------------------------------------------
 */

int
Tk_SetWindowVisual(tkwin, visual, depth, colormap)
    Tk_Window tkwin;		/* Window to manipulate. */
    Visual *visual;		/* New visual for window. */
    unsigned int depth;		/* New depth for window. */
    Colormap colormap;		/* An appropriate colormap for the visual. */
{
    register TkWindow *winPtr = (TkWindow *) tkwin;

    if( winPtr->window != None ){
	/* Too late! */
	return 0;
    }

    winPtr->visual = visual;
    winPtr->depth = depth;
    winPtr->atts.colormap = colormap;

    /*
     * The following code is needed to make sure that the window doesn't
     * inherit the parent's border pixmap, which would result in a BadMatch
     * error.
     */

    if (!(winPtr->dirtyAtts & CWBorderPixmap)) {
	winPtr->dirtyAtts |= CWBorderPixel;
    }
    return 1;
}

/*
 *----------------------------------------------------------------------
 *
 * DoConfigureNotify --
 *
 *	Generate a ConfigureNotify event describing the current
 *	configuration of a window.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	An event is generated and processed by Tk_HandleEvent.
 *
 *----------------------------------------------------------------------
 */

static void
DoConfigureNotify(winPtr)
    register TkWindow *winPtr;		/* Window whose configuration
					 * was just changed. */
{
    XEvent event;

    event.type = ConfigureNotify;
    event.xconfigure.serial = LastKnownRequestProcessed(winPtr->display);
    event.xconfigure.send_event = False;
    event.xconfigure.display = winPtr->display;
    event.xconfigure.event = winPtr->window;
    event.xconfigure.window = winPtr->window;
    event.xconfigure.x = winPtr->changes.x;
    event.xconfigure.y = winPtr->changes.y;
    event.xconfigure.width = winPtr->changes.width;
    event.xconfigure.height = winPtr->changes.height;
    event.xconfigure.border_width = winPtr->changes.border_width;
    if (winPtr->changes.stack_mode == Above) {
	event.xconfigure.above = winPtr->changes.sibling;
    } else {
	event.xconfigure.above = None;
    }
    event.xconfigure.override_redirect = winPtr->atts.override_redirect;
    Tk_HandleEvent(&event);
}

/*
 *----------------------------------------------------------------------
 *
 * Tk_SetClass --
 *
 *	This procedure is used to give a window a class.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	A new class is stored for tkwin, replacing any existing
 *	class for it.
 *
 *----------------------------------------------------------------------
 */

void
Tk_SetClass(tkwin, className)
    Tk_Window tkwin;		/* Token for window to assign class. */
    char *className;		/* New class for tkwin. */
{
    register TkWindow *winPtr = (TkWindow *) tkwin;

    winPtr->classUid = Tk_GetUid(className);
    if (winPtr->flags & TK_TOP_LEVEL) {
	TkWmSetClass(winPtr);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Tk_NameToWindow --
 *
 *	Given a string name for a window, this procedure
 *	returns the token for the window, if there exists a
 *	window corresponding to the given name.
 *
 * Results:
 *	The return result is either a token for the window corresponding
 *	to "name", or else NULL to indicate that there is no such
 *	window.  In this case, an error message is left in interp->result.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

Tk_Window
Tk_NameToWindow(interp, pathName, tkwin)
    Tcl_Interp *interp;		/* Where to report errors. */
    char *pathName;		/* Path name of window. */
    Tk_Window tkwin;		/* Token for window:  name is assumed to
				 * belong to the same main window as tkwin. */
{
    Tcl_HashEntry *hPtr;

    hPtr = Tcl_FindHashEntry(&((TkWindow *) tkwin)->mainPtr->nameTable,
	    pathName);
    if (hPtr == NULL) {
	Tcl_AppendResult(interp, "bad window path name \"",
		pathName, "\"", (char *) NULL);
	return NULL;
    }
    return (Tk_Window) Tcl_GetHashValue(hPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Tk_DisplayName --
 *
 *	Return the textual name of a window's display.
 *
 * Results:
 *	The return value is the string name of the display associated
 *	with tkwin.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

char *
Tk_DisplayName(tkwin)
    Tk_Window tkwin;		/* Window whose display name is desired. */
{
    return ((TkWindow *) tkwin)->dispPtr->name;
}

/*
 *----------------------------------------------------------------------
 *
 * Tk_SetColorModel --
 *
 *	This procedure changes the current color model for a window
 *	(actually, for the window's screen).
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The color model for tkwin's screen is set to "model".
 *
 *----------------------------------------------------------------------
 */

void
Tk_SetColorModel(tkwin, model)
    Tk_Window tkwin;		/* Token for window;  this selects a screen
				 * whose color model is to be modified. */
    Tk_ColorModel model;	/* New model for tkwin's screen. */
{
    TkWindow *winPtr = (TkWindow *) tkwin;
    winPtr->dispPtr->colorModels[winPtr->screenNum] = model;
}

/*
 *----------------------------------------------------------------------
 *
 * Tk_GetColorModel --
 *
 *	This procedure returns the current color model for a window
 *	(actually, for the window's screen).
 *
 * Results:
 *	A color model.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

Tk_ColorModel
Tk_GetColorModel(tkwin)
    Tk_Window tkwin;		/* Token for window;  this selects a screen
				 * whose color model is returned. */
{
    TkWindow *winPtr = (TkWindow *) tkwin;
    return winPtr->dispPtr->colorModels[winPtr->screenNum];

}

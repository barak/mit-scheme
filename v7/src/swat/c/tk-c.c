/* -*- C -*-
/* tk-c.c - Support routines for Tk Widgets called from Scheme */
/* $Id: tk-c.c,v 1.1 1995/08/02 21:21:00 adams Exp $ */

/**********************************************************************
 This file contains the C code shared between MIT CScheme and DEC
 Scheme-To-C for interfacing to general TK things.   There are similar
 files for particular widgets, named things like "button-c.c".  The
 Scheme implementation specific interface files for this are tk-sc.sc,
 tk-c-mit.c, and tk-mit.scm.
**********************************************************************/

#include "tk.h"
#include  <tcl/tclInt.h>
#include  <tclHash.h>

/* structure for passing callbacks to the TK Shell */

typedef	struct {
    char	*name;  /* Name of command       */
    Tcl_CmdProc *proc;  /* Pointer to procedure  */
    ClientData  data;   /* Client data           */
} TKCallbacks, *TKCallbackPtr;

/* shell procedure declaration */

static void	TKShell
(
 Tk_Window,         /* Application main window */
 char *,	    /* Name of shell window    */
 char *,	    /* Class name              */
 TKCallbackPtr	    /* Array of callbacks      */
);

/* This procedure is registered with TCL under the name
   "SchemeCallBack".  TK widgets are given command lines of the form
   "-command SchemeCallBack n" where "n" is the object ID of the
   Scheme call back procedure.  Thus, when TK actually calls this
   procedure, it will pass as argv[1] the Scheme object ID (as a
   string), followed by any TK-supplied arguments.

   This procedure side-effects the C global variable TK_CallBack_List
   (in file tk-c-mit.c).  The value of this variable is tested in
   %tkOwnsEvent? to generate callbacks int Scheme.

   Tk_SchemeCallBack COPIES all of the arguments passed in, since I
   haven't the vaguest idea how TK handles garbage collection.
*/

static int NDigits(unsigned long N)
{ register Ans = 1;
  while (N > 9)
  { Ans += 1;
    N = N/10;
  }
  return Ans;
}

#define TK_CALLBACK_CHUNK_SIZE	256
static long Size_Of_TK_Callbacks = 0;

void Allocate_TK_Callback(long NChars)
{ /* Size_Of_TK_Callbacks will always be a multiple of              */
  /* TK_CALLBACK_CHUNK_SIZE.  It is the total number of bytes       */
  /* available, and includes space for the terminating null.        */
  /* NChars_In_TK_Callbacks, however, is the number of useful bytes */
  /* and does NOT include the terminating null byte.  NChars is the */
  /* number of bytes to be added to the current contents.           */

  extern char *TK_CallBack_List;
  extern long NChars_In_TK_Callbacks;

  Size_Of_TK_Callbacks = 
    (((NChars_In_TK_Callbacks+NChars)/TK_CALLBACK_CHUNK_SIZE)+1) *
      TK_CALLBACK_CHUNK_SIZE;
  if (NChars_In_TK_Callbacks == 0)
    TK_CallBack_List = malloc(Size_Of_TK_Callbacks);
  else
    TK_CallBack_List =
      (char *) realloc(TK_CallBack_List, Size_Of_TK_Callbacks);
  return;
}
    
extern void AddSchemeCallBack(int argc, char **argv, long *countv)
{ /* argc is the number of arguments to be transmitted.  They start at */
  /* argv[0].  This isn't the usual C convention, but it is more       */
  /* sensible.                                                         */
  extern char *TK_CallBack_List;
  extern long NChars_In_TK_Callbacks;
  register long ThisEntryLength = 0;
  register long i;
  register char **This;
  register long *Count;
  char *NextEntry;
  long NChars_To_Add;

  /* First, calculate how much space we need */
  for (i=0, Count=countv; i < argc; i++)
  { register long N = *Count++;
    ThisEntryLength += N + 2 + NDigits(N); /* 2 for < > */
  }
  NChars_To_Add =
    ThisEntryLength + 2 + NDigits(ThisEntryLength); /* 2 more for < > */
  if ((NChars_In_TK_Callbacks+NChars_To_Add+1) > Size_Of_TK_Callbacks)
    Allocate_TK_Callback(NChars_To_Add);
  NextEntry = &(TK_CallBack_List[NChars_In_TK_Callbacks]);
  NChars_In_TK_Callbacks += NChars_To_Add;
  /* And start putting in the information */
  NextEntry += sprintf(NextEntry, "<%d>", ThisEntryLength);
  for (i=0, This=argv, Count=countv; i < argc; i++, This++, Count++)
  { NextEntry += sprintf(NextEntry, "<%d>", *Count);
    memcpy(NextEntry, *This, *Count);
    NextEntry += *Count;
  }
  if (NextEntry != TK_CallBack_List+(NChars_In_TK_Callbacks))
    fprintf(stderr, "Tk_SchemeCallback %d %s\n",
	    NChars_In_TK_Callbacks, TK_CallBack_List);
  *NextEntry = '\0';		/* Null terminate the string */
  return;
}

int
Tk_TkError(ClientData clientData,
	   Tcl_Interp *interp,
	   int argc,
	   char **argv)
{ if (argc==2)
  { fprintf(stderr, "TCL Error: %s\n", argv[1]);
    fputs(Tcl_GetVar(interp, "errorInfo", 0), stderr);
  }
  else
    fprintf(stderr, "TCL Error with argc=%d!\n", argc);
  error_external_return();
}

int
Tk_SchemeCallBack(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window associated with
				 * interpreter. NOT USED. */
    Tcl_Interp *interp;		/* Current interpreter. NOT USED. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{ /* As usual, argv[0] is *NOT* used for anything! */
  long *Counts = (long *) malloc(argc*sizeof(long));
  register long i, *Count;
  register char **This;

  if (Counts == NULL)
  { fprintf(stderr, "Out of space in Tk_SchemeCallBack\n");
    exit (1);
  }
  for (i=1, This=argv+1, Count=Counts+1; i < argc; i++)
    *Count++ = strlen(*This++);
  AddSchemeCallBack(argc-1, argv+1, Counts+1);
  /* Deliberately not changing interp->result, 'cause the TCL manual */
  /* says we don't have to if we don't want to.                      */
  return TCL_OK;
}

/*
 * External Interface Routines
 */

int Scheme_TK_X_error_handler(ClientData D, XErrorEvent *E)
{ extern void Scheme_x_error_handler(Display *Disp, XErrorEvent *Event);

  fprintf(stderr, "Our Handler for %d 0x%x\n", D, E);
  Scheme_x_error_handler((Display *) D, E);
  return 0;
}

extern Tk_Window
InitTkApplication(Display *Disp, char *Name)
{ Tk_Window Result;
  extern Tk_Window
    Tk_CreateMainWindow_from_display(Tcl_Interp *interp,
				     Display *display,
				     char *baseName);
  Tcl_Interp *tclInterp = Tcl_CreateInterp();
/*
  static char initTCLCmd[] =
    "source /scheme/users/jmiller/uitk/tk/tcl/library/init.tcl;";
  static char initTKCmd[] =
    "source /scheme/users/jmiller/uitk/tk/library/tk.tcl";
  static char initEmacsCmd[] =
    "source /scheme/users/jmiller/uitk/tk/library/emacs.tcl";
*/

  static char initTCLCmd[] =   "source [info library]/init.tcl;";
  static char initTKCmd[] =    "source $tk_library/tk.tcl";
  static char initEmacsCmd[] = "source $tk_library/emacs.tcl";
    
  Result = Tk_CreateMainWindow_from_display(tclInterp, Disp, Name);
  if (Result == (Tk_Window) NULL)
    fprintf(stderr,
	    "Error from Tk_CreateMainWindow: %s\n"
	    , tclInterp->result);
  if (Tcl_Eval(tclInterp, initTCLCmd, 0, (char **) NULL) != TCL_OK)
  { char * msg = Tcl_GetVar(tclInterp, "errorInfo", TCL_GLOBAL_ONLY);
    if (msg == NULL) msg = tclInterp->result;
    fprintf(stderr, "%s\n", msg);
    return (Tk_Window) NULL;
  }
  /* This must be read for EVERY new main window, since it     */
  /* establishes bindings and so forth that use "." implicitly */
  if (Tcl_Eval(tclInterp, initTKCmd, 0, (char **) NULL) != TCL_OK)
  { char * msg = Tcl_GetVar(tclInterp, "errorInfo", TCL_GLOBAL_ONLY);
    if (msg == NULL) msg = tclInterp->result;
    fprintf(stderr, "%s\n", msg);
    return (Tk_Window) NULL;
  }
  if (Tcl_Eval(tclInterp, initEmacsCmd, 0, (char **) NULL) != TCL_OK)
  { char * msg = Tcl_GetVar(tclInterp, "errorInfo", TCL_GLOBAL_ONLY);
    if (msg == NULL) msg = tclInterp->result;
    fprintf(stderr, "%s\n", msg);
    return (Tk_Window) NULL;
  }
  Tcl_CreateCommand(tclInterp,
		    "SchemeCallBack",
		    Tk_SchemeCallBack,
		    (ClientData) 0 /* not used */,
		    (void (*)()) NULL);	/* Delete Procedure */
  Tcl_CreateCommand(tclInterp,
		    "tkerror",
		    Tk_TkError,
		    (ClientData) 0 /* not used */,
		    (void (*) ()) NULL); /* Delete Procedure */
  Tk_CreateErrorHandler(Disp, -1, -1, -1,
			Scheme_TK_X_error_handler, (ClientData) Disp);
  return Result;
}

/*
 * Process all pending Tk events, then return
 */

void
DoTkEvents ()
{ while (Tk_DoOneEvent (TK_DONT_WAIT|TK_TIMER_EVENTS|TK_IDLE_EVENTS) > 0)
  { /* fprintf(stderr, "Did TK Event"); */ }
}

/*  Access the Client Data for a command.  For widget commands,
 *  this is a pointer to the widget data structure.
 */

ClientData
GetCmdClientData (Tcl_Interp *interp, char *cmd)

{
    Tcl_HashEntry   *hPtr;

    hPtr = Tcl_FindHashEntry (&((Interp *)interp)->commandTable, cmd);
    return ((Command *) Tcl_GetHashValue (hPtr))->clientData;
}

/*  Window structure routines.
 *  These are Macros, so need a functional interface for Scheme
 */

Display *
tk_display (Tk_Window tkwin)

{
    return Tk_Display (tkwin);
}

Window
tk_windowid (Tk_Window tkwin)

{
    return Tk_WindowId (tkwin);
}

int
tk_width (Tk_Window tkwin)

{
    return Tk_Width (tkwin);
}

int
tk_height (Tk_Window tkwin)

{
    return Tk_Height (tkwin);
}

void
tk_set_width (Tk_Window tkwin, long W)
{ Tk_Width(tkwin) = W;
}

void
tk_set_height (Tk_Window tkwin, long H)
{ Tk_Height(tkwin) = H;
}

/*****************************************************************/
/* The following procedures OUGHT to be here, but they require   */
/* internal data structure from tkButton.c to work               */
/*                                                               */
/* void                                                          */
/* tk_map_widget (Button *button, Tk_Window tkMainWindow,        */
/*                char *name, Window xwindow, int x, int y)      */
/* Tk_Window                                                     */
/* tk_tkwin_widget (Button *button)                              */
/*****************************************************************/

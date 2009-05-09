/* Cover routines to make MIT/GNU Scheme primitives out of the procedures */
/* in button-c.c. */

#include "scheme.h"
#include "prims.h"
#include "ansidecl.h"
#include "X11/Xlib.h"

DEFINE_PRIMITIVE ("%tkDeleteDisplay", Prim_tk_delete_display, 1, 1, 0)
{ /* (%tkDeleteDisplay XDisplayNumber) */
  extern void tk_delete_display (Display *disp);
  PRIMITIVE_HEADER(1);
  tk_delete_display((Display *) arg_integer(1));
  PRIMITIVE_RETURN(UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%tkDestroyWidget", Prim_tk_destroy_widget, 1, 1, 0)
{ /* (%tkDestroyWidget tk-handle ) */
  extern void tk_destroy_widget(long /*Button **/ button);
  PRIMITIVE_HEADER(1);
  tk_destroy_widget((long /*Button **/) arg_integer(1));
  PRIMITIVE_RETURN(UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%tkMakeButton", Prim_tk_make_button, 2, 2, 0)
{ /* (%tkMakeButton ParentTKWindow name-string)
  */
  extern int *MakeButton(long /*Tk_Window*/ parent_window, char *name);
  PRIMITIVE_HEADER(2);
  PRIMITIVE_RETURN
    (long_to_integer
     ((long) MakeButton((long /*Tk_Window*/) arg_integer(1),
			STRING_ARG(2))));
}

DEFINE_PRIMITIVE ("%tkMakeCanvas", Prim_tk_make_canvas, 2, 2, 0)
{ /* (%tkMakeCanvas ParentTKWindow name-string)
  */
  extern int *MakeCanvas(long /*Tk_Window*/ parent_window, char *name);
  PRIMITIVE_HEADER(2);
  PRIMITIVE_RETURN
    (long_to_integer
     ((long) MakeCanvas((long /*Tk_Window*/) arg_integer(1),
			STRING_ARG(2))));
}

DEFINE_PRIMITIVE ("%tkMakeCheckButton", Prim_tk_make_check_button, 2, 2, 0)
{ /* (%tkMakeCheckButton ParentTKWindow name-string)
  */
  extern int *MakeCheckButton(long /*Tk_Window*/ parent_window, char *name);
  PRIMITIVE_HEADER(2);
  PRIMITIVE_RETURN
    (long_to_integer
     ((long) MakeCheckButton((long /*Tk_Window*/) arg_integer(1),
			     STRING_ARG(2))));
  
}

DEFINE_PRIMITIVE ("%tkMakeEntry", Prim_tk_make_entry, 2, 2, 0)
{ /* (%tkMakeEntry ParentTKWindow name-string)
  */
  extern int *MakeEntry(long /*Tk_Window*/ parent_window, char *name);
  PRIMITIVE_HEADER(2);
  PRIMITIVE_RETURN
    (long_to_integer
     ((long) MakeEntry((long /*Tk_Window*/) arg_integer(1),
		       STRING_ARG(2))));
}

DEFINE_PRIMITIVE ("%tkMakeLabel", Prim_tk_make_label, 2, 2, 0)
{ /* (%tkMakeLabel ParentTKWindow name-string)
  */
  extern int *MakeLabel(long /*Tk_Window*/ parent_window, char *name);
  PRIMITIVE_HEADER(2);
  PRIMITIVE_RETURN
    (long_to_integer
     ((long) MakeLabel((long /*Tk_Window*/) arg_integer(1),
		       STRING_ARG(2))));
}

DEFINE_PRIMITIVE ("%tkMakeListbox", Prim_tk_make_listbox, 2, 2, 0)
{ /* (%tkMakeListbox ParentTKWindow name-string)
  */
  extern int *MakeListbox(long /*Tk_Window*/ parent_window, char *name);
  PRIMITIVE_HEADER(2);
  PRIMITIVE_RETURN
    (long_to_integer
     ((long) MakeListbox((long /*Tk_Window*/) arg_integer(1),
			 STRING_ARG(2))));
}

DEFINE_PRIMITIVE ("%tkMakeMenu", Prim_tk_make_menu, 2, 2, 0)
{ /* (%tkMakeMenu ParentTKWindow name-string)
  */
  extern int *MakeMenu(long /*Tk_Window*/ parent_window, char *name);
  PRIMITIVE_HEADER(2);
  PRIMITIVE_RETURN
    (long_to_integer
     ((long) MakeMenu((long /*Tk_Window*/) arg_integer(1),
		       STRING_ARG(2))));

}

DEFINE_PRIMITIVE ("%tkMakeMenuButton", Prim_tk_make_menu_button, 2, 2, 0)
{ /* (%tkMakeMenuButton ParentTKWindow name-string)
  */
  extern int *MakeMenuButton(long /*Tk_Window*/ parent_window, char *name);
  PRIMITIVE_HEADER(2);
  PRIMITIVE_RETURN
    (long_to_integer
     ((long) MakeMenuButton((long /*Tk_Window*/) arg_integer(1),
			    STRING_ARG(2))));

}

DEFINE_PRIMITIVE ("%tkMakeMessage", Prim_tk_make_message, 2, 2, 0)
{ /* (%tkMakeMessage ParentTKWindow name-string)
  */
  extern int *MakeMessage(long /*Tk_Window*/ parent_window, char *name);
  PRIMITIVE_HEADER(2);
  PRIMITIVE_RETURN
    (long_to_integer
     ((long) MakeMessage((long /*Tk_Window*/) arg_integer(1),
			 STRING_ARG(2))));
}

DEFINE_PRIMITIVE ("%tkMakeRadioButton", Prim_tk_make_radio_button, 2, 2, 0)
{ /* (%tkMakeRadioButton ParentTKWindow name-string)
  */
  extern int *MakeRadioButton(long /*Tk_Window*/ parent_window, char *name);
  PRIMITIVE_HEADER(2);
  PRIMITIVE_RETURN
    (long_to_integer
     ((long) MakeRadioButton((long /*Tk_Window*/) arg_integer(1),
			     STRING_ARG(2))));
  
}

DEFINE_PRIMITIVE ("%tkMakeScale", Prim_tk_make_scale, 2, 2, 0)
{ /* (%tkMakeScale ParentTKWindow name-string)
  */
  extern int *MakeScale(long /*Tk_Window*/ parent_window, char *name);
  PRIMITIVE_HEADER(2);
  PRIMITIVE_RETURN
    (long_to_integer
     ((long) MakeScale((long /*Tk_Window*/) arg_integer(1),
		       STRING_ARG(2))));
}

DEFINE_PRIMITIVE ("%tkMakeScrollBar", Prim_tk_make_scrollbar, 2, 2, 0)
{ /* (%tkMakeScrollBar ParentTKWindow name-string)
  */
  extern int *MakeScrollBar(long /*Tk_Window*/ parent_window, char *name);
  PRIMITIVE_HEADER(2);
  PRIMITIVE_RETURN
    (long_to_integer
     ((long) MakeScrollBar((long /*Tk_Window*/) arg_integer(1),
			   STRING_ARG(2))));
}

DEFINE_PRIMITIVE ("%tkMakeText", Prim_tk_make_text, 2, 2, 0)
{ /* (%tkMakeText ParentTKWindow name-string)
  */
  extern int *MakeText(long /*Tk_Window*/ parent_window, char *name);
  PRIMITIVE_HEADER(2);
  PRIMITIVE_RETURN
    (long_to_integer
     ((long) MakeText((long /*Tk_Window*/) arg_integer(1),
		      STRING_ARG(2))));

}

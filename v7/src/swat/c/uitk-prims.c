#include "scheme.h"
#include "prims.h"

#define External_Primitive(fn_name)		\
   extern SCHEME_OBJECT EXFUN (fn_name, (void))

External_Primitive(Prim_tcl_eval);
External_Primitive(Prim_tk_completely_handles_event);
External_Primitive(Prim_tk_create_tl_window);
External_Primitive(Prim_tk_do_events);
External_Primitive(Prim_tk_drain);
External_Primitive(Prim_tk_generate_scheme_event);
External_Primitive(Prim_tk_init);
External_Primitive(Prim_tk_invoke);
External_Primitive(Prim_tk_kill_app);
External_Primitive(Prim_tk_manage_geom);
External_Primitive(Prim_tk_map_widget);
External_Primitive(Prim_tk_map_window);
External_Primitive(Prim_tk_move);
External_Primitive(Prim_tk_move_resize);
External_Primitive(Prim_tk_next_wakeup);
External_Primitive(Prim_tk_resize);
External_Primitive(Prim_tk_unmap_window);
External_Primitive(Prim_tk_win_req_height);
External_Primitive(Prim_tk_win_req_width);
External_Primitive(Prim_tk_widget_get_tkwin);
External_Primitive(Prim_tk_win_display);
External_Primitive(Prim_tk_win_is_mapped);
External_Primitive(Prim_tk_win_height);
External_Primitive(Prim_tk_win_width);
External_Primitive(Prim_tk_win_window);
External_Primitive(Prim_tk_win_x);
External_Primitive(Prim_tk_win_y);
External_Primitive(Prim_tk_win_name);
External_Primitive(Prim_tk_win_pathname);
External_Primitive(Prim_tk_delete_display);
External_Primitive(Prim_tk_destroy_widget);
External_Primitive(Prim_tk_make_button);
External_Primitive(Prim_tk_make_canvas);
External_Primitive(Prim_tk_make_check_button);
External_Primitive(Prim_tk_make_entry);
External_Primitive(Prim_tk_make_label);
External_Primitive(Prim_tk_make_listbox);
External_Primitive(Prim_tk_make_menu);
External_Primitive(Prim_tk_make_menu_button);
External_Primitive(Prim_tk_make_message);
External_Primitive(Prim_tk_make_radio_button);
External_Primitive(Prim_tk_make_scale);
External_Primitive(Prim_tk_make_scrollbar);
External_Primitive(Prim_tk_make_text);

extern char *EXFUN (dload_initialize_file, (void));

char *
  DEFUN_VOID (dload_initialize_file)
{ /* Primitives in tk-c-mit.c */
  declare_primitive ("%tclGlobalEval", Prim_tcl_eval, 2, 2, 0);
  declare_primitive ("%tkCompletelyHandlesEvent?",
		     Prim_tk_completely_handles_event, 1, 1, 0);
  declare_primitive ("%tkCreateTopLevelWindow",
		     Prim_tk_create_tl_window, 3, 3, 0);
  declare_primitive ("%tkDoEvents", Prim_tk_do_events, 0, 0, 0);
  declare_primitive ("%tkDrainCallBacks", Prim_tk_drain, 2, 2, 0);
  declare_primitive ("%tkGenerateSchemeEvent",
		     Prim_tk_generate_scheme_event, 2, 2, 0);
  declare_primitive ("%tkInit", Prim_tk_init, 2, 2, 0);
  declare_primitive ("%tkInvokeCommand", Prim_tk_invoke, 2, LEXPR, 0);
  declare_primitive ("%tkKillApplication", Prim_tk_kill_app, 1, 1, 0);
  declare_primitive ("%tkManageGeometry", Prim_tk_manage_geom, 2, 2, 0);
  declare_primitive ("%tkMapWidget", Prim_tk_map_widget, 6, 6, 0);
  declare_primitive ("%tkMapWindow", Prim_tk_map_window, 1, 1, 0);
  declare_primitive ("%tkMoveWindow", Prim_tk_move, 3, 3, 0);
  declare_primitive ("%tkMoveResizeWindow", Prim_tk_move_resize, 5, 5, 0);
  declare_primitive ("%tkNextWakeup", Prim_tk_next_wakeup, 0, 0, 0);
  declare_primitive ("%tkResizeWindow", Prim_tk_resize, 3, 3, 0);
  declare_primitive ("%tkUnmapWindow", Prim_tk_unmap_window, 1, 1, 0);
  declare_primitive ("%tkWinReqHeight", Prim_tk_win_req_height, 1, 1, 0);
  declare_primitive ("%tkWinReqWidth", Prim_tk_win_req_width, 1, 1, 0);
  declare_primitive ("%tkWidget.tkwin", Prim_tk_widget_get_tkwin, 1, 1, 0);
  declare_primitive ("%tkWinDisplay", Prim_tk_win_display, 1, 1, 0);
  declare_primitive ("%tkWinIsMapped?", Prim_tk_win_is_mapped, 1, 1, 0);
  declare_primitive ("%tkWinHeight", Prim_tk_win_height, 1, 1, 0);
  declare_primitive ("%tkWinWidth", Prim_tk_win_width, 1, 1, 0);
  declare_primitive ("%tkWinWindow", Prim_tk_win_window, 1, 1, 0);
  declare_primitive ("%tkWinX", Prim_tk_win_x, 1, 1, 0);
  declare_primitive ("%tkWinY", Prim_tk_win_y, 1, 1, 0);
  declare_primitive ("%tkWinName", Prim_tk_win_name, 1, 1, 0);
  declare_primitive ("%tkWinPathName", Prim_tk_win_pathname, 1, 1, 0);
  /* Primitive in widget-c-mit.c */
  declare_primitive ("%tkDeleteDisplay", Prim_tk_delete_display, 1, 1, 0);
  declare_primitive ("%tkDestroyWidget", Prim_tk_destroy_widget, 1, 1, 0);
  declare_primitive ("%tkMakeButton", Prim_tk_make_button, 2, 2, 0);
  declare_primitive ("%tkMakeCanvas", Prim_tk_make_canvas, 2, 2, 0);
  declare_primitive ("%tkMakeCheckButton", Prim_tk_make_check_button, 2, 2, 0);
  declare_primitive ("%tkMakeEntry", Prim_tk_make_entry, 2, 2, 0);
  declare_primitive ("%tkMakeLabel", Prim_tk_make_label, 2, 2, 0);
  declare_primitive ("%tkMakeListbox", Prim_tk_make_listbox, 2, 2, 0);
  declare_primitive ("%tkMakeMenu", Prim_tk_make_menu, 2, 2, 0);
  declare_primitive ("%tkMakeMenuButton", Prim_tk_make_menu_button, 2, 2, 0);
  declare_primitive ("%tkMakeMessage", Prim_tk_make_message, 2, 2, 0);
  declare_primitive ("%tkMakeRadioButton", Prim_tk_make_radio_button, 2, 2, 0);
  declare_primitive ("%tkMakeScale", Prim_tk_make_scale, 2, 2, 0);
  declare_primitive ("%tkMakeScrollBar", Prim_tk_make_scrollbar, 2, 2, 0);
  declare_primitive ("%tkMakeText", Prim_tk_make_text, 2, 2, 0);
  return "#UITK";
}

##
## $Id: 1efd96f95fe9557fe8dd2a7ded08af8dfd932216 $
##
## WARNING: This makefile is designed to be used only from the master
## makefile in the parent directory.

#_______________________________________________________________________
#
# How this makefile works.
#
# This makefile builds four libraries.  Two are based directly on
# Tk/Tcl.  The other two are an interface between Scheme, X and
# Tk/Tcl.
#
# The build process works by customizing an off-the-shelf version of tk
# (currently version 3.2).  The SWAT versions of some of the files are
# copied over the off-the-shelf version.  The customized version is
# compiled to produce the libtk.a and libtcl.a libraries.  These
# libraries are not acutally used but they ensure that all the targets
# that we are interested in are up to date.  Then we construct our own
# libraries (tk.sl and tcl.sl).
#
# The other libraries are more straight-forward.
#_______________________________________________________________________

# The location of the customized files.  The directory structure matches
# that of tk3.2 and is sparsely populated with customized files:

CUSTOM = tk3.2-custom

# Important: select a microcode source directory with the correct scheme
# object representation:

SCHEME_INCLUDE_DIRS =  -I/scheme/8.0/src/microcode

TK =        tk3.2
TCL =       $(TK)/tcl

#UITK_INCLUDE_DIRS = -I $(UITK) -I $(UITK_C) -I $(TK) -I $(TCL) $(SCHEME_INCLUDE_DIRS)
UITK_INCLUDE_DIRS = -I $(TK) -I $(TCL) $(SCHEME_INCLUDE_DIRS)
CFLAGS = -DMIT_SCHEME -O  -Ae -D_HPUX -I/usr/include/X11R5 +z

XLIB		= -L /usr/lib/X11R5 -lX11

# These library paths are overruled by the master makefile.

#TK_LIBRARY	= /scheme/8.0/700/swat/c/tk3.2/library
#TCL_LIBRARY	= /scheme/8.0/700/swat/c/tk3.2/tcl/library
TK_LIBRARY	= Built_incorrectly__TK_LIBRARY__not_specified
TCL_LIBRARY	= Built_incorrectly__TCL_LIBRARY__not_specified

TCL_GENERIC_OBJS = \
	$(TCL)/regexp.o     $(TCL)/tclAssem.o   $(TCL)/tclBasic.o \
	$(TCL)/tclCkalloc.o $(TCL)/tclCmdAH.o   $(TCL)/tclCmdIL.o \
	$(TCL)/tclCmdMZ.o   $(TCL)/tclExpr.o    $(TCL)/tclGet.o \
	$(TCL)/tclHash.o    $(TCL)/tclHistory.o $(TCL)/tclParse.o \
	$(TCL)/tclProc.o    $(TCL)/tclUtil.o    $(TCL)/tclVar.o

TCL_UNIX_OBJS = \
        $(TCL)/panic.o $(TCL)/tclEnv.o $(TCL)/tclGlob.o $(TCL)/tclUnixAZ.o \
	$(TCL)/tclUnixStr.o $(TCL)/tclUnixUtil.o 

TCL_OBJS = $(TCL_GENERIC_OBJS) $(TCL_UNIX_OBJS) MITScheme.o

TK_WIDGOBJS = \
	$(TK)/tkButton.o $(TK)/tkEntry.o    $(TK)/tkFrame.o $(TK)/tkListbox.o \
	$(TK)/tkMenu.o $(TK)/tkMenubutton.o $(TK)/tkMessage.o $(TK)/tkScale.o \
	$(TK)/tkScrollbar.o

TK_CANVOBJS = \
	$(TK)/tkCanvas.o   $(TK)/tkCanvArc.o  $(TK)/tkCanvBmap.o \
	$(TK)/tkCanvLine.o $(TK)/tkCanvPoly.o $(TK)/tkCanvPs.o \
	$(TK)/tkCanvText.o $(TK)/tkCanvWind.o $(TK)/tkRectOval.o $(TK)/tkTrig.o

TK_TEXTOBJS = $(TK)/tkText.o $(TK)/tkTextBTree.o $(TK)/tkTextDisp.o \
	$(TK)/tkTextIndex.o $(TK)/tkTextTag.o

TK_OBJS = \
	$(TK)/tk3d.o     $(TK)/tkArgv.o  $(TK)/tkAtom.o  $(TK)/tkBind.o \
	$(TK)/tkBitmap.o $(TK)/tkCmds.o  $(TK)/tkColor.o $(TK)/tkConfig.o \
	$(TK)/tkCursor.o $(TK)/tkError.o $(TK)/tkEvent.o $(TK)/tkFocus.o \
	$(TK)/tkFont.o   $(TK)/tkGet.o   $(TK)/tkGC.o    $(TK)/tkGeometry.o \
	$(TK)/tkGrab.o   $(TK)/tkOption.o $(TK)/tkPack.o $(TK)/tkPlace.o \
	$(TK)/tkPreserve.o $(TK)/tkSelect.o $(TK)/tkSend.o $(TK)/tkWindow.o \
	$(TK)/tkWm.o $(TK_WIDGOBJS) $(TK_CANVOBJS) $(TK_TEXTOBJS) MITScheme.o

UITK_OBJS = tk-c.o tk-c-mit.o widget-c.o widget-c-mit.o uitk-prims.o
SCXL_OBJS = scxl.o

# Contents of custom directory structure:

CUSTOM_SRC = \
	$(CUSTOM)/Makefile \
	$(CUSTOM)/tkEvent.c $(CUSTOM)/tkWindow.c \
	$(CUSTOM)/tcl/Makefile $(CUSTOM)/tcl/tclUnix.h \
	$(CUSTOM)/library/emacs.tcl

all:  scxl.sl tcl.sl tk.sl uitk.sl

scxl.sl: $(SCXL_OBJS)
	rm -f scxl.sl
	ld -b -o scxl.sl  $(SCXL_OBJS) $(XLIB) -lc

tcl.sl: $(TCL)/libtcl.a MITScheme.o
	rm -f tcl.sl
	ld -b -o tcl.sl  $(TCL_OBJS) -lc

tk.sl: $(TK)/libtk.a MITScheme.o
	rm -f tk.sl
	ld -b -o tk.sl  $(TK_OBJS) $(XLIB) -lm -lc

uitk.sl: $(UITK_OBJS)
	rm -f uitk.sl
	ld -b -o uitk.sl  $(UITK_OBJS) -lm -lc

scxl.o MITScheme.o uitk-prims.o:
	$(CC) $(CFLAGS) $(SCHEME_INCLUDE_DIRS) -c $*.c

MITScheme.o: MITScheme.c
scxl.o: scxl.c
uitk-prims.o: uitk-prims.c

widget-c.o widget-c-mit.o tk-c-mit.o: $(TK)/tkInt.h $(TK)/default.h
	$(CC) $(CFLAGS) $(UITK_INCLUDE_DIRS) -c $*.c

tk-c.o: tk-c.c $(TK)/tkInt.h $(TK)/default.h
	$(CC) $(CFLAGS) $(UITK_INCLUDE_DIRS) TK_LIBRARY=$(TK_LIBRARY) \
		TCL_LIBRARY=$(TCL_LIBRARY) -c $*.c
widegt-c.o: widget-c.c
widget-c-mit.o: widget-c-mit.c
tk-c-mit.o: tk-c-mit.c

#$(TCL_OBJS) $(TK_OBJS): customization
$(TCL)/libtcl.a $(TK)/libtk.a: customization

customization: $(CUSTOM_SRC)
	(cd $(TCL); ./config)
	# overwrite standard code with customized files
	cp -f -R tk3.2-custom/* tk3.2
	rm -f $(TK)/libtk.a $(TCL)/libtcl.a
	(cd tk3.2; \
	 make -f Makefile TCL_LIBRARY=$(TCL_LIBRARY) TK_LIBRARY=$(TK_LIBRARY) \
                 libtk.a tcl/libtcl.a \
	)
	date > customization



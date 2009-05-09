#
# This is a simplified Makefile for use in Tk distributions.  Before using
# it to compile Tk, you may wish to reset some of the following variables:
#
# TCL_DIR -		Name of directory holding tcl.h and tcl.a.
# XLIB -		If your Xlib library isn't in the standard place,
#			you can replace "-lX11" with the name of the file
#			containing your library archive.
# INSTALL_DIR -		Full path name of top-level directory where
#			information is installed.
# TK_LIBRARY -		Full path name of directory to contain scripts
#			and other library files used by Tk.  This value
#			is available to applications as the variable
#			$tk_library.  If the environment variable
#			TK_LIBRARY is defined by a user, it will override
#			the value specified in this Makefile.
# LIB_DIR -		Directory in which to install the archive libtcl.a
# BIN_DIR -		Directory in which to install executables such as wish.
# INCLUDE_DIR -		Directory in which to install header files.
# MANx_DIR -		Directories in which to install manual entries.
# RANLIB -		If you're using a System-V-based UNIX that doesn't
#			have ranlib, change this definition to "echo" or
#			something else harmless.
# SHELL -		Some versions of make (e.g. SGI's) use this variable
#			to determine which shell to use for executing
#			commands.
#

TCL_DIR		= tcl
XLIB		= -lX11
INSTALL_DIR	= /usr/local
LIB_DIR		= $(INSTALL_DIR)/lib
TK_LIBRARY	= $(INSTALL_DIR)/lib/tk
BIN_DIR		= $(INSTALL_DIR)/bin
INCLUDE_DIR	= $(INSTALL_DIR)/include
MAN1_DIR	= $(INSTALL_DIR)/man/man1
MAN3_DIR	= $(INSTALL_DIR)/man/man3
MANN_DIR	= $(INSTALL_DIR)/man/mann
RANLIB		= ranlib
SHELL		= /bin/sh

# ANSI-C procedure prototypes are turned on by default if supported
# by the compiler.  To turn them off, uncomment the following line:

# NP =		-DNO_PROTOTYPE

# To compile under OpenWindows, uncomment the following line:

# OW =		-I/usr/openwin/include -L/usr/openwin/lib
CC		= cc
CFLAGS		= -I. -I$(TCL_DIR) -O -DTK_LIBRARY=\"${TK_LIBRARY}\" +z \
	${NP} ${OW}

LIBS = libtk.a $(TCL_DIR)/libtcl.a

WIDGOBJS = tkButton.o tkEntry.o tkFrame.o tkListbox.o \
	tkMenu.o tkMenubutton.o tkMessage.o tkScale.o \
	tkScrollbar.o

CANVOBJS = tkCanvas.o tkCanvArc.o tkCanvBmap.o tkCanvLine.o \
	tkCanvPoly.o tkCanvPs.o tkCanvText.o tkCanvWind.o \
	tkRectOval.o tkTrig.o

TEXTOBJS = tkText.o tkTextBTree.o tkTextDisp.o tkTextIndex.o tkTextTag.o

OBJS = tk3d.o tkArgv.o tkAtom.o tkBind.o tkBitmap.o tkCmds.o \
	tkColor.o tkConfig.o tkCursor.o tkError.o tkEvent.o \
	tkFocus.o tkFont.o tkGet.o tkGC.o tkGeometry.o tkGrab.o \
        tkOption.o tkPack.o tkPlace.o tkPreserve.o tkSelect.o \
        tkSend.o tkWindow.o tkWm.o $(WIDGOBJS) \
	$(CANVOBJS) $(TEXTOBJS)

WIDGSRCS = tkButton.c tkEntry.c tkFrame.c tkListbox.c \
	tkMenu.c tkMenubutton.c tkMessage.c tkScale.c \
	tkScrollbar.c tkText.c tkTextBTree.c tkTextDisp.c \
	tkTextIndex.c

CANVSRCS = tkCanvas.c tkCanvArc.c tkCanvBmap.c tkCanvLine.c \
	tkCanvPoly.c tkCanvPs.c tkCanvText.c tkCanvWind.c \
	tkRectOval.c tkTrig.c

TEXTSRCS = tkText.c tkTextBTree.c tkTextDisp.c tkTextIndex.c tkTextTag.c

SRCS = tk3d.c tkArgv.c tkAtom.c tkBind.c tkBitmap.c tkCmds.c \
	tkColor.c tkConfig.c tkCursor.c tkError.c tkEvent.c \
	tkFocus.c tkFont.c tkGet.c tkGC.c tkGeometry.c tkGrab.c \
        tkOption.c tkPack.c tkPlace.c tkPreserve.c tkSelect.c \
        tkSend.c tkWindow.c tkWm.c $(WIDGSRCS) \
	$(CANVSRCS) $(TEXTSRCS)

all: libtk.a wish

wish: main.o $(LIBS)
	$(CC) $(CFLAGS) main.o $(LIBS) $(XLIB) -lm -o wish

libtk.a: $(OBJS)
	rm -f libtk.a
	ar cr libtk.a $(OBJS)
	$(RANLIB) libtk.a

$(TCL_DIR)/libtcl.a:
	cd $(TCL_DIR); $(MAKE) $(MFLAGS) TCL_LIBRARY=$(TCL_LIBRARY) libtcl.a

install: libtk.a wish $(TCL_DIR)/libtcl.a
	-if [ ! -d $(LIB_DIR) ] ; then mkdir -p $(LIB_DIR); fi
	-if [ ! -d $(INCLUDE_DIR) ] ; then mkdir -p $(INCLUDE_DIR); fi
	-if [ ! -d $(TK_LIBRARY) ] ; then mkdir -p $(TK_LIBRARY); fi
	-if [ ! -d $(BIN_DIR) ] ; then mkdir -p $(BIN_DIR); fi
	-if [ ! -d $(MAN1_DIR) ] ; then mkdir -p $(MAN1_DIR); fi
	-if [ ! -d $(MAN3_DIR) ] ; then mkdir -p $(MAN3_DIR); fi
	-if [ ! -d $(MANN_DIR) ] ; then mkdir -p $(MANN_DIR); fi
	rm -rf $(TK_LIBRARY)/*
	cp -r library/*.tcl library/tclIndex library/demos $(TK_LIBRARY)
	cp library/prolog.ps $(TK_LIBRARY)
	rm -f $(LIB_DIR)/libtk.a
	cp libtk.a $(LIB_DIR)
	$(RANLIB) $(LIB_DIR)/libtk.a
	rm -f $(BIN_DIR)/wish
	cp wish $(BIN_DIR)
	rm -f $(INCLUDE_DIR)/tk.h
	cp tk.h $(INCLUDE_DIR)
	cd doc; for i in *.1; \
	    do \
	    rm -f $(MAN1_DIR)/$$i; \
	    sed -e '/man\.macros/r man.macros' -e '/man\.macros/d' \
		    $$i > $(MAN1_DIR)/$$i; \
	    done; cd ..
	cd doc; for i in *.3; \
	    do \
	    rm -f $(MAN3_DIR)/$$i; \
	    sed -e '/man\.macros/r man.macros' -e '/man\.macros/d' \
		    $$i > $(MAN3_DIR)/$$i; \
	    done; cd ..
	cd doc; for i in *.n; \
	    do \
	    rm -f $(MANN_DIR)/$$i; \
	    sed -e '/man\.macros/r man.macros' -e '/man\.macros/d' \
		    $$i > $(MANN_DIR)/$$i; \
	    done; cd ..
	cd $(TCL_DIR); $(MAKE) $(MFLAGS) INSTALL_DIR=$(INSTALL_DIR) install

clean:
	rm -f $(OBJS) main.o libtk.a wish
	cd $(TCL_DIR); $(MAKE) $(MFLAGS) clean

$(OBJS): tk.h tkInt.h tkConfig.h
$(WIDGOBJS): default.h
$(CANVOBJS): default.h tkCanvas.h
$(TEXTOBJS): default.h tkText.h
main.o: tk.h tkInt.h

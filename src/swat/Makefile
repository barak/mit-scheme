##
## $Id: 7365311c5f6930c2c03bfaa3a6e84d63d3086ecc $
##
## 
##

#_______________________________________________________________________
#
# Installation configuration
#
# Where the installed version of SWAT lives

INSTALL_DIR=/scheme/8.0/700/lib/swat

# SWAT's own copies of tcl and tk.  These definitions are passed down to
# the compilation of Tk and Tcl and become hard paths in the tcl.sl
# and tk.sl libraries.  DO NOT point these to source directories: the
# installation cleans them out.

TCL_LIBRARY = $(INSTALL_DIR)/tcl_lib
TK_LIBRARY = $(INSTALL_DIR)/tk_lib

#_______________________________________________________________________

C_LIBRARIES = c/scxl.sl c/tk.sl c/tcl.sl c/uitk.sl

all:
	echo "No all target."
	echo "Only make install"
	exit 1

install: scheme/compiled
	(cd c; make TCL_LIBRARY=$(TCL_LIBRARY) TK_LIBRARY=$(TK_LIBRARY) all)
	# Remove dynload separatly first: will fail if SWAT is in use.
	rm -rf $(INSTALL_DIR)/dynload
	rm -rf $(INSTALL_DIR) $(TCL_LIBRARY) $(TK_LIBRARY)
	mkdir  $(INSTALL_DIR) $(INSTALL_DIR)/dynload \
               $(TCL_LIBRARY) $(TK_LIBRARY)
	cp -p $(C_LIBRARIES) $(INSTALL_DIR)/dynload
	(cd c/tk3.2/library;     cp -p *.tcl tclIndex prolog.ps  $(TK_LIBRARY))
	(cd c/tk3.2/tcl/library; cp -p *.tcl tclIndex  $(TCL_LIBRARY))
	(cd scheme; cp -p load.scm *.com *.bci demo-*.scm $(INSTALL_DIR))
	echo "Installation complete"


#$(C_LIBRARIES):


scheme/compiled:
	echo "Build Scheme subsystem  with scheme compiler"
	exit 1
	(cd scheme; \
	 echo '(load "swat.sf") (load "swat.cbf")' | $(SCHEME) -compiler \
	)

clean:

TAGS:
	etags scheme/*.scm c/*.c

## Process this file with automake to produce Makefile.in

SUBDIRS = ffi ref-manual sos user-manual

dist_man1_MANS = mit-scheme.1

EXTRA_DIST = index.html

# The manuals install under $(htmldir); this is the page that links them.
install-html-local:
	$(MKDIR_P) $(DESTDIR)$(htmldir)
	$(INSTALL_DATA) $(srcdir)/index.html $(DESTDIR)$(htmldir)/.

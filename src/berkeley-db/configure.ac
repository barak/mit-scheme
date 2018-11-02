AC_ARG_WITH([db-4],
    AS_HELP_STRING([--with-db-4],
	[Use Berkeley DB v4 library if available [[yes]]]))
: ${with_db_4='yes'}


dnl DB v4 support
if test "${with_db_4}" != no; then
    if test "${with_db_4}" != yes; then
	CPPFLAGS="${CPPFLAGS} -I${with_db_4}/include"
	LDFLAGS="${LDFLAGS} -L${with_db_4}/lib"
    fi
    AC_CHECK_HEADER([db.h],
	[
	AC_DEFINE([HAVE_DB_H], [1],
	    [Define to 1 if you have the <db.h> header file.])
	AC_MSG_CHECKING([for db_create in -ldb-4])
	save_LIBS=${LIBS}
	LIBS="${LIBS} -ldb-4"
	AC_LINK_IFELSE(
	    [AC_LANG_PROGRAM(
		[[#include <db.h>]],
		[[db_create (0, 0, 0)]])],
	    [
	    AC_MSG_RESULT([yes])
	    AC_DEFINE([HAVE_LIBDB_4], [1],
		[Define to 1 if you have the `db-4' library (-ldb-4).])
	    MODULE_LIBS="-ldb-4 ${MODULE_LIBS}"
	    MODULE_BASES="${MODULE_BASES} prdb4"
	    ],
	    [
	    AC_MSG_RESULT([no])
	    ])
	LIBS=${save_LIBS}
	])
fi

MIT_SCHEME_PROJECT=mit-scheme

AC_SUBST([MIT_SCHEME_PROJECT])

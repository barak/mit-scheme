#!/bin/sh
#
# $Id: c-bundle.sh,v 1.1 2007/04/04 05:08:19 riastradh Exp $
#
# Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
#     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
#     2005, 2006, 2007 Massachusetts Institute of Technology
#
# This file is part of MIT/GNU Scheme.
#
# MIT/GNU Scheme is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of the
# License, or (at your option) any later version.
#
# MIT/GNU Scheme is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with MIT/GNU Scheme; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

set -e

if [ ! $# -gt 2 ]; then
  echo "usage: ${0} TYPE SYSTEM FILES ..."
  exit 1
fi

TYPE="${1}"
SYSTEM="${2}"
shift 2

case "${TYPE}" in
  library | static)
    ;;
  *)
    echo "usage: ${0} TYPE SYSTEM FILES ..."
    echo "  TYPE must be \`library' or \`static'."
    exit 1
    ;;
esac

(grep '^DECLARE_COMPILED_CODE' "${@}"	&& \
 grep '^DECLARE_COMPILED_DATA' "${@}"	&& \
 grep '^DECLARE_DATA_OBJECT'   "${@}")	\
| sed -e 's/.*:/  /' >${SYSTEM}.h

cat <<EOF >"${SYSTEM}.c"

#define LIARC_IN_MICROCODE
#include "liarc.h"

#undef DECLARE_COMPILED_CODE
#undef DECLARE_COMPILED_DATA
#undef DECLARE_COMPILED_DATA_NS
#undef DECLARE_DATA_OBJECT

#define DECLARE_COMPILED_CODE(name, nentries, decl_code, code)		\\
extern int EXFUN (decl_code, (void));					\\
extern SCHEME_OBJECT * EXFUN (code, (SCHEME_OBJECT *, entry_count_t));

#define DECLARE_COMPILED_DATA(name, decl_data, data)			\\
extern int EXFUN (decl_data, (void));					\\
extern SCHEME_OBJECT * EXFUN (data, (entry_count_t));

#define DECLARE_COMPILED_DATA_NS(name, data)				\\
extern SCHEME_OBJECT * EXFUN (data, (entry_count_t));

#define DECLARE_DATA_OBJECT(name, data)					\\
extern SCHEME_OBJECT EXFUN (data, (void));

#include "${SYSTEM}.h"

#undef DECLARE_COMPILED_CODE
#undef DECLARE_COMPILED_DATA
#undef DECLARE_COMPILED_DATA_NS
#undef DECLARE_DATA_OBJECT
EOF

case "${TYPE}" in
  library)
    cat <<EOF >>"${SYSTEM}.c"

#define DECLARE_COMPILED_CODE(name, nentries, decl_code, code)		\\
  if (0 != (declare_compiled_code (name, nentries, decl_code, code)))	\\
    return ((char *) NULL);

#define DECLARE_COMPILED_DATA(name, decl_data, data)			\\
  if (0 != (declare_compiled_code (name, decl_data, data)))		\\
    return ((char *) NULL);

#define DECLARE_COMPILED_DATA_NS(name, data)				\\
  if (0 != (declare_compiled_data_ns (name, data)))			\\
    return ((char *) NULL);

#define DECLARE_DATA_OBJECT(name, data)					\\
  if (0 != (declare_data_object (name, data)))				\\
    return ((char *) NULL);

char *
DEFUN_VOID (dload_initialize_file)
{
#include "${SYSTEM}.h"
  return (0);
}
EOF
  ;;
  static)
    cat <<EOF >>"${SYSTEM}.c"

#define DECLARE_COMPILED_CODE(name, nentries, decl_code, code)		\\
  result = (declare_compiled_code (name, nentries, decl_code, code));	\\
  if (result != 0)							\\
    return (result);

#define DECLARE_COMPILED_DATA(name, decl_data, data)			\\
  result = (declare_compiled_data (name, decl_data, data));		\\
  if (result != 0)							\\
    return (result);

#define DECLARE_COMPILED_DATA_NS(name, data)				\\
  result = (declare_compiled_data_ns (name, data));			\\
  if (result != 0)							\\
    return (result);

#define DECLARE_DATA_OBJECT(name, data)					\\
  result = (declare_data_object (name, data));				\\
  if (result != 0)							\\
    return (result);

int
DEFUN_VOID (initialize_compiled_code_blocks)
{
  int result;
#include "${SYSTEM}.h"
  return (0);
}
EOF
  ;;
esac

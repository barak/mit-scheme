#!/bin/sh
#
# $Id: c-bundle.sh,v 1.6 2007/06/06 19:42:39 cph Exp $
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
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
# 02110-1301, USA.

set -e

usage ()
{
    echo "usage: ${0} AUXDIR TYPE SYSTEM FILES ..."
    echo "  TYPE must be \`library' or \`static'."
    exit 1
}

if [ ${#} -lt 4 ]; then
    usage
fi

AUXDIR=${1}
TYPE=${2}
SYSTEM=${3}
shift 3

GEN_NONCE=${AUXDIR}/gen-nonce
EXTRACT_DECLS=${AUXDIR}/extract-liarc-decls

"${EXTRACT_DECLS}" "${@}" > "${SYSTEM}.h"

cat <<EOF > "${SYSTEM}.c"

#define LIARC_IN_MICROCODE
#include "liarc.h"

#undef DECLARE_COMPILED_CODE
#undef DECLARE_COMPILED_DATA
#undef DECLARE_COMPILED_DATA_NS
#undef DECLARE_DATA_OBJECT

#define DECLARE_COMPILED_CODE(name, nentries, decl_code, code)		\\
extern liarc_decl_code_t decl_code;					\\
extern liarc_code_proc_t code;

#define DECLARE_COMPILED_DATA(name, decl_data, data)			\\
extern liarc_decl_data_t decl_data;					\\
extern liarc_data_proc_t data;

#define DECLARE_COMPILED_DATA_NS(name, data)				\\
extern liarc_data_proc_t data;

#define DECLARE_DATA_OBJECT(name, data)					\\
extern liarc_object_proc_t data;

#include "${SYSTEM}.h"

#undef DECLARE_COMPILED_CODE
#undef DECLARE_COMPILED_DATA
#undef DECLARE_COMPILED_DATA_NS
#undef DECLARE_DATA_OBJECT
EOF

case "${TYPE}" in
static)
    cat <<EOF >> "${SYSTEM}.c"

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
initialize_compiled_code_blocks (void)
{
  int result;
#include "${SYSTEM}.h"
  return (0);
}
EOF
    ;;
library)
    NONCE=`"${GEN_NONCE}" 8`
    cat <<EOF >> "${SYSTEM}.c"

#define DECLARE_COMPILED_CODE(name, nentries, decl_code, code)		\\
  if (0 != (declare_compiled_code (name, nentries, decl_code, code)))	\\
    return (0);

#define DECLARE_COMPILED_DATA(name, decl_data, data)			\\
  if (0 != (declare_compiled_code (name, decl_data, data)))		\\
    return (0);

#define DECLARE_COMPILED_DATA_NS(name, data)				\\
  if (0 != (declare_compiled_data_ns (name, data)))			\\
    return (0);

#define DECLARE_DATA_OBJECT(name, data)					\\
  if (0 != (declare_data_object (name, data)))				\\
    return (0);

char *
dload_initialize_file (void)
{
#include "${SYSTEM}.h"
  return (0);
}

const char dload_nonce [] = "${NONCE}";
EOF
    ;;
*)
    usage
    ;;
esac

/* -*-C-*-

$Id: 77ff59b7391ae51ab55c130ed5c9af0855440490 $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

*/

/* Interface to PostgreSQL libpq library */

#include "scheme.h"
#include "prims.h"
#include "usrdef.h"
#include "os.h"

#ifdef HAVE_LIBPQ_FE_H
#  include <libpq-fe.h>
#endif

#define ARG_CONN(n) ((PGconn *) (arg_ulong_integer (n)))
#define ARG_RESULT(n) ((PGresult *) (arg_ulong_integer (n)))
#define ARG_EXEC_STATUS(n) ((ExecStatusType) (arg_ulong_integer (n)))

#define ANY_TO_UINT(x) (ulong_to_integer ((unsigned long) (x)))
#define ANY_TO_UNSPECIFIC(x) ((x), UNSPECIFIC)

#define ONE_ARG(get_arg, fn, cvt)					\
{									\
  PRIMITIVE_HEADER (1);							\
  PRIMITIVE_RETURN (cvt (fn (get_arg (1))));				\
}

#define STRING_TO_UINT(fn) ONE_ARG (STRING_ARG, fn, ANY_TO_UINT)

#define CONN_TO_UINT(fn) ONE_ARG (ARG_CONN, fn, ANY_TO_UINT)
#define CONN_TO_INT(fn) ONE_ARG (ARG_CONN, fn, long_to_integer)
#define CONN_TO_UNSPECIFIC(fn) ONE_ARG (ARG_CONN, fn, ANY_TO_UNSPECIFIC)
#define CONN_TO_STRING(fn) ONE_ARG (ARG_CONN, fn, char_pointer_to_string)

#define RESULT_TO_UINT(fn) ONE_ARG (ARG_RESULT, fn, ANY_TO_UINT)
#define RESULT_TO_INT(fn) ONE_ARG (ARG_RESULT, fn, long_to_integer)
#define RESULT_TO_UNSPECIFIC(fn) ONE_ARG (ARG_RESULT, fn, ANY_TO_UNSPECIFIC)
#define RESULT_TO_STRING(fn) ONE_ARG (ARG_RESULT, fn, char_pointer_to_string)

DEFINE_PRIMITIVE ("PQ-CONNECT-DB", Prim_pq_connect_db, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (2, WEAK_PAIR_P);
  SET_PAIR_CDR ((ARG_REF (2)), (ANY_TO_UINT (PQconnectdb (STRING_ARG (1)))));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("PQ-CONNECT-START", Prim_pq_connect_start, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (2, WEAK_PAIR_P);
  SET_PAIR_CDR ((ARG_REF (2)),
		(ANY_TO_UINT (PQconnectStart (STRING_ARG (1)))));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("PQ-CONNECT-POLL", Prim_pq_connect_poll, 1, 1, 0)
  CONN_TO_UINT (PQconnectPoll)

DEFINE_PRIMITIVE ("PQ-STATUS", Prim_pq_status, 1, 1, 0)
  CONN_TO_UINT (PQstatus)

DEFINE_PRIMITIVE ("PQ-FINISH", Prim_pq_finish, 1, 1, 0)
  CONN_TO_UNSPECIFIC (PQfinish)

DEFINE_PRIMITIVE ("PQ-RESET", Prim_pq_reset, 1, 1, 0)
  CONN_TO_UNSPECIFIC (PQreset)

DEFINE_PRIMITIVE ("PQ-RESET-START", Prim_pq_reset_start, 1, 1, 0)
  CONN_TO_INT (PQresetStart)

DEFINE_PRIMITIVE ("PQ-RESET-POLL", Prim_pq_reset_poll, 1, 1, 0)
  CONN_TO_UINT (PQresetPoll)

DEFINE_PRIMITIVE ("PQ-DB", Prim_pq_db, 1, 1, 0)
  CONN_TO_STRING (PQdb)

DEFINE_PRIMITIVE ("PQ-USER", Prim_pq_user, 1, 1, 0)
  CONN_TO_STRING (PQuser)

DEFINE_PRIMITIVE ("PQ-PASS", Prim_pq_pass, 1, 1, 0)
  CONN_TO_STRING (PQpass)

DEFINE_PRIMITIVE ("PQ-HOST", Prim_pq_host, 1, 1, 0)
  CONN_TO_STRING (PQhost)

DEFINE_PRIMITIVE ("PQ-PORT", Prim_pq_port, 1, 1, 0)
  CONN_TO_STRING (PQport)

DEFINE_PRIMITIVE ("PQ-TTY", Prim_pq_tty, 1, 1, 0)
  CONN_TO_STRING (PQtty)

DEFINE_PRIMITIVE ("PQ-OPTIONS", Prim_pq_options, 1, 1, 0)
  CONN_TO_STRING (PQoptions)

DEFINE_PRIMITIVE ("PQ-ERROR-MESSAGE", Prim_pq_error_message, 1, 1, 0)
  CONN_TO_STRING (PQerrorMessage)

DEFINE_PRIMITIVE ("PQ-EXEC", Prim_pq_exec, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  CHECK_ARG (3, WEAK_PAIR_P);
  SET_PAIR_CDR ((ARG_REF (3)),
		(ANY_TO_UINT (PQexec ((ARG_CONN (1)), (STRING_ARG (2))))));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("PQ-MAKE-EMPTY-PG-RESULT", Prim_pq_make_empty_pg_result,
		  3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  CHECK_ARG (3, WEAK_PAIR_P);
  SET_PAIR_CDR ((ARG_REF (3)),
		(ANY_TO_UINT (PQmakeEmptyPGresult ((ARG_CONN (1)),
						   (ARG_EXEC_STATUS (1))))));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("PQ-RESULT-STATUS", Prim_pq_result_status, 1, 1, 0)
  RESULT_TO_UINT (PQresultStatus)

DEFINE_PRIMITIVE ("PQ-RES-STATUS", Prim_pq_res_status, 1, 1, 0)
  ONE_ARG (ARG_EXEC_STATUS, PQresStatus, char_pointer_to_string)

DEFINE_PRIMITIVE ("PQ-RESULT-ERROR-MESSAGE", Prim_pq_result_error_message,
		  1, 1, 0)
  RESULT_TO_STRING (PQresultErrorMessage)

DEFINE_PRIMITIVE ("PQ-CLEAR", Prim_pq_clear, 1, 1, 0)
  RESULT_TO_UNSPECIFIC (PQclear)

DEFINE_PRIMITIVE ("PQ-N-TUPLES", Prim_pq_n_tuples, 1, 1, 0)
  RESULT_TO_INT (PQntuples)

DEFINE_PRIMITIVE ("PQ-N-FIELDS", Prim_pq_n_fields, 1, 1, 0)
  RESULT_TO_INT (PQnfields)

DEFINE_PRIMITIVE ("PQ-FIELD-NAME", Prim_pq_fname, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (char_pointer_to_string (PQfname ((ARG_RESULT (1)),
				      (arg_integer (2)))));
}

DEFINE_PRIMITIVE ("PQ-GET-VALUE", Prim_pq_get_value, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  PRIMITIVE_RETURN
    (char_pointer_to_string (PQgetvalue ((ARG_RESULT (1)),
					 (arg_integer (2)),
					 (arg_integer (3)))));
}

DEFINE_PRIMITIVE ("PQ-GET-IS-NULL?", Prim_pq_get_is_null, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT (PQgetisnull ((ARG_RESULT (1)),
				     (arg_integer (2)),
				     (arg_integer (3)))));
}

DEFINE_PRIMITIVE ("PQ-CMD-STATUS", Prim_pq_cmd_status, 1, 1, 0)
  RESULT_TO_STRING (PQcmdStatus)

DEFINE_PRIMITIVE ("PQ-CMD-TUPLES", Prim_pq_cmd_tuples, 1, 1, 0)
  RESULT_TO_STRING (PQcmdTuples)

DEFINE_PRIMITIVE ("PQ-GET-LINE", Prim_pq_get_line, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (2, STRING_P);
  PRIMITIVE_RETURN
    (long_to_integer (PQgetline ((ARG_CONN (1)),
				 (STRING_POINTER (ARG_REF (2))),
				 (STRING_LENGTH (ARG_REF (2))))));
}

DEFINE_PRIMITIVE ("PQ-PUT-LINE", Prim_pq_put_line, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (2, STRING_P);
  PRIMITIVE_RETURN
    (long_to_integer (PQputnbytes ((ARG_CONN (1)),
				   (STRING_POINTER (ARG_REF (2))),
				   (STRING_LENGTH (ARG_REF (2))))));
}

DEFINE_PRIMITIVE ("PQ-END-COPY", Prim_pq_end_copy, 1, 1, 0)
  CONN_TO_INT (PQendcopy)

DEFINE_PRIMITIVE ("PQ-ESCAPE-STRING", Prim_pq_escape_string, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, STRING_P);
  PRIMITIVE_RETURN
    (ulong_to_integer (PQescapeString ((STRING_ARG (2)),
				       (STRING_POINTER (ARG_REF (1))),
				       (STRING_LENGTH (ARG_REF (1))))));
}

DEFINE_PRIMITIVE ("PQ-ESCAPE-BYTEA", Prim_pq_escape_bytea, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, STRING_P);
  {
    size_t escaped_length;
    unsigned char * escaped
      = (PQescapeBytea ((STRING_BYTE_PTR (ARG_REF (1))),
			(STRING_LENGTH (ARG_REF (1))),
			(&escaped_length)));
    SCHEME_OBJECT s = (memory_to_string ((escaped_length - 1), escaped));
    PQfreemem (escaped);
    PRIMITIVE_RETURN (s);
  }
}

DEFINE_PRIMITIVE ("PQ-UNESCAPE-BYTEA", Prim_pq_unescape_bytea, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    size_t unescaped_length;
    unsigned char * unescaped
      = (PQunescapeBytea (((unsigned char *) (STRING_ARG (1))),
			  (&unescaped_length)));
    if (unescaped == 0)
      error_bad_range_arg (1);
    {
      SCHEME_OBJECT s = (memory_to_string (unescaped_length, unescaped));
      PQfreemem (unescaped);
      PRIMITIVE_RETURN (s);
    }
  }
}

#ifdef COMPILE_AS_MODULE

char *
dload_initialize_file (void)
{
  declare_primitive ("PQ-CONNECT-DB", Prim_pq_connect_db, 2, 2, 0);
  declare_primitive ("PQ-CONNECT-START", Prim_pq_connect_start, 2, 2, 0);
  declare_primitive ("PQ-CONNECT-POLL", Prim_pq_connect_poll, 1, 1, 0);
  declare_primitive ("PQ-STATUS", Prim_pq_status, 1, 1, 0);
  declare_primitive ("PQ-FINISH", Prim_pq_finish, 1, 1, 0);
  declare_primitive ("PQ-RESET", Prim_pq_reset, 1, 1, 0);
  declare_primitive ("PQ-RESET-START", Prim_pq_reset_start, 1, 1, 0);
  declare_primitive ("PQ-RESET-POLL", Prim_pq_reset_poll, 1, 1, 0);
  declare_primitive ("PQ-DB", Prim_pq_db, 1, 1, 0);
  declare_primitive ("PQ-USER", Prim_pq_user, 1, 1, 0);
  declare_primitive ("PQ-PASS", Prim_pq_pass, 1, 1, 0);
  declare_primitive ("PQ-HOST", Prim_pq_host, 1, 1, 0);
  declare_primitive ("PQ-PORT", Prim_pq_port, 1, 1, 0);
  declare_primitive ("PQ-TTY", Prim_pq_tty, 1, 1, 0);
  declare_primitive ("PQ-OPTIONS", Prim_pq_options, 1, 1, 0);
  declare_primitive ("PQ-ERROR-MESSAGE", Prim_pq_error_message, 1, 1, 0);
  declare_primitive ("PQ-EXEC", Prim_pq_exec, 3, 3, 0);
  declare_primitive
    ("PQ-MAKE-EMPTY-PG-RESULT", Prim_pq_make_empty_pg_result, 3, 3, 0);
  declare_primitive ("PQ-RESULT-STATUS", Prim_pq_result_status, 1, 1, 0);
  declare_primitive ("PQ-RES-STATUS", Prim_pq_res_status, 1, 1, 0);
  declare_primitive
    ("PQ-RESULT-ERROR-MESSAGE", Prim_pq_result_error_message, 1, 1, 0);
  declare_primitive ("PQ-CLEAR", Prim_pq_clear, 1, 1, 0);
  declare_primitive ("PQ-N-TUPLES", Prim_pq_n_tuples, 1, 1, 0);
  declare_primitive ("PQ-N-FIELDS", Prim_pq_n_fields, 1, 1, 0);
  declare_primitive ("PQ-FIELD-NAME", Prim_pq_fname, 2, 2, 0);
  declare_primitive ("PQ-GET-VALUE", Prim_pq_get_value, 3, 3, 0);
  declare_primitive ("PQ-GET-IS-NULL?", Prim_pq_get_is_null, 3, 3, 0);
  declare_primitive ("PQ-CMD-STATUS", Prim_pq_cmd_status, 1, 1, 0);
  declare_primitive ("PQ-CMD-TUPLES", Prim_pq_cmd_tuples, 1, 1, 0);
  declare_primitive ("PQ-GET-LINE", Prim_pq_get_line, 2, 2, 0);
  declare_primitive ("PQ-PUT-LINE", Prim_pq_put_line, 2, 2, 0);
  declare_primitive ("PQ-END-COPY", Prim_pq_end_copy, 1, 1, 0);
  declare_primitive ("PQ-ESCAPE-STRING", Prim_pq_escape_string, 2, 2, 0);
  declare_primitive ("PQ-ESCAPE-BYTEA", Prim_pq_escape_bytea, 1, 1, 0);
  declare_primitive ("PQ-UNESCAPE-BYTEA", Prim_pq_unescape_bytea, 1, 1, 0);
  return ("#prpgsql");
}

#endif /* COMPILE_AS_MODULE */

/* -*-C-*-

$Id: prpgsql.c,v 1.2 2003/06/06 20:34:02 cph Exp $

Copyright 2003 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

*/

/* Interface to PostgreSQL libpq library */

#include "scheme.h"
#include "prims.h"
#include "usrdef.h"
#include "os.h"

#include <postgresql/libpq-fe.h>

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
  PRIMITIVE_RETURN (unspecific);
}

DEFINE_PRIMITIVE ("PQ-CONNECT-START", Prim_pq_connect_start, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (2, WEAK_PAIR_P);
  SET_PAIR_CDR ((ARG_REF (2)),
		(ANY_TO_UINT (PQconnectStart (STRING_ARG (1)))));
  PRIMITIVE_RETURN (unspecific);
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

DEFINE_PRIMITIVE ("PQ-EXEC", Prim_pq_exec, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN (ANY_TO_UINT (PQexec ((ARG_CONN (1)), (STRING_ARG (2)))));
}

DEFINE_PRIMITIVE ("PQ-MAKE-EMPTY-PG-RESULT", Prim_pq_make_empty_pg_result,
		  2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (ANY_TO_UINT (PQmakeEmptyPGresult ((ARG_CONN (1)),
				       (ARG_EXEC_STATUS (1)))));
}

DEFINE_PRIMITIVE ("PQ-RESULT-STATUS", Prim_pq_result_status, 1, 1, 0)
  RESULT_TO_UINT (PQresultStatus)

DEFINE_PRIMITIVE ("PQ-RES-STATUS", Prim_pq_res_status, 1, 1, 0)
  ONE_ARG (ARG_EXEC_STATUS, PQresStatus, ANY_TO_UINT)

DEFINE_PRIMITIVE ("PQ-RESULT-ERROR-MESSAGE", Prim_pq_result_error_message,
		  1, 1, 0)
  RESULT_TO_STRING (PQresultErrorMessage)

DEFINE_PRIMITIVE ("PQ-CLEAR", Prim_pq_clear, 1, 1, 0)
  RESULT_TO_UNSPECIFIC (PQclear)

DEFINE_PRIMITIVE ("PQ-ESCAPE-STRING", Prim_pq_escape_string, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, STRING_P);
  PRIMITIVE_RETURN
    (ulong_to_integer (PQescapeString ((STRING_ARG (2)),
				       (STRING_LOC ((ARG_REF (1)), 0)),
				       (STRING_LENGTH (ARG_REF (1))))));
}

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

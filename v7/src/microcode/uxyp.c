/* -*-C-*-

$Id: uxyp.c,v 1.1 1993/08/24 18:16:50 bal Exp $

Copyright (c) 1987-92 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. */

/* Interfacte to the Yellow Pages server */

#include "scheme.h"
#include "prims.h"
#include <stdio.h>
#include <malloc.h>
#include <ntl.h>
#include <rpc/rpc.h>
#include "yp.h"

#define YP_HOST "polar.lcs.mit.edu"

/*
 * Please do not edit this procedure.
 * It was generated using rpcgen.
 */

/* Default timeout can be changed using clnt_control() */
static struct timeval TIMEOUT = { 25, 0 };

char **
do_yp_frame_1(argp, clnt)
        char **argp;
        CLIENT *clnt;
{
        static char *res;

        bzero((char *)&res, sizeof(res));
        if (clnt_call(clnt, do_yp_frame, xdr_wrapstring, argp, xdr_wrapstring, &res, TIMEOUT) != RPC_SUCCESS) {
                return (NULL);
        }
        return (&res);
}

static int yp_debug = 0;
CLIENT *cl = NULL;

DEFINE_PRIMITIVE ("YELLOW-PAGES-LOOKUP", Prim_yellow_pages_lookup, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, STRING_P);
  {
    fast SCHEME_OBJECT string = (ARG_REF (1));
    unsigned char *c_string = STRING_LOC(string,0);
    unsigned char **result;
    
    /*
     * Do remote call
     */
    if(cl == NULL){
      cl = clnt_create(YP_HOST,yp_server,yp_server_version,"tcp");
      if(cl == NULL){
	clnt_pcreateerror(YP_HOST);
	return(NULL);
      }
    }
    result = (unsigned char **) do_yp_frame_1(&c_string,cl);
    PRIMITIVE_RETURN (char_pointer_to_string(*result));
  }
}

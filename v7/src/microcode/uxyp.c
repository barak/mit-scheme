/* -*-C-*-

$Id: uxyp.c,v 1.4 2002/11/20 19:46:16 cph Exp $

Copyright (c) 1987-1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

/* Interfacte to the Yellow Pages server */

#include "scheme.h"
#include "prims.h"
#include <stdio.h>
#include <malloc.h>
#include <ntl.h>
#include <rpc/rpc.h>
#include "uxyp.h"

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

/* -*- C -*- */

#include "liarc.h"

#undef DECLARE_COMPILED_CODE

#define DECLARE_COMPILED_CODE(name, decl, code) do			\
{									\
  extern void EXFUN (decl, (void));					\
  extern SCHEME_OBJECT * EXFUN (code, (SCHEME_OBJECT *));		\
  if ((declare_compiled_code (name, decl, code)) == 0)			\
    lose_big_1 ("DECLARE_COMPILED_CODE: duplicate tag", name);		\
} while (0)

extern void EXFUN (lose_big_1, (char *, char *));

void
DEFUN_VOID (initialize_compiled_code_blocks)
{
#include "compinit.h"
  return;
}

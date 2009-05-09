#include "scheme.h"
#include "prims.h"

extern char *EXFUN (dload_initialize_file, (void));

char *
  DEFUN_VOID (dload_initialize_file)
{ return "#NoMITSchemePrimitives";
}

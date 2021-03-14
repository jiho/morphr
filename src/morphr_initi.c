#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Fortran calls */
extern void F77_NAME(moments)(double *p, int *nr, int *nc, double *m, int *no);
extern void F77_NAME(moments_central)(double *p, int *nr, int *nc, double *xbar, double *ybar, double *m, int *no);

static const R_FortranMethodDef FortranEntries[] = {
  {"moments",         (DL_FUNC) &F77_NAME(moments),         5},
  {"moments_central", (DL_FUNC) &F77_NAME(moments_central), 7},
  {NULL, NULL, 0}
};

void R_init_morphr(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
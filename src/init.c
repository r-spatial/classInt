#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>


/* .Fortran calls */
extern void F77_NAME(fish)(void *, void *, void *, void *, void *, void *, void *, void *);

static const R_FortranMethodDef FortranEntries[] = {
    {"fish", (DL_FUNC) &F77_NAME(fish), 8},
    {NULL, NULL, 0}
};

void R_init_classInt(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

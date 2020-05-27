#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _RcppShapeDTW_kNNShapeDTWCpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _RcppShapeDTW_rcpp_hello_world();
extern SEXP _RcppShapeDTW_RcppAccumulatedCostMatrix(SEXP, SEXP);
extern SEXP _RcppShapeDTW_RcppasShapeDescriptor(SEXP, SEXP);
extern SEXP _RcppShapeDTW_RcppComplexDTWResults(SEXP, SEXP, SEXP, SEXP);
extern SEXP _RcppShapeDTW_RcppdistanceFromWarpingPaths(SEXP, SEXP, SEXP);
extern SEXP _RcppShapeDTW_RcppDistanceMatrices(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _RcppShapeDTW_RcppSimpleDTW(SEXP, SEXP);
extern SEXP _RcppShapeDTW_RcppsubsequencesMatrix(SEXP, SEXP);
extern SEXP _RcppShapeDTW_RcpptrigonometicTransform(SEXP, SEXP);
extern SEXP _RcppShapeDTW_RcppTSNormalization(SEXP, SEXP);
extern SEXP _RcppShapeDTW_RcpptsTransformation(SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_RcppShapeDTW_kNNShapeDTWCpp",               (DL_FUNC) &_RcppShapeDTW_kNNShapeDTWCpp,               10},
    {"_RcppShapeDTW_rcpp_hello_world",             (DL_FUNC) &_RcppShapeDTW_rcpp_hello_world,              0},
    {"_RcppShapeDTW_RcppAccumulatedCostMatrix",    (DL_FUNC) &_RcppShapeDTW_RcppAccumulatedCostMatrix,     2},
    {"_RcppShapeDTW_RcppasShapeDescriptor",        (DL_FUNC) &_RcppShapeDTW_RcppasShapeDescriptor,         2},
    {"_RcppShapeDTW_RcppComplexDTWResults",        (DL_FUNC) &_RcppShapeDTW_RcppComplexDTWResults,         4},
    {"_RcppShapeDTW_RcppdistanceFromWarpingPaths", (DL_FUNC) &_RcppShapeDTW_RcppdistanceFromWarpingPaths,  3},
    {"_RcppShapeDTW_RcppDistanceMatrices",         (DL_FUNC) &_RcppShapeDTW_RcppDistanceMatrices,          7},
    {"_RcppShapeDTW_RcppSimpleDTW",                (DL_FUNC) &_RcppShapeDTW_RcppSimpleDTW,                 2},
    {"_RcppShapeDTW_RcppsubsequencesMatrix",       (DL_FUNC) &_RcppShapeDTW_RcppsubsequencesMatrix,        2},
    {"_RcppShapeDTW_RcpptrigonometicTransform",    (DL_FUNC) &_RcppShapeDTW_RcpptrigonometicTransform,     2},
    {"_RcppShapeDTW_RcppTSNormalization",          (DL_FUNC) &_RcppShapeDTW_RcppTSNormalization,           2},
    {"_RcppShapeDTW_RcpptsTransformation",         (DL_FUNC) &_RcppShapeDTW_RcpptsTransformation,          5},
    {NULL, NULL, 0}
};

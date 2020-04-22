// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// RcppsubsequencesMatrix
NumericMatrix RcppsubsequencesMatrix(NumericVector values, int subsequenceWidth);
RcppExport SEXP _RcppShapeDTW_RcppsubsequencesMatrix(SEXP valuesSEXP, SEXP subsequenceWidthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type values(valuesSEXP);
    Rcpp::traits::input_parameter< int >::type subsequenceWidth(subsequenceWidthSEXP);
    rcpp_result_gen = Rcpp::wrap(RcppsubsequencesMatrix(values, subsequenceWidth));
    return rcpp_result_gen;
END_RCPP
}
// RcppasShapeDescriptor
NumericMatrix RcppasShapeDescriptor(NumericMatrix subsequenceSeries, S4 shapeDescriptorParams);
RcppExport SEXP _RcppShapeDTW_RcppasShapeDescriptor(SEXP subsequenceSeriesSEXP, SEXP shapeDescriptorParamsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type subsequenceSeries(subsequenceSeriesSEXP);
    Rcpp::traits::input_parameter< S4 >::type shapeDescriptorParams(shapeDescriptorParamsSEXP);
    rcpp_result_gen = Rcpp::wrap(RcppasShapeDescriptor(subsequenceSeries, shapeDescriptorParams));
    return rcpp_result_gen;
END_RCPP
}
// RcpptrigonometicTransform
NumericVector RcpptrigonometicTransform(NumericVector input, std::string transformType);
RcppExport SEXP _RcppShapeDTW_RcpptrigonometicTransform(SEXP inputSEXP, SEXP transformTypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type input(inputSEXP);
    Rcpp::traits::input_parameter< std::string >::type transformType(transformTypeSEXP);
    rcpp_result_gen = Rcpp::wrap(RcpptrigonometicTransform(input, transformType));
    return rcpp_result_gen;
END_RCPP
}
// RcppTSNormalization
NumericVector RcppTSNormalization(NumericVector input, std::string normType);
RcppExport SEXP _RcppShapeDTW_RcppTSNormalization(SEXP inputSEXP, SEXP normTypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type input(inputSEXP);
    Rcpp::traits::input_parameter< std::string >::type normType(normTypeSEXP);
    rcpp_result_gen = Rcpp::wrap(RcppTSNormalization(input, normType));
    return rcpp_result_gen;
END_RCPP
}
// RcpptsTransformation
List RcpptsTransformation(NumericMatrix timeSeries, S4 shapeDescriptorParams, int subsequenceWidth, std::string normalizationType, Rcpp::Nullable<S4> trigonometricTransformParams);
RcppExport SEXP _RcppShapeDTW_RcpptsTransformation(SEXP timeSeriesSEXP, SEXP shapeDescriptorParamsSEXP, SEXP subsequenceWidthSEXP, SEXP normalizationTypeSEXP, SEXP trigonometricTransformParamsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type timeSeries(timeSeriesSEXP);
    Rcpp::traits::input_parameter< S4 >::type shapeDescriptorParams(shapeDescriptorParamsSEXP);
    Rcpp::traits::input_parameter< int >::type subsequenceWidth(subsequenceWidthSEXP);
    Rcpp::traits::input_parameter< std::string >::type normalizationType(normalizationTypeSEXP);
    Rcpp::traits::input_parameter< Rcpp::Nullable<S4> >::type trigonometricTransformParams(trigonometricTransformParamsSEXP);
    rcpp_result_gen = Rcpp::wrap(RcpptsTransformation(timeSeries, shapeDescriptorParams, subsequenceWidth, normalizationType, trigonometricTransformParams));
    return rcpp_result_gen;
END_RCPP
}
// RcppDistanceMatrices
List RcppDistanceMatrices(NumericMatrix timeSeriesRef, NumericMatrix timeSeriesTest, S4 shapeDescriptorParams, int subsequenceWidth, std::string normalizationType, Rcpp::Nullable<S4> trigonometricTransformParams, std::string distanceType);
RcppExport SEXP _RcppShapeDTW_RcppDistanceMatrices(SEXP timeSeriesRefSEXP, SEXP timeSeriesTestSEXP, SEXP shapeDescriptorParamsSEXP, SEXP subsequenceWidthSEXP, SEXP normalizationTypeSEXP, SEXP trigonometricTransformParamsSEXP, SEXP distanceTypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type timeSeriesRef(timeSeriesRefSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type timeSeriesTest(timeSeriesTestSEXP);
    Rcpp::traits::input_parameter< S4 >::type shapeDescriptorParams(shapeDescriptorParamsSEXP);
    Rcpp::traits::input_parameter< int >::type subsequenceWidth(subsequenceWidthSEXP);
    Rcpp::traits::input_parameter< std::string >::type normalizationType(normalizationTypeSEXP);
    Rcpp::traits::input_parameter< Rcpp::Nullable<S4> >::type trigonometricTransformParams(trigonometricTransformParamsSEXP);
    Rcpp::traits::input_parameter< std::string >::type distanceType(distanceTypeSEXP);
    rcpp_result_gen = Rcpp::wrap(RcppDistanceMatrices(timeSeriesRef, timeSeriesTest, shapeDescriptorParams, subsequenceWidth, normalizationType, trigonometricTransformParams, distanceType));
    return rcpp_result_gen;
END_RCPP
}
// RcppAccumulatedCostMatrix
NumericMatrix RcppAccumulatedCostMatrix(NumericMatrix x);
RcppExport SEXP _RcppShapeDTW_RcppAccumulatedCostMatrix(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(RcppAccumulatedCostMatrix(x));
    return rcpp_result_gen;
END_RCPP
}
// RcppSimpleDTW
List RcppSimpleDTW(NumericMatrix x);
RcppExport SEXP _RcppShapeDTW_RcppSimpleDTW(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(RcppSimpleDTW(x));
    return rcpp_result_gen;
END_RCPP
}
// RcppdistanceFromWarpingPaths
double RcppdistanceFromWarpingPaths(NumericMatrix distMatrix, IntegerVector path1, IntegerVector path2);
RcppExport SEXP _RcppShapeDTW_RcppdistanceFromWarpingPaths(SEXP distMatrixSEXP, SEXP path1SEXP, SEXP path2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type distMatrix(distMatrixSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type path1(path1SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type path2(path2SEXP);
    rcpp_result_gen = Rcpp::wrap(RcppdistanceFromWarpingPaths(distMatrix, path1, path2));
    return rcpp_result_gen;
END_RCPP
}
// RcppComplexDTWResults
List RcppComplexDTWResults(ListOf<NumericMatrix> RawSeriesDistMat, ListOf<NumericMatrix> ShapeDescriptorMatrix, std::string DistanceType);
RcppExport SEXP _RcppShapeDTW_RcppComplexDTWResults(SEXP RawSeriesDistMatSEXP, SEXP ShapeDescriptorMatrixSEXP, SEXP DistanceTypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< ListOf<NumericMatrix> >::type RawSeriesDistMat(RawSeriesDistMatSEXP);
    Rcpp::traits::input_parameter< ListOf<NumericMatrix> >::type ShapeDescriptorMatrix(ShapeDescriptorMatrixSEXP);
    Rcpp::traits::input_parameter< std::string >::type DistanceType(DistanceTypeSEXP);
    rcpp_result_gen = Rcpp::wrap(RcppComplexDTWResults(RawSeriesDistMat, ShapeDescriptorMatrix, DistanceType));
    return rcpp_result_gen;
END_RCPP
}
// kNNShapeDTWCpp
List kNNShapeDTWCpp(NumericMatrix referenceSeries, NumericMatrix testSeries, int forecastHorizon, int subsequenceWidth, int subsequenceBreaks, S4 shapeDescriptorParams, std::string normalizationType, std::string distanceType, Rcpp::Nullable<S4> ttParams);
RcppExport SEXP _RcppShapeDTW_kNNShapeDTWCpp(SEXP referenceSeriesSEXP, SEXP testSeriesSEXP, SEXP forecastHorizonSEXP, SEXP subsequenceWidthSEXP, SEXP subsequenceBreaksSEXP, SEXP shapeDescriptorParamsSEXP, SEXP normalizationTypeSEXP, SEXP distanceTypeSEXP, SEXP ttParamsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type referenceSeries(referenceSeriesSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type testSeries(testSeriesSEXP);
    Rcpp::traits::input_parameter< int >::type forecastHorizon(forecastHorizonSEXP);
    Rcpp::traits::input_parameter< int >::type subsequenceWidth(subsequenceWidthSEXP);
    Rcpp::traits::input_parameter< int >::type subsequenceBreaks(subsequenceBreaksSEXP);
    Rcpp::traits::input_parameter< S4 >::type shapeDescriptorParams(shapeDescriptorParamsSEXP);
    Rcpp::traits::input_parameter< std::string >::type normalizationType(normalizationTypeSEXP);
    Rcpp::traits::input_parameter< std::string >::type distanceType(distanceTypeSEXP);
    Rcpp::traits::input_parameter< Rcpp::Nullable<S4> >::type ttParams(ttParamsSEXP);
    rcpp_result_gen = Rcpp::wrap(kNNShapeDTWCpp(referenceSeries, testSeries, forecastHorizon, subsequenceWidth, subsequenceBreaks, shapeDescriptorParams, normalizationType, distanceType, ttParams));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_RcppShapeDTW_RcppsubsequencesMatrix", (DL_FUNC) &_RcppShapeDTW_RcppsubsequencesMatrix, 2},
    {"_RcppShapeDTW_RcppasShapeDescriptor", (DL_FUNC) &_RcppShapeDTW_RcppasShapeDescriptor, 2},
    {"_RcppShapeDTW_RcpptrigonometicTransform", (DL_FUNC) &_RcppShapeDTW_RcpptrigonometicTransform, 2},
    {"_RcppShapeDTW_RcppTSNormalization", (DL_FUNC) &_RcppShapeDTW_RcppTSNormalization, 2},
    {"_RcppShapeDTW_RcpptsTransformation", (DL_FUNC) &_RcppShapeDTW_RcpptsTransformation, 5},
    {"_RcppShapeDTW_RcppDistanceMatrices", (DL_FUNC) &_RcppShapeDTW_RcppDistanceMatrices, 7},
    {"_RcppShapeDTW_RcppAccumulatedCostMatrix", (DL_FUNC) &_RcppShapeDTW_RcppAccumulatedCostMatrix, 1},
    {"_RcppShapeDTW_RcppSimpleDTW", (DL_FUNC) &_RcppShapeDTW_RcppSimpleDTW, 1},
    {"_RcppShapeDTW_RcppdistanceFromWarpingPaths", (DL_FUNC) &_RcppShapeDTW_RcppdistanceFromWarpingPaths, 3},
    {"_RcppShapeDTW_RcppComplexDTWResults", (DL_FUNC) &_RcppShapeDTW_RcppComplexDTWResults, 3},
    {"_RcppShapeDTW_kNNShapeDTWCpp", (DL_FUNC) &_RcppShapeDTW_kNNShapeDTWCpp, 9},
    {NULL, NULL, 0}
};

RcppExport void R_init_RcppShapeDTW(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
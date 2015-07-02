// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// doBilinear
NumericVector doBilinear(NumericMatrix xy, NumericMatrix x, NumericMatrix y, NumericMatrix v);
RcppExport SEXP raster_doBilinear(SEXP xySEXP, SEXP xSEXP, SEXP ySEXP, SEXP vSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type xy(xySEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type v(vSEXP);
    __result = Rcpp::wrap(doBilinear(xy, x, y, v));
    return __result;
END_RCPP
}
// doCellFromRowCol
NumericVector doCellFromRowCol(IntegerVector nrow, IntegerVector ncol, IntegerVector rownr, IntegerVector colnr);
RcppExport SEXP raster_doCellFromRowCol(SEXP nrowSEXP, SEXP ncolSEXP, SEXP rownrSEXP, SEXP colnrSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< IntegerVector >::type nrow(nrowSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type ncol(ncolSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type rownr(rownrSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type colnr(colnrSEXP);
    __result = Rcpp::wrap(doCellFromRowCol(nrow, ncol, rownr, colnr));
    return __result;
END_RCPP
}
// getPolygons
NumericMatrix getPolygons(NumericMatrix xyv, NumericVector res, int nodes);
RcppExport SEXP raster_getPolygons(SEXP xyvSEXP, SEXP resSEXP, SEXP nodesSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type xyv(xyvSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type res(resSEXP);
    Rcpp::traits::input_parameter< int >::type nodes(nodesSEXP);
    __result = Rcpp::wrap(getPolygons(xyv, res, nodes));
    return __result;
END_RCPP
}
// getMode
double getMode(NumericVector values, int ties);
RcppExport SEXP raster_getMode(SEXP valuesSEXP, SEXP tiesSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type values(valuesSEXP);
    Rcpp::traits::input_parameter< int >::type ties(tiesSEXP);
    __result = Rcpp::wrap(getMode(values, ties));
    return __result;
END_RCPP
}
// doSpmin
NumericVector doSpmin(NumericVector x, NumericVector y);
RcppExport SEXP raster_doSpmin(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    __result = Rcpp::wrap(doSpmin(x, y));
    return __result;
END_RCPP
}
// doSpmax
NumericVector doSpmax(NumericVector x, NumericVector y);
RcppExport SEXP raster_doSpmax(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    __result = Rcpp::wrap(doSpmax(x, y));
    return __result;
END_RCPP
}
// ppmin
NumericVector ppmin(NumericVector x, NumericVector y, bool narm);
RcppExport SEXP raster_ppmin(SEXP xSEXP, SEXP ySEXP, SEXP narmSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< bool >::type narm(narmSEXP);
    __result = Rcpp::wrap(ppmin(x, y, narm));
    return __result;
END_RCPP
}
// ppmax
NumericVector ppmax(NumericVector x, NumericVector y, bool narm);
RcppExport SEXP raster_ppmax(SEXP xSEXP, SEXP ySEXP, SEXP narmSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< bool >::type narm(narmSEXP);
    __result = Rcpp::wrap(ppmax(x, y, narm));
    return __result;
END_RCPP
}
// doRowMin
NumericVector doRowMin(NumericMatrix x, bool narm);
RcppExport SEXP raster_doRowMin(SEXP xSEXP, SEXP narmSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< bool >::type narm(narmSEXP);
    __result = Rcpp::wrap(doRowMin(x, narm));
    return __result;
END_RCPP
}
// doRowMax
NumericVector doRowMax(NumericMatrix x, bool narm);
RcppExport SEXP raster_doRowMax(SEXP xSEXP, SEXP narmSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< bool >::type narm(narmSEXP);
    __result = Rcpp::wrap(doRowMax(x, narm));
    return __result;
END_RCPP
}
// doCellFromXY
NumericVector doCellFromXY(int ncols, int nrows, double xmin, double xmax, double ymin, double ymax, NumericVector x, NumericVector y);
RcppExport SEXP raster_doCellFromXY(SEXP ncolsSEXP, SEXP nrowsSEXP, SEXP xminSEXP, SEXP xmaxSEXP, SEXP yminSEXP, SEXP ymaxSEXP, SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< int >::type ncols(ncolsSEXP);
    Rcpp::traits::input_parameter< int >::type nrows(nrowsSEXP);
    Rcpp::traits::input_parameter< double >::type xmin(xminSEXP);
    Rcpp::traits::input_parameter< double >::type xmax(xmaxSEXP);
    Rcpp::traits::input_parameter< double >::type ymin(yminSEXP);
    Rcpp::traits::input_parameter< double >::type ymax(ymaxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    __result = Rcpp::wrap(doCellFromXY(ncols, nrows, xmin, xmax, ymin, ymax, x, y));
    return __result;
END_RCPP
}
// doXYFromCell
NumericMatrix doXYFromCell(int ncols, int nrows, double xmin, double xmax, double ymin, double ymax, NumericVector cell);
RcppExport SEXP raster_doXYFromCell(SEXP ncolsSEXP, SEXP nrowsSEXP, SEXP xminSEXP, SEXP xmaxSEXP, SEXP yminSEXP, SEXP ymaxSEXP, SEXP cellSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< int >::type ncols(ncolsSEXP);
    Rcpp::traits::input_parameter< int >::type nrows(nrowsSEXP);
    Rcpp::traits::input_parameter< double >::type xmin(xminSEXP);
    Rcpp::traits::input_parameter< double >::type xmax(xmaxSEXP);
    Rcpp::traits::input_parameter< double >::type ymin(yminSEXP);
    Rcpp::traits::input_parameter< double >::type ymax(ymaxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type cell(cellSEXP);
    __result = Rcpp::wrap(doXYFromCell(ncols, nrows, xmin, xmax, ymin, ymax, cell));
    return __result;
END_RCPP
}
// doFourCellsFromXY
NumericMatrix doFourCellsFromXY(int ncols, int nrows, double xmin, double xmax, double ymin, double ymax, NumericMatrix xy, bool duplicates, bool isGlobalLonLat);
RcppExport SEXP raster_doFourCellsFromXY(SEXP ncolsSEXP, SEXP nrowsSEXP, SEXP xminSEXP, SEXP xmaxSEXP, SEXP yminSEXP, SEXP ymaxSEXP, SEXP xySEXP, SEXP duplicatesSEXP, SEXP isGlobalLonLatSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< int >::type ncols(ncolsSEXP);
    Rcpp::traits::input_parameter< int >::type nrows(nrowsSEXP);
    Rcpp::traits::input_parameter< double >::type xmin(xminSEXP);
    Rcpp::traits::input_parameter< double >::type xmax(xmaxSEXP);
    Rcpp::traits::input_parameter< double >::type ymin(yminSEXP);
    Rcpp::traits::input_parameter< double >::type ymax(ymaxSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type xy(xySEXP);
    Rcpp::traits::input_parameter< bool >::type duplicates(duplicatesSEXP);
    Rcpp::traits::input_parameter< bool >::type isGlobalLonLat(isGlobalLonLatSEXP);
    __result = Rcpp::wrap(doFourCellsFromXY(ncols, nrows, xmin, xmax, ymin, ymax, xy, duplicates, isGlobalLonLat));
    return __result;
END_RCPP
}

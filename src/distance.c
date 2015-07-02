/* Robert Hijmans, October 2011 */

#include <Rinternals.h>
#include "dist_util.h"
#include "util.h"



SEXP distanceToNearestPoint(SEXP d, SEXP p, SEXP geo) {

	R_len_t i, j;
    SEXP res;	
	double md, *xd, *xp, *xres;

	SEXP dim1 = getAttrib(d, R_DimSymbol);
	int n = INTEGER(dim1)[0];
	SEXP dim2 = getAttrib(p, R_DimSymbol);
	int m = INTEGER(dim2)[0];

	PROTECT(d = coerceVector(d, REALSXP));
	PROTECT(p = coerceVector(p, REALSXP));
	int lonlat = INTEGER(geo)[0];

	xd = REAL(d);
	xp = REAL(p);

	PROTECT( res = allocVector(REALSXP, n) );
	xres = REAL(res);
	
	double r = 6378137;
	
	if (lonlat) {
		for (i=0; i<n; i++) {
			xres[i] = distCos(xd[i], xd[i+n], xp[0], xp[m], r);
			for (j=1; j<m; j++) {
				md = distCos(xd[i], xd[i+n], xp[j], xp[j+m], r);
				if (md < xres[i]) {
					xres[i] = md;
				}
			}
		}
	} else {
		for (i=0; i<n; i++) {
			xres[i] = distPlane(xd[i], xd[i+n], xp[0], xp[m]) ;
			for (j=1; j<m; j++) {
				md = distPlane(xd[i], xd[i+n], xp[j], xp[j+m]) ;
				if (md < xres[i]) {
					xres[i] = md;
				}
			}
		}
	}
	UNPROTECT(3);
	return(res) ;
}




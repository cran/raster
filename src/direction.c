/* Robert Hijmans, October 2011 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <Rmath.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"
#include "dist_util.h"
#include "util.h"



double directionPlane(double x1, double y1, double x2, double y2, int degrees) {
	double a, b;
	a = y2 - y1;
	b = x2 - x1;
	a = atan2(b, a);
	a = mod(a, M_2PI) ;
	if (a < 0) {
		a = a + M_2PI;
	}
	if (degrees) {
		return( toDeg(a)) ;
	} else {
		return(a);
	}
}


double directionSphere(double lon1, double lat1, double lon2, double lat2, int degrees) {
	double dLon, y, x, azm;
	lat1 = toRad(lat1);
	lat2 = toRad(lat2);
	lon1 = toRad(lon1);
	lon2 = toRad(lon2);
	dLon = lon2 - lon1;
	y = sin(dLon)  * cos( lat2 ) ;
	x = cos( lat1 ) * sin( lat2) - sin( lat1 ) * cos( lat2 ) * cos(dLon) ;
	azm = atan2(y, x);
	if (azm < 0) {
		azm = azm + M_2PI;
	}
	if (degrees) {
		azm = toDeg(azm);
	}
	return(azm);
}



SEXP directionToNearestPoint(SEXP d, SEXP p, SEXP geo, SEXP degrees, SEXP fromto) {

	R_len_t i, j;
    SEXP res;	
	int pt;
	double md, *xd, *xp, *xres, dist;

	SEXP dim1 = getAttrib(d, R_DimSymbol);
	int n = INTEGER(dim1)[0];
	SEXP dim2 = getAttrib(p, R_DimSymbol);
	int m = INTEGER(dim2)[0];
	int deg = INTEGER(degrees)[0];

	PROTECT(d = coerceVector(d, REALSXP));
	PROTECT(p = coerceVector(p, REALSXP));
	int lonlat = INTEGER(geo)[0];
	int from = INTEGER(fromto)[0];

	xd = REAL(d);
	xp = REAL(p);

	PROTECT( res = allocVector(REALSXP, n) );
	xres = REAL(res);
	
	double r = 6378137;
	
	if (lonlat) {
		for (i=0; i<n; i++) {
			dist = distCos(xd[i], xd[i+n], xp[0], xp[m], r);
			pt = 0;
			for (j=1; j<m; j++) {
				md = distCos(xd[i], xd[i+n], xp[j], xp[j+m], r);
				if (md < dist) {
					dist = md;
					pt = j;
				}
			}
			if (dist == 0) {
				xres[i] = R_NaReal;
			} else {
				if (from) {
					xres[i] = directionSphere(xp[pt], xp[pt+m], xd[i], xd[i+n], deg);
				} else {
					xres[i] = directionSphere(xd[i], xd[i+n], xp[pt], xp[pt+m], deg);				
				}
			}
		}
		
	} else {
	
		for (i=0; i<n; i++) {
			dist = distPlane(xd[i], xd[i+n], xp[0], xp[m]) ;
			pt = 0;
			for (j=1; j<m; j++) {
				md = distPlane(xd[i], xd[i+n], xp[j], xp[j+m]) ;
				if (md < dist) {
					dist = md;
					pt = j;
				}
			}
			if (dist == 0) {
				xres[i] = R_NaReal;
			} else {
				if (from) {
					xres[i] = directionPlane(xp[pt], xp[pt+m], xd[i], xd[i+n], deg);
				} else {
					xres[i] = directionPlane(xd[i], xd[i+n], xp[pt], xp[pt+m], deg);
				}
			}
		}
	}
	
	UNPROTECT(3);
	return(res) ;
}




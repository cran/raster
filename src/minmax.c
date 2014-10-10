/* Robert Hijmans, October 2014 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <Rmath.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"


SEXP minmax(SEXP d, SEXP pars) {

	R_len_t i, j;
	
	SEXP val;
	int nrow, ncol, na, mnmx, q;
	double *xd, *xval, v;

	nrow = INTEGER(pars)[0];
	ncol = INTEGER(pars)[1];
	mnmx = INTEGER(pars)[2];
	na = INTEGER(pars)[3];
	
	PROTECT( d = coerceVector(d, REALSXP) );
	PROTECT( val = allocVector(REALSXP, nrow) );

	xd = REAL(d);
	xval = REAL(val);

	q = 0;
	if (na==1) {
		if (mnmx==0) {
			for (i = 0; i < nrow; i++) {
				v = xd[q];
				q++;
				for (j = 1; j < ncol; j++) {
					if (R_IsNA(v)) {
						v = xd[q];
					} else if ( !R_IsNA(xd[q])) {
						if (xd[q] < v ) {
							v = xd[q];
						}
					}
					q++;
				}
				xval[i] = v;
			}
		} else {
			for (i = 0; i < nrow; i++) {
				v = xd[q];
				q++;
				for (j = 1; j < ncol; j++) {
					if (R_IsNA(v)) {
						v = xd[q];
					} else if ( !R_IsNA(xd[q]) ) {
						if ( xd[q] > v ) {
							v = xd[q];
						}
					}
					q++;
				}
				xval[i] = v;
			} 				
		}
	} else { //na.rm=FALSE
		if (mnmx==0) {
			for (i = 0; i < nrow; i++) {
				q = i * nrow;
				v = xd[q];
				for (j = 1; j < ncol; j++) {
					q++;
					if (R_IsNA(v) | R_IsNA(xd[q])) {
						v = R_NaReal;
						break;
					} else if ( xd[q] < v ) {
						v = xd[q];
					}
				}
				xval[i] = v;
			} 				
		} else {
			for (i = 0; i < nrow; i++) {
				q = i * nrow;
				v = xd[q];
				for (j = 1; j < ncol; j++) {
					q++;
					if (R_IsNA(v) | R_IsNA(xd[q])) {
						v = R_NaReal;
						break;
					} else if ( xd[q] > v ) {
						v = xd[q];
					}
				}
				xval[i] = v;
			} 				
		}
	}

	UNPROTECT(2);
	return(val);
}




/* Robert Hijmans, October 2011 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"

#define min(a, b) (((a) < (b)) ? (a) : (b))
#define max(a,b) ((a)>(b)?(a):(b))


SEXP aggregate(SEXP d, SEXP op, SEXP na, SEXP dim) {
					
	R_len_t i, j, k, n;
	SEXP v;
	double *xd, *xv, dv;
	int *cnts, row, col, newrow, newcol, newcell, mean, expected;

	int oper = INTEGER(op)[0];
	int narm = INTEGER(na)[0];
	
//	int nrin = INTEGER(dim)[0];
	int ncin = INTEGER(dim)[1];
	int nl = INTEGER(dim)[2];
	int nrout = INTEGER(dim)[3];
	int ncout = INTEGER(dim)[4];
	int xfact = INTEGER(dim)[5];
	int yfact = INTEGER(dim)[6];

	
	PROTECT(d = coerceVector(d, REALSXP));
	xd = REAL(d);
	
	if (oper==1) {
		oper = 0;
		mean = 1;
	} else {
		mean = 0;
	}
	
	cnts=(int *) malloc(nrout*ncout*nl*sizeof(int));
	PROTECT( v = allocVector(REALSXP, nrout*ncout*nl) );
	xv = REAL(v);
	
	if (oper==2) {
		for (i=0; i<length(v); i++) {
			xv[i] = R_PosInf;
			cnts[i] = 0;
		}
	
	} else if (oper ==3) {
		for (i=0; i<length(v); i++) {
			xv[i] = R_NegInf;
			cnts[i] = 0;
		}
	
	} else {
		for (i=0; i<length(v); i++) {
			xv[i] = 0;
			cnts[i] = 0;
		}
	}
	
	int ncell = length(d) / nl;
	int ncellout = ncout * nrout;

	if (oper==0) { // sum or mean
		for (i=0; i<ncell; i++) {
			row = i / ncin;
			col = i - (row*ncin);
			newrow = row / yfact;
			newcol = col / xfact;
			newcell = newrow * ncout + newcol;
			if ((newcol < ncout) & (newrow < nrout)) {	
				for (k=0; k<nl; k++) {
					j = i + ncell * k;
					if (!R_IsNA(xd[j])) {
						n = newcell + ncellout * k;	
						xv[n] = xv[n] + xd[j];
						cnts[n] = cnts[n] + 1;
					}
				}
			}
		}
	} else {
	
		for (i=0; i<ncell; i++) {
			row = i / ncin;
			col = i - (row*ncin);
			newrow = row / yfact;
			newcol = col / xfact;
			newcell = newrow * ncout + newcol;
			if ((newcol < ncout) & (newrow < nrout)) {	
				for (k=0; k<nl; k++) {
					j = i + ncell * k;
					if (!R_IsNA(xd[j])) {
						n = newcell + ncellout * k;	
						if (oper==2) {
							xv[n] = min(xv[n], xd[j]);
						} else if (oper==3) {
							xv[n] = max(xv[n], xd[j]);
						}
						cnts[n] = cnts[n] + 1;
					}
				}
			}
		}
	}
	//Rprintf ("newcell = %i \n", newcell);
	if (mean==1) {
		expected = xfact * yfact;
		dv = 1.0 / expected;
		if (narm==0) {
			for (i=0; i<length(v); i++) {
				if (cnts[i] == expected) {
					xv[i] = xv[i] * dv;
				} else {
					xv[i] = R_NaReal;
				}
			}
		} else {
			for (i=0; i<length(v); i++) {
				if (cnts[i] == expected) {
					xv[i] = xv[i] * dv;
				} else if (cnts[i] == 0) {
					xv[i] = R_NaReal;
				} else {
					xv[i] = xv[i] / cnts[i];
				}
			}
		}
	} else if (narm==0) {
		expected = xfact * yfact;
		for (i=0; i<length(v); i++) {
			if (cnts[i] < expected) {
				xv[i] = R_NaReal;
			}
		}
	} else {
		for (i=0; i<length(v); i++) {
			if (cnts[i] == 0) {
				xv[i] = R_NaReal;
			}
		}
	}	
	UNPROTECT(2);
	return(v);
}




/* Robert Hijmans, November 2011 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <Rmath.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"


SEXP edge(SEXP d, SEXP dim, SEXP classes, SEXP type, SEXP directions) {

	R_len_t i, j;
	SEXP val;
	int nrow, ncol, n;
	int *xd, *xval;

	int class = INTEGER(classes)[0];
	int edgetype = INTEGER(type)[0];
//	int falseval = INTEGER(fval)[0];
//	int aszero = INTEGER(asz)[0];

	int dirs = INTEGER(directions)[0];

	int falseval = 0;
	
	nrow = INTEGER(dim)[0];
	ncol = INTEGER(dim)[1];
	n = nrow * ncol;
	
	PROTECT(d = coerceVector(d, INTSXP));
	xd = INTEGER(d);

	PROTECT( val = allocVector( INTSXP, n) );
	xval = INTEGER(val);

	int r[8] = { -1,0,0,1 , -1,-1,1,1};
	int c[8] = { 0,-1,1,0 , -1,1,-1,1};	
	
	if (class == 0) {
		if (edgetype == 0) { // inner
			for (i = ncol; i < ncol * (nrow-1); i++) {
				xval[i] = R_NaInt;
				if ( xd[i] != R_NaInt ) {
					xval[i] = falseval;
					for (j=0; j< dirs; j++) {			
						if ( xd[r[j] * ncol + c[j] + i] == R_NaInt ) {
							xval[i] = 1;
							break;
						}
					}
				}
			}
		
		} else if (edgetype == 1) { //outer

			for (i = ncol; i < ncol * (nrow-1); i++) {
				xval[i] = falseval;
				if ( (xd[i] == R_NaInt) ) {
					xval[i] = R_NaInt;
					for (j=0; j < dirs; j++) {			
						if ( xd[r[j] * ncol + c[j] + i] != R_NaInt ) {
							xval[i] = 1;
							break;
						}
					}
				}
			}
		} 
		
	} else { // by class

		int test;
		for (i = ncol; i < ncol * (nrow-1); i++) {
			test = xd[ r[0]*ncol+c[0]+i ];
			if (test == R_NaInt) {
				xval[i] = R_NaInt;			
			} else {
				xval[i] = falseval;
			}
			for (j=1; j < dirs; j++) {
				if (test != xd[ r[j]*ncol +c[j] +i ]) {
					xval[i] = 1;
					break;
				}
			}
		}

	}
	
	UNPROTECT(2);
	return(val);
}



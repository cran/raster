/* Robert Hijmans, October 2014 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <Rmath.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"


SEXP aggregate_get(SEXP d, SEXP dim) {

	R_len_t i, j, k, q;
	
	SEXP val;
	int nrow, ncol, dx, dy, dz, ro, co, lo, c1, c2, g;
	double *xd, *xval;

	
	nrow = INTEGER(dim)[0];
	ncol = INTEGER(dim)[1];
	//nlyr = INTEGER(dim)[2];
	dy = INTEGER(dim)[3];
	dx = INTEGER(dim)[4];
	dz = INTEGER(dim)[5];
	PROTECT(d = coerceVector(d, REALSXP));

	int n = length(d);
	int blockcells = dx * dy * dz;
	int blocks = n / blockcells;
	int ncell = nrow * ncol;
//	int dxdy = dx * dy;
	int bpRow = ncol / dx;
	int bpCol = nrow / dy;
	int bpLyr = bpRow * bpCol;
	
	
	PROTECT( val = allocVector(REALSXP, n) );

	xd = REAL(d);
	xval = REAL(val);

	int f;
	
	for (i = 0; i < blocks; i++) {
		ro = (dy * (i / bpRow)) % nrow;
		co = dx * (i % bpRow);
		lo = dz * (i / bpLyr);
		
		f = 0;
		g = i * blockcells;
	//	Rprintf ("\n\nlo = %i \n\n", lo);

		for (j = lo; j < (lo+dz); j++) {
			c1 = (j * ncell);
			for (k = ro; k < (ro+dy); k++) {
				c2 = k * ncol;
				for (q=co; q < (co+dx); q++) {
					//int cell = c1 + c2 + q;
					xval[g+f] = xd[c1 + c2 + q];
					f++;
				}
			}
		}
	}
	UNPROTECT(2);
	return(val);
}




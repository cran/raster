#include    <R.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<math.h>

#include <Rinternals.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"


int intmode(int *d, int size, int ties) { 
	int i, j, mod, bestmod, count, bestcount;

	bestmod = 0;
	bestcount = 0;
	
	for(i=0; i<size; i++) {
		if ((size-i+1) < bestcount) {
			continue;
		}
		mod = d[i];
		count = 1;
		
		for(j = i+1; j < size; j++) 
			if(d[j]==mod) 
				count++;
			
		if (count > bestcount) {
			bestmod = mod;
			bestcount = count;
		} else if (count == bestcount) {
			if (ties == 1) { 
			// lowest
				if ((bestmod == R_NaInt) | (mod < bestmod)) {
					bestmod = mod;
					bestcount = count;
				}
			} else if (ties == 2) { 
			// highest
				if ((bestmod == R_NaInt) | (mod > bestmod)) {
					bestmod = mod;
					bestcount = count;
				}
			} else if (ties == 3) { 
			//NA
				bestmod = R_NaInt;
				bestcount = count;
			} else if (ties == 4) { 
			// random draw
			}
		}
	}
	return bestmod;
}





SEXP modal(SEXP d, SEXP ties) {
	int ln, *xd, *xval, ts;
	SEXP val;
	
	ts = INTEGER(ties)[1];
	ln = length(d);
	PROTECT(d = coerceVector(d, INTSXP));
	xd = INTEGER(d);
	
	PROTECT( val = allocVector(INTSXP, 1) );
	xval = INTEGER(val);
	xval[0] = intmode(xd, ln, ts);
	
	UNPROTECT(2);
	return(val);
}


/* Robert Hijmans, October 2011 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <Rmath.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"

SEXP rasterize_poly(SEXP object) {
	
	PROTECT(object);

	SEXP slot;
	PROTECT(slot = allocVector(STRSXP,1));
	SET_STRING_ELT(slot, 0, mkChar("ncols"));
	SEXP nc = R_do_slot(object, slot);
	int intnc = INTEGER(nc)[0];
	Rprintf ("ncol = %i \n", intnc);
	
	UNPROTECT(2);
	return(object);
}


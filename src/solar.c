/* Robert Hijmans, October 2011 

Solar radiation according to:
 Kumar, L., Skidmore, A.K., Knowles, E., 1997.
 Modelling Topographic Variation in Solar Radiation in a GIS Environment. 
 International Journal of Geographical Information Science, 11: 475-497.

 Based on Matlab code by Felix Hebeler, Dept. of Geography, University Zurich, May 2008.
 And AML code by  Niklaus E. Zimmermann, Swiss Federal Research Institute WSL
*/

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"
#include "Rmath.h"


SEXP solradiat(SEXP elev, SEXP s, SEXP a, SEXP lat, SEXP ref, SEXP days, SEXP dim) {

	R_len_t i, j, h;
	SEXP rad;
	double *dem, *slope, *aspect, *srad, *latitude, r;
	int *day;
	int nrow, ncol, n, nd, d;
	double L, sinL, cosL, tanL, sinSlop, sinSlop2, cosSlop, cosSlop2, sinAsp, cosAsp, term1, term2, term3; 
	double hsr, It, I, hs, sinAlpha, M, tau_b, tau_r, tau_d, Id, Ir, cos_i, R, Is; 
	double dr = M_PI / 180 ; // degree to radians conversion factor
	
	PROTECT(elev = coerceVector(elev, REALSXP)); // currently not used, but should be used.
	PROTECT(s = coerceVector(s, REALSXP));
	PROTECT(a = coerceVector(a, REALSXP));
	PROTECT(lat = coerceVector(lat, REALSXP));
	PROTECT(days = coerceVector(days, INTSXP));

	r = REAL(ref)[0];
    if ((r > 1) | (r <= 0) ) error("reflection should be between 0 and 1");
	
	nrow = INTEGER(dim)[0];
	ncol = INTEGER(dim)[1];
	n = nrow * ncol;
	Rprintf(" n: %d \n", n); 

	dem = REAL(elev);
	slope = REAL(s);
	aspect = REAL(a);
	latitude = REAL(lat);

	nd = length(days);
	day = INTEGER(days);
	PROTECT( rad = allocVector(REALSXP, n*nd) );
	srad = REAL(rad);


	double S0 = 1367;      // solar constant W m^-2   default 1367
	double I0[365], dS[365];
	
	for (i=0; i<365; i++) {
		// extraterrestrial solar radiation
		I0[i] = S0 * (1 + 0.0344 * cos(M_2PI * (i+1) / 365.)); 
		// sun declination
		dS[i] = 23.45 * dr * sin(M_2PI * ( (284 + i + 1) / 365. )); 
	}
	
	int row = -1;
	L = 0;
	tanL = 0;
	cosL = 0;
	sinL = 0;
// loop over cells	
	for (i=0; i<n; i++) {
		
		if (i % ncol == 0) {
			row++;
			L = latitude[row];
			L = L * dr;
			sinL = sin(L);
			cosL = cos(L);
			tanL = tan(L);			
		}

		if (slope[i] == R_NaReal) {
			srad[i] = R_NaReal;
			continue;
		}
	
		sinSlop = sin(slope[i]);
		cosSlop = cos(slope[i]);
		cosSlop2 = cosSlop * cosSlop;
		sinSlop2 = sinSlop * sinSlop;
		sinAsp = sin(aspect[i]);
		cosAsp = cos(aspect[i]);
		term1 = ( sinL * cosSlop - cosL * sinSlop * cosAsp );
		term2 = ( cosL * cosSlop + sinL * sinSlop * cosAsp );
		term3 = sinSlop * sinAsp;

		// loop over days
		for (j=0; j < nd; j++) {
			d = day[j];
			// angle at sunrise/sunset
			
			hsr = -1.0 * tanL * tan(dS[d]); 
			// this only works for latitudes up to 66.5 deg N! Workaround:
			if (hsr < -1) { 
				hsr = acos(-1);
			} else if (hsr > 1) {
				hsr = acos(1);	
			} else {
				hsr = acos(hsr);
			}
			if (i==77) {
				Rprintf ("hsr: %lf\n", hsr);
			}
			It = round(12*(1 + hsr / M_PI) - 12*(1-hsr / M_PI)); // calc daylength

			//  daily loop
			I = 0;
			for (h=0; h < It; h++) {  // loop over sunshine hours
				// if accounting for shading should be included, calc hillshade here
		
				// hourangle of sun hs  
				hs = hsr-(M_PI * h/It); // hs(h)
				// solar angle and azimuth
				// alpha = asin(sinL*sin(dS)+cosL*cos(dS)*cos(hs));// solar altitude angle
				sinAlpha = sinL * sin(dS[d]) + cosL * cos(dS[d]) * cos(hs);
				// alpha_s = asin(cos(dS)*sin(hs)/cos(alpha)); // solar azimuth angle
				//  correction  using atmospheric transmissivity taub_b
				M = sqrt(1229+ pow(614 * sinAlpha, 2)) - 614 * sinAlpha; // Air mass ratio
				tau_b = 0.56 * (exp(-0.65 * M) + exp(-0.095 * M));
				tau_d = 0.271 - 0.294 * tau_b; // radiation diffusion coefficient for diffuse insolation
				tau_r = 0.271 + 0.706 * tau_b; // reflectance transmitivity
				//  correct for local incident angle
				cos_i = (sin(dS[d]) * term1) + (cos(dS[d]) * cos(hs) * term2) + (cos(dS[d]) * term3 * sin(hs));
				Is = I0[d] * tau_b; // potential incoming shortwave radiation at surface normal (equator)
				//  R = potential clear sky solar radiation W m2
				R = Is * cos_i;
				if (R < 0) { 
					R = 0;	
				}
				Id = I0[d] * tau_d * cosSlop2 / 2*sinAlpha; //diffuse radiation;
				Ir = I0[d] * r * tau_r * sinSlop2/ 2* sinAlpha; // reflectance
				R = R + Id + Ir;
				if (R < 0) { R = 0;	}
				I = I + R; // solar radiation per day (sunshine hours) 
			} 
			srad[i + n*j] = I;
		}
	}
	UNPROTECT(6);	
	return(rad);
}



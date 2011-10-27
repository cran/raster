// Derived from code in the sp package by Pebesma, Bivand, and others
// Robert Hijmans, October 2011

#define USING_R 1
#include "S.h" 

#ifdef USING_R
# include <R.h>
# include <Rdefines.h>
/* # include <Rinternals.h> */
# define R_UNIFORM unif_rand()
# define R_NORMAL  norm_rand()
# define RANDIN seed_in((long *) NULL)
# define RANDOUT seed_out((long *) NULL)
# define S_EVALUATOR
#endif


#ifndef MIN
# define MIN(a,b) ((a)>(b)?(b):(a))
#endif
#ifndef MAX
# define MAX(a,b) ((a)>(b)?(a):(b))
#endif


/* polygon structs: */
typedef struct {
	double		x, y;
} PLOT_POINT;

typedef struct {
	PLOT_POINT	min, max;
} MBR;

typedef struct polygon {
	MBR mbr;
	int lines;
	PLOT_POINT	*p;
    int close; /* 1 - is closed polygon */
} POLYGON;


/*
This code is described in "Computational Geometry in C" (Second Edition),
Chapter 7.  It is not written to be comprehensible without the 
explanation in that book.

For each query point q, InPoly returns one of four char's:
	i : q is strictly interior to P
	o : q is strictly exterior to P
	v : q is a vertex of P
	e : q lies on the relative interior of an edge of P
These represent mutually exclusive categories.
For an explanation of the code, see Chapter 7 of 
"Computational Geometry in C (Second Edition)."

Written by Joseph O'Rourke, contributions by Min Xu, June 1997.
Questions to orourke@cs.smith.edu.
--------------------------------------------------------------------
This code is Copyright 1998 by Joseph O'Rourke.  It may be freely 
redistributed in its entirety provided that this copyright notice is 
not removed.
--------------------------------------------------------------------
*/

/*
InPoly returns a char in {i,o,v,e}.  See above for definitions.
changed to 1 or 0 by RH
*/

int InPoly(PLOT_POINT q, POLYGON *Poly)
{
    int n = Poly->lines;
    PLOT_POINT *P=Poly->p;
    
    int	 i, i1;      /* point index; i1 = i-1 mod n */
    double x;          /* x intersection of e with ray */
    double xx=q.x, yy=q.y;
    int	 Rcross = 0; /* number of right edge/ray crossings */
    int    Lcross = 0; /* number of left edge/ray crossings */

    /* For each edge e=(i-1,i), see if crosses ray. */
    for( i = 0; i < n; i++ ) {
        /* First see if q=(0,0) is a vertex. */
        if (( P[i].x - xx )==0 &&( P[i].y - yy )==0 ) return 1;
        i1 = ( i + n - 1 ) % n;
        /* printf("e=(%d,%d)\t", i1, i); */
    
        /* if e "straddles" the x-axis... */
        /* The commented-out statement is logically equivalent to the one 
           following. */
        /* if( ( ( P[i].y > 0 ) && ( P[i1].y <= 0 ) ) ||
           ( ( P[i1].y > 0 ) && ( P[i] .y <= 0 ) ) ) { }*/
    
        if( (( P[i].y - yy ) > 0 ) != (( P[i1].y - yy ) > 0 ) ) {
      
            /* e straddles ray, so compute intersection with ray. */
            x = (( P[i].x - xx) *( P[i1].y - yy ) -( P[i1].x - xx ) *( P[i].y - yy )) /
                (P[i1].y - P[i].y );
            /* printf("straddles: x = %g\t", x); */
      
            /* crosses ray if strictly positive intersection. */
            if (x > 0) Rcross++;
        }
        /* printf("Right cross=%d\t", Rcross); */
    
        /* if e straddles the x-axis when reversed... */
        /* if( ( ( P[i] .y < 0 ) && ( P[i1].y >= 0 ) ) ||
           ( ( P[i1].y < 0 ) && ( P[i] .y >= 0 ) ) )  { }*/
    
        if ( (( P[i].y - yy ) < 0 ) != (( P[i1].y - yy ) < 0 ) ) { 
      
            /* e straddles ray, so compute intersection with ray. */
            x = (( P[i].x - xx) *( P[i1].y - yy ) -( P[i1].x - xx ) *( P[i].y - yy )) /
                (P[i1].y - P[i].y);
            /* printf("straddles: x = %g\t", x); */

            /* crosses ray if strictly positive intersection. */
            if (x < 0) Lcross++;
        }
        /* printf("Left cross=%d\n", Lcross); */
    }	
  
    /* q on the edge if left and right cross are not the same parity. */
    if( ( Rcross % 2 ) != (Lcross % 2 ) )
        return 1;
  
    /* q inside iff an odd number of crossings. */
    if( (Rcross % 2) == 1 )
        return 1;
    else
		return 0;
}



void setup_poly_minmax(POLYGON *pl) {
    int i, n=pl->lines;
    double minx,maxx,miny,maxy;
    
    minx=miny=DBL_MAX;
    maxx=maxy=-DBL_MAX;
    
    for (i=0;i<n;i++) {
        minx = MIN(minx, pl->p[i].x);
        miny = MIN(miny, pl->p[i].y);
        maxx = MAX(maxx, pl->p[i].x);
        maxy = MAX(maxy, pl->p[i].y);
    }
    pl->mbr.min.x = minx;
    pl->mbr.min.y = miny;
    pl->mbr.max.x = maxx;
    pl->mbr.max.y = maxy;
}



SEXP point_in_polygon(SEXP px, SEXP py, SEXP polx, SEXP poly) {
	int i;
	PLOT_POINT p;
	POLYGON pol;
	SEXP ret;

	S_EVALUATOR
	pol.lines = LENGTH(polx); /* check later that first == last */
	pol.p = (PLOT_POINT *) Calloc(pol.lines, PLOT_POINT); /* Calloc does error handling */
	for (i = 0; i < LENGTH(polx); i++) {
		pol.p[i].x = NUMERIC_POINTER(polx)[i];
		pol.p[i].y = NUMERIC_POINTER(poly)[i];
	}
    pol.close = (pol.p[0].x == pol.p[pol.lines - 1].x && 
			pol.p[0].y == pol.p[pol.lines - 1].y);
	setup_poly_minmax(&pol);

	PROTECT(ret = NEW_INTEGER(LENGTH(px)));
	for (i = 0; i < LENGTH(px); i++) {
		p.x = NUMERIC_POINTER(px)[i];
		p.y = NUMERIC_POINTER(py)[i];
		if ((p.x > pol.mbr.min.x) & (p.x <= pol.mbr.max.y) & (p.y > pol.mbr.min.y) & (p.y <= pol.mbr.max.y)) {
			INTEGER_POINTER(ret)[i] = InPoly(p, &pol);
		} else {
			INTEGER_POINTER(ret)[i] = 0;
		}
	}
	Free(pol.p);
	UNPROTECT(1);
	return(ret);
}



SEXP point_in_polygon2(SEXP px, SEXP py, SEXP polx, SEXP poly, SEXP id, SEXP holes) {
	int i, j;
	PLOT_POINT p;
	POLYGON pol;
	SEXP ret, polxx, polyy;
	int isHole, nHoles, lastPart;
	int *hole, *tmp;
	
	
	PROTECT(px);
	PROTECT(py);
	PROTECT(id = coerceVector(id, INTSXP));
	PROTECT(holes = coerceVector(holes, INTSXP));
	PROTECT(polx = coerceVector(polx, VECSXP));
	PROTECT(poly = coerceVector(poly, VECSXP));
	
	PROTECT(ret = NEW_INTEGER(LENGTH(px)));
	tmp = (int *) malloc(LENGTH(px)*sizeof(double));	
	hole = (int *) malloc(LENGTH(px)*sizeof(int));	
	
	for (i = 0; i < LENGTH(ret); i++) {
		INTEGER_POINTER(ret)[i] = R_NaInt;
		hole[i] = 0;
		tmp[i] = R_NaInt;
	}
	
	lastPart = 0;
	nHoles = 0;
	int oldid = INTEGER_POINTER(id)[0];
	
	int n = LENGTH(polx);
	for (i = 0; i < n; i++) {
		if (i == (n-1) ) {
			lastPart = 1;
		} else if ((INTEGER_POINTER(id)[i+1]) != oldid) {
			lastPart = 1;
			oldid = INTEGER_POINTER(id)[i+1];
		}
		if (INTEGER_POINTER(holes)[i]) {
			isHole = 1;
			nHoles++;
		} else {
			isHole = 0;		
		}
		
		polxx = VECTOR_ELT(polx, i);
		polyy = VECTOR_ELT(poly, i);
		pol.lines = LENGTH(polxx); /* check later that first == last */
		
		pol.p = (PLOT_POINT *) Calloc(pol.lines, PLOT_POINT); /* Calloc does error handling */
		for (j = 0; j < LENGTH(polxx); j++) {
			pol.p[j].x = NUMERIC_POINTER(polxx)[j];
			pol.p[j].y = NUMERIC_POINTER(polyy)[j];
		}
		pol.close = (pol.p[0].x == pol.p[pol.lines - 1].x && pol.p[0].y == pol.p[pol.lines - 1].y);
		// call error if pol.close = FALSE ?

		setup_poly_minmax(&pol);

		if (isHole) {
			for (j = 0; j < LENGTH(px); j++) {
				p.x = NUMERIC_POINTER(px)[j];
				p.y = NUMERIC_POINTER(py)[j];
				if ((p.x > pol.mbr.min.x) & (p.x <= pol.mbr.max.x) & (p.y > pol.mbr.min.y) & (p.y <= pol.mbr.max.y)) {
					if (InPoly(p, &pol)) {
						hole[j] = 1;
					}
				} 
			}
		} else {	
			for (j = 0; j < LENGTH(px); j++) {
				p.x = NUMERIC_POINTER(px)[j];
				p.y = NUMERIC_POINTER(py)[j];
				if ((p.x > pol.mbr.min.x) & (p.x <= pol.mbr.max.x) & (p.y > pol.mbr.min.y) & (p.y <= pol.mbr.max.y)) {
					if (InPoly(p, &pol)) {
						tmp[j] = INTEGER_POINTER(id)[i];
					}
				} 
			}
		}
		Free(pol.p);
		if (lastPart) {
			lastPart = 0;
			if (nHoles > 0) {
				nHoles = 0;
				for (j = 0; j < LENGTH(px); j++) {
					if ((tmp[j] != NA_INTEGER ) & (! hole[j])) {
				//		Rprintf ("%s \n", "a");
						INTEGER_POINTER(ret)[j] = tmp[j];
					}
					tmp[j] = R_NaInt;
					hole[j] = 0;
				}
			} else {
				for (j = 0; j < LENGTH(px); j++) {
					if (NA_INTEGER != tmp[j]) {
						INTEGER_POINTER(ret)[j] = tmp[j];
						tmp[i] = R_NaInt;
					}
				}
			}
		}
		
	}
	
	UNPROTECT(7);
	return(ret);
}

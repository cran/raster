/* Robert Hijmans, June 2011, July 2016

*/


#include <Rcpp.h>
using namespace Rcpp;
using namespace std;
#include <vector>
#include "geodesic.h"
#include <math.h>
#include "util.h"


double distance_lonlat(double lon1, double lat1, double lon2, double lat2, double a, double f) {
	double s12, azi1, azi2;
	struct geod_geodesic g;
	geod_init(&g, a, f);
	geod_inverse(&g, lat1, lon1, lat2, lon2, &s12, &azi1, &azi2);
  	return s12;
}


std::vector<double> distance_lonlat(std::vector<double> lon1, std::vector<double> lat1, std::vector<double> lon2, std::vector<double> lat2, double a, double f) {
// lonlat1 and lonlat2 should have the same length
	std::vector<double> r (lon1.size()); 
	double azi1, azi2;
	struct geod_geodesic g;
	geod_init(&g, a, f);
	int n = lat1.size();
  	for (int i=0; i < n; i++) {
		geod_inverse(&g, lat1[i], lon1[i], lat2[i], lon2[i], &r[i], &azi1, &azi2);
	}
  	return r;
}


std::vector<double> distanceToNearest_lonlat(std::vector<double> lon1, std::vector<double> lat1, std::vector<double> lon2, std::vector<double> lat2, double a, double f) {
	double azi1, azi2, s12;
	int n = lon1.size();
	int m = lon2.size();
	std::vector<double> r(n); 
	
	struct geod_geodesic g;
	geod_init(&g, a, f);
  	for (int i=0; i < n; i++) {
		geod_inverse(&g, lat1[i], lon1[i], lat2[0], lon2[0], &r[i], &azi1, &azi2);
		for (int j=1; j<m; j++) {
			geod_inverse(&g, lat1[i], lon1[i], lat2[j], lon2[j], &s12, &azi1, &azi2);
			if (s12 < r[i]) {
				r[i] = s12;
			}
		}
	}
  	return r;
}


double distance_plane(double x1, double y1, double x2, double y2) {
	return( sqrt(pow((x2-x1),2) + pow((y2-y1), 2)) );
}


std::vector<double> distance_plane(std::vector<double> x1, std::vector<double> y1, std::vector<double> x2, std::vector<double> y2) {
// xy1 and xy2 should have the same length
	std::vector<double> r (x1.size()); 
	int n = x1.size();
  	for (int i=0; i < n; i++) {
		r[i] = sqrt(pow((x2[i]-x1[i]),2) + pow((y2[i]-y1[i]), 2));
	}
  	return r;
}

std::vector<double> distanceToNearest_plane(std::vector<double> x1, std::vector<double> y1, std::vector<double> x2, std::vector<double> y2) {
	int n = x1.size();
	int m = x2.size();
	std::vector<double> r(n); 
	double d;
  	for (int i=0; i < n; i++) {
		r[i] = sqrt(pow((x2[0]-x1[i]),2) + pow((y2[0]-y1[i]), 2));
		for (int j=1; j < m; j++) {
			d = sqrt(pow((x2[j]-x1[i]),2) + pow((y2[j]-y1[i]), 2));
			if (d < r[i]) {
				r[i] = d;
			}
		}
	}
  	return r;
}




double direction_lonlat(double lon1, double lat1, double lon2, double lat2, bool degrees, double a, double f) {
	double s12, azi1, azi2;
	struct geod_geodesic g;
	geod_init(&g, a, f);
	geod_inverse(&g, lat1, lon1, lat2, lon2, &s12, &azi1, &azi2);
	if (!degrees) {
		return(toRad(azi1));
	}
	return( azi1) ;
}

std::vector<double> direction_lonlat(std::vector<double> lon1, std::vector<double> lat1, std::vector<double> lon2, std::vector<double> lat2, bool degrees, double a, double f) {
// lonlat1 and lonlat2 should have the same length

	std::vector<double> azi1(lon1.size()); 
	double s12, azi2;
	struct geod_geodesic g;
	geod_init(&g, a, f);
	int n = lat1.size();
	if (degrees) {
		for (int i=0; i < n; i++) {
			geod_inverse(&g, lat1[i], lon1[i], lat2[i], lon2[i], &s12, &azi1[i], &azi2);
		}
	} else {
		for (int i=0; i < n; i++) {
			geod_inverse(&g, lat1[i], lon1[i], lat2[i], lon2[i], &s12, &azi1[i], &azi2);
			azi1[i] = toRad(azi1[i]);
		}		
	}
  	return azi1;
}


std::vector<double> directionToNearest_lonlat(std::vector<double> lon1, std::vector<double> lat1, std::vector<double> lon2, std::vector<double> lat2, bool degrees, bool from, double a, double f) {
	double azi1, azi2, s12, dist;
	int n = lon1.size();
	int m = lon2.size();
	std::vector<double> azi(n); 
	
	struct geod_geodesic g;
	geod_init(&g, a, f);
	
	if (from) {
		for (int i=0; i < n; i++) {
			geod_inverse(&g, lat2[0], lon2[0], lat1[i], lon1[i], &dist, &azi1, &azi2);
			azi[i] = azi1;
			for (int j=1; j<m; j++) {
				geod_inverse(&g, lat2[j], lon2[j], lat1[i], lon1[i], &s12, &azi1, &azi2);
				if (s12 < dist) {
					azi[i] = azi1;
				}
			}
			if (!degrees) {
				azi[i] = toRad(azi[i]);
			}
		}
		
	} else {
		for (int i=0; i < n; i++) {
			geod_inverse(&g, lat1[i], lon1[i], lat2[0], lon2[0], &dist, &azi1, &azi2);
			azi[i] = azi1;
			for (int j=1; j<m; j++) {
				geod_inverse(&g, lat1[i], lon1[i], lat2[j], lon2[j], &s12, &azi1, &azi2);
				if (s12 < dist) {
					azi[i] = azi1;
				}
			}
			if (!degrees) {
				azi[i] = toRad(azi[i]);
			}
		}
	}
  	return azi;
}



double direction_plane(double x1, double y1, double x2, double y2, bool degrees) {
	double a;
	a = fmod(atan2( x2 - x1, y2 - y1), M_2PI);
	a = (a < 0 ? a + M_2PI : a );
	return (degrees ? toDeg(a) : a);
}



std::vector<double> direction_plane(std::vector<double> x1, std::vector<double> y1, std::vector<double> x2, std::vector<double> y2, bool degrees) {
// xy1 and xy2 should have the same length
	std::vector<double> r (x1.size()); 
	//double a;
	int n = x1.size();
  	for (int i=0; i < n; i++) {
		r[i] = direction_plane(x1[i], y1[i], x2[i], y2[i], degrees);
	}
  	return r;
}



std::vector<double> directionToNearest_plane(std::vector<double> x1, std::vector<double> y1, std::vector<double> x2, std::vector<double> y2, bool degrees, bool from) {
	int n = x1.size();
	int m = x2.size();
	std::vector<double> r(n); 
	double d, mind;
	int minj;
	if (from) {
		for (int i = 0; i < n; i++) {
			mind = distance_plane(x1[i], y1[i], x2[0], y2[0]);
			minj = 0;
			for (int j = 1; j < m; j++) {
				d = distance_plane(x1[i], y1[i], x2[j], y2[j]);
				if (d < mind) {
					mind = d;
					minj = j;
				}
			}
			r[i] = direction_plane(x2[minj], y2[minj], x1[i], y1[i], degrees);
		}		
	} else {
		for (int i = 0; i < n; i++) {
			mind = distance_plane(x1[i], y1[i], x2[0], y2[0]);
			minj = 0;
			for (int j = 1; j < m; j++) {
				d = distance_plane(x1[i], y1[i], x2[j], y2[j]);
				if (d < mind) {
					mind = d;
					minj = j;
				}
			}
			r[i] = direction_plane(x1[i], y1[i], x2[minj], y2[minj], degrees);					
		}
	}
  	return r;
}





std::vector<double> destpoint_lonlat(double longitude, double latitude, double  bearing, double distance, double a, double f) {
	struct geod_geodesic g;
	geod_init(&g, a, f);
	double lat2, lon2, azi2;
	geod_direct(&g, latitude, longitude, bearing, distance, &lat2, &lon2, &azi2);
	std::vector<double> out = {lon2, lat2, azi2 };
	return out;
}


std::vector<std::vector<double> > destpoint_lonlat(std::vector<double> longitude, std::vector<double> latitude, std::vector<double> bearing, std::vector<double> distance, double a, double f) {
	struct geod_geodesic g;
	geod_init(&g, a, f);
	int n = longitude.size();
	std::vector<std::vector<double> > out;
	double lat2, lon2, azi2;
	for (int i=0; i < n; i++) {
		geod_direct(&g, latitude[i], longitude[i], bearing[i], distance[i], &lat2, &lon2, &azi2);
		out.push_back( {lon2, lat2, azi2 });
	}
	return out;
}
 
 
std::vector<double> destpoint_plane(double x, double y, double bearing, double distance) {
	bearing = bearing * M_PI / 180;
	x += distance * cos(bearing);
	y += distance * sin(bearing);
	std::vector<double> out = {x, y};
	return(out);
}


std::vector<std::vector<double> > destpoint_plane(std::vector<double>  x, std::vector<double>  y, std::vector<double>  bearing, std::vector<double>  distance) {
	int n = x.size();
	std::vector<std::vector<double> > out(n, std::vector<double>(3));
	double xd, yd, b;
	for (int i=0; i < n; i++) {
		b = bearing[i] * M_PI / 180;
		xd = x[i] + distance[i] * cos(b);
		yd = y[i] + distance[i] * sin(b);
		out.push_back( {xd, yd });
	}
	return(out);
}


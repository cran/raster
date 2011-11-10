/* (Euclidian) distance on a plane */
double distPlane(double x1, double y1, double x2, double y2) ;

/* law of cosines distance */
double distCos(double lon1, double lat1, double lon2, double lat2, double r) ;

/* haversine distance */
double distHav(double lon1, double lat1, double lon2, double lat2, double r);


/* direction on a plane */
double directionPlane(double x1, double y1, double x2, double y2, int degrees);

/* direction on a sphere */
double directionSphere(double lon1, double lat1, double lon2, double lat2, int degrees) ;


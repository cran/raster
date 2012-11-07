# Etienne B. Racine, Robert J. Hijmans

# needs to be generalized to n input rasters and to multi-layer objects
.blend <- function(r1, r2) {
	i <- intersect(raster(r1), raster(r2))
	j <- raster::expand(i, c(1,1)) 
	a <- crop(r1, j)
	b <- crop(r2, j)
	values(a) <- 1
	values(b) <- 2
	ab <- merge(a, b)
	ba <- merge(b, a)
	p1 <- rasterToPoints(ab, function(x) x==2)
	p2 <- rasterToPoints(ba, function(x) x==1)
	d1 <- distanceFromPoints(i, p1[,1:2])
	d2 <- distanceFromPoints(i, p2[,1:2])
	dsum <- d1 + d2

	z1 <- d1 * crop(r1, d1) / dsum
	z2 <- d2 * crop(r2, d2) / dsum
	merge(z1 + z2, r1, r2)
}



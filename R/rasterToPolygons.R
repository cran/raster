# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : December 2009
# Version 0.9
# Licence GPL v3


rasterToPolygons <- function(raster, fun=NULL) {

	if (dataSource(raster) == 'ram' & dataContent(raster) != 'all') {
		xyv <- xyFromCell(raster, 1:ncell(raster))
		xyv <- cbind(xyv, NA)
	} else if (dataContent(raster) == 'all') {
		xyv <- cbind(xyFromCell(raster, 1:ncell(raster)), values(raster))
		raster <- clearValues(raster)
		xyv <- subset(xyv, !(is.na(xyv[,3])))
		if (!is.null(fun)) {
			xyv <- subset(xyv, fun(xyv[,3]))
		}
	} else {
		xyv <- matrix(NA, ncol=3, nrow=0)
		colnames(xyv) <- c('x', 'y', 'v')
		x <- xFromCol(raster, 1:ncol(raster))
		for (r in 1:nrow(raster)) {
			y <- yFromRow(raster, r)
			raster <- readRow(raster, r)
			xyvr <- cbind(x, y, values(raster))
			xyvr <- subset(xyvr, !(is.na(xyvr[,3])))
			if (!is.null(fun)) {
				xyvr <- subset(xyvr, fun(xyvr[,3]))
			}
			xyv <- rbind(xyv, xyvr)
		}
	}
	xr <- xres(raster)/2
	yr <- yres(raster)/2

	cr <- matrix(ncol=8, nrow=nrow(xyv))
	cr[,1] <- xyv[,1] - xr
	cr[,5] <- xyv[,2] + yr
	cr[,2] <- xyv[,1] + xr
	cr[,6] <- xyv[,2] + yr
	cr[,3] <- xyv[,1] + xr
	cr[,7] <- xyv[,2] - yr
	cr[,4] <- xyv[,1] - xr
	cr[,8] <- xyv[,2] - yr
	
	polys <- list()
	for (i in 1:nrow(cr)) {
		p <- matrix( cr[i,], ncol=2 )
		p <- rbind(p, p[1,])
		polys[i] <- Polygons(list(Polygon( p )), i)
	}
	
	sp <- SpatialPolygons(polys, proj4string=projection(raster, FALSE))
	SpatialPolygonsDataFrame(sp, data.frame(value=xyv[,3]), FALSE)
}



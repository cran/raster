# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  February 2009
# Version 0.9
# Licence GPL v3



rasterToPoints <- function(raster, fun=NULL, asSpatialPoints=FALSE) {

	if (dataSource(raster) == 'ram' & dataContent(raster) != 'all') {
		if (asSpatialPoints) {
			coords <- xyFromCell(raster, 1:ncell(raster))
			row.names(coords) <- 1:nrow(coords)
			return(SpatialPoints(coords=coords, proj4string=projection(raster, asText=FALSE)))
		} else {
			return(xyFromCell(raster, 1:ncell(raster)))
		}
	}
	
	if (dataContent(raster) == 'all') {
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
	if (asSpatialPoints) {
		coords <- xyv[,1:2]
		row.names(coords) <- 1:nrow(coords)
		colnames(coords) <- c('x', 'y')
		rastvals <- as.data.frame(xyv[,3])
		colnames(rastvals) <- 'value'
		return(SpatialPointsDataFrame(coords=coords, data=rastvals, proj4string=projection(raster, asText=FALSE)))
	} else {
		return(xyv)
	}
}

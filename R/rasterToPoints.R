# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  February 2009
# Version 0.9
# Licence GPL v3



rasterToPoints <- function(x, fun=NULL, asSpatialPoints=FALSE) {
	
	nl = nlayers(x)
	if (nl > 1) {
		if (! is.null(fun)) {
			stop('you can only supply a fun argument if "x" has a single layer')		
		}
	}
	
	if (dataSource(x) == 'ram' & dataContent(x) != 'all') {
		if (asSpatialPoints) {
			coords <- xyFromCell(x, 1:ncell(x))
			row.names(coords) <- 1:nrow(coords)
			return(SpatialPoints(coords=coords, proj4string=projection(x, asText=FALSE)))
		} else {
			return(xyFromCell(x, 1:ncell(x)))
		}
	}
	
	if (canProcessInMemory(x, 3)) {
		xyv <- cbind(xyFromCell(x, 1:ncell(x)), getValues(x))
		x = NULL
		if (nl > 1) {
			notna = apply(xyv[,3:ncol(xyv)], 1, function(x){ sum(is.na(x)) < length(x) })
			xyv <- xyv[notna,]
		} else {
			xyv <- na.omit(xyv)
		}
		if (!is.null(fun)) {
			xyv <- subset(xyv, fun(xyv[,3]))
		}
	} else {
		xyv <- matrix(NA, ncol=2+nlayers(x), nrow=0)
		colnames(xyv) <- c('x', 'y', 'v')
		x <- xFromCol(x, 1:ncol(x))
		y <- yFromRow(x, 1:nrow(x))
		for (r in 1:nrow(x)) {
			xyvr <- cbind(x, y[r], getValues(x, r))
			notna = apply(xyv[,3:ncol(xyv)], 1, function(x){ sum(is.na(x)) < length(x) })
			xyvr <- xyvr[notna, ]
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
		return(SpatialPointsDataFrame(coords=coords, data=rastvals, proj4string=projection(x, asText=FALSE)))
	} else {
		return(xyv)
	}
}

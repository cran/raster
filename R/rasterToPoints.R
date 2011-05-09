# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  February 2009
# Version 0.9
# Licence GPL v3


rasterToPoints <- function(x, fun=NULL, spatial=FALSE, ...) {
	
	nl = nlayers(x)
	if (nl > 1) {
		if (! is.null(fun)) {
			stop('you can only supply a fun argument if "x" has a single layer')		
		}
	}
	crs = projection(x, asText=FALSE)
	
	if (! inherits(x, 'RasterStack' )) {
		if ( ! fromDisk(x) & ! inMemory(x) ) {
			if (spatial) {
				return(SpatialPoints(coords=xyFromCell(x, 1:ncell(x)), proj4string=crs) )
			} else {
				return(xyFromCell(x, 1:ncell(x)))
			}
		}
	}

	if (spatial) {
		laynam <- .enforceGoodLayerNames(x, returnNames=TRUE)
	}
	
	if (canProcessInMemory(x, 3)) {
		
		xyv <- cbind(xyFromCell(x, 1:ncell(x)), getValues(x))
		x = NULL
		if (nl > 1) {
			notna = apply(xyv[,3:ncol(xyv)], 1, function(x){ sum(is.na(x)) < length(x) })
			xyv <- xyv[notna, ,drop=FALSE]
		} else {
			xyv <- na.omit(xyv)
		}
		if (!is.null(fun)) {
			xyv <- subset(xyv, fun(xyv[,3]))
		}
		
	} else {
		xyv <- matrix(NA, ncol=2+nlayers(x), nrow=0)
		colnames(xyv) <- c('x', 'y', layerNames(x))
		X <- xFromCol(x, 1:ncol(x))
		Y <- yFromRow(x, 1:nrow(x))

		tr <- blockSize(x)
		pb <- pbCreate(tr$n, type=.progress(...))

		for (i in 1:tr$n) {
			r <- tr$row[i]:(tr$row[i]+tr$nrows[i]-1)
			xyvr <- cbind(rep(X, tr$nrows[i]), rep(Y[r], each=ncol(x)), getValues(x, row=tr$row[i], nrows=tr$nrows[i]))
		
			notna = apply(xyvr[,3:ncol(xyvr), drop=FALSE], 1, function(z){ sum(is.na(z)) < length(z) })
			
			xyvr <- xyvr[notna, ,drop=FALSE]
			
			if (!is.null(fun)) {
				xyvr <- subset(xyvr, fun(xyvr[,3]))
			}
			xyv <- rbind(xyv, xyvr)
			
			pbStep(pb, i)
		}
		pbClose(pb)
	}
	
	if (spatial) {
		v = data.frame(xyv[ ,-c(1:2), drop=FALSE])
		colnames(v) <- laynam
		return( SpatialPointsDataFrame(coords=xyv[,1:2], data=v, proj4string=crs ) )
		
	} else {
		return(xyv)
	}
}


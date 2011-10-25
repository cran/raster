# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : December 2009
# Version 0.9
# Licence GPL v3


rasterToPolygons <- function(x, fun=NULL, n=4, na.rm=TRUE, digits=12) {

	stopifnot(n %in% c(4,8,16))
	if (nlayers(x) > 1) {
		if (!is.null(fun)) {
			stop('you cannot supply a "fun" argument when "x" has multiple layers')
		}
	}

		
	if (! fromDisk(x) & ! inMemory(x)) {
		xyv <- xyFromCell(x, 1:ncell(x))
		xyv <- cbind(xyv, NA)
	} else if ( !(na.rm) | inMemory(x) | canProcessInMemory(x) ) {
		xyv <- cbind(xyFromCell(x, 1:ncell(x)), getValues(x))
		x <- clearValues(x)
		if (na.rm) {
			nas <- apply(xyv[,3:ncol(xyv), drop=FALSE], 1, function(x)all(is.na(x)))
			xyv <- xyv[!nas, ]
		}
		if (!is.null(fun)) {
			xyv <- subset(xyv, fun(xyv[,3]))
		}
	} else {
		tr <- blockSize(x)
		xyv <- matrix(ncol=3, nrow=0)
		colnames(xyv) <- c('x', 'y', 'v')
		for (i in 1:tr$n) {
			start <- cellFromRowCol(x, tr$row[i], 1)
			end <- start+tr$nrows[i]*ncol(x)-1
			xyvr <- cbind(xyFromCell(x, start:end), getValues(x, row=tr$row[i], nrows=tr$nrows[i]))
			if (na.rm) {
				nas <- apply(xyvr[,3:ncol(xyvr), drop=FALSE], 1, function(x)all(is.na(x)))
				xyvr <- xyvr[!nas, ]
			}
			if (!is.null(fun)) {
				xyvr <- subset(xyvr, fun(xyvr[,3]))
			}
			xyv <- rbind(xyv, xyvr)
		}
	}

	xr <- xres(x)/2
	yr <- yres(x)/2

	if (n==4) {
		cr <- matrix(ncol=10, nrow=nrow(xyv))
		cr[,c(1,4:5)] <- xyv[,1] - xr
		cr[,2:3] <- xyv[,1] + xr
		cr[,c(6:7,10)] <- xyv[,2] + yr
		cr[,8:9] <- xyv[,2] - yr
	} else if (n == 8) {
		cr <- matrix(ncol=18, nrow=nrow(xyv))
		cr[,c(1,7:9)] <- xyv[,1] - xr
		cr[,c(2,6)] <- xyv[,1] 
		cr[,3:5] <- xyv[,1] + xr
		cr[,c(10:12,18)] <- xyv[,2] + yr
		cr[,c(13:17)] <- xyv[,2] 
		cr[,14:16] <- xyv[,2] - yr
	} else if (n == 16) {
		cr <- matrix(ncol=34, nrow=nrow(xyv))
		cr[,c(1,13:17)] <- xyv[,1] - xr
		cr[,c(2,12)] <- xyv[,1] - 0.5 * xr
		cr[,c(3,11)] <- xyv[,1] 
		cr[,c(4,10)] <- xyv[,1] + 0.5 * xr
		cr[,5:9] <- xyv[,1] + xr
		
		cr[,c(18:22,34)] <- xyv[,2] + yr
		cr[,c(23,33)] <- xyv[,2] + 0.5 * yr
		cr[,c(24,32)] <- xyv[,2] 
		cr[,c(25,31)] <- xyv[,2] - 0.5 * yr
		cr[,26:30] <- xyv[,2] - yr
	}
	cr <- round(cr, digits=digits)
	
	sp <- lapply(1:nrow(cr), function(i) Polygons(list(Polygon( matrix( cr[i,], ncol=2 ) )), i))
	sp <- SpatialPolygons(sp, proj4string=projection(x, FALSE))
	if (ncol(xyv)==3) {
		sp <- SpatialPolygonsDataFrame(sp, data.frame(value=xyv[,3]), match.ID=FALSE)
	} else {
		sp <- SpatialPolygonsDataFrame(sp, data.frame(xyv[,3:ncol(xyv)]), match.ID=FALSE)	
	}
	
	
#	require(rgeos)
#	u <- unique(x@data[,1])
#	lst <- list()
#	for (i in 1:length(u)) {
#		s <- x[x@data[,1]==u[i], ]
#		d <- gUnionCascaded(s)@polygons[[1]]
#		d@ID <- as.character(i)
#		lst[[i]] <- d
#	}
#	sp <- SpatialPolygons(lst, proj4string=projection(r, FALSE))
#	sp <- SpatialPolygonsDataFrame(sp, data.frame(value=u), match.ID=FALSE)
	
	sp
}



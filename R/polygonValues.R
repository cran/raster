# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : December 2009
# Version 0.9
# Licence GPL v3


polygonValues <- function(p, x, ...) {	
	.warnExtract(6)
	extract(x, p, ...)
}


.polygonValues <- function(x, p, fun, na.rm=FALSE, weights=FALSE, cellnumbers=FALSE, ...) {
	spbb <- bbox(p)
	rsbb <- bbox(x)
	addres <- max(res(x))
	npol <- length(p@polygons)
	res <- list()
	res[[npol+1]] = NA

	if (spbb[1,1] >= rsbb[1,2] | spbb[1,2] <= rsbb[1,1] | spbb[2,1] >= rsbb[2,2] | spbb[2,2] <= rsbb[2,1]) {
		return(res[1:npol])
	}
	rr <- raster(x)
	for (i in 1:npol) {
		pp <- p[i,]
		spbb <- bbox(pp)
		
		if (spbb[1,1] >= rsbb[1,2] | spbb[1,2] <= rsbb[1,1] | spbb[2,1] >= rsbb[2,2] | spbb[2,2] <= rsbb[2,1]) {
			# do nothing; res[[i]] <- NULL
		} else {
			rc <- crop(rr, extent(pp)+addres)
			if (weights) {
				rc <- .polygonsToRaster(pp, rc, getCover=TRUE, silent=TRUE)
				rc[rc==0] <- NA
				xy <- rasterToPoints(rc)
				weight <- xy[,3] / 100
				xy <- xy[,-3]
			} else {
				rc <- .polygonsToRaster(pp, rc, silent=TRUE)
				xy <- rasterToPoints(rc)[,-3]
			}
			
			if (length(xy) > 0)  {  # catch holes or very small polygons
				if (weights) {
					value <- .xyValues(x, xy, ...)
					if (cellnumbers) {
						cell <- cellFromXY(x, xy)
						res[[i]] <- cbind(cell, value, weight)
					} else {				
						res[[i]] <- cbind(value, weight)
					}
				} else {
					res[[i]] <- .xyValues(x, xy, ...)
				}
			} else {
				# do nothing; res[[i]] <- NULL
			}
		}
	}
	
	res = res[1:npol]
	
	if (! missing(fun)) {
		if (weights) {
			res <- unlist(lapply(res, function(x) if (!is.null(x)) {sum(apply(x, 1, prod)) / sum(x[,2])} else NA  ))
		} else {
			i <- sapply(res, is.null)
			if (nlayers(x) > 1) {
				j <- matrix(ncol=nlayers(x), nrow=length(res))
				j[!i] <- t(sapply(res[!i], function(x) apply(x, 2, fun, na.rm)))
				colnames(j) <- layerNames(x)
			} else {
				j <- vector(length=length(i))
				j[i] <- NA
				j[!i] <- sapply(res[!i], fun, na.rm)
			}
			res <- j
		}
	}
	res
}




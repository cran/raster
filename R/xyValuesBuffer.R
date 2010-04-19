# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : December 2009
# Version 0.9
# Licence GPL v3


.xyvBuf <- function(object, xy, buffer, fun=NULL, na.rm=TRUE) { 
	
	buffer <- abs(buffer)
	if (length(buffer == 1)) {
		buffer <- rep(buffer, times=nrow(xy))
	} else if (length(buffer) != nrow(xy)  | ! is.vector(buffer) ) {
		stop('buffer should be a single value or a vector of length==nrow(xy)')
	}
	buffer[is.na(buffer)] <- 0

	cv <- list()
	obj <- raster(object) 
# ?	centralcells <- cellFromXY(obj, xy)
	if (isLonLat(obj)) { 
		# from m to degrees
		bufy <- buffer / 111319.5
		ymx <- pmin(90, xy[,2] + bufy)
		ymn <- pmax(-90, xy[,2] - bufy)
		bufx1 <- buffer / pointDistance(cbind(0, ymx), cbind(1, ymx), 'GreatCircle')
		bufx2 <- buffer / pointDistance(cbind(0, ymn), cbind(1, ymn), 'GreatCircle')
		bufx <- pmax(bufx1, bufx2)
		cn <- colFromX(obj, xy[,1]-bufx)
		cx <- colFromX(obj, xy[,1]+bufx)
		cn[is.na(cn)] <- 1
		cx[is.na(cx)] <- ncol(obj)
		rn <- rowFromY(obj, xy[,2]+bufy)
		rx <- rowFromY(obj, xy[,2]-bufy)
		rn[is.na(rn) & !is.na(rx)] <- 1
		rx[is.na(rx) & !is.na(rn)] <- nrow(obj)
		for (i in 1:nrow(xy)) {
			s <- sum(rn[i], rx[i], cn[i], cx[i])
			if (is.na(s)) {
				cv[[i]] <- NA
			} else {
				vals <- getValuesBlock(object, rn[i], rx[i]-rn[i]+1, cn[i], cx[i]-cn[i]+1)
				cells <- cellFromRowColCombine(obj, rn[i]:rx[i], cn[i]:cx[i])
				coords <- xyFromCell(obj, cells)
				pd <- cbind(pointDistance(xy[i,], coords, 'GreatCircle'), vals)
				if (nrow(pd) > 1) {
					cv[[i]] <- pd[pd[,1] <= buffer[i], -1]
				} else { 
					cv[[i]] <- pd[,-1]
				}
			}
		}
	} else { 

		cn <- colFromX(obj, xy[,1]-buffer)
		cx <- colFromX(obj, xy[,1]+buffer)
		cn[is.na(cn) & !is.na(cx)] <- 1
		cx[is.na(cx) & !is.na(cn)] <- ncol(obj)
		rn <- rowFromY(obj, xy[,2]+buffer)
		rx <- rowFromY(obj, xy[,2]-buffer)
		rn[is.na(rn) & !is.na(rx)] <- 1
		rx[is.na(rx) & !is.na(rn)] <- nrow(obj)
		for (i in 1:nrow(xy)) {
			s <- sum(rn[i], rx[i], cn[i], cx[i])
			if (is.na(s)) {
				cv[[i]] <- NA
			} else {
				vals <- getValuesBlock(object, rn[i], rx[i]-rn[i]+1, cn[i], cx[i]-cn[i]+1)
				cells <- cellFromRowColCombine(obj, rn[i]:rx[i], cn[i]:cx[i])
				coords <- xyFromCell(obj, cells)
				pd <- cbind(pointDistance(xy[i,], coords, type='Euclidean'), vals)
				if (nrow(pd) > 1) {
					cv[[i]] <- pd[pd[,1] <= buffer[i], -1]
				#	cells <- unique(c(cells, centralcells[i]))
				} else { 
					cv[[i]] <- pd[,-1]
				}
			}
		}
	}
	
	if (! is.null(fun)) {
		if (na.rm) {
			fun2 <- function(x){
						x <- na.omit(x)
						if (length(x) > 0) { return(fun(x)) 
						} else { return(NA) 
						}
					}
		} else {
			fun2 <- fun
		}
		if (class(object) == 'RasterLayer') {
			cv <- unlist(lapply(cv, fun2))
		} else {
			cv <- lapply(cv, function(x) {apply(x,2,fun2)})
			cv <- matrix(unlist(cv), ncol=nlayers(object), byrow=TRUE)
			colnames(cv) <- layerNames(object)
		}
	}
	return(cv)
}
 

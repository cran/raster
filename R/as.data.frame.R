# Author: Robert J. Hijmans
# Date : July 2011
# Version 1.0
# Licence GPL v3



if (!isGeneric("as.data.frame")) {
	setGeneric("as.data.frame", function(x, row.names = NULL, optional = FALSE, ...)
		standardGeneric("as.data.frame"))
}	


.insertColsInDF <- function(x, y, col, combinenames=TRUE) {
	cnames <- NULL
	if (combinenames) {
		if (ncol(y) > 1) {
			cnames <- paste(colnames(x)[col], '_', colnames(y), sep='')
		}
	}
	if (ncol(y) == 1) {
		x[, col] <- y
		return(x)
	} else if (col==1) {
		z <- cbind(y, x[, -1, drop=FALSE])
	} else if (col==ncol(x)) {
		z <- cbind(x[, -ncol(x), drop=FALSE], y)
	} else {
		z <- cbind(x[,1:(col-1), drop=FALSE], y, x[,(col+1):ncol(x), drop=FALSE])
	}
	if (!is.null(cnames)) {
		colnames(z)[col:(col+ncol(y)-1)] <- cnames
	}
	z
}


setMethod('as.data.frame', signature(x='Raster'), 
	function(x, row.names = NULL, optional = FALSE, ...) {

		v <- as.data.frame(values(x), row.names=row.names, optional=optional, ...)
		colnames(v) <- names(x)  # for nlayers = 1
		
		i <- is.factor(x)
		if (any(is.factor(x))) {
			if (ncol(v) == 1) {
				v <- data.frame(factorValues(x, v[,1], 1))
			} else {
				v <- .insertFacts(x, v, 1:nlayers(x))
			}
		}
		v
	}
)



setMethod('as.data.frame', signature(x='SpatialPolygons'), 
	function(x, row.names=NULL, optional=FALSE, xy=FALSE, centroids=TRUE, sepNA=FALSE, ...) {
		
		if (!xy) {
			if (.hasSlot(x, 'data')) {
				return( x@data )
			} else {
				return(NULL)
			}
		}		
		if (centroids) {
			xy <- coordinates(x)
			xy <- cbind(1:nrow(xy), xy)
			colnames(xy) <- c('object', 'x', 'y')
			xy <- as.data.frame(xy, row.names=row.names, optional=optional, ...)
			if (.hasSlot(x, 'data')) {
				return( cbind(xy, x@data ) )
			} else {
				return(xy)
			}
		}
		
		nobs <- length(x@polygons)
		objlist <- list()
		cnt <- 0
		if (sepNA) {
			sep <- rep(NA,5)
			for (i in 1:nobs) {
				nsubobs <- length(x@polygons[[i]]@Polygons)
				ps <- lapply(1:nsubobs, 
						function(j)
							rbind(cbind(j, j+cnt, x@polygons[[i]]@Polygons[[j]]@hole, x@polygons[[i]]@Polygons[[j]]@coords), sep)
						)
				objlist[[i]] <- cbind(i, do.call(rbind, ps))
				cnt <- cnt+nsubobs
			}
		} else {
			for (i in 1:nobs) {
				nsubobs <- length(x@polygons[[i]]@Polygons)
				ps <- lapply(1:nsubobs, 
						function(j) 
							cbind(j, j+cnt, x@polygons[[i]]@Polygons[[j]]@hole, x@polygons[[i]]@Polygons[[j]]@coords)
						)
				objlist[[i]] <- cbind(i, do.call(rbind, ps))
				cnt <- cnt+nsubobs
			}
		}
		
		obs <- do.call(rbind, objlist)
		colnames(obs) <- c('object', 'part', 'cump', 'hole', 'x', 'y')
		rownames(obs) <- NULL
		
		obs <- as.data.frame(obs, row.names=row.names, optional=optional, ...)
		
		if (.hasSlot(x, 'data')) {
			d <- x@data
			d <- data.frame(object=1:nrow(x), x@data)
			obs <- merge(obs, d, by=1)
		} 
		if (sepNA) {
			obs[is.na(obs[,2]), ] <- NA
		}
		return( obs )
	}
)



setMethod('as.data.frame', signature(x='SpatialLines'), 
	function(x, row.names=NULL, optional=FALSE, xy=FALSE, sepNA=FALSE, ...) {
		
		if (!xy) {
			if (.hasSlot(x, 'data')) {
				return( x@data )
			} else {
				return(NULL)
			}
		}
				
		nobj <- length(x@lines)
		objlist <- list()
		cnt <- 0
		if (sepNA) {
			sep <- rep(NA,4)
			for (i in 1:nobs) {
				nsubobj <- length(x@line[[i]]@Lines)
				ps <- lapply(1:nsubobj, 
						function(j) 
							rbind(cbind(j, j+cnt, x@lines[[i]]@Lines[[j]]@coords), sep)
						)
				objlist[[i]] <- cbind(i, do.call(rbind, ps))
				cnt <- cnt+nsubobj
			}
		} else {
			for (i in 1:nobj) {
				nsubobj <- length(x@line[[i]]@Lines)
				ps <- lapply(1:nsubobj, function(j) cbind(j, j+cnt, x@lines[[i]]@Lines[[j]]@coords))
				objlist[[i]] <- cbind(i, do.call(rbind, ps))
				cnt <- cnt+nsubobj
			}
		}
		obs <- do.call(rbind, objlist)
		colnames(obs) <- c('object', 'part', 'cump', 'x', 'y')
		rownames(obs) <- NULL

		obs <- as.data.frame(obs, row.names=row.names, optional=optional, ...)
		
		if (.hasSlot(x, 'data')) {
			d <- x@data
			d <- data.frame(object=1:nrow(x), x@data)
			obs <- merge(obs, d, by=1)
		} 

		if (sepNA) {
			obs[is.na(obs[,2]), ] <- NA
		}
		return (obs)
	}
)




setMethod('as.data.frame', signature(x='SpatialPoints'), 
	function(x, row.names=NULL, optional=FALSE, xy=FALSE, ...) {
		
		if (!xy) {
			if (.hasSlot(x, 'data')) {
				return( x@data )
			} else {
				return(NULL)
			}
		} else {
			xy <- coordinates(x)
			xy <- cbind(1:nrow(xy), xy)
			colnames(xy) <- c('object', 'x', 'y')
			xy <- as.data.frame(xy, row.names=row.names, optional=optional, ...)
			if (.hasSlot(x, 'data')) {
				xy <- data.frame(xy, x@data )
			} 
			return(xy)
		}
	}
)
		

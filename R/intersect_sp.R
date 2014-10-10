# Author: Robert J. Hijmans
# Date : December 2011
# Version 1.0
# Licence GPL v3

	
if (!isGeneric("intersect")) {
	setGeneric("intersect", function(x, y)
		standardGeneric("intersect"))
}	


setMethod('intersect', signature(x='SpatialPolygons', y='SpatialPolygons'), 
function(x, y) {

	require(rgeos)

	x <- spChFIDs(x, as.character(1:length(x)))
	y <- spChFIDs(y, as.character(1:length(y)))
		
	if (! identical(proj4string(x), proj4string(y)) ) {
		warning('non identical CRS')
		y@proj4string <- x@proj4string
	}	
	
	subs <- rgeos::gIntersects(x, y, byid=TRUE)
	if (sum(subs)==0) {
		warning('polygons do not intersect')
		return(NULL)
	}
		
	xdata <- .hasSlot(x, 'data')
	ydata <- .hasSlot(y, 'data')
	dat <- NULL
	if (xdata & ydata) {
		nms <- .goodNames(c(colnames(x@data), colnames(y@data)))
		colnames(x@data) <- xnames <- nms[1:ncol(x@data)]
		colnames(y@data) <- ynames <- nms[(ncol(x@data)+1):length(nms)]
		dat <- cbind(x@data[NULL, ,drop=FALSE], y@data[NULL, ,drop=FALSE])
	} else if (xdata) {
		dat <- x@data[NULL, ,drop=FALSE]
		xnames <- colnames(dat)
	} else if (ydata) {
		dat <- y@data[NULL, ,drop=FALSE]
		ynames <- colnames(dat)
	}

	subsx <- apply(subs, 2, any)
	subsy <- apply(subs, 1, any)
		
	int  <- rgeos::gIntersection(x[subsx,], y[subsy,], byid=TRUE, drop_lower_td=TRUE)
#	if (inherits(int, "SpatialCollections")) {
#		if (is.null(int@polyobj)) { # merely touching, no intersection
#			#warning('polygons do not intersect')
#			return(NULL)
#		}
#		int <- int@polyobj
#	}
	if (!inherits(int, 'SpatialPolygons')) {
		# warning('polygons do not intersect')
		return(NULL)
	}

	if (!is.null(dat)) {
		ids <- do.call(rbind, strsplit(row.names(int), ' '))
		rows <- 1:length(ids[,1])
		if (xdata) {
			idsx <- match(ids[,1], rownames(x@data))
			dat[rows, xnames] <- x@data[idsx, ]
		} 
		if (ydata) {
			idsy <- match(ids[,2], rownames(y@data))
			dat[rows, ynames] <- y@data[idsy, ]
		}
		rownames(dat) <- 1:nrow(dat)
		int <- spChFIDs(int, as.character(1:nrow(dat)))
		int <- SpatialPolygonsDataFrame(int, dat)
	}
	
	if (length(int) > 0) {
		j <- which(gIsValid(int, byid=TRUE, reason=FALSE))
		int <- int[j, ]
	}
	int	
} 
)



setMethod('intersect', signature(x='SpatialPoints', y='SpatialPolygons'), 
function(x, y) {
   if (!identical(proj4string(x), proj4string(y))) {
        warning("non identical CRS")
        y@proj4string <- x@proj4string
    }
    i <- over(as(x, "SpatialPoints"), as(y, "SpatialPolygons"))
    i <- which(!is.na(i))
    x[i, ]
}	
)


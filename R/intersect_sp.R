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

	requireNamespace("rgeos")

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
		j <- which(rgeos::gIsValid(int, byid=TRUE, reason=FALSE))
		int <- int[j, ]
	}
	int	
} 
)


setMethod('intersect', signature(x='SpatialPolygons', y='SpatialLines'), 
function(x, y) {
	intersect(y, x)
}
)

setMethod('intersect', signature(x='SpatialLines', y='SpatialPolygons'), 
function(x, y) {

	requireNamespace("rgeos")

	x <- spChFIDs(x, as.character(1:length(x)))
	y <- spChFIDs(y, as.character(1:length(y)))
		
	if (! identical(proj4string(x), proj4string(y)) ) {
		warning('non identical CRS')
		y@proj4string <- x@proj4string
	}	
	
	subs <- rgeos::gIntersects(x, y, byid=TRUE)
	if (sum(subs)==0) {
		warning('lines and polygons do not intersect')
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
	if (!inherits(int, 'SpatialLines')) {
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
		int <- SpatialLinesDataFrame(int, dat)
	}
	
	if (length(int) > 0) {
		j <- which(rgeos::gIsValid(int, byid=TRUE, reason=FALSE))
		int <- int[j, ]
	}
	int	
} 
)


setMethod('intersect', signature(x='SpatialPolygons', y='SpatialPoints'), 
function(x, y) {
	intersect(y, x)
}
)

setMethod('intersect', signature(x='SpatialPoints', y='SpatialPolygons'), 
function(x, y) {

	stopifnot(requireNamespace("rgeos"))
	
	if (! identical(proj4string(x), proj4string(y)) ) {
		warning('non identical CRS')
		y@proj4string <- x@proj4string
	}
    i <- rgeos::gIntersects(y, x, byid=TRUE)
	
	j <- cbind(1:length(y), rep(1:length(x), each=length(y)), as.vector(t(i)))
	j <- j[j[,3] == 1, -3]
	j <- j[order(j[,2]), ]
	x <- x[j[,2], ]
	
	if (.hasSlot(y, 'data')) {
		d <- y@data[j[,1], ]
		if (!.hasSlot(x, 'data')) {
			x <- SpatialPointsDataFrame(x, d)
		} else {
			x@data <- cbind(x@data, d)
		}
	} 
	x
}
)


setMethod('intersect', signature(x='SpatialVector', y='ANY'), 
function(x, y) {
	y <- extent(y)
	if (inherits(x, 'SpatialPoints')) {
		xy <- coordinates(x)
		i <- xy[,1] >= y@xmin & xy[,1] <= y@xmax & xy[,2] >= y@ymin & xy[,2] <= y@ymax
		x[i, ]
	} else {
		y <- as(y, 'SpatialPolygons')
		crs(y) <- crs(x)
		intersect(x, y)
	}
}
)



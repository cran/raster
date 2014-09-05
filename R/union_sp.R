# Author: Robert J. Hijmans
# Date : November 2011
# Version 1.0
# Licence GPL v3

if (!isGeneric("union")) {
	setGeneric("union", function(x, y)
		standardGeneric("union"))
}	


setMethod('union', signature(x='SpatialPolygons', y='SpatialPolygons'), 
function(x, y) {

	stopifnot(require(rgeos))

	x <- spChFIDs(x, as.character(1:length(x)))
	y <- spChFIDs(y, as.character(1:length(y)))

	if (! identical(proj4string(x), proj4string(y)) ) {
		warning('non identical CRS')
		y@proj4string <- x@proj4string
	}
	
	subs <- rgeos::gIntersects(x, y, byid=TRUE)
	
	if (!any(subs)) {
	
		x <- bind(x, y)
		
	} else {
	
		xdata <- .hasSlot(x, 'data')
		ydata <- .hasSlot(y, 'data')
		if (xdata & ydata) {
			nms <- .goodNames(c(colnames(x@data), colnames(y@data)))
			colnames(x@data) <- nms[1:ncol(x@data)]
			colnames(y@data) <- nms[(ncol(x@data)+1):length(nms)]
		} 
		
		dif1 <- erase(x, y)
		dif2 <- erase(y, x)
		
		x <- intersect(x, y)
		x <- list(dif1, dif2, x)
		x <- x[!sapply(x, is.null)]
		i <- sapply(x, length) # 
		x <- x[ i > 0]
		if (length(x) > 1) {
			x <- do.call(bind, x)
		} else {
			x <- x[[1]]
		}
		
		# remove slivers
		area <- sapply(x@polygons, function(i) slot(i, 'area'))
		x <- x[area > 0, ]
	}

	x
}
)



...simpleUnion <- function(x, y) {
	subs <- rgeos::gIntersects(x, y)
	if (!any(subs)) {
		x@polygons <- c(x@polygons, y@polygons)
		x <- spChFIDs(x, as.character(1:length(x)))
	} else {
		dif1 <- erase(x, y)
		dif2 <- erase(y, x)
		x <- intersect(x, y)
		x <- list(dif1, dif2, x)
		x <- x[!sapply(x, is.null)]
		i <- sapply(x, length) # 
		x <- x[ i > 0]
		y <- x[[1]]
		if (length(x) == 2) {
			y@polygons <- c(y@polygons, x[[2]]@polygons)
		} else if (length(x) == 3) {
			y@polygons <- c(y@polygons, x[[2]]@polygons, x[[3]]@polygons)		
		}
		# remove slivers
		y <- spChFIDs(y, as.character(1:length(y)))
		area <- sapply(y@polygons, function(i) slot(i, 'area'))
		x <- y[area > 0, ]
	}
	
	x
}





setMethod('union', signature(x='SpatialPolygons', y='missing'), 
function(x, y) {
	stopifnot(require(rgeos))
	n <- length(x)
	if (n < 2) {
		return(x)
	}
	if (.hasSlot(x, 'data')) {
		x <- as(x, 'SpatialPolygons')
	}
	
	x <- spChFIDs(x, as.character(1:length(x)))
	x <- SpatialPolygonsDataFrame(x, data.frame(ID=1:n))

	u <- x[1,]
	names(u) <- 'ID.1'
	for (i in 2:n) {
		u <- union(u, x[i, ])
		names(u)[i] <- paste('ID.', i, sep='')
	}
	
	u@data[!is.na(u@data)] <- 1
	u@data[is.na(u@data)] <- 0
	u$count <- rowSums(u@data)
	
	u
}	
)



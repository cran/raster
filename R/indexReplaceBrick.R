# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  January 2009
# Version 1.0
# Licence GPL v3


setReplaceMethod("[", c("RasterStackBrick", "Raster", "missing"),
	function(x, i, j, value) {
	
		if (! hasValues(i) ) {
			i <- cellsFromExtent(x, i)
		} else if (compare(x, i, stopiffalse=FALSE, showwarning=FALSE)) {
			dims <- dim(i)
			i <- as.logical(getValues(i))
			dim(i) <- c(prod(dims[1:2]), dims[3])
		} else {
			i <- cellsFromExtent(x, i)
		}			
		.replace(x, i, value=value) 
	}
)


setReplaceMethod("[", c("Raster", "Extent", "missing"),
	function(x, i, j, value) {
		i <- cellsFromExtent(x, i)
		nl <- nlayers(x)
		if (nl > 1) {
			add <- ncell(x) * 0:(nl-1)
			i <- as.vector(t((matrix(rep(i, nl), nr=nl, byrow=TRUE)) + add))
		}
		.replace(x, i, value=value)
	}
)



setReplaceMethod("[", c("Raster", "Spatial", "missing"),
	function(x, i, j, value) {

		if (inherits(i, 'SpatialPolygons')) {
			v <- 1:length(i@polygons)
			v[] <- value
			return( .polygonsToRaster(i, x, field=v, fun='last', mask=FALSE, update=TRUE, updateValue="all", silent=TRUE) )
			
		} else if (inherits(i, 'SpatialLines')) {
			v <- 1:length(i@lines)
			v[] <- value
			return( .linesToRaster(i, x, field=v, fun='last', mask=FALSE, update=TRUE, updateValue="all", silent=TRUE) )
			
		} else { # if (inherits(i, 'SpatialPoints')) {
			i <- cellsFromXY(x, coordinates(i))

			return( .replace(x, i, value=value) )
		}
	}
)


setReplaceMethod("[", c("RasterStackBrick","missing","missing"),
	function(x, i, j, value) {
	
		nl <- nlayers(x)
		if (inherits(x, 'RasterStack')) {
			x <- brick(x, values=FALSE)
		}
		
		if (is.matrix(value)) {
			if (all(dim(value)) == c(ncell(x), nl)) {
				x <- try( setValues(x, value))
			} else {
				stop('dimensions of the matrix do not match the Raster* object')
			}
			
		} else {
			v <- try( matrix(nrow=ncell(x), ncol=nl) )
			if (class(x) != 'try-error') {
				v[] <- value
				x <- try( setValues(x, v) )
			}
		}
		if (class(x) == 'try-error') {
			stop('cannot set values on this raster (it is too large)')
		}
		return(x)
	
	}
)

setReplaceMethod("[", c("Raster", "numeric", "numeric"),
	function(x, i, j, value) {
		i <- cellFromRowColCombine(x, i, j)
		.replace(x, i, value)
	}
)	

setReplaceMethod("[", c("Raster","missing", "numeric"),
	function(x, i, j, value) {
		j <- cellFromCol(x, j)
		.replace(x, j, value=value)
	}
)


setReplaceMethod("[", c("Raster","numeric", "missing"),
	function(x, i, j, value) {
		theCall <- sys.call(-1)
		narg <- length(theCall)-length(match.call(call=sys.call(-1)))
		if (narg > 0) {
			i <- cellFromRow(x, i)
		}
		.replace(x, i=i, value=value)
	}
)


setReplaceMethod("[", c("Raster", "matrix", "missing"),
	function(x, i, j, value) {
		if (ncol(i) == 2) {
			i <- cellFromRowCol(x, i[,1], i[,2])
		} else {
			i <- as.vector(i)
		}
		.replace(x, i=i, value=value)
	}
)



setReplaceMethod("[", c("Raster", "logical", "missing"),
	function(x, i, j, value) {
		.replace(x, i, value)
	}
)	


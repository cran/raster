# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : November 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("xyValues")) {
	setGeneric("xyValues", function(object, xy, ...)
		standardGeneric("xyValues"))
}	


setMethod("xyValues", signature(object='Raster', xy='SpatialPoints'), 
	function(object, xy, ...) { 
		callGeneric(object, coordinates(xy), ...)
	}	
)


setMethod("xyValues", signature(object='Raster', xy='data.frame'), 
	function(object, xy, method='simple', ...) { 
		callGeneric(object, as.matrix(xy), ...)
	}	
)


setMethod("xyValues", signature(object='Raster', xy='vector'), 
	function(object, xy, method='simple', ...) { 
		callGeneric(object, matrix(xy, ncol=2), ...)
	}
)

	
setMethod("xyValues", signature(object='RasterLayer', xy='matrix'), 
	function(object, xy, method='simple', buffer=NULL, fun=NULL, na.rm=TRUE) { 

		if (dim(xy)[2] != 2) {
			stop('xy has wrong dimensions; it should have 2 columns' )
		}

		if (! is.null(buffer)) {
			buffer <- abs(buffer)
			if (is.atomic(buffer)) {
				if (buffer!=0)  {
					return( .xyvBuf(object, xy, buffer, fun, na.rm=na.rm) )
				}
			} else {
				if (min(buffer)<0 | max(buffer)>0) {
					.xyvBuf(object, xy, buffer, fun, na.rm=na.rm)
				}
			}
		}

		if (method=='bilinear') {
			return(.bilinearValue(object, xy))
		} else if (method=='simple') {
			cells <- cellFromXY(object, xy)
			return(.readCells(object, cells))
		} else {
			stop('invalid method argument. Should be simple or bilinear.')
		}
	}	
)	


setMethod("xyValues", signature(object='RasterStack', xy='matrix'), 
	function(object, xy, method='simple', buffer=NULL, fun=NULL, na.rm=TRUE) { 
		if (dim(xy)[2] != 2) {
			stop('xy has wrong dimensions; there should be 2 columns only' )
		}
		
		if (! is.null(buffer)) {
			buffer <- abs(buffer)
			if (is.atomic(buffer)) {
				if (buffer!=0)  {
					return( .xyvBuf(object, xy, buffer, fun, na.rm) )
				}
			} else {
				if (min(buffer)<0 | max(buffer)>0) {
					.xyvBuf(object, xy, buffer, fun, na.rm)
				}
			}
		}

		if (method == 'bilinear') {
			result <- matrix(ncol=nlayers(object), nrow=nrow(xy))
			for (i in seq(nlayers(object))) {
				result[,i] <- .bilinearValue(object@layers[[i]], xy)
			}
			if (!(is.null(dim(result)))) {
				colnames(result) <- layerNames(object)
			}	
			return( result )
	
		} else if (method=='simple') {
			cells <- cellFromXY(object, xy)
			return( cellValues(object, cells) )
		} else {
			stop('invalid method argument. Should be simple or bilinear.')
		}
	}
)



setMethod("xyValues", signature(object='RasterBrick', xy='matrix'), 
	function(object, xy, method='simple', buffer=NULL, fun=NULL, na.rm=TRUE) { 
		if (dim(xy)[2] != 2) {
			stop('xy has wrong dimensions; there should be 2 columns only' )
		}
		
		if (! is.null(buffer)) {
			buffer <- abs(buffer)
			if (is.atomic(buffer)) {
				if (buffer!=0)  {
					return( .xyvBuf(object, xy, buffer, fun, na.rm) )
				}
			} else {
				if (min(buffer)<0 | max(buffer)>0) {
					.xyvBuf(object, xy, buffer, fun, na.rm)
				}
			}
		}

		if (method == 'bilinear') {
			for (i in seq(nlayers(object))) {
				r <- raster(object, i)
				v <- .bilinearValue(r, xy)
				if (i == 1) {
					result <- v
				} else {
					result <- cbind(result, v)
				}
			}
			if (!(is.null(dim(result)))) {
				colnames(result) <- layerNames(object)
			}	
			return(result)		
	
		} else if (method=='simple') {
			cells <- cellFromXY(object, xy)
			return(.brickReadCells(object, cells))
		} else {
			stop('invalid method argument. Should be simple or bilinear.')
		}
	}
)


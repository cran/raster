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
	function(object, xy, ...) { 
		callGeneric(object, as.matrix(xy), ...)
	}	
)


setMethod("xyValues", signature(object='Raster', xy='vector'), 
	function(object, xy, ...) { 
		if (length(xy) == 2) {
			callGeneric(object, matrix(xy, ncol=2), ...)
		} else {
			stop('xy coordinates should be a two-column matrix or data.frame, or a vector of two numbers.')
		}
	} )

	
setMethod("xyValues", signature(object='RasterLayer', xy='matrix'), 
	function(object, xy, method='simple', buffer=NULL, fun=NULL, na.rm=TRUE) { 

		if (dim(xy)[2] != 2) {
			stop('xy has wrong dimensions; it should have 2 columns' )
		}

		if (! is.null(buffer)) {
			return( .xyvBuf(object, xy, buffer, fun, na.rm=na.rm) )
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
	function(object, xy, method='simple', buffer=NULL, fun=NULL, na.rm=TRUE, ...) { 
		.xyvStackBrick(object, xy, method, buffer, fun, na.rm, ...)
} )

setMethod("xyValues", signature(object='RasterBrick', xy='matrix'), 
	function(object, xy, method='simple', buffer=NULL, fun=NULL, na.rm=TRUE, ...) { 
		.xyvStackBrick(object, xy, method, buffer, fun, na.rm, ...)
} )


.xyvStackBrick <- function(object, xy, method='simple', buffer=NULL, fun=NULL, na.rm=TRUE, ...) { 

		dots <- list(...)
		layer <- dots$layer
		n <- dots$nlayers
		nl <- nlayers(object)
		if (is.null(layer)) { layer <- 1 } 
		if (is.null(n)) { n <- nl } 
	
		layer <- min(max(1, round(layer)), nl)
		maxnl = nl - layer + 1
		nlayers <- min(max(1, round(n)), maxnl)
	
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
			lyrs <- layer:(layer+nlayers-1)
			result <- matrix(nrow=nrow(xy), ncol=nlayers)
			for (i in 1:nlayers ) {
				j <- lyrs[i]
				r <- raster(object, j)
				result[,i] <- .bilinearValue(r, xy)
			}
			if (!(is.null(dim(result)))) {
				colnames(result) <- layerNames(object)
			}	
			return(result)		
	
		} else if (method=='simple') {
		
			cells <- cellFromXY(object, xy)
			return( cellValues(object, cells, layer=layer, n=n) )
			
		} else {
			stop('invalid method argument. Should be simple or bilinear.')
		}
	}



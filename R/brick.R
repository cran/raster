# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  September 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric("brick")) {
	setGeneric("brick", function(x, ...)
		standardGeneric("brick"))
}	



setMethod('brick', signature(x='missing'), 
	function(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90, nlayers=1, crs) {
		e <- extent(xmn, xmx, ymn, ymx)
		if (missing(crs)) {
			if (e@xmin > -400 & e@xmax < 400 & e@ymin > -90.1 & e@ymax < 90.1) { 
				crs ="+proj=longlat +datum=WGS84"
			} else {
				crs=NA
			}
		}
		b <- brick(e, nrows=nrows, ncols=ncols, crs=crs, nlayers=nlayers)
		return(b)
	}
)



setMethod('brick', signature(x='character'), 
	function(x, values=FALSE, crs=NULL, ...) {
		b <- .rasterObjectFromFile(x, objecttype='RasterBrick', ...)
		if (values) {
			b <- readAll(b)
		}
		if (!is.null(crs)) {
			projection(b) <- crs
		}
		return(b)
	}
)


setMethod('brick', signature(x='Raster'), 
	function(x, ...) {
		b <- new('RasterBrick')
		return( addLayer(b, x, ..., keepone=TRUE) )
	}
)


setMethod('brick', signature(x='RasterStack'), 
	function(x, values=TRUE){
		if (values) {
			b <- new('RasterBrick')
			b <- addLayer(b, x@layers)
		} else {
			b <- brick(raster(x))
			b@data@nlayers <- nlayers(x)
			b@data@min <- rep(Inf, b@data@nlayers)
			b@data@max <- rep(-Inf, b@data@nlayers)
			b@data@source <- 'ram'

		}
		layerNames(b) <- layerNames(x)
		
		return(b)
	}
)

setMethod('brick', signature(x='RasterBrick'), 
	function(x, ...){
		x <- clearValues(x)
		x@data@source <- 'ram'
		filename(x) <- ''
		return(x)
	}
)



setMethod('brick', signature(x='Extent'), 
	function(x, nrows=10, ncols=10, crs=NA, nlayers=1) {
		bb <- extent(x)
		nr = as.integer(round(nrows))
		nc = as.integer(round(ncols))
		if (nc < 1) { stop("ncols should be > 0") }
		if (nr < 1) { stop("nrows should be > 0") }
		b <- new("RasterBrick", extent=bb, ncols=nc, nrows=nr)
		projection(b) <- crs
		b@data@nlayers <- as.integer(nlayers)
		return(b) 
	}
)


setMethod('brick', signature(x='SpatialGrid'), 
	function(x){
		b <- brick()
		extent(b) <- extent(x)
		projection(b) <- x@proj4string
		rowcol(b) <- c(x@grid@cells.dim[2], x@grid@cells.dim[1])	
		
		if (class(x) == 'SpatialGridDataFrame') {
			for (i in 1:ncol(x@data)) {
				if (is.character(x@data[,i])) {
					x@data[,i] =  as.factor(x@data[,i])
				}
				if (is.factor(x@data[,i])) {
					x@data[,i] = as.numeric(x@data[,i])
				}
			}
			b <- setValues(b, as.matrix(x@data))
			b@layernames <- colnames(x@data)
		}
		return(b)
	}	
)


setMethod('brick', signature(x='SpatialPixels'), 
	function(x) {
		if (inherits( x, 'SpatialPixelsDataFrame')) {
			x <- as(x, 'SpatialGridDataFrame')
		} else {	
			x <- as(x, 'SpatialGrid')
		}
		return(brick(x))
	}
)



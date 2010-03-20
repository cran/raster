# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  September 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric("brick")) {
	setGeneric("brick", function(x, ...)
		standardGeneric("brick"))
}	



setMethod('brick', signature(x='missing'), 
	function(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90, projs="+proj=longlat +datum=WGS84", nlayers=1) {
		ext <- extent(xmn, xmx, ymn, ymx)
		b <- brick(ext, nrows=nrows, ncols=ncols, projs=projs, nlayers=nlayers)
		return(b)
	}
)



setMethod('brick', signature(x='character'), 
	function(x, values=FALSE, proj=NULL, ...) {
		b <- .rasterObjectFromFile(x, objecttype='RasterBrick', ...)
		if (values) {
			b <- readAll(b)
		}
		if (!is.null(proj)) {
			projection(b) <- proj
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
	function(x){
		b <- new('RasterBrick')
		b <- addLayer(b, x@layers)
		layerNames(b) <- layerNames(x)
		return(b)
	}
)

setMethod('brick', signature(x='RasterBrick'), 
	function(x){
		x <- clearValues(x)
		filename(x) <- ''
		return(x)
	}
)



setMethod('brick', signature(x='Extent'), 
	function(x, nrows=10, ncols=10, projs=NA, nlayers=1) {
		bb <- extent(x)
		nr = as.integer(round(nrows))
		nc = as.integer(round(ncols))
		if (nc < 1) { stop("ncols should be > 0") }
		if (nr < 1) { stop("nrows should be > 0") }
		b <- new("RasterBrick", extent=bb, ncols=nc, nrows=nr)
		projection(b) <- projs
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
			b <- setValues(r, x@data)
		}
		return(b)
	}	
)


setMethod('brick', signature(x='SpatialPixels'), 
	function(x) {
		b <- brick()
		exent(b) <- extent(x)
		projection(b) <- x@proj4string
		rowcol(b) <- c(x@grid@cells.dim[2], x@grid@cells.dim[1])
		if (class(x) == 'SpatialPixelsDataFrame') {
			x <- as(x, 'SpatialGridDataFrame')
			b <- setValues(b, x@data)
		}
		return(b)
	}
)



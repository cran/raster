# R raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2008
# Version 0.9
# Licence GPL v3
	
if (!isGeneric("raster")) {
	setGeneric("raster", function(x, ...)
		standardGeneric("raster"))
}	



setMethod('raster', signature(x='missing'), 
	function(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90, crs) {
		e <- extent(xmn, xmx, ymn, ymx)
		if (missing(crs)) {
			if (e@xmin > -400 & e@xmax < 400 & e@ymin > -90.1 & e@ymax < 90.1) { 
				crs ="+proj=longlat +datum=WGS84"
			} else {
				crs=NA
			}
		}
		r <- raster(e, nrows=nrows, ncols=ncols, crs=crs)
		return(r)
	}
)


setMethod('raster', signature(x='matrix'), 
	function(x, xmn=0, xmx=1, ymn=0, ymx=1, crs=NA) {
		r <- raster(ncols=ncol(x), nrows=nrow(x), xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx, crs=crs)
		r <- setValues(r, as.vector(t(x)))
		return(r)
	}
)


setMethod('raster', signature(x='character'), 
	function(x, band=1, values=FALSE, crs=NULL, ...) {
		r <- .rasterObjectFromFile(x, band=band, objecttype='RasterLayer', ...)
		if (! is.null(crs)) {
			projection(r) = crs
		}
		if (values) {
			r <- readAll(r)
		}
		return(r)
	}
)


setMethod('raster', signature(x='Raster'), 
	function(x, filename="", values=NULL) {
		r <- raster(xmn=xmin(x), xmx=xmax(x), ymn=ymin(x), ymx=ymax(x), nrows=nrow(x), ncols=ncol(x), crs=projection(x))
		filename(r) <- filename
		if (!is.null(values)) {
			x <- setValues(x, values)
		}
		return(r)
	}
)


setMethod('raster', signature(x='RasterStack'), 
	function(x, layer=0){
		newindex = -1
		if (nlayers(x) > 0) {
			if (!is.numeric(layer)) {
				newindex <- which(layerNames(x) == layer)[1]
				if (is.na (newindex) ) { 
					warning('variable', layer, 'does not exist')
					newindex = -1
				} 
				layer <- newindex
			}
		}
		if ( layer > 0 ) {
			dindex <- max(1, min(nlayers(x), layer))
			if (dindex != layer) { warning(paste("layer was changed to", dindex))}
			r <- x@layers[[dindex]]
			layerNames(r) <- layerNames(x)[dindex]
		} else {
			r <- raster(extent(x))
			rowcol(r) <- c(nrow(x), ncol(x))
			projection(r) <- projection(x)
		}
		extent(r) <- extent(x) # perhaps it was changed by user and different on disk
		return(r)
	}
)


setMethod('raster', signature(x='RasterBrick'), 
	function(x, layer=0){
		newindex = -1
		if (nlayers(x) > 0) {
			if (!is.numeric(layer)) {
				newindex <- which(layerNames(x) == layer)[1]
				if (is.na (newindex) ) { 
					warning('variable', layer, 'does not exist')
					newindex = -1
				} 
				layer <- newindex
			}
			layer <- round(layer)
		}
		if (layer > 0) {
			dindex <- max(1, min(nlayers(x), layer))
			if ( fromDisk(x) ) {
				if (dindex != layer) { warning(paste("layer was changed to", dindex))}
				if (x@file@driver == 'netcdf') {
					r <- raster(x@file@name, varname=x@data@zvar, band=dindex)				
				} else {
					r <- raster(filename(x), band=dindex)
				}
				layerNames(r) <- layerNames(x)[dindex]
				extent(r) <- extent(x) # perhaps it was changed by user and different on disk
			} else {
				r <- raster(extent(x), nrows=nrow(x), ncols=ncol(x), crs=projection(x))	
				if ( inMemory(x) ) {
					if (dindex != layer) { warning(paste("layer was changed to", dindex))}
					r <- setValues(r, getValues(x)[,dindex])
				}
			}
		} else {
			r <- raster(extent(x), nrows=nrow(x), ncols=ncol(x), crs=projection(x))	
		}
		return(r)
	}
)


setMethod('raster', signature(x='Extent'), 
	function(x, nrows=10, ncols=10, crs=NA) {
		nrows = as.integer(max(1, round(nrows)))
		ncols = as.integer(max(1, round(ncols)))
		r <- new("RasterLayer", extent=x, ncols=ncols, nrows=nrows)
		projection(r) <- crs
		return(r)
	}
)


setMethod('raster', signature(x='Spatial'), 
	function(x){
		raster(extent(x))
	}
)


setMethod('raster', signature(x='SpatialGrid'), 
	function(x, layer=1){
		r <- raster(extent(x))
		projection(r) <- x@proj4string
		rowcol(r) <- c(x@grid@cells.dim[2], x@grid@cells.dim[1])	
		if (class(x) == 'SpatialGridDataFrame') {
			if (dim(x@data)[2] > 0) {
				layer = layer[1]
				if (is.numeric(layer)) {
					if (layer > 0) {
						dindex <- max(1, min(dim(x@data)[2], layer))
						if (dindex != layer) { warning(paste("layer was changed to", dindex))}
						layer <- dindex
					}
					layerNames(r) <- colnames(x@data)[layer]
				} else if (!(layer %in% names(x))) {
					stop(layer, 'does not exist')
				} else {
					layerNames(r) <- layer
				}

				if (is.character( x@data[[layer]]) ) { 
					x@data[[layer]] <- as.factor(x@data[[layer]])
				}
				if (is.factor( x@data[[layer]]) ) { 
					r@data@isfactor <- TRUE 
					r@data@levels <- levels(x@data[[layer]])
					r <- setValues(r, as.numeric(x@data[[layer]]))
				} else {
					r <- setValues(r, x@data[[layer]])
				}
			}
			
		}
		return(r)
	}	
)


setMethod('raster', signature(x='SpatialPixels'), 
	function(x, layer=1){
		if (class(x) == 'SpatialPixelsDataFrame') {
			x <- as(x[layer], 'SpatialGridDataFrame')
			return(raster(x, 1))
		} else {
			x <- as(x, 'SpatialGrid')
			return(raster(x))		
		}
		return(r)
	}
)


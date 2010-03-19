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
	function(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90, projs="+proj=longlat +datum=WGS84") {
		ext <- extent(xmn, xmx, ymn, ymx)
		r <- raster(ext, nrows=nrows, ncols=ncols, projs=projs)
		return(r)
	}
)


setMethod('raster', signature(x='matrix'), 
	function(x, xmn=0, xmx=1, ymn=0, ymx=1, projs=NA) {
		r <- raster(ncols=ncol(x), nrows=nrow(x), projs=projs, xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx)
		r <- setValues(r, as.vector(t(x)))
		return(r)
	}
)


setMethod('raster', signature(x='character'), 
	function(x, values=FALSE, band=1, proj=NULL, ...) {
		r <- .rasterObjectFromFile(x, band=band, objecttype='RasterLayer', ...)
		if (! is.null(proj)) {
			projection(r) = proj
		}
		if (values) {
			r <- readAll(r)
		}
		return(r)
	}
)


setMethod('raster', signature(x='Raster'), 
	function(x, filename="", values=NULL) {
		r <- raster(xmn=xmin(x), xmx=xmax(x), ymn=ymin(x), ymx=ymax(x), nrows=nrow(x), ncols=ncol(x), projs=projection(x))
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
			if (dataSource(x) == 'disk') {
				if (dindex != layer) { warning(paste("layer was changed to", dindex))}
				r <- raster(filename(x), band=dindex)
				layerNames(r) <- layerNames(x)[dindex]
			} else {
				r <- raster(extent(x), nrows=nrow(x), ncols=ncol(x), projs=projection(x))	
				if (dataContent(x) == 'all') {
					if (dindex != layer) { warning(paste("layer was changed to", dindex))}
					r <- setValues(r, values(x)[,dindex])
				}
			}
		} else {
			r <- raster(extent(x), nrows=nrow(x), ncols=ncol(x), projs=projection(x))	
		}
		return(r)
	}
)


setMethod('raster', signature(x='Extent'), 
	function(x, nrows=10, ncols=10, projs=NA) {
		nrows = as.integer(max(1, round(nrows)))
		ncols = as.integer(max(1, round(ncols)))
		r <- new("RasterLayer", extent=x, ncols=ncols, nrows=nrows)
		projection(r) <- projs
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
				} else if (!(layer %in% names(x))) {
					stop(layer, 'does not exist')
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
		r <- raster()
		extent(r) <- extent(x)
		projection(r) <- x@proj4string
		rowcol(r) <- c(x@grid@cells.dim[2], x@grid@cells.dim[1])
		if (class(x) == 'SpatialPixelsDataFrame') {
			if (dim(x@data)[2] > 0) {
				layer = layer[1]
				if (is.numeric(layer)) {
					if (layer > 0) {
						dindex <- max(1, min(dim(x@data)[2], layer))
						if (dindex != layer) { warning(paste("layer was changed to", dindex))}
						layer <- dindex
					}
				} else if (!(layer %in% names(x))) {
					stop(layer, 'does not exist')
				}
				x <- x[layer]
				x <- as(x, 'SpatialGridDataFrame')
				if (is.character( x@data[[1]]) ) { 
					x@data[[1]] <- as.factor(x@data[[1]])
				}
				if (is.factor( x@data[[1]]) ) { 
					r@data@isfactor <- TRUE 
					r@data@levels <- levels(x@data[[1]])
					r <- setValues(r, as.numeric(x@data[[1]]))
				} else {
					r <- setValues(r, x@data[[1]])
				}
			}
		}
		return(r)
	}
)


# R raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2008
# Version 0.9
# Licence GPL v3
	
if ( !isGeneric("raster") ) {
	setGeneric("raster", function(x, ...)
		standardGeneric("raster"))
}	



setMethod('raster', signature(x='missing'), 
	function(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90, crs) {
		e <- extent(xmn, xmx, ymn, ymx)
		if (missing(crs)) {
			if (e@xmin > -360.1 & e@xmax < 360.1 & e@ymin > -90.1 & e@ymax < 90.1) { 
				crs ="+proj=longlat +datum=WGS84"
			} else {
				crs=NA
			}
		}
		r <- raster(e, nrows=nrows, ncols=ncols, crs=crs)
		return(r)
	}
)


setMethod('raster', signature(x='list'), 
	function(x, crs) {
	# list should represent an "image"
		if (is.null(x$x)) { stop('list has no "x"') }
		if (is.null(x$y)) { stop('list has no "y"') }
		if (is.null(x$z)) { stop('list has no "z"') }
		if (! all(dim(x$z) == c(length(x$x), length(x$y)))) { stop('"z" does not have the right dimensions') }

		resx <- ( x$x[length(x$x)] - x$x[1] ) / length(x$x)
		resy <- ( x$y[length(x$y)] - x$y[1] ) / length(x$y)
		xmn <- min(x$x) - 0.5 * resx
		xmx <- max(x$x) + 0.5 * resx
		ymn <- min(x$y) - 0.5 * resy
		ymx <- max(x$y) + 0.5 * resy

		if (missing(crs)) {
			if (xmn > -360.1 & xmx < 360.1 & ymn > -90.1 & ymx < 90.1) { 
				crs = "+proj=longlat +datum=WGS84"
			} else {
				crs = NA
			}
		} 
		
		x <- t(x$z)
		x <- x[nrow(x):1, ]
		r <- raster( x, xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx, crs=crs )
		
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
	function(x, band=1, crs=NULL, ...) {
	
		x <- .fullFilename(x)
		
		r <- .rasterObjectFromFile(x, band=band, objecttype='RasterLayer', ...)
		if (! is.null(crs)) {
			projection(r) = crs
		}
		return(r)
	}
)


setMethod('raster', signature(x='BasicRaster'), 
	function(x) {
		e <- x@extent
		r <- raster(xmn=e@xmin, xmx=e@xmax, ymn=e@ymin, ymx=e@ymax, nrows=x@nrows, ncols=x@ncols, crs=x@crs)
		if (rotated(x)) {
			r@rotated <- TRUE
			r@rotation <- x@rotation
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
			dim(r) <- c(nrow(x), ncol(x))
			projection(r) <- projection(x)
		}
		extent(r) <- extent(x) # perhaps it was changed by user and different on disk
		if (rotated(x@layers[[1]])) {
			r@rotated <- TRUE
			r@rotation <- x@layers[[1]]@rotation
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
			dindex <- as.integer(max(1, min(nlayers(x), layer)))
			
			if ( fromDisk(x) ) {
				if (dindex != layer) { warning(paste("layer was changed to", dindex))}
				
				r <- raster(extent(x), nrows=nrow(x), ncols=ncol(x), crs=projection(x))	
				r@file <- x@file

				r@data@offset <- x@data@offset
				r@data@gain <- x@data@gain
				r@data@inmemory <- x@data@inmemory
				r@data@fromdisk <- x@data@fromdisk
				r@data@isfactor <- x@data@isfactor
				r@data@haveminmax <- x@data@haveminmax

				r@data@band <- dindex
				r@data@min <- x@data@min[dindex]
				r@data@max <- x@data@max[dindex]
				ln <- x@layernames[dindex]
				if (! is.na(ln) ) { r@layernames <- ln }
				zv <- x@zvalue[dindex]
				if (! is.na(zv) ) { r@zvalue <- zv }
				if ( x@data@inmemory ) {
					r@data@values <- x@data@values[,dindex]
				}
				zvar <- try(slot(x@data, 'zvar'), silent=TRUE)
				if (class(zvar) != 'try-error') {
					attr(r@data, "zvar") <- zvar
					attr(r@data, "dim3") <- x@data@dim3
					attr(r@data, "level") <- x@data@level
				}
				
			} else {
			
				r <- raster(extent(x), nrows=nrow(x), ncols=ncol(x), crs=projection(x))	
				if ( inMemory(x) ) {
					if ( dindex != layer ) { warning(paste("layer was changed to", dindex)) }
					r <- setValues(r, x@data@values[,dindex])
					ln <- x@layernames[dindex]
					if (! is.na(ln) ) { r@layernames <- ln }
				}
			}
			r@data@offset <- x@data@offset
			r@data@gain <- x@data@gain
			r@file@nodatavalue <- x@file@nodatavalue
			
		} else {
			r <- raster(extent(x), nrows=nrow(x), ncols=ncol(x), crs=projection(x))	
		}

		if (rotated(x)) {
			r@rotated <- TRUE
			r@rotation <- x@rotation
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
		r <- raster(extent(x))
		projection(r) <- x@proj4string
		r
	}
)


setMethod('raster', signature(x='SpatialGrid'), 
	function(x, layer=1, values=TRUE){
		r <- raster(extent(x))
		projection(r) <- x@proj4string
		dim(r) <- c(x@grid@cells.dim[2], x@grid@cells.dim[1])	
		
		if (inherits(x, 'SpatialGridDataFrame') & values) {
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
					#r@data@levels <- levels(x@data[[layer]])
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
		if (inherits(x, 'SpatialPixelsDataFrame')) {
			x <- as(x[layer], 'SpatialGridDataFrame')
			return(raster(x, 1))
		} else {
			x <- as(x, 'SpatialGrid')
			return(raster(x))		
		}
		return(r)
	}
)



setMethod('raster', signature(x='kasc'), 
	function(x, crs) {
		x <- as(x, 'RasterLayer')
		if (missing(crs)) {
			e <- x@extent
			if (e@xmin > -360.1 & e@xmax < 360.1 & e@ymin > -90.1 & e@ymax < 90.1) { 
				crs = "+proj=longlat +datum=WGS84"
			} else {
				crs = NA
			}
		}
		projection(x) <- crs
		return(x)
	}
)



setMethod('raster', signature(x='asc'), 
	function(x, crs) {
		x <- as(x, 'RasterLayer')
		if (missing(crs)) {
			e <- x@extent
			if (e@xmin > -360.1 & e@xmax < 360.1 & e@ymin > -90.1 & e@ymax < 90.1) { 
				crs = "+proj=longlat +datum=WGS84"
			} else {
				crs = NA
			}
		}
		projection(x) <- crs
		return(x)
	}
)

	
setMethod('raster', signature(x='kde'), 
	function(x, crs) {
		x <- as(x, 'RasterLayer')
		if (missing(crs)) {
			e <- x@extent
			if (e@xmin > -360.1 & e@xmax < 360.1 & e@ymin > -90.1 & e@ymax < 90.1) { 
				crs = "+proj=longlat +datum=WGS84"
			} else {
				crs = NA
			}
		}
		projection(x) <- crs
		return(x)
	}
)




setMethod('raster', signature(x='grf'), 
	function(x, i=1) {
		i <- max(1, i[1])
		if (i != 1) {
			nc <- NCOL(x$data)
			if (i <= nc) {
				x$data <- x$data[,i]
			} else {
				stop('i is higher than the number of simulations in x')
			}
		}
		as(x, 'RasterLayer')
	}
)


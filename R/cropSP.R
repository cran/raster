# Author: Robert J. Hijmans
# Date : November 2011
# Version 1.0
# Licence GPL v3


setMethod('crop', signature(x='Spatial', y='ANY'), 

function(x, y, ...) {

	if (inherits(x, 'SpatialPolygons')) {
	
		require(rgeos)

		if (! inherits(y, 'SpatialPolygons')) {
			if (inherits(y, 'Extent')) {
				y <- as(y, 'SpatialPolygons')
				y@proj4string <- x@proj4string
			} else { 
				y <- extent(y)
				validObject(y)
				y <- as(y, 'SpatialPolygons')
			}
			y@proj4string <- x@proj4string		
		}
		if (version_GEOS0() < "3.3.0") {
			y <- gUnionCascaded(y)
		} else {
			y <- gUnaryUnion(y)
		}	
		row.names(y) <- '1'
		rnx <- row.names(x)
		row.names(x) <- as.character(1:length(rnx))

		if (! identical(projection(x), projection(y)) ) {
			warning('non identical CRS')
			y@proj4string <- x@proj4string
		}
		
		
		if (.hasSlot(x, 'data')) {
			
			# to keep the correct IDs
			# in future versions of rgeos, this intermediate step won't be necessary
			i <- as.vector( gIntersects(x, y, byid=TRUE) )
			if (sum(i) == 0) {
				return(NULL)
			}
			y <- gIntersection(x[i,], y, byid=TRUE)
			if (inherits(y, "SpatialCollections")) {
				y <- y@polyobj
			}
			if (is.null(y)) { return(y) }
			
			ids <- strsplit(row.names(y), ' ') 
			ids <- as.numeric(do.call(rbind, ids)[,1])
			row.names(y) <- as.character(rnx[ids])
			data <- x@data[ids, ,drop=FALSE]
			rownames(data) <- rnx[ids]
			
			return( SpatialPolygonsDataFrame(y, data) )
		} else {
			y <- gIntersection(x, y)
			if (inherits(y, "SpatialCollections")) {
				y <- y@polyobj
			}
			return(y)
		}
		
	} else if (inherits(x, 'SpatialLines')) {
	
		require(rgeos)

		if (! inherits(y, 'SpatialPolygons')) {
			if (inherits(y, 'Extent')) {
				y <- as(y, 'SpatialPolygons')
			} else { 
				y <- as(extent(y), 'SpatialPolygons')
			}
			y@proj4string <- x@proj4string		
		}
		
		if (.hasSlot(x, 'data')) {
		
			# in future versions of rgeos, this intermediate step should not be necessary
			i <- as.vector( gIntersects(x, y, byid=TRUE) )
			if (sum(i) == 0) {
				return(NULL)
			}
			y <- gIntersection(x[i,], y, byid=TRUE)
			if (inherits(y, "SpatialCollections")) {
				y <- y@lineobj
			}
			
			ids <- strsplit(row.names(y), ' ') 
			ids <- as.numeric(do.call(rbind, ids)[,1])
			row.names(y) <- as.character(rnx[ids])
			data <- x@data[ids, ,drop=FALSE]
			rownames(data) <- rnx[ids]
			
			SpatialLinesDataFrame(y, data)
		} else {
			y <- gIntersection(x, y)
			if (inherits(y, "SpatialCollections")) {
				y <- y@lineyobj
			}
			return(y)
			
		}
		
	} else if (inherits(x, 'SpatialGrid')) {
	
		y <- extent(y)
		r <- raster(x)
		
		if (.hasSlot(x, 'data')) {
			i <- cellsFromExtent(r, y)
			r <- crop(r, y)
			r <- as(r, 'SpatialGridDataFrame')
			r@data <- x@data[i,,drop=FALSE]
			return(r)
			
		} else {
			return( as( crop(raster(x), y), 'SpatialGrid' ) )
		}
		
	} else if (inherits(x, 'SpatialPixels')) {

		y <- extent(y)
		i <- x@coords[,1] >= y@xmin &  x@coords[,1] <= y@xmax & x@coords[,2] >= y@ymin &  x@coords[,2] <= y@ymax
		
		r <- raster(x)
		i <- cellsFromExtent(r, y)
		r <- crop(r, y)
		if (.hasSlot(x, 'data')) {
			r <- as(r, 'SpatialPixelsDataFrame')
		} else {
			r <- as(r, 'SpatialPixels')
		} 
		return( r[i,] )

	} else if (inherits( x, 'SpatialPoints')) { 

		y <- extent(y)
		i <- x@coords[,1] >= y@xmin &  x@coords[,1] <= y@xmax & x@coords[,2] >= y@ymin &  x@coords[,2] <= y@ymax
		x[i,]
		
	}
}
)



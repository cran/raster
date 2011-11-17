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
			} else { 
				y <- extent(y)
				validObject(y)
				y <- as(y, 'SpatialPolygons')
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
			ids <- sapply(y@polygons, function(x) strsplit(slot(x, 'ID'), ' '))
			ids <- as.numeric(do.call(rbind, ids)[,1])
			for (i in 1:length(y@polygons)) {
				y@polygons[[i]]@ID <- as.character(ids[i])
			}
			data <- x@data[ids, ,drop=FALSE]
			rownames(data) <- ids
			
			return( SpatialPolygonsDataFrame(y, data) )
		} else {
			return( gIntersection(x, y) )
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
			ids <- sapply(y@lines, function(x) strsplit(slot(x, 'ID'), ' '))
			ids <- as.numeric(do.call(rbind, ids)[,1])
			for (i in 1:length(y@lines)) {
				y@lines[[i]]@ID <- as.character(ids[i])
			}
			data <- x@data[ids, ,drop=FALSE]
			rownames(data) <- ids
			SpatialLinesDataFrame(y, data)
		} else {
			gIntersection(x, y)
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

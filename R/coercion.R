# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3


.hasValues <- function(x) {
	if (class(x) == 'BasicRaster') { return(FALSE) }
	if (dataSource(x) != 'disk' & dataContent(x) != 'all') {
		return(FALSE)
	} else {
		return(TRUE)
	}
}


.asSpGrid <- function(object, type='grid', dataframe)  {
		
	if (type=='pixel') {
		values <- rasterToPoints(object, fun=NULL, spatial=FALSE)
		pts <- SpatialPoints(values[,1:2])
		if (dataframe) {
			sp <- SpatialPixelsDataFrame(points=pts, data=data.frame(values=values[,3]), proj4string=projection(object, FALSE)) 	
		} else {
			sp <- SpatialPixels(points=pts, proj4string=projection(object, FALSE))
		}
	} else {
		bb <- bbox(object)
		cs <- res(object)
		cc <- bb[,1] + (cs/2)
		cd <- cbind(ncol(object), nrow(object))
		grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
		if (dataframe) {
			values <- data.frame(getValues(object))
			sp <- SpatialGridDataFrame(grd, proj4string=projection(object, FALSE), data=values)
		} else { 
			sp  <- SpatialGrid(grd, proj4string=projection(object, FALSE))
		}	
	}
	return(sp)
}

setAs('Raster', 'SpatialPointsDataFrame', 
	function(from) { 
		return(rasterToPoints(from, spatial=TRUE)) 
	}
)

setAs('Raster', 'SpatialPoints', 
	function(from) { 
		sp <- SpatialPoints(rasterToPoints(from, spatial=FALSE)[,1:2])
		return(sp)
	}
)

setAs('Raster', 'SpatialPixels', 
	function(from) { return(.asSpGrid(from, type='pixel', FALSE)) }
)

setAs('Raster', 'SpatialPixelsDataFrame', 
	function(from) { return(.asSpGrid(from, type='pixel', TRUE)) }
)

setAs('Raster', 'SpatialGrid', 
	function(from) { return(.asSpGrid(from, type='grid', FALSE)) }
)

setAs('Raster', 'SpatialGridDataFrame', 
	function(from) { return(.asSpGrid(from, type='grid', TRUE)) }
)

# to RasterLayer

setAs('SpatialGrid', 'RasterLayer', 
	function(from){ return(raster (from)) }
)

setAs('SpatialPixels', 'RasterLayer', 
	function(from){ return(raster (from)) }
)

# to RasterStack
setAs('SpatialGrid', 'RasterStack',
	function(from){ return(stack(from)) }
)

setAs('SpatialPixels', 'RasterStack', 
	function(from){ return(stack(from)) }
)


# to RasterBrick

setAs('SpatialGrid', 'RasterBrick',
	function(from){ return(brick(from)) }
)


setAs('SpatialPixels', 'RasterBrick', 
	function(from){ return(brick(from)) }
)




# Between Raster objects
setAs('RasterStack', 'RasterLayer', 
	function(from){ return( raster(from)) }
)

setAs('RasterBrick', 'RasterLayer', 
	function(from){ return( raster(from)) }
)


setAs('RasterLayer', 'RasterStack', 
	function(from){ return( stack(from)) }
)

setAs('RasterLayer', 'RasterBrick', 
	function(from){ return( brick(from)) }
)

setAs('matrix', 'RasterLayer',
	function(from){ return(raster(from)) }
)

setAs('RasterLayer', 'matrix',
	function(from){ return( getValues(from, format='matrix')) }
)


	
# Between Raster and sp vector objects	
setAs('RasterLayer', 'SpatialPointsDataFrame', 
	function(from){ return( rasterToPoints(from)) }
)

setAs('RasterLayer', 'SpatialPolygonsDataFrame', 
	function(from){ return( rasterToPolygons(from)) }
)

setAs('Extent', 'SpatialPolygonsDataFrame', 
	function(from){ return( polygonFromExtent(from)) }
)


# spatstat
setAs('im', 'RasterLayer', 
	function(from) {
		r = raster(nrows=from$dim[1], ncols=from$dim[2], xmn=from$xrange[1], xmx=from$xrange[2], ymn=from$yrange[1], ymx=from$yrange[2], crs='')
		r = setValues(r, from$v)
		flip(r, direction='y')
	}
)

# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3



.asSpGrid <- function(object, type='grid', dataframe)  {
		
	if (rotated(object)) {
		stop('\n Cannot coerce because object is rotated.\n Either coerce to SpatialPoints* object\n or first use the "rectify" function')
	}	
	crs <- projection(object, FALSE)
	if (type=='pixel') {
		v <- rasterToPoints(object, fun=NULL, spatial=FALSE)
		pts <- SpatialPoints(v[,1:2])
		if (dataframe) {
			sp <- SpatialPixelsDataFrame(points=pts, data=data.frame(values=v[,3]), proj4string=crs) 	
		} else {
			sp <- SpatialPixels(points=pts, proj4string=crs)
		}
	} else {
		bb <- bbox(object)
		cs <- res(object)
		cc <- bb[,1] + (cs/2)
		cd <- cbind(ncol(object), nrow(object))
		grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
		if (dataframe) {
			sp <- SpatialGridDataFrame(grd, proj4string=crs, data=data.frame(values=getValues(object)))
		} else { 
			sp  <- SpatialGrid(grd, proj4string=crs)
		}	
	}
	return(sp)
}


# To sp pixel/grid objects	


setAs('Raster', 'SpatialPixels', 
	function(from) {
		.asSpGrid(from, type='pixel', FALSE)
	}
)

setAs('Raster', 'SpatialPixelsDataFrame', 
	function(from) { 
		.asSpGrid(from, type='pixel', TRUE)
	}
)

setAs('Raster', 'SpatialGrid', 
	function(from) { 
		.asSpGrid(from, type='grid', FALSE)
	}
)

setAs('Raster', 'SpatialGridDataFrame', 
	function(from) { 
		.asSpGrid(from, type='grid', TRUE)
	}
)


# To sp vector objects	

setAs('RasterLayer', 'SpatialPolygonsDataFrame', 
	function(from){ 
		return( rasterToPolygons(from)) 
	}
)

setAs('Extent', 'SpatialPolygons', 
	function(from){ 
		return( polygonFromExtent(from)) 
	}
)



setAs('Raster', 'SpatialPoints', 
	function(from) { 
		return( SpatialPoints(rasterToPoints(from, spatial=FALSE)[,1:2]) )
	}
)

setAs('Raster', 'SpatialPointsDataFrame', 
	function(from) { 
		return( rasterToPoints(from, spatial=TRUE) ) 
	}
)


# to RasterLayer

setAs('SpatialGrid', 'RasterLayer', 
	function(from){ return(raster (from)) }
)

setAs('SpatialPixels', 'RasterLayer', 
	function(from){ return(raster (from)) }
)


setAs('SpatialGrid', 'BasicRaster', 
	function(from){ 
		to <- new('BasicRaster')
		to@extent <- extent(from)
		projection(to) <- from@proj4string
		dim(to) <- c(from@grid@cells.dim[2], from@grid@cells.dim[1])	
		return(to)
	}
)


setAs('SpatialPixels', 'BasicRaster', 
	function(from){ 
		to <- new('BasicRaster')
		to@extent <- extent(from)
		projection(to) <- from@proj4string
		dim(to) <- c(from@grid@cells.dim[2], from@grid@cells.dim[1])	
		return(to)
	}
)



# to RasterStack
setAs('SpatialGrid', 'RasterStack',
	function(from){ 
		stack(from)
	}
)

setAs('SpatialPixels', 'RasterStack', 
	function(from){
		stack(from)
	}
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


	

# spatstat
setAs('im', 'RasterLayer', 
	function(from) {
		r = raster(nrows=from$dim[1], ncols=from$dim[2], xmn=from$xrange[1], xmx=from$xrange[2], ymn=from$yrange[1], ymx=from$yrange[2], crs='')
		r = setValues(r, from$v)
		flip(r, direction='y')
	}
)

# adehabitat
setAs('asc', 'RasterLayer', 
	function(from) {
		d <- t(from[])
		d <- d[nrow(d):1, ]
		type <- attr(from, "type") 
		if (type == 'factor') {
			warning('factor type converted to numeric')
		}
		cz <- attr(from, "cellsize")
		xmn <- attr(from, 'xll') - 0.5 * cz
		ymn <- attr(from, 'yll') - 0.5 * cz
		xmx <- xmn + ncol(d) * cz
		ymx <- ymn + nrow(d) * cz
		e <- extent(xmn, xmx, ymn, ymx)
		d <- raster(d)
		extent(d) = e
		return(d)
	}
)


setAs('RasterLayer', 'asc', 
	function(from) {
		asc <- getValues(from, format='matrix')
		asc <- asc[nrow(asc):1, ]
		attr(asc, "cellsize") <- xres(from)
		attr(asc, "xll") <- xmin(from) + 0.5 * xres(from)
		attr(asc, "yll") <- ymin(from) + 0.5 * yres(from)
		attr(asc, "type") <- 'numeric'
		class(asc) <- "asc"		
		return(asc)	
	}
)



setAs('kasc', 'RasterBrick', 
	function(from) {
		names <- colnames(from)
		cz <- attr(from, "cellsize")
		ncol <- attr(from, 'ncol')
		nrow <- attr(from, 'nrow')
		xmn <- attr(from, 'xll') - 0.5 * cz
		ymn <- attr(from, 'yll') - 0.5 * cz
		xmx <- xmn + ncol * cz
		ymx <- ymn + nrow * cz
		e <- extent(xmn, xmx, ymn, ymx)
		b <- brick(e, nrow=nrow, ncol=ncol)
		m = matrix(NA, ncol=ncol(from), nrow=nrow(from))
		for (i in 1:ncol(m)) {
			m[,i] <- as.numeric(from[,i])
		}	
		dim(m) <- dim(from)
		b <- setValues(b, m)
		layerNames(b) <- names
		return(b)
	}
)


setAs('kasc', 'RasterStack', 
	function(from) {
		names <- colnames(from)
		cz <- attr(from, "cellsize")
		ncol <- attr(from, 'ncol')
		nrow <- attr(from, 'nrow')
		xmn <- attr(from, 'xll') - 0.5 * cz
		ymn <- attr(from, 'yll') - 0.5 * cz
		xmx <- xmn + ncol * cz
		ymx <- ymn + nrow * cz
		e <- extent(xmn, xmx, ymn, ymx)
		r <- raster(e, nrow=nrow, ncol=ncol)
		r <- setValues(r, as.numeric(from[,1]))
		layerNames(r) <- names[1]
		s <- stack(r)
		if (ncol(from) > 1) {
			for (i in 2:ncol(from)) {
				r <- setValues(r, as.numeric(from[,i]))
				layerNames(r) <- names[i]
				s <- addLayer(s, r)
			}	
		}
		return(s)
	}
)


# kernel density estimate (kde) from package ks

setAs('kde', 'RasterLayer', 
	function(from) {
		x <- t(from$estimate)
		x <- x[nrow(x):1,]
		raster(x, xmn=min(from$eval.points[[1]]), xmx=max(from$eval.points[[1]]), 
					ymn=min(from$eval.points[[2]]), ymx=max(from$eval.points[[2]]) ) 
	}
)


setAs('grf', 'RasterBrick', 
	function(from) {
		x <- from$data
		if (!is.matrix(x)) {
			x <- matrix(x)
		}
		ncell <- nrow(x)
		nl <- ncol(x)
		nc <- nr <- as.integer(sqrt(ncell))
		dim(x) <- c(nr, nc, nl)
		
		x = aperm(x, perm=c(2,1,3))
		b <- brick(x)
		b <- flip(b, 'y')
		extent(b) <- extent(as.vector(apply(from$coords, 2, range)))
		b
	}
)


setAs('grf', 'RasterLayer', 
	function(from) {
		x <- from$data
		if (is.matrix(x)) {
			x <- x[,1]
		}
		ncell <- length(x)
		nc <- nr <- as.integer(sqrt(ncell))
		dim(x) <- c(nr, nc)
		x <- t(x)[nrow(x):1,]
		r <- raster(x)
		extent(r) <- extent(as.vector(apply(from$coords, 2, range)))
		r
	}
)


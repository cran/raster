# R classes for raster (grid) type spatial data
# Robert J. Hijmans, r.hijmans@gmail.com
# November 2008
# Version 1.0
# Licence GPL v3


setClass('Extent',
	representation (
		xmin = 'numeric',
		xmax = 'numeric',
		ymin = 'numeric',
		ymax = 'numeric'
	),	
	prototype (	
		xmin = 0,
		xmax = 1,
		ymin = 0,
		ymax = 1
	),
	validity = function(object)	{
		c1 <- (object@xmin <= object@xmax)
		if (!c1) { stop('invalid extent: xmin >= xmax') }
		c2 <- (object@ymin <= object@ymax)
		if (!c2) { stop('invalid extent: ymin >= ymax') }
		v <- c(object@xmin, object@xmax, object@ymin, object@ymax)
		c3 <- all(!is.infinite(v))
		if (!c3) { stop('invalid extent: infinite value') }		
		return(c1 & c2 & c3)
	}
)


setClass('.Rotation',
	representation (
		geotrans = 'numeric',
		transfun = 'function'
		#upperleft = 'numeric',
		#lowerleft = 'numeric',
		#upperright = 'numeric',
		#lowerright = 'numeric'
	),	
	prototype (	
		#upperleft = c(0, 1),
		#lowerleft = c(0, 0),
		#upperright = c(1, 1),
		#lowerright = c(1, 0)
	)
)


setClass ('BasicRaster',
	representation (
		title = 'character',
		extent = 'Extent',
		rotated = 'logical',
		rotation = '.Rotation',
		ncols ='integer',
		nrows ='integer',
		crs = 'CRS',
		layernames = 'vector',
		z = 'list',
		zname='character',
		zvalue='vector',
		unit = 'vector'
	),
	prototype (	
		rotated = FALSE,
		ncols= as.integer(1),
		nrows= as.integer(1),
		layernames=c(""),
		unit=c(""),
		z = list(),
		zname='',
		zvalue='',
		crs = CRS(as.character(NA))
	),
	validity = function(object) {
		validObject(extent(object))
		c1 <- (object@ncols > 0)
		if (!c1) { stop('ncols < 1') }
		c2 <- (object@nrows > 0)
		if (!c2) { stop('nrows < 1') }		
		return(c1 & c2)
	}
)

setClass ('Raster', contains = c('BasicRaster', 'VIRTUAL') )

	
setClass('.RasterFile', 
	representation (
		name ='character',
		datanotation='character',
		byteorder ='character',
		nodatavalue ='numeric', # on disk, in ram it is NA
		nbands ='integer',
		bandorder ='character',
		offset='integer',
		toptobottom='logical',
		driver ='character'
		),
	prototype (	
	    name = '',
		datanotation='FLT4S',
		byteorder = .Platform$endian,
		nodatavalue = -Inf,
		nbands = as.integer(1),
		bandorder = 'BIL',
		offset = as.integer(0),
		toptobottom = TRUE,
		driver = '' # raster or gdal
	),
	validity = function(object) {
		c1 <- datanotation %in% c('LOG1S', 'INT1S', 'INT2S', 'INT4S', 'INT1U', 'INT2U', 'FLT4S', 'FLT8S')
		return(c1)
	}
)


setClass('.SingleLayerData', 
	representation (
		values='vector', 
		offset='numeric',
		gain='numeric',
		
		inmemory='logical',
		fromdisk='logical',
		
		isfactor = 'logical',
		attributes = 'list',
		haveminmax = 'logical',
		min = 'vector',
		max = 'vector',
		band = 'integer'
		),
	prototype (	
		values=vector(),
		offset=0,
		gain=1,
		
		inmemory=FALSE,
		fromdisk=FALSE,

		isfactor = FALSE,
		attributes = list(),
		
		haveminmax = FALSE,
		min = c(Inf),
		max = c(-Inf),
		band = as.integer(1)		
	),	
	validity = function(object) {
	}
)



setClass('.SingleLayerDataSparse', 
	contains = '.SingleLayerData',
	representation (
		indices = 'vector'
	),
	prototype (	
		indices = vector(mode='numeric')
	)
)



setClass ('.RasterLegend',
	representation (
		type = 'character',
		values = 'vector',
		color = 'vector',
		names = 'vector',
		colortable = 'vector'
		),
	prototype (
		)
	)
	

	
setClass ('RasterLayer',
	contains = 'Raster',
	representation (
		file = '.RasterFile',
		data = '.SingleLayerData',
		legend = '.RasterLegend',
		history = 'vector'
		),
	prototype (
		history = vector(mode='character')
		)
	)


setClass ('.RasterLayerSparse',
	contains = 'Raster',
	representation (
		file = '.RasterFile',
		data = '.SingleLayerData',
		legend = '.RasterLegend',
		history = 'vector'
		),
	prototype (
		history = vector(mode='character')
		)
	)
	

setClass('.MultipleRasterData', 
	representation (
		values='matrix', 

		offset='numeric',
		gain='numeric',

		inmemory='logical',
		fromdisk='logical',

		nlayers='integer',
		
		dropped = 'vector',
		isfactor = 'logical',
		attributes = 'list',
		
		haveminmax = 'logical',
		min = 'vector',
		max = 'vector'
		
		),
	prototype (	
		values=matrix(NA,0,0),
		offset=0,
		gain=1,
		#indices =vector(mode='numeric'),

		inmemory=FALSE,
		fromdisk=FALSE,

		nlayers=as.integer(0),
		
		dropped=NULL,
		
		isfactor = FALSE,
		attributes = list(),
		
		haveminmax = FALSE,
		min = c(Inf),
		max = c(-Inf)
	),	
	validity = function(object) {
	}
)


setClass ('RasterBrick',
	contains = 'Raster',
	representation (
		file = '.RasterFile',
		data = '.MultipleRasterData',
		legend = '.RasterLegend',
		history = 'vector'
		),
	prototype (
		history = vector(mode='character')
		),
	validity = function(object)
	{
	}
)

	
	
setClass ('RasterStack',
	contains = 'Raster',
	representation (
	    filename ='character',
		layers ='list'
		),
	prototype (
		filename='',
		layers = list()
		),
	validity = function(object) {
		if (length(object@layers) > 1) {
			cond <- compare(object@layers[[1]], object@layers, extent=TRUE, rowcol=TRUE, tolerance=0.05, stopiffalse=FALSE, showwarning=FALSE) 
		} else {
			cond <- TRUE
		}
		return(cond)
	}
)


setClassUnion("RasterStackBrick", c("RasterStack", "RasterBrick"))



setClass ('.RasterList',
	representation (
	    filename ='character',
		layers ='list'
		),
	prototype (
		filename='',
		layers = list()
		),
	validity = function(object) {
		return( length(object@layers) == object@data@nlayers )
	}
)


#setClassUnion("RasterStackBrickList", c("RasterStack", "RasterBrick", "RasterList"))



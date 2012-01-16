		
setClass ('.VectorLayer', 
	contains = 'VIRTUAL', 
	representation (
		file = 'character',
		driver = 'character',
		crs = 'CRS',
		nrecords = 'integer',
		index = 'matrix',
		coords = 'matrix',
		fields = 'matrix',
		data = 'data.frame'
	),
	prototype (	
		file = '',
		nrecords = as.integer(0),
		crs = CRS(as.character(NA))
	),
	validity = function(object) {
		return(TRUE)
	}
)


setClass ('.VectorLayerPolygons', 
	contains = '.VectorLayer', 
	representation (
	), 
	prototype (	
	),
	validity = function(object) {
		return(TRUE)
	}
)

setClass ('.VectorLayerLines', 
	contains = '.VectorLayer', 
	representation (
	), 
	prototype (	
	),
	validity = function(object) {
		return(TRUE)
	}
)

setClass ('.VectorLayerPoints', 
	contains = '.VectorLayer', 
	representation (
	), 
	prototype (	
	),
	validity = function(object) {
		return(TRUE)
	}
)


setMethod ('show' , '.VectorLayer', 
	function(object) {
		cat('class       :' , class(object), '\n')
		cat('filename    :' , object@file, '\n')
		cat('nrecords    :' , object@nrecords, '\n')
		cat('nfields     :' , nrow(object@fields), '\n')
		cat('coord. ref. :' , projection(object, TRUE), '\n')
	}
)	

if (!isGeneric("extent")) {
	setGeneric("extent", function(x, ...)
		standardGeneric("extent"))
}	


#setMethod('extent', signature(x='.VectorLayer'), 
#	function(x, ...) {
#		extent(as.vector(range(x@data@xy)))
#	}
#)


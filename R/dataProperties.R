# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  October 2008
# Version 0.9
# Licence GPL v3

#dataSize <- function(object) {return(object@file@datasize)}
dataSize <- function(object) {
	if (class(object) != 'character'){
		object <- dataType(object)
	}
	return( as.integer (substr(object, 4, 4)) )
}

dataSigned <- function(object) {
	if (class(object) != 'character'){object <- dataType(object)}
	ifelse(substr(object, 5, 5) == 'U', FALSE, TRUE )
}

.shortDataType <- function(object) {
	if (class(object) != 'character') {
		object <- dataType(object)
	}
	return( substr(object, 1, 3)) 
}


dataType <- function(x) {
	return(x@file@datanotation)
}

dataContent <- function(object) {
	return(object@data@content)
}

dataIndices <- function(object) {
	return(object@data@indices)
}

dataSource <- function(object) {
	return(object@data@source)
}

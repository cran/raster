# Author: Robert J. Hijmans
# Date: June 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("summary")) {
	setGeneric("summary", function(object, ...)
		standardGeneric("summary"))
}	



setMethod('summary', signature(object='Raster'), 
	function(object, maxsamp=100000, ...) {
		
		warn <- NULL
		if ( inMemory(object) ) {
			values <- as.matrix( summary(object@data@values) )
		} else if (  fromDisk(object) ) {
			if (ncell(object) > maxsamp) {
				v <- sampleRandom(object, maxsamp)
				cat(paste('summary based on a sample of', maxsamp, 'cells, which is', round(100*maxsamp/ncell(object), 2), '% of all cells\n'))
			} else {
				v <- getValues(object)
			}
			values <- as.matrix( summary(v) )
		} else {
			values <- as.matrix(rep(NA, 7))
		}
		colnames(values) <- ""
		return(values)
	}	
)


setMethod('summary', signature(object='RasterStack'), 
	function(object, maxsamp=100000, ...) {
		if (nlayers(object) == 0) {	stop('no layers in this RasterStack') }
		
		ncell <- ncell(object)
		values <- NULL
		warn <- NULL
		for (n in 1:nlayers(object)) {
			if ( inMemory(object@layers[[n]]) ) {
				sm <- as.matrix( summary(object@layers[[n]]@data@values) )
			} else {
				if (ncell > maxsamp) {
					v <- sampleRandom(object@layers[[n]], maxsamp)
				    cat(paste('summary based on a sample of', maxsamp, 'cells, which is', round(100*maxsamp/ncell(object), 2), '% of all cells\n'))
				} else {
					v <- getValues(object@layers[[n]])
				}
				sm <- as.matrix( summary(v) )					
			}
			values <- cbind(values, as.matrix(as.vector(sm)))
		}
		rownames(values) <- rownames(sm)
		colnames(values) <- names(object)
		return(values) 
	}
)	




setMethod('summary', signature(object='RasterBrick'), 
	function(object, maxsamp=100000, ...) {
	
		ncell <- ncell(object)
		values <- NULL
		warn <- NULL
		
		if ( inMemory(object) ) {
			for (n in 1:nlayers(object)) {
				sm <- as.matrix( summary( object@data@values[,n] ) )
				values <- cbind(values, as.matrix(sm))
			} 

		} else if (  fromDisk(object) ) {
		
			if (ncell > maxsamp) {
				v <- sampleRandom(object, maxsamp)
				warn <- paste('summary based on a sample of', maxsamp, 'cells, which is', round(100*maxsamp/ncell(object), 2), '% of all cells')
			} else {
				v <- getValues(object)
			}

			for (n in 1:nlayers(object)) {
				sm <- as.matrix( summary( v[,n] ) )
				values <- cbind(values, as.matrix(sm))
			} 
			
		} else {
			stop('no cell values associated with this RasterBrick')
		}
		rownames(values) <- rownames(sm)
		colnames(values) <- names(object)
		print(values)
		if (!is.null(warn)) cat(warn, '\n')
		invisible(values) 
	}
)	


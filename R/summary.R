# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: June 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("summary")) {
	setGeneric("summary", function(object, ...)
		standardGeneric("summary"))
}	



setMethod('summary', signature(object='Raster'), 
	function(object, maxsamp=100000, ...) {
		sumobj <- new(".RasterSummary")
		sumobj@ncell <- ncell(object)
		sumobj@inmemory <- inMemory(object) 
		if ( sumobj@inmemory ) {
			sumobj@NAs <- sum(is.na(object@data@values))
			sumobj@values <- as.matrix( summary(object@data@values) )
		} else if (  fromDisk(object) ) {
			if (ncell(object) > maxsamp) {
				v = sampleRandom(object, maxsamp)
				sumobj@warning = paste('summary based on a sample of', maxsamp, 'cells, which is', 100*maxsamp/ncell(object), '% of all cells')
			} else {
				v = getValues(object)
			}
			sumobj@NAs <- sum(is.na(v))
			sumobj@values <- as.matrix( summary(v) )
		} else {
			sumobj@NAs <- sumobj@ncell
			sumobj@values <- as.matrix(rep(NA, 6))
		}
		colnames(sumobj@values)=""
		return(sumobj)
	}	
)


setMethod('summary', signature(object='RasterStack'), 
	function(object, maxsamp=100000, ...) {
		if (nlayers(object) == 0) {	stop('no layers in this RasterStack') }
		
		sumobj <- new(".RasterSummary")
		sumobj@ncell <- ncell(object)
		for (n in 1:nlayers(object)) {
			sumobj@inmemory <- c(sumobj@inmemory, inMemory(object@layers[[n]]) )
			if ( inMemory(object@layers[[n]]) ) {
				nas <- sum(is.na(object@layers[[n]]@data@values))
				sm = as.matrix( summary(object@layers[[n]]@data@values) )
			} else {
				if (sumobj@ncell > maxsamp) {
					v = sampleRandom(object@layers[[n]], maxsamp)
					sumobj@warning = paste('summary based on a sample of', maxsamp, 'cells, which is', 100*maxsamp/ncell(object), '% of all cells')
				} else {
					v = getValues(object@layers[[n]])
				}
				sm <- as.matrix( summary(v) )					
				nas = sum(is.na(v)) 
			}
			sumobj@NAs <- c(sumobj@NAs, nas)
			sumobj@values <- cbind(sumobj@values, as.matrix(sm))
		}
		rownames(sumobj@values) <- rownames(sm)
		colnames(sumobj@values) <- 1:nlayers(object)
		return(sumobj)
	}
)	




setMethod('summary', signature(object='RasterBrick'), 
	function(object, maxsamp=100000, ...) {
		sumobj <- new(".RasterSummary")
		sumobj@ncell <- ncell(object)
		sumobj@inmemory <- inMemory(object)

		if ( inMemory(object) ) {
			for (n in 1:nlayers(object)) {
				sumobj@NAs <- c(sumobj@NAs, sum(is.na(object@data@values[, n])))
				sm = as.matrix( summary( object@data@values[,n] ) )
				sumobj@values <- cbind(sumobj@values, as.matrix(sm))
				rownames(sumobj@values) <- rownames(sm)
			} 
			colnames(sumobj@values) <- 1:nlayers(object)
			
		} else if (  fromDisk(object) ) {
		
			if (sumobj@ncell > maxsamp) {
				v = sampleRandom(object, maxsamp)
				sumobj@warning = paste('summary based on a sample of', maxsamp, 'cells, which is', 100*maxsamp/ncell(object), '% of all cells')
			} else {
				v = getValues(object)
			}

			for (n in 1:nlayers(object)) {
				sumobj@NAs <- c(sumobj@NAs, sum(is.na(v[, n])))
				sm = as.matrix( summary( v[,n] ) )
				sumobj@values <- cbind(sumobj@values, as.matrix(sm))
				rownames(sumobj@values) <- rownames(sm)
			} 
			colnames(sumobj@values) <- 1:nlayers(object)
			
		} else {
			stop('no cell values associated with this RasterBrick')
		}
		
		return(sumobj)
	}
)	


setClass('.RasterSummary',
	representation (
		ncell = 'numeric',
		inmemory = 'logical',
		NAs = 'vector',
		values = 'matrix',
		warning = 'character'
	),
	prototype (	
		inmemory = vector(mode='logical'),
		NAs = vector(mode='numeric'),
		values = matrix(nrow=6, ncol=0),
		warning = ''
	),
)
	

setMethod('show', signature(object='.RasterSummary'), 	
	function(object) {
		cat ("Cells: " , object@ncell, "\n")
		if (object@inmemory) {
			cat("NAs  : ", object@NAs, "\n")
		}	
		cat("\n")
		print(object@values) 
		if (object@warning != '') {
			cat(object@warning, '\n')
		}
	}	
)
	

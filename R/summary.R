# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: June 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("summary")) {
	setGeneric("summary", function(object, ...)
		standardGeneric("summary"))
}	



setMethod('summary', signature(object='Raster'), 
	function(object, maxsamp=5000, ...) {
		sumobj <- new("RasterSummary")
		sumobj@ncell <- ncell(object)
		sumobj@dataContent <- dataContent(object) 
		if ( sumobj@dataContent == "all") {
			sumobj@NAs <- sum(is.na(object@data@values))
			sumobj@values <- as.matrix( summary(object@data@values) )
		} else if (dataSource(object) == 'disk') {
			if (ncell(object) > maxsamp) {
				v = sampleRandom(object, maxsamp)
				sumobj@warning = paste('summary based on a sample of:', maxsamp, ' cells, which is ', 100*maxsamp/ncell(object), '% of all cells')
			} else {
				v = getValues(object)
			}
			sumobj@NAs <- sum(is.na(v))
			sumobj@values <- as.matrix( summary(v) )
		} else {
			stop('no cell values associated with this RasterLayer')
		}
		colnames(sumobj@values)=""
		return(sumobj)
	}	
)


setMethod('summary', signature(object='RasterStack'), 
	function(object, maxsamp=5000, ...) {
		if (nlayers(object) == 0) {	stop('no layers in this RasterStack') }
		
		sumobj <- new("RasterSummary")
		sumobj@ncell <- ncell(object)
		for (n in 1:nlayers(object)) {
			sumobj@dataContent <- c(sumobj@dataContent, dataContent(object@layers[[n]]) )
			if (dataContent(object@layers[[n]]) == 'all') {
				nas <- sum(is.na(object@layers[[n]]@data@values))
				sm = as.matrix( summary(object@layers[[n]]@data@values) )
			} else {
				if (sumobj@ncell > maxsamp) {
					v = sampleRandom(object@layers[[n]], maxsamp)
					sumobj@warning = paste('summary based on a sample of:', maxsamp, ' cells, which is ', 100*maxsamp/ncell(object), '% of all cells')
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
	function(object, maxsamp=5000, ...) {
		sumobj <- new("RasterSummary")
		sumobj@ncell <- ncell(object)
		sumobj@dataContent <- dataContent(object)

		if (dataContent(object) == 'all') {
			for (n in 1:nlayers(object)) {
				sumobj@NAs <- c(sumobj@NAs, sum(is.na(object@data@values[, n])))
				sm = as.matrix( summary( object@data@values[,n] ) )
				sumobj@values <- cbind(sumobj@values, as.matrix(sm))
				rownames(sumobj@values) <- rownames(sm)
			} 
			colnames(sumobj@values) <- 1:nlayers(object)
			
		} else if (dataSource(object) == 'disk') {
		
			if (sumobj@ncell > maxsamp) {
				v = sampleRandom(object, maxsamp)
				sumobj@warning = paste('summary based on a sample of:', maxsamp, ' cells, which is ', 100*maxsamp/ncell(object), '% of all cells')
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


setClass('RasterSummary',
	representation (
		ncell = 'numeric',
		dataContent = 'vector',
		NAs = 'vector',
		values = 'matrix',
		warning = 'character'
	),
	prototype (	
		dataContent = vector(mode='character'),
		NAs = vector(mode='numeric'),
		values = matrix(nrow=6, ncol=0),
		warning = ''
	),
)
	

setMethod('show', signature(object='RasterSummary'), 	
	function(object) {
		cat ("Cells: " , object@ncell, "\n")
		cat("NAs  : ", object@NAs, "\n")
		cat("\n")
		print(object@values) 
		if (object@warning != '') {
			cat(object@warning, '\n')
		}
	}	
)
	


# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3



if (!isGeneric('setMinMax')) {
	setGeneric('setMinMax', function(x)
		standardGeneric('setMinMax')) 
	}	

	
setMethod('setMinMax', signature(x='RasterLayer'), 
function(x) {
	clear <- FALSE
	if (dataContent(x) != 'all') {
		if (dataSource(x) == 'ram') {
			stop('no, or not enough, values associated with this Layer')
		}
	}
	
	if (dataContent(x)=='all' ) {
		vals <- na.omit(values(x)) 
		if (length(vals) > 0) {
			x@data@min <- min(vals)
			x@data@max <- max(vals)
		} else {
			x@data@min <- NA
			x@data@max <- NA
		}
	} else {
		x@data@min <- Inf
		x@data@max <- -Inf
		tr <- blockSize(x)
		pb <- pbCreate(tr$n, type=.progress())			
		for (i in 1:tr$n) {
			v <- na.omit ( getValuesBlock(x, row=tr$row[i], nrows=tr$nrows[i]) )
			if (length(v) > 0) {
				x@data@min <- min(x@data@min, min(v))
				x@data@max <- max(x@data@max, max(v))
			}	
		}
	}
	
#	if (datatype == 'logical') {
#		x@data@min <- as.logical(x@data@min)
#		x@data@max <- as.logical(x@data@max)
#	}

	x@data@haveminmax <- TRUE
	return(x)
}
)


setMethod('setMinMax', signature(x='RasterBrick'), 
function(x) {
	clear <- FALSE
	if (dataContent(x) != 'all') {
		if (dataSource(x) == 'ram') {
			stop('no values associated with this Layer')
		}
		if (canProcessInMemory(x, (2 + nlayers(x)))) {
			x <- readAll(x)
			clear <- TRUE
		}
	}
	
	if (dataContent(x)=='all') {
		rge <- apply(x@data@values, 2, FUN=function(x){range(x, na.rm=TRUE)})
		x@data@min <- rge[1,]
		x@data@max <- rge[2,]
		
#		for (i in 1:nlayers(x)) {
#			vals <- na.omit(values(x)[,i]) # min and max values
#			if (length(vals) > 0) {
#				x@data@min[i] <- min(vals)
#				x@data@max[i] <- max(vals)
#			} else {
#				x@data@min[i] <- NA
#				x@data@max[i] <- NA
#			}
#		}
		if (clear) {x <- clearValues(x)}
	} else {
		x@data@min <- rep(Inf, nlayers(x))
		x@data@max <- rep(-Inf, nlayers(x))
		for (r in 1:nrow(x)) {
			x <- readRow(x, r)
			for (i in 1:nlayers(x)) {
				rsd <- na.omit(values(x)[,i]) # min and max values
				if (length(rsd) > 0) {
					x@data@min[i] <- min(x@data@min[i], min(rsd))
					x@data@max[i] <- max(x@data@max[i], max(rsd))
				}	
			}
		}
		x <- clearValues(x)
	}
#	if (datatype == 'logical') {
#		x@data@min <- as.logical(x@data@min)
#		x@data@max <- as.logical(x@data@max)
#	}

	x@data@haveminmax <- TRUE
	return(x)
}
)


setMethod('setMinMax', signature(x='RasterStack'), 
	function(x) {
		for (i in 1:nlayers(x)) {
			x@layers[[i]] <- setMinMax(x@layers[[i]])
		}
		return(x)
	}
)

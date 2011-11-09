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
	
	if ( inMemory(x) ) {
		vals <- na.omit(x@data@values) 
		if (length(vals) > 0) {
			x@data@min <- min(vals)
			x@data@max <- max(vals)
		} else {
			x@data@min <- NA
			x@data@max <- NA
		}
	} else {
		if (! fromDisk(x)) {
			stop('no values associated with this RasterLayer')
		}
		x@data@min <- Inf
		x@data@max <- -Inf
		tr <- blockSize(x)
		pb <- pbCreate(tr$n)			
		for (i in 1:tr$n) {
			v <- na.omit ( getValues(x, row=tr$row[i], nrows=tr$nrows[i]) )
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
	inMem <- inMemory(x)

	if ( ! inMem ) {
		if (! fromDisk(x) ) {
			stop('no values associated with this RasterBrick')
		}
	} else if (canProcessInMemory(x, (2 + nlayers(x)))) {
		inMem <- TRUE
	}

	w <- getOption('warn')
	on.exit(options('warn' = w))
	options('warn'=-1) 
	
	if ( inMem ) {
	
		rge <- apply( getValues(x), 2, FUN=function(x){ range(x, na.rm=TRUE) } )
		x@data@min <- as.vector(rge[1,])
		x@data@max <- as.vector(rge[2,])
		
	} else {
	
		minv <- rep(Inf, nlayers(x))
		maxv <- rep(-Inf, nlayers(x))
		minmax <- rbind(minv, maxv)
		
		tr <- blockSize(x)
		for (i in 1:tr$n) {		
			rsd <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
			minmax[1,] <- apply(rbind(rsd, minmax[1,]), 2, min, na.rm=TRUE)
			minmax[2,] <- apply(rbind(rsd, minmax[2,]), 2, max, na.rm=TRUE)
		}
		x@data@min <- minmax[1,]
		x@data@max <- minmax[2,]
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

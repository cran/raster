# Author: Robert J. Hijmans
# Date : September 2012
# Version 1.0
# Licence GPL v3

.startBigMatrixWriting <- function(x, filename, update=FALSE, ...) {

	require(bigmemory)

 	filename <- trim(filename)
	if (filename == "") {
		stop('missing filename')
	}
	filetype <- 'big.matrix'
	filename <- .setFileExtensionHeader(filename, filetype)
	fnamevals <- .setFileExtensionValues(filename, filetype)
	datatype <- .datatype(...)
	dataType(x) <- datatype
	if (.shortDataType(datatype) == 'INT') {
		dtype <- 'integer'
	} else {
		dtype <- 'double'
	}
	
	overwrite <- .overwrite( ...)
	if (!overwrite & (file.exists(filename) | file.exists(fnamevals))) {
		stop(paste(filename,"or", fnamevals, "exists.","use 'overwrite=TRUE' if you want to overwrite it")) 
	}
	
	if (! update) {
		dscfile <- extension(basename(fnamevals), 'big.dsc')
		
		if (inherits(x, 'RasterLayer')) {
			attr(x@file, "big.matrix") <- filebacked.big.matrix(nrow(x), ncol(x), type=dtype, 
			backingfile=basename(fnamevals), backingpath=dirname(fnamevals), descriptorfile=dscfile)
		} else {
			attr(x@file, "big.matrix") <- filebacked.big.matrix(ncell(x), nlayers(x),
			backingfile=basename(fnamevals), backingpath=dirname(fnamevals), descriptorfile=dscfile )
		}
	}
	
	x@data@min <- rep(Inf, nlayers(x))
	x@data@max <- rep(-Inf, nlayers(x))
	x@data@haveminmax <- FALSE
	x@file@driver <- filetype
	x@file@name <- filename
	return(x)
}



.stopBigMatrixWriting <- function(x) {

	x@data@haveminmax <- TRUE
#	if (x@file@dtype == "INT") {
#		x@data@min <- round(x@data@min)
#		x@data@max <- round(x@data@max)
#	} 
	.writeHdrRaster(x, type='big.matrix')
	filename <- .setFileExtensionValues(filename(x), 'big.matrix')
	
	if (inherits(x, 'RasterBrick')) {
		r <- brick(filename, native=TRUE)
	} else {
		r <- raster(filename, native=TRUE)
	}
	if (! r@data@haveminmax) {
		r@data@min <- x@data@min
		r@data@max <- x@data@max
		r@data@haveminmax <- TRUE
	}
	return(r)
}		
 
 
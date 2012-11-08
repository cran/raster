# Author: Robert J. Hijmans
# Date :  September 2012
# Version 1.0
# Licence GPL v3

.writeBigMatrix <- function(x, filename, ... ) {

	require(bigmemory)
	
	filetype <- 'big.matrix'
	x@file@driver <- filetype
 	filename <- trim(filename)
	fnamevals <- .setFileExtensionValues(filename, filetype)
	fnamehdr <- .setFileExtensionHeader(filename, filetype)
	filename <- fnamevals
	x@file@name <- filename
	
	overwrite <- .overwrite(...)
	if ( ! overwrite & (file.exists(fnamehdr) | file.exists(fnamevals))) {
		stop(paste(filename, "exists. Use 'overwrite=TRUE' if you want to overwrite it"))
	}
	
#	x <- setMinMax(x)
	datatype <- .datatype(...)
	dataType(x) <- datatype
	if (.shortDataType(datatype) == 'INT') {
		dtype <- 'integer'
	} else {
		dtype <- 'double'
	}

	dscfile <- extension(basename(fnamevals), 'big.dsc')
	
	if (inherits(x, 'RasterLayer')) {
		b <- filebacked.big.matrix(nrow(x), ncol(x), type=dtype, backingfile=basename(fnamevals),
			backingpath=dirname(fnamevals), descriptorfile=dscfile)
		b[] <- as.matrix(x)
		
	} else {
		b <- filebacked.big.matrix(ncell(x), nlayers(x), type=dtype, backingfile=basename(fnamevals),
			backingpath=dirname(fnamevals), descriptorfile=dscfile)
		b[] <- getValues(x)
	}
	#flush(b)
	
#	if (canProcessInMemory(r)) {
#		r <- setValues(r, as.vector(t(x[])))
#		if (filename != '') {
#			r <- writeRaster(r, filename, ...)
#		}
#	} else {
#		tr <- blockSize(r)
#		pb <- pbCreate(tr$n, ...)
#		r <- writeStart(r, filename, ...)
#		for (i in 1:tr$n) {
#			r <- writeValues(r, as.vector( t ( x[tr$row[i]:(tr$row[i]+tr$nrows[i]-1), ] ) ), tr$row[i] )
#			pbStep(pb) 
#		}
#	}
#	mn <- minValue(x)
#	mx <- maxValue(x)
#	dsize <- dataSize(x@file@datanotation)
	
	.writeHdrRaster(x, type='big.matrix')
	return(raster(filename))
}
 
 
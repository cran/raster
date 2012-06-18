# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

.writeHdrRaster <- function(x) {
	rastergrd <- .setFileExtensionHeader(filename(x), 'raster')
	thefile <- file(rastergrd, "w")  # open an txt file connectionis
	cat("[general]", "\n", file = thefile)
	cat("creator=R package 'raster'", "\n", file = thefile)
	cat("created=", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", file = thefile)

	cat("[georeference]", "\n", file = thefile)
	cat("nrows=",  nrow(x), "\n", file = thefile)
	cat("ncols=",  ncol(x), "\n", file = thefile)
	cat("xmin=", as.character(xmin(x)), "\n", file = thefile)
	cat("ymin=", as.character(ymin(x)), "\n", file = thefile)
	cat("xmax=", as.character(xmax(x)), "\n", file = thefile)
	cat("ymax=", as.character(ymax(x)), "\n", file = thefile)
	cat("projection=", projection(x), "\n", file = thefile)

	cat("[data]", "\n", file = thefile)
	cat("datatype=",  x@file@datanotation, "\n", file = thefile)
	cat("byteorder=",  .Platform$endian, "\n", file = thefile)
	cat("nbands=",  nlayers(x), "\n", file = thefile)
	cat("bandorder=",  x@file@bandorder, "\n", file = thefile)

	fact <- x@data@isfactor
	cat("categorical=",  fact, "\n", file = thefile)
	if (any(fact)) {
		# currently only for first layer!
		r <- x@data@attributes[[1]]
		cat("ratnames=", paste(colnames(r), collapse=':'), "\n", file = thefile)
		cat("rattypes=", paste(sapply(r, class), collapse=':'), "\n", file = thefile)
		cat("ratvalues=", paste(trim(as.character(as.matrix(r))), collapse=':'), "\n", file = thefile)
	} 
	
#	cat("levels=",  x@data@levels, "\n", file = thefile)

	cat("minvalue=",  paste(minValue(x,-1), collapse=':'), "\n", file = thefile)
	cat("maxvalue=",  paste(maxValue(x,-1), collapse=':'), "\n", file = thefile)
	cat("nodatavalue=", .nodatavalue(x), "\n", file = thefile)
#	cat("Sparse=", x@sparse, "\n", file = thefile)
#	cat("nCellvals=", x@data@ncellvals, "\n", file = thefile)	

	cat("[legend]", "\n", file = thefile)
	cat("legendtype=",  x@legend@type, "\n", file = thefile)
	cat("values=",  paste(x@legend@values, collapse=':'), "\n", file = thefile)
	cat("color=",  paste(x@legend@color, collapse=':'), "\n", file = thefile)

	cat("[description]", "\n", file = thefile)
	ln <- gsub(":", ".", names(x))
	cat("layername=", paste(ln, collapse=':'), "\n", file = thefile)
	z <- getZ(x)
	if (! is.null(z)) {
		zname <- names(x@z)[1]
		if (is.null(zname)) {
			zname <- 'z-value'
		}
		cat("zvalues=", paste(c(zname, z), collapse=':'), "\n", file = thefile)
	}
	cat("history=",  x@history, "\n", file = thefile)
	close(thefile)
	return(TRUE)
}



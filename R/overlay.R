# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


setMethod('overlay', signature(x='RasterLayer', y='missing'), 
function(x, y, fun=sum, filename="", ...){ 
	return(calc(x, fun=fun, filename=filename, ...))
}
)


setMethod('overlay', signature(x='RasterLayer', y='RasterLayer'), 
function(x, y, ..., fun, filename="", datatype, format, overwrite, progress){ 
	if (missing(fun)) { stop('you must provide a function "fun"') }
	if (missing(datatype)) { datatype <- .datatype(datatype) }
	if (missing(format)) { format <- .filetype() } 
	if (missing(overwrite)) { overwrite <- .overwrite() }
	if (missing(progress)) { progress <- .progress() }

	rasters <- .makeRasterList(x, y, ...)
	nl <- sapply(rasters, nlayers)
	if (max(nl) > 1) {
		stop("Only single layer (RasterLayer) objects can be used if 'x' and 'y' have a single layer")
	} 
	
	return(.overlayList(rasters, fun=fun, filename=filename, datatype=datatype, format=format, overwrite=overwrite, progress=progress))
}
)



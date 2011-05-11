# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


setMethod('overlay', signature(x='Raster', y='Raster'), 
function(x, y, ..., fun, filename="", datatype, format, overwrite, progress){ 
	if (missing(fun)) { 
		stop("you must supply a function 'fun'.\nE.g., 'fun=function(x,y){return(x+y)} or fun=sum'") 
	}
	filename <- trim(filename)
	if (missing(format)) { format <- .filetype(filename=filename) } 
	if (missing(datatype)) { datatype <- .datatype() }
	if (missing(overwrite)) { overwrite <- .overwrite() }
	if (missing(progress)) { progress <- .progress() }

	x <- .makeRasterList(x, y, ..., unstack=FALSE)
	
	return(.overlayList(x, fun=fun, filename=filename, datatype=datatype, format=format, overwrite=overwrite, progress=progress))
}
)


setMethod('overlay', signature(x='Raster', y='missing'), 
function(x, y, ..., fun, filename="", datatype, format, overwrite, progress, unstack=TRUE){ 
	if (missing(fun)) { 
		stop("you must supply a function 'fun'.\nE.g., 'fun=function(x,y){return(x+y)} or fun=sum'") 
	}
	filename <- trim(filename)
	if (missing(format)) { format <- .filetype(filename=filename) } 
	if (missing(datatype)) { datatype <- .datatype() }
	if (missing(overwrite)) { overwrite <- .overwrite() }
	if (missing(progress)) { progress <- .progress() }
	
	x <- .makeRasterList(x, ..., unstack=unstack)
	
	return(.overlayList(x, fun=fun, filename=filename, datatype=datatype, format=format, overwrite=overwrite, progress=progress))
}
)


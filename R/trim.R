# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : December 2009
# Version 0.9
# Licence GPL v3


if (!isGeneric("trim")) {
	setGeneric("trim", function(x, ...)
		standardGeneric("trim"))
}	


setMethod('trim', signature(x='character'), 
	function(x, ...) {
		f <- function(s) {return( gsub('^[[:space:]]+', '',  gsub('[[:space:]]+$', '', s) ) )}
		return(unlist(lapply(x, f)))
	}
)


	
setMethod('trim', signature(x='RasterLayer'), 
function(x, padding=0, filename='', ...) {

	filename <- trim(filename)

	if (dataContent(x) != 'all' & dataSource(x) == 'disk')  {
		if (canProcessInMemory(x, 3)) {
			x <- readAll(x)
		}
	}
	
	nr <- nrow(x)
	nc <- ncol(x)

	for (r in 1:nr) {
		v <- getValues(x, r)
		if (sum(is.na(v)) < nc) { break }
	}
	if ( r == nr) { stop('only NA values found') }
	firstrow <- min(max(r-padding, 1), nr)
	
	for (r in nr:1) {
		v <- getValues(x, r)
		if (sum(is.na(v)) < nc) { break }
	}
	lastrow <- max(min(r+padding, nr), 1)
	
	if (lastrow < firstrow) { 
		tmp <- firstrow
		firstrow <- lastrow
		lastrow <- tmp
	}
	
	cells <- cellFromCol(x, 1)
	for (c in 1:nc) {
		v <- cellValues(x, cells)
		if (sum(is.na(v)) < nr) { break }
		cells <- cells + 1
	}
	firstcol <- min(max(c-padding, 1), nc) 
	
	cells <- cellFromCol(x, nc)
	for (c in nc:1) {
		v <- cellValues(x, cells)
		if (sum(is.na(v)) < nr) { break }
		cells <- cells - 1
	}
	lastcol <- max(min(c+padding, nc), 1)
	
	if (lastcol < firstcol) { 
		tmp <- firstcol
		firstcol <- lastcol
		lastcol <- tmp
	}
	
	xr <- xres(x)
	yr <- yres(x)
	e <- extent(xFromCol(x, firstcol)-0.5*xr, xFromCol(x, lastcol)+0.5*xr, yFromRow(x, lastrow)-0.5*yr, yFromRow(x, firstrow)+0.5*yr)
	
	return( crop(x, e, filename=filename, ...) )
}
)


# author Robert Hijmans
# June 2010
# version 1.0
# license GPL3


.compareCRS <- function(x, y) {
	if (class(x) == 'CRS') { x <- x@projargs }
	if (class(y) == 'CRS') { y <- y@projargs }
	if (inherits(x, 'Raster')) { x <- projection(x) }
	if (inherits(x, 'Raster')) { y <- projection(y) }
	x <- trim(x)
	y <- trim(y)
	x = unlist( strsplit(x, ' ') )
	y = unlist( strsplit(y, ' ') )
	xl <- length(x)
	yl <- length(y)
	
	if (x[[1]] != y[[1]]) { return(FALSE) }

	# etc...
	
	return(TRUE)
}


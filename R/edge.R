# Author: Robert J. Hijmans
# Date : November 2011
# Version 1.0
# Licence GPL v3

edge <- function(x, ...) {
	warning('"edge" is obsolete. Use "edges"')
	edges(x, ...)
}


if (!isGeneric("edges")) {
	setGeneric("edges", function(x, ...)
		standardGeneric("edges"))
}	

setMethod('edges', signature(x='RasterLayer'), 
function(x, filename="", type='inner', classes=FALSE, directions=8, ...) {

	dots <- list(...)
	if (!is.null(dots$asZero)) {
		warning("argument 'asZero' is currently ignored")
	}
	if (!is.null(dots$asNA)) {
		warning("argument 'asNA' is currently ignored")
	}

	stopifnot( nlayers(x) == 1 )
	stopifnot( hasValues(x) )
	filename <- trim(filename)
	
	out <- raster(x)
	gll <- as.integer( .isGlobalLonLat(out) )

	type <- tolower(type)
	if (! type %in% c('inner', 'outer')) {
		stop("type must be 'inner', or 'outer'")
	}
		
	if (type=='inner') { 
		type <- as.integer(0) 
	} else { 
		type <- as.integer(1) 
	}
	classes <- as.integer(as.logical(classes))
	directions <- as.integer(directions)
	stopifnot(directions %in% c(4,8))
	
	
#	if (asNA) {	fval <- as.integer(NA) } else { fval <- as.integer(0) }
#	asZero <- as.integer(as.logical(asZero))
	
	
	datatype <- list(...)$datatype
	if (is.null(datatype)) {
		datatype <- 'INT2S'
	}
	
	if (canProcessInMemory(out, 4)) {
		x <- as.matrix(x)
		if (gll) {
			x <- cbind(x[, ncol(x)], x, x[, 1]) 
		} else {
			x <- cbind(x[, 1], x, x[, ncol(x)]) 
		}
		x <- rbind(x[1,], x, x[nrow(x),])
		paddim <- as.integer(dim(x))
		x <- .Call('edge', as.integer(t(x)), paddim, classes, type, directions, NAOK=TRUE, PACKAGE='raster')
		x <- matrix(x, nrow=paddim[1], ncol=paddim[2], byrow=TRUE)
		x <- x[2:(nrow(x)-1), 2:(ncol(x)-1)]
		x <- setValues(out, as.vector(t(x)))
		if (filename  != '') {
			x <- writeRaster(x, filename, datatype=datatype, ...)
		}
		return(x)

	} else {
	
		out <- writeStart(out, filename, datatype=datatype, ...)
		tr <- blockSize(out, minblocks=3, minrows=3)
		pb <- pbCreate(tr$n, label='edge', ...)
		
		nc <- ncol(out)+2
		v <- getValues(x, row=1, nrows=tr$nrows[1]+1)
		v <- matrix(v, ncol=tr$nrows[1]+1)
		if (gll) {
			v <- rbind(v[nrow(v),], v, v[1,])
		} else {
			v <- rbind(v[1,], v, v[nrow(v),])
		}
		v <- as.integer(cbind(v[,1], v))
		
		v <- .Call('edge', v, as.integer(c(tr$nrows[1]+2, nc)),  classes, type, directions, NAOK=TRUE, PACKAGE='raster')
		v <- matrix(v, ncol=nc, byrow=TRUE)
		v <- as.integer(t(v[2:(nrow(v)-1), 2:(ncol(v)-1)]))
		out <- writeValues(out, v, 1)
		pbStep(pb, 1)
		for (i in 2:(tr$n-1)) {
			v <- getValues(x, row=tr$row[i]-1, nrows=tr$nrows[i]+2)
			v <- matrix(v, ncol=tr$nrows[1]+3)
			if (gll) {
				v <- rbind(v[nrow(v),], v, v[1,])
			} else {
				v <- rbind(v[1,], v, v[nrow(v),])
			}
			v <- .Call('edge', as.integer(v), as.integer(c(tr$nrows[i]+2, nc)), classes, type, directions, NAOK=TRUE, PACKAGE='raster')
			v <- matrix(v, ncol=nc, byrow=TRUE)
			v <- as.integer(t(v[2:(nrow(v)-1), 2:(ncol(v)-1)]))
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb, i)
		}
		i <- tr$n
		v <- getValues(x, row=tr$row[i]-1, nrows=tr$nrows[i]+1)
		v <- matrix(v, ncol=tr$nrows[1]+1)
		if (gll) {
			v <- rbind(v[nrow(v),], v, v[1,])
		} else {
			v <- rbind(v[1,], v, v[nrow(v),])
		}
		v <- as.integer(cbind(v, v[,ncol(v)]))
		v <- .Call('edge', v, as.integer(c(tr$nrows[i]+2, nc)), classes, type, directions, NAOK=TRUE, PACKAGE='raster')
		v <- matrix(v, ncol=nc, byrow=TRUE)
		v <- as.integer(t(v[2:(nrow(v)-1), 2:(ncol(v)-1)]))
		out <- writeValues(out, v, tr$row[i])
		pbStep(pb, tr$n)

		out <- writeStop(out)
		pbClose(pb)
	}
	return(out)
}
)



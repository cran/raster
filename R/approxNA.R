
if (!isGeneric("approxNA")) {
	setGeneric("approxNA", function(x, ...)
		standardGeneric("approxNA"))
}	



setMethod('approxNA', signature(x='RasterStackBrick'), 
function(x, filename="", method="linear", yleft, yright, rule=1, f=0, ties=mean, z=NULL, ...) { 

	filename <- trim(filename)
	out <- brick(x, values=FALSE)
	nl <- nlayers(out)
	if (nl < 2) {
		warning('cannot interpolate with a single layer')
		return(x)
	}
	
	if (is.null(z)) {
		xout <- getZ(x)
		if (is.null(xout)) {
			xout <- 1:nl
		} else if (length(xout)!= nl) {
			stop('length of values returned by getZ(x) does not match the number of layers of x')
		}
	} else {
		if (length(z)!= nl) {
			stop('length of z does not match the number of layers of x')
		}
		xout <- z		
	}
	
	ifelse((missing(yleft) & missing(yright)), ylr <- 0L, ifelse(missing(yleft), ylr <- 1L, ifelse(missing(yright), ylr <- 2L, ylr <- 3L)))
	
	if (canProcessInMemory(x)) {
		x <- getValues(x)
		i <- rowSums(is.na(x))
		i <- i < nl & i > 1 # need at least two
		if (length(i) > 0 ) {
			if (ylr==0) {
				x[i,] <- t(apply(x[i,], 1, function(x) approx(x=xout, y=x, xout=xout, method=method, rule=rule, f=f, ties=ties)$y ))
			} else if (ylr==1) {
				x[i,] <- t(apply(x[i,], 1, function(x) approx(x=xout, y=x, xout=xout, method=method, yright=yright, rule=rule, f=f, ties=ties)$y ))			
			} else if (ylr==2) {
				x[i,] <- t(apply(x[i,], 1, function(x) approx(x=xout, y=x, xout=xout, method=method, yleft=yleft, rule=rule, f=f, ties=ties)$y ))						
			} else {
				x[i,] <- t(apply(x[i,], 1, function(x) approx(x=xout, y=x, xout=xout, method=method, yright=yright, yleft=yleft, rule=rule, f=f, ties=ties)$y ))
			}
		} else {
			warning('no NA values to approximate')
		}
		x <- setValues(out, x)
		if (filename != '') {
			x <- writeRaster(x, filename=filename, ...)
		}
		return(x)
	} 
	
	tr <- blockSize(out)
	pb <- pbCreate(tr$n, ...)
	out <- writeStart(out, filename=filename, ...)

	for (j in 1:tr$n) {
		v <- getValues(x, row=tr$row[j], nrows=tr$nrows[j])
		i <- rowSums(is.na(v))
		i <- (i < nl) & (i > 1) # need at least two
		if (length(i) > 0 ) {
			if (ylr==0) {
				v[i,] <- t( apply(v[i,], 1, function(x) approx(x=xout, y=x, xout=xout, method=method, rule=rule, f=f, ties=ties)$y ) )
			} else if (ylr==1) {
				v[i,] <- t( apply(v[i,], 1, function(x) approx(x=xout, y=x, xout=xout, method=method, yright=yright, rule=rule, f=f, ties=ties)$y ) )
			} else if (ylr==2) {
				v[i,] <- t( apply(v[i,], 1, function(x) approx(x=xout, y=x, xout=xout, method=method, yleft=yleft, rule=rule, f=f, ties=ties)$y ) )
			} else {
				v[i,] <- t( apply(v[i,], 1, function(x) approx(x=xout, y=x, xout=xout, method=method, yright=yright, yleft=yleft, rule=rule, f=f, ties=ties)$y ) )
			}
		}
		out <- writeValues(out, v, tr$row[j])
		pbStep(pb)
	}
	
	pbClose(pb)
	out <- writeStop(out)
	return(out)
}
)


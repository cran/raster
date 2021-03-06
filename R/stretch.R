# author Josh Gray
# http://spatiallyexplicit.wordpress.com/2011/06/07/crop-circles/
# minor modifications by Robert Hijmans
# Note: these functions only work (correctly) for single layer objects 

.linStretchVec <- function (x) {
    v <- stats::quantile(x, c(0.02, 0.98), na.rm = TRUE)
    temp <- (255 * (x - v[1]))/(v[2] - v[1])
    temp[temp < 0] <- 0
    temp[temp > 255] <- 255
    return(temp)
}


.linStretch <- function (x) {
    v <- stats::quantile(x, c(0.02, 0.98), na.rm = TRUE)
    temp <- calc(x, fun = function(x) (255 * (x - v[1]))/(v[2] - v[1]))
    temp[temp < 0] <- 0
    temp[temp > 255] <- 255
    return(temp)
}

# Histogram equalization stretch
.eqStretch <- function(x){
	ecdfun <- stats::ecdf(getValues(x))
	return( calc(x, fun=function(x) ecdfun(x)*255) )
}

.eqStretchVec <- function(x){
	ecdfun <- stats::ecdf(x)
	ecdfun(x)*255
}


setMethod("stretch", signature(x="Raster"), 
function(x, minv=0, maxv=255, minq=0, maxq=1, smin=NA, smax=NA, samplesize=1000000, filename="", ...) {

	if ((length(minq) > 1) | (length(maxq) > 1)) {
		minq <- minq[1]
		maxq <- maxq[1]
	}
	stopifnot(maxq > minq)
	
	if ((length(minv) > 1) | (length(maxv) > 1)) {
		warning("only the first element of minv and maxv is used")
		maxv <- maxv[1]
		minv <- minv[1]
	}
	stopifnot(maxv > minv)

	if (!any(is.na(smin)) & !(any(is.na(smax)))) {
		stopifnot(all(smin < smax))
		q <- cbind(smin, smax)
	} else {
		minq <- max(0,minq)
		maxq <- min(1,maxq)
		stopifnot(minq < maxq)

		if ((minq==0 & maxq==1) & .haveMinMax(x)) {
			q <- cbind(minValue(x), maxValue(x))
		} else {
			if (samplesize[1] < ncell(x)) {
				stopifnot(samplesize[1] > 1) 
				y <- sampleRegular(x, samplesize, asRaster=TRUE)
				q <- quantile(y, c(minq, maxq), na.rm=TRUE)
			} else {
				q <- quantile(x, c(minq, maxq), na.rm=TRUE)
			}
		}
	}
	
	if (nlayers(x) == 1) {	
		out <- raster(x)
		mult <- maxv / (q[2]-q[1])
		if (canProcessInMemory(out)) {
			x <- getValues(x)			
			x <- mult * (x-q[1])
			x[x < minv] <- minv
			x[x > maxv] <- maxv
			out <- setValues(out, x)
		} else {
			tr <- blockSize(out)
			pb <- pbCreate(tr$n, label='stretch', ...)		
			out <- writeStart(out, filename, ...)
			for (i in 1:tr$n) {
				v <- getValues(x, tr$row[i], tr$nrows[i])
				v <- mult*(v-q[1])
				v[v < minv] <- minv
				v[v > maxv] <- maxv
				out <- writeValues(out, v, tr$row[i])
			}
			out <- writeStop(out)
		}
	} else {
		out <- brick(x, values=FALSE)
		mult <- maxv / (q[,2]-q[,1])
		if (canProcessInMemory(out)) {
			x <- getValues(x)
			x <- t(mult*(t(x)-q[,1]))
			x[x < minv] <- minv
			x[x > maxv] <- maxv
			out <- setValues(out, x)
		} else {
			tr <- blockSize(out)
			pb <- pbCreate(tr$n, label='stretch', ...)		
			out <- writeStart(out, filename, ...)
			for (i in 1:tr$n) {
				v <- getValues(x, tr$row[i], tr$nrows[i])
				v <- t(mult*(t(v)-q[,1]))
				v[v < minv] <- minv
				v[v > maxv] <- maxv
				out <- writeValues(out, v, tr$row[i])
			}
			out <- writeStop(out)
		}
	}
	return(out)
}
)


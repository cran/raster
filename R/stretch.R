# author Josh Gray
# http://spatiallyexplicit.wordpress.com/2011/06/07/crop-circles/
# minor modifications by Robert Hijmans
# Note: these functions only work (correctly) for single layer objects 


# Linear stretch between min and max values
.linStretch <- function(x) {
#	if (!x@data@haveminmax) {
#		x <- setMinMax(x)
#	}
#	minv <- minValue(x)
#	maxv <- maxValue(x)
#	temp <- calc(x, fun=function(x) (255*(x-minv))/(maxv-minv))
#   or:
	v <- quantile(x, c(0.02, 0.98), na.rm=TRUE)
	temp <- calc(x, fun=function(x) (255*(x-v[1]))/(v[2]-v[1]))	
	temp[temp < 0] <- 0
	temp[temp > 255] <- 255
	return(temp)
}

# Histogram equalization stretch
.eqStretch <- function(x){
	ecdfun <- ecdf(getValues(x))
	return( calc(x, fun=function(x) ecdfun(x)*255) )
}


# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  November 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric("plot3D")) {
	setGeneric("plot3D", function(x,...)
		standardGeneric("plot3D"))
}	

setMethod("plot3D", signature(x='RasterLayer'), 
function(x, maxpixels=100000, zfac=6, col=terrain.colors, rev=TRUE, ...) { 
# maxpixels=100000; zfac=6; col=terrain.colors; rev=TRUE
# most of the below code was taken from example(surface3d) in the rgl package
	if (!require(rgl)){ stop("to use this function you need to install the 'rgl' package") }
	x <- sampleRegular(x, n=maxpixels, asRaster=TRUE)
	X <- xFromCol(x,1:ncol(x))
	Y <- yFromRow(x, nrow(x):1)
	Z <- t((values(x, format='matrix'))[nrow(x):1,])
	background <- min(Z, na.rm=TRUE) - 1
	Z[is.na(Z)] <- background
	zlim <- range(Z)
	zlen <- zlim[2] - zlim[1] + 1
	xlen <- max(X) - min(X)
	ylen <- max(Y) - min(Y)
	adj <- zlen/min(ylen,xlen)
	X <- X * adj * zfac
	Y <- Y * adj * zfac
	colorlut <- col(zlen) # height color lookup table
	if (rev) { colorlut <- rev(colorlut) }
	color <- colorlut[ Z-zlim[1]+1 ] # assign colors to heights for each point
	open3d()
	if (background==min(Z)) {
		trans <- Z
		trans[] <- 1.0
		trans[Z==background] <- 0
		surface3d(X, Y, Z, color=color, back="lines", alpha=trans, ...)
	} else {
		surface3d(X, Y, Z, color=color, back="lines", ...)
	}
}
)

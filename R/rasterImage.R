
.plotr <- function(x, maxpixels=500000, col=rev(terrain.colors(25)), interpolate=FALSE, xlab='', ylab='', extent=NULL, classes=FALSE, ...) {

	x <- sampleRegular(x, maxpixels, extent=extent, asRaster=TRUE, corners=TRUE)
	bb <- as.vector(t(bbox(x)))
	dm <- dim(x)
	x <- as.matrix(x)
	if (classes) {
		if (length(unique(x)) <= length(col)) {
			x <- col(x)
		}
	} else {
		x <- as.numeric(cut(x, length(col)))
		x <- col[x]
	}
	dim(x) <- dm
	plot(c(bb[1], bb[2]), c(bb[3], bb[4]), type = "n", xlab=xlab, ylab=ylab, ...)
	rasterImage(x, bb[1], bb[3], bb[2], bb[4], interpolate=interpolate, ...)
	
}

#.plotr(r)




.img <- function(x, maxpixels=500000, col=rainbow(25), interpolate=FALSE, axes=TRUE, xlab='', ylab='', ext=NULL, alpha=1, classes=FALSE, add=FALSE, box=TRUE, asp, ticks=c(4,4), ...) {

	if (!add) {
	#	if (!axes) par(plt=c(0,1,0,1))
	}
	
	if (missing(asp)) {
		if (raster:::.couldBeLonLat(x)) {
			ym <- mean(x@extent@ymax + x@extent@ymin)
			asp <- min(5, 1/cos((ym * pi)/180))
		} else {
			asp = 1
		}
	}
	
	r <- raster(x)
	if (alpha < 1) {
		alpha <- max(alpha, 0) * 255 + 1
		a    <- c(0:9, LETTERS[1:6])
		alpha <- paste(rep(a, each=16), rep(a, times=16), sep='')[alpha]
		col <- paste(substr(col, 1, 7), alpha, sep="")
	}

	x <- sampleRegular(x, maxpixels, ext=ext, asRaster=TRUE)
	bb <- as.vector(t(bbox(x)))
	dm <- dim(x)[1:2]
	x <- as.matrix(x)
	if (classes) {
		if (length(unique(x)) <= length(col)) {
			x <- col[x]
		}
	} else {
		x <- as.numeric(cut(x, length(col)))
		x <- col[x]
	}
	dim(x) <- dm
	if (!add) {
		plot(c(bb[1], bb[2]), c(bb[3], bb[4]), type = "n", xlab=xlab, ylab=ylab, axes=FALSE, asp=asp, ...)
		if (axes) {
			xt <- ticks[1]
			if (length(ticks) > 1) {
				yt <- ticks[2]
			} else {
				yt <- xt
			}
			xticks <- axTicks(1, c(bb[1], bb[2], xt))
			yticks <- axTicks(2, c(bb[3], bb[4], yt))
			if (xres(r) %% 1 == 0) xticks = round(xticks)
			if (yres(r) %% 1 == 0) yticks = round(yticks)
			axis(1, at=xticks, pos=bb[3])
			axis(2, at=yticks, las = 1, pos=bb[1])
			#axis(1, pos=bb[3], ...)
			#axis(2, pos=bb[1], ...)
		}
	}
	rasterImage(x, bb[1], bb[3], bb[2], bb[4], interpolate=interpolate, ...)
	if (box) {
		axis(1, at=c(-180,180), labels=FALSE, lwd.ticks=0, pos=-90)
		axis(3, at=c(-180,180), labels=FALSE, lwd.ticks=0, pos=90)
		axis(2, at=c(-90,90), labels=FALSE, lwd.ticks=0, pos=-180)
		axis(4, at=c(-90,90), labels=FALSE, lwd.ticks=0, pos=180)
	} else if (axes) {
		axis(1, at=c(-180,180), labels=FALSE, lwd.ticks=0, pos=-90)
		axis(2, at=c(-90,90), labels=FALSE, lwd.ticks=0, pos=-180)
	}
}

#.img(r)

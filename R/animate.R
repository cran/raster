
# this needs to be built into the plot function to avoid redrawing the legend each time.

.anim <- function(x, pause=0.25, main, zlim, maxpixels=50000, ...) {
	nl <- nlayers(x)
	if (missing(main)) {
		main <- getZ(x)
		if (is.null(main)) {
			main <- 1:nl
		}
	}

	x <- sampleRegular(x, size=maxpixels, asRaster=TRUE, useGDAL=TRUE)
	
	if (missing(zlim)) {
		zlim <- c(min(minValue(x)), max(maxValue(x)))
	}
	
    i <- 1
	plot(x[[i]], main=main[i], zlim=zlim)	
	while (TRUE) {
		i <- i + 1
		if (i > nl) i <- 1
		plot(x[[i]], main=main[i], zlim=zlim, maxpixels=maxpixels, ...)
		dev.flush()
		Sys.sleep(pause)
	}
}

#anim(st, tvals)

# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  April 2010
# Version 0.9
# Licence GPL v3

# partly based on functions in the pixmap package by Friedrich Leisch

if (!isGeneric("plotRGB")) {
	setGeneric("plotRGB", function(x, ...)
		standardGeneric("plotRGB"))
}	


setMethod("plotRGB", signature(x='RasterStackBrick'), 
function(x, r=1, g=2, b=3, scale, maxpixels=500000, stretch=NULL, ext=NULL, interpolate=FALSE, bgcol='white', alpha, bgalpha, ...) { 

	if (missing(scale)) {
		scale <- 255
		if (! inherits(x, 'RasterStack')) {
			if ( x@data@haveminmax ) {
				scale <- max(max(x@data@max), 255)
			}
		}
	}
	
	r <- sampleRegular(raster(x,r), maxpixels, ext=ext, asRaster=TRUE, useGDAL=TRUE)
	g <- sampleRegular(raster(x,g), maxpixels, ext=ext, asRaster=TRUE, useGDAL=TRUE)
	b <- sampleRegular(raster(x,b), maxpixels, ext=ext, asRaster=TRUE, useGDAL=TRUE)
	
	if (!is.null(stretch)) {
		stretch = tolower(stretch)
		if (stretch == 'lin') {
			r <- .linStretch(r)
			g <- .linStretch(g)
			b <- .linStretch(b)
		} else if (stretch == 'hist') {
			r <- .eqStretch(r)
			g <- .eqStretch(g)
			b <- .eqStretch(b)
		} else if (stretch != '') {
			warning('invalid stretch value')
		}
	}
	
	scale = as.vector(scale)[1]
	
	RGB <- na.omit(cbind(getValues(r), getValues(g), getValues(b)))
	
	naind <- as.vector( attr(RGB, "na.action") )
	if (!is.null(naind)) {
		bg <- col2rgb(bgcol)
		bg <- rgb(bg[1], bg[2], bg[3], alpha=bgalpha, max=255)
		z <- rep( bg, times=ncell(r))
		z[-naind] <- rgb(RGB[,1], RGB[,2], RGB[,3], alpha=alpha, max=scale)
	} else {
		z <- rgb(RGB[,1], RGB[,2], RGB[,3], alpha=alpha, max=scale)
	}
	
	z <- matrix(z, nrow=nrow(r), ncol=ncol(r), byrow=T)

	require(grDevices)
	bb <- as.vector(t(bbox(r)))

	dots <- list(...)
	add <- ifelse(is.null(dots$add), FALSE, dots$add)
	
	if (!add) {
		xlab <- ifelse(is.null(dots$xlab), '', dots$xlab)
		ylab <- ifelse(is.null(dots$ylab), '', dots$ylab)
		axes <- ifelse(is.null(dots$axes), FALSE, dots$axes)
		
		if (!axes) par(plt=c(0,1,0,1))

		asp <- dots$asp
		if (is.null(asp)) {
			if (.couldBeLonLat(x)) {
				ym <- mean(x@extent@ymax + x@extent@ymin)
				asp <- min(5, 1/cos((ym * pi)/180))
				asp <- NA
			} else {
				asp <- 1
			}
		}
		
		plot(NA, NA, xlim=c(bb[1], bb[2]), ylim=c(bb[3], bb[4]), type = "n", xaxs='i', yaxs='i', xlab=xlab, ylab=ylab, asp=asp, axes=FALSE)
		if (axes) {
			xticks <- axTicks(1, c(xmin(r), xmax(r), 4))
			yticks <- axTicks(2, c(ymin(r), ymax(r), 4))
			if (xres(r) %% 1 == 0) xticks = round(xticks)
			if (yres(r) %% 1 == 0) yticks = round(yticks)
			axis(1, at=xticks)
			axis(2, at=yticks, las = 1)
			#axis(3, at=xticks, labels=FALSE, lwd.ticks=0)
			#axis(4, at=yticks, labels=FALSE, lwd.ticks=0)
		}
	}
	rasterImage(z, bb[1], bb[3], bb[2], bb[4], interpolate=interpolate)
	
	addfun <- dots$addfun
	if (is.function(addfun)) {
		addfun()
	}
}
)


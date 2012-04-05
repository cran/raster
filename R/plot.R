# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("plot")) {
	setGeneric("plot", function(x,y,...)
		standardGeneric("plot"))
}	


setMethod("plot", signature(x='RasterStackBrick', y='ANY'), 
	function(x, y, col=rev(terrain.colors(255)), maxpixels=100000, alpha=1, main, useRaster=TRUE, nc, nr, ...)  {
	
		if (alpha < 1) {
			alpha <- max(alpha, 0) * 255 + 1
			a <- c(0:9, LETTERS[1:6])
			alpha <- paste(rep(a, each=16), rep(a, times=16), sep='')[alpha]
			col <- paste(substr(col, 1, 7), alpha, sep="")
		}

	
		if (missing(y)) {
			y <- 1:nlayers(x)
			if (length(y) > 16) {
				warning('only first 16 layers are plotted')
				y <- 1:16
			}
		} else {
			if (is.character(y)) {
				y = .nameToIndex(y, layerNames(x))
			}
			y <- unique(as.integer(round(y)))
			y <- na.omit(y)
		}
		
		if (missing(main)) {
			main <- layerNames(x)
		}
		
		if (length(y) == 1) {
			if (useRaster) {
				.plotraster2(raster(x, y), col=col, maxpixels=maxpixels, main=main[y], ...) 
			} else {
				.plotraster(raster(x, y), col=col, maxpixels=maxpixels, main=main[y], ...) 			
			}
		} else {

			nl <- length(y)
			if (missing(nc)) {
				nc <- ceiling(sqrt(nl))
			} else {
				nc <- max(1, min(nl, round(nc)))
			}
			if (missing(nr)) {
				nr <- ceiling(nl / nc)
			} else {
				nr <- max(1, min(nl, round(nr)))
				nc <- ceiling(nl / nr)
			}
		
			old.par <- par(no.readonly = TRUE) 
			on.exit(par(old.par))
			par(mfrow=c(nr, nc), mar=c(2, 2, 2, 4))
			xa='n'
			rown=1
			coln=0
			maxpixels=maxpixels/nl
			for (i in 1:nl) {
				coln = coln + 1
				if (coln > nc) {
					coln <- 1
					rown = rown + 1
				}
				if (rown==nr) xa='s'
				if (coln==1) ya='s' else ya='n'
				if (useRaster) {
					.plotraster2(raster(x, y[i]), col=col, maxpixels=maxpixels, xaxt=xa, yaxt=ya, main=main[y[i]], ...) 
				} else {
					.plotraster(raster(x, y[i]), col=col, maxpixels=maxpixels, xaxt=xa, yaxt=ya, main=main[y[i]], ...) 				
				}
			}		
		}
	}
)	


setMethod("plot", signature(x='RasterLayer', y='missing'), 
	function(x, col=rev(terrain.colors(255)), maxpixels=500000, useRaster=TRUE, alpha=1, add=FALSE, ...)  {

		# maxpixels = min( prod(par()$din * 72), maxpixels)
	
		if (alpha < 1) {
			alpha <- max(alpha, 0) * 255 + 1
			a <- c(0:9, LETTERS[1:6])
			alpha <- paste(rep(a, each=16), rep(a, times=16), sep='')[alpha]
			col <- paste(substr(col, 1, 7), alpha, sep="")
		}

		if (length(x@legend@colortable) > 0) {
			.plotCT(x, maxpixels=maxpixels, ...)
		} else if (! useRaster) {
			.plotraster(x, col=col, maxpixels=maxpixels, add=add, ...) 
		} else {
			.plotraster2(x, col=col, maxpixels=maxpixels, add=add, ...) 
			#.plot2(x, col=col, maxpixels=maxpixels, ...)
		}
	}
)	




setMethod("plot", signature(x='Raster', y='Raster'), 
	function(x, y, maxpixels=100000, cex=0.1, xlab, ylab, ...)  {
		compare(c(x, y), extent=TRUE, rowcol=TRUE, crs=FALSE, stopiffalse=TRUE) 
		nl <- nlayers(x)
		nl2 <- nlayers(y)
		if (nl != nl2) {
			warning('number of layers does not match')
		}
		nl <- min(nl, nl2)
		if (nl > 16) {
			warning('only first 16 layers are plotted')
			nl <- 16
		}
		if (missing(xlab)) {
			ln1 <- layerNames(x)
		} else {
			ln1 <- xlab
			if (length(ln1) == 1) {
				ln1 <- rep(ln1, nl)
			}
		}
		if (missing(ylab)) {
			ln2 <- layerNames(y)
		} else {
			ln2 <- ylab
			if (length(ln1) == 1) {
				ln2 <- rep(ln2, nl)
			}
		}

		cells <- ncell(x)
		
		# gdal selects a slightly different set of cells than raster does for other formats.
		# using gdal directly to subsample is faster.
		dx <- .driver(x, warn=FALSE)
		dy <- .driver(y, warn=FALSE)
		if ( all(dx =='gdal') & all(dy == 'gdal')) {
			x <- sampleRegular(x, size=maxpixels, useGDAL=TRUE) 
			y <- sampleRegular(y, size=maxpixels, useGDAL=TRUE)
		} else {
			x <- sampleRegular(x, size=maxpixels)
			y <- sampleRegular(y, size=maxpixels)
		}
		if (length(x) < cells) {
			warning(paste('plot used a sample of ', round(100*length(x)/cells), "% of the cells", sep=""))
		}
			
		if (nl > 1) {
			old.par <- par(no.readonly = TRUE) 
			on.exit(par(old.par))
			nc <- ceiling(sqrt(nl))
			nr <- ceiling(nl / nc)
			par(mfrow=c(nr, nc), mar=c(4, 4, 2, 2))
			for (i in 1:nl) {
				plot(x[,i], y[,i], cex=cex, xlab=ln1[i], ylab=ln2[i], ...)			
			}		
		} else  {
			plot(x, y, cex=cex, xlab=ln1[1], ylab=ln2[1], ...)			
		}		
	}
)



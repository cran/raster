# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("plot")) {
	setGeneric("plot", function(x,y,...)
		standardGeneric("plot"))
}	


setMethod("plot", signature(x='RasterStackBrick', y='ANY'), 
	function(x, y, col=rev(terrain.colors(255)), maxpixels=100000, alpha=1, main, ...)  {
	
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
			.plotraster2(raster(x, y), col=col, maxpixels=maxpixels, main=main[y], ...) 
		} else {

			nl <- length(y)
			nc <- ceiling(sqrt(nl))
			nr <- ceiling(nl / nc)
		
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
				.plotraster2(raster(x, y[i]), col=col, maxpixels=maxpixels, xaxt=xa, yaxt=ya, main=main[y[i]], ...) 
			}		
		}
	}
)	


setMethod("plot", signature(x='RasterLayer', y='missing'), 

function(x, col=rev(terrain.colors(255)), maxpixels=500000, oldstyle=FALSE, alpha=1, ...)  {

		if (alpha < 1) {
			alpha <- max(alpha, 0) * 255 + 1
			a <- c(0:9, LETTERS[1:6])
			alpha <- paste(rep(a, each=16), rep(a, times=16), sep='')[alpha]
			col <- paste(substr(col, 1, 7), alpha, sep="")
		}

		if (length(x@legend@colortable) > 0) {
			.plotCT(x, maxpixels=maxpixels, ...)
		} else if (oldstyle) {
			.plotraster(x, col=col, maxpixels=maxpixels, ...) 
		} else {
			.plotraster2(x, col=col, maxpixels=maxpixels, ...) 
			#.plot2(x, col=col, maxpixels=maxpixels, ...)
		}
	}
)	


setMethod("plot", signature(x='RasterStackBrick', y='RasterStackBrick'), 
	function(x, y, maxpixels=100000, cex=0.1, ...)  {
		plot(x[[1]], y[[1]], maxpixels=maxpixels, cex=cex, ...)
	}
)

setMethod("plot", signature(x='RasterLayer', y='RasterLayer'), 
	function(x, y, maxpixels=100000, cex=0.1, ...)  {
		compare(c(x, y), extent=TRUE, rowcol=TRUE, prj=FALSE, stopiffalse=TRUE) 
		nc <- ncell(x)
		x <- sampleRegular(x, size=maxpixels)
		y <- sampleRegular(y, size=maxpixels)
		if (length(x) < nc) {
			warning(paste('plot used a sample of ', round(100*length(x)/nc), "% of the cells", sep=""))
		}
		plot(x, y, cex=cex, ...)			
	}
)


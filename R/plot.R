# Author: Robert J. Hijmans
# Date :  June 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("plot")) {
	setGeneric("plot", function(x,y,...)
		standardGeneric("plot"))
}	


setMethod("plot", signature(x='Raster', y='ANY'), 
	function(x, y, col=rev(terrain.colors(255)), maxpixels=500000, alpha=1, colNA=NA, add=FALSE, ext=NULL, useRaster=TRUE, interpolate=FALSE, addfun=NULL, nc, nr, maxnl=16, main, ...)  {
	
		if (alpha < 1) {
			alpha <- max(alpha, 0) * 255 + 1
			a <- c(0:9, LETTERS[1:6])
			alpha <- paste(rep(a, each=16), rep(a, times=16), sep='')[alpha]
			col <- paste(substr(col, 1, 7), alpha, sep="")
		}
		
		nl <- nlayers(x)
		if (nl == 0) {
			stop('Raster object has no cell values')
		}

		if (nl == 1) {
			if (!missing(y)) {
				if (is.character(y)) {
					if (is.factor(x)) {
						x <- deratify(x, y)														
					} else {
						y <- match(y, names(x))
						if (is.na(y)) {
							warning('argument "y" ignored')
						}
					}
				}
			}
			if (missing(main)) {
				main <- ''
			}
				
			if (length(x@legend@colortable) > 0) {
				.plotCT(x, maxpixels=maxpixels, ext=ext, interpolate=interpolate, main=main, ...)
			} else if (! useRaster) {
				.plotraster(x, col=col, maxpixels=maxpixels, add=add, ext=ext, main=main,...) 
			} else {
				.plotraster2(x, col=col, maxpixels=maxpixels, add=add, ext=ext, interpolate=interpolate, colNA=colNA, main=main,...) 
				#.plot2(x, col=col, maxpixels=maxpixels, ...)
			}
			return(invisible(NULL))
		}
	
		if (missing(y)) {
			y <- 1:nl
			if (length(y) > maxnl) {
				y <- 1:maxnl
			}
		} else {
			if (is.character(y)) {
				y <- match(y, names(x))
			}
			y <- unique(as.integer(round(y)))
			y <- na.omit(y)
		}
		
		if (missing(main)) {
			main <- names(x)
		}
		
		if (length(y) == 1) {
			x <- raster(x, y)
			if (length(x@legend@colortable) > 0) {
				.plotCT(x, maxpixels=maxpixels, ext=ext, interpolate=interpolate, main=main, ...)
			} else if (useRaster) {
				.plotraster2(x, col=col, colNA=colNA, maxpixels=maxpixels, main=main[y], ext=ext, interpolate=interpolate, ...) 
			} else {
				.plotraster(x, col=col, maxpixels=maxpixels, main=main[y], ext=ext, interpolate=interpolate, ...) 
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
				
				obj <- raster(x, y[i])
				if (length(obj@legend@colortable) > 0) {
					.plotCT(obj, maxpixels=maxpixels, ext=ext, interpolate=interpolate, main=main, ...)
				} else if (useRaster) {
					.plotraster2(obj, col=col, maxpixels=maxpixels, xaxt=xa, yaxt=ya, main=main[y[i]], 
						ext=ext, interpolate=interpolate, colNA=colNA, ...) 
				} else {
					.plotraster(obj, col=col, maxpixels=maxpixels, xaxt=xa, yaxt=ya, main=main[y[i]], 
						ext=ext, interpolate=interpolate, ...) 
				}
			}		
		}
		return(invisible(NULL))
	}
)	



setMethod("plot", signature(x='Raster', y='Raster'), 
	function(x, y, maxpixels=100000, cex=0.2, xlab, ylab, maxnl=16, ...)  {
		compare(c(x, y), extent=TRUE, rowcol=TRUE, crs=FALSE, stopiffalse=TRUE) 
		nlx <- nlayers(x)
		nly <- nlayers(y)

		maxnl <- max(1, round(maxnl))
		nl <- max(nlx, nly)
		if (nl > maxnl) {
			nl <- maxnl
			if (nlx > maxnl) {
				x <- x[[1:maxnl]]
				nlx <- maxnl
			}
			if (nly > maxnl) {
				y <- y[[1:maxnl]]
				nly <- maxnl
			}
		}
		
		if (missing(xlab)) {
			ln1 <- names(x)
		} else {
			ln1 <- xlab
			if (length(ln1) == 1) {
				ln1 <- rep(ln1, nlx)
			}
		}
		if (missing(ylab)) {
			ln2 <- names(y)
		} else {
			ln2 <- ylab
			if (length(ln1) == 1) {
				ln2 <- rep(ln2, nly)
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
			
		if (nlx != nly) {	
			# recycling
			d <- cbind(as.vector(x), as.vector(y))
			x <- matrix(d[,1], ncol=nl)
			y <- matrix(d[,2], ncol=nl)
			lab <- vector(length=nl)
			lab[] <- ln1
			ln1 <- lab
			lab[] <- ln2
			ln2 <- lab		
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



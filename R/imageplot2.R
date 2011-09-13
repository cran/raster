# The functions is based on a function in the fields package
# Copyright 2004-2007, Institute for Mathematics Applied Geosciences
# University Corporation for Atmospheric Research
# Licensed under the GPL -- www.gpl.org/licenses/gpl.html
#
# Adjustments by Robert Hijmans
# July 2011


.rasterImagePlot <- function(x, add=FALSE, legend=TRUE, nlevel = 64, horizontal = FALSE, 
    legend.shrink = 0.5, legend.width = 0.6, legend.mar = ifelse(horizontal, 3.1, 5.1), legend.lab = NULL, graphics.reset = FALSE, 
    bigplot = NULL, smallplot = NULL, legend.only = FALSE, col = heat.colors(nlevel), 
    lab.breaks = NULL, axis.args = NULL, legend.args = NULL, interpolate=FALSE, box=TRUE, breaks=NULL, zlim=NULL, 
	fun=NULL, ...) {

	
	asRaster <- function(x, col, breaks=NULL) {
		if (!is.null(breaks)) {
			x[] <- as.numeric(cut(x, breaks))
		}
		if (is.function(fun)) {
			x[] <- fun(x)
		}
		r <- range(x, na.rm=TRUE)
		if (r[1] == r[2]) {
			r[1] <- r[1] - 0.001
			r[2] <- r[2] + 0.001
		}
		x <- (x - r[1])/ (r[2] - r[1])
		x <- round(x * (length(col)-1) + 1)
		x[] <- col[x]
		as.raster(x)
	}
	

	e <- as.vector(t(bbox(extent(x))))
	x <- as.matrix(x)
	x[is.infinite(x)] <- NA
	if (!is.null(zlim)) {
		x[x<zlim[1] | x>zlim[2]] <- NA
	}
	zrange <- range(x, na.rm=TRUE)
	x <- asRaster(x, col, breaks)
	
    old.par <- par(no.readonly = TRUE)
    if (add) {
        big.plot <- old.par$plt
    }
    if (legend.only) {
        graphics.reset <- TRUE
    }
	
    if (is.null(legend.mar)) {
        legend.mar <- ifelse(horizontal, 3.1, 5.1)
    }
    temp <- .imageplotplt(add = add, legend.shrink = legend.shrink, legend.width = legend.width, legend.mar = legend.mar, 
									horizontal = horizontal, bigplot = bigplot, smallplot = smallplot)
		
    smallplot <- temp$smallplot
    bigplot <- temp$bigplot


    if (legend.only) {
		box <- FALSE
	} else {
        if (!add) {
            par(plt = bigplot)
			plot(NA, NA, xlim=e[1:2], ylim=e[3:4], type = "n", , xaxs ='i', yaxs = 'i', ...)
        }
		rasterImage(x, e[1], e[3], e[2], e[4], interpolate=interpolate)
        big.par <- par(no.readonly = TRUE)
    } 
	
	if (legend) {
		if ((smallplot[2] < smallplot[1]) | (smallplot[4] < smallplot[3])) {
			par(old.par)
			stop("plot region is too small. Cannot add a legend\n")
		}
		ix <- 1
		minz <- zrange[1]
		maxz <- zrange[2]
		if (minz == maxz) {
			minz <- minz - 0.001
			maxz <- maxz + 0.001
		}


		par(new=TRUE, pty = "m", plt=smallplot, err = -1)
		
		if (!is.null(breaks)) {
			binwidth <- (maxz - minz)/100
			midpoints <- seq(minz, maxz, by = binwidth)
		}

		if (!is.null(breaks)) {
			axis.args <- c(list(side=ifelse(horizontal,1,4), mgp=c(3,1,0), las=ifelse(horizontal,0,2)), axis.args)
			if (is.null(axis.args$at)) {
				axis.args$at <- breaks
			}
			if (is.null(axis.args$labels) ) {
				axis.args$labels=lab.breaks
			}
							
		} else {
			axis.args <- c(list(side = ifelse(horizontal, 1, 4), mgp = c(3, 1, 0), las = ifelse(horizontal, 0, 2)), axis.args)
		}
		
		if (!horizontal) {
			plot(NA, NA, xlim=c(0, 1), ylim=c(minz, maxz), type="n", xlab="", ylab="", xaxs ='i', yaxs = 'i', axes=FALSE)
			
			if (is.null(breaks)) {
				xx <- asRaster(length(col):1, col) 
			} else {
				xx <- rev(asRaster(midpoints, col, breaks=breaks))
			}

			rasterImage(xx, 0, minz, 1, maxz, interpolate=FALSE)
			do.call("axis", axis.args)
			box()
		} else {
			plot(NA, NA, ylim=c(0, 1), xlim=c(minz, maxz), type="n", xlab="", ylab="", xaxs ='i', yaxs = 'i', axes=FALSE)
			
			if (is.null(breaks)) {
				xx <- t(rev(asRaster(1:length(col), col) ))
			} else {
				xx <- t(asRaster(midpoints, col, breaks=breaks))
			}
			rasterImage(xx, minz, 0, maxz, 1, interpolate=FALSE)
			do.call("axis", axis.args)
			box()
		}
	
		if (!is.null(legend.lab)) {
			legend.args <- list(text = legend.lab, side = ifelse(horizontal, 1, 4), line = legend.mar - 2)
		}
		if (!is.null(legend.args)) {
			do.call(mtext, legend.args)
		}
	}
	
	mfg.save <- par()$mfg
	if (graphics.reset | add) {
		par(old.par)
		par(mfg = mfg.save, new = FALSE)
	} else {
		par(big.par)
		par(plt = big.par$plt, xpd = FALSE)
		par(mfg = mfg.save, new = FALSE)
	}
	if (!add & box ) box()
	invisible()
	
}

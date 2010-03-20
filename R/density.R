# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: December 2009
# Version 0.1
# Licence GPL v3


if (!isGeneric("density")) {
	setGeneric("density", function(x, ...)
		standardGeneric("density"))
}	

setMethod('density', signature(x='RasterLayer'), 
	function(x, maxpixels=100000, plot=TRUE, main='', ...) {
		d = sampleRegular(x, maxpixels)
		x = density(na.omit(d))
		if (plot) {
			plot(x, main=main, ...)
			return(invisible(x))
		} else {
			return(x)
		}
	}
)
 
setMethod('density', signature(x='RasterStackBrick'), 
	function(x, layer, maxpixels=100000, plot=TRUE, main, ...) {
		
		if (missing(layer)) y = 1:nlayers(x)
		else if (is.character(layer)) {
			y = .nameToIndex(layer, layerNames(x))
		} else { 
			y = layer 
		}
		y <- unique(as.integer(round(y)))
		y = na.omit(y)
		y = subset(y, y >= 1 & y <= nlayers(x))
		nl <- length(y)
		if (nl == 0) {stop('no existing layers selected')}
		
		if (nl > 1)	{
			res=list()
			if (nl > 16) {
				warning('only the first 16 layers are plotted')
				nl <- 16
				y <- y[1:16]
			}
			if (missing(main)) {	main=layerNames(x) }

			nc <- ceiling(sqrt(nl))
			nr <- ceiling(nl / nc)
			
			mfrow = par("mfrow")
			spots = mfrow[1] * mfrow[2]
			if (spots < nl) {
				par(mfrow=c(nr, nc))
			}
			for (i in 1:length(y)) {	
				r <- raster(x, y[i])
				m <- main[y[i]]
				res[[i]] = density(r, maxpixels=maxpixels, main=m, plot=plot, ...)
			}		
		} else if (nl==1) {
			if (missing(main)) main = layerNames(x)[y]
			r <- raster(x, y)
			res = density(r, maxpixels=maxpixels, main=main, plot=plot, ...)
		}
		if (plot) return(invisible(res))
		else return(res)
	}
)


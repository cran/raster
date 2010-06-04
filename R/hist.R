# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

setMethod('hist', signature(x='RasterStackBrick'), 
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
				if (plot) { res[[i]] = hist(r, maxpixels=maxpixels, main=m, ...)
				} else  { res[[i]] = hist(r, maxpixels=maxpixels, plot=FALSE, ...) }
			}		

		} else if (nl==1) {
			if (missing(main)) main = layerNames(x)[y]
			x <- raster(x, y)
			if (plot) { res = hist(x, maxpixels=maxpixels, main=main, ...)
			} else { res = hist(r, maxpixels=maxpixels, plot=FALSE, ...) }
		}
		
		if (plot) return(invisible(res))
		else return(res)
		
	}
)


setMethod('hist', signature(x='RasterLayer'), 
	function(x, layer=1, maxpixels=100000, main=NA,  plot=TRUE, ...){
		if (dataContent(x) == 'all') {
			values <- getValues(x)
		} else if (dataSource(x) == 'disk') {
			
			if (ncell(x) <= maxpixels) {
				values <- na.omit(getValues(x))
			} else {

			# TO DO: make a function that does this by block and combines  all data into a single histogram
				values <- sampleRandom(x, maxpixels)
				msg <- paste(round(100 * maxpixels / ncell(x)), "% of the raster cells were used", sep="")
				if (maxpixels > length(values)) {
					msg <- paste(msg, " (of which ", 100 - round(100 * length(values) / maxpixels ), "% were NA)", sep="")
				}
				warning( paste(msg, ". ",length(values)," values used.", sep="") )
			}	
		} else { 
			stop('cannot make a histogram; need data on disk or in memory')
		}		
		if (.shortDataType(x) == 'LOG') {
			values <- values * 1
		}
		if (plot) { res = hist(values, main=main, ...)  
		} else { res = hist(values, plot=FALSE, ...)  }
		
		if (plot) return(invisible(res))
		else return(res)
	}	
)



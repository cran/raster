# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

setMethod ('print' , 'Raster', 
	function(x, ...) {
		if (x@file@driver == 'netcdf') {
			nc <- open.ncdf(x@file@name)
			print(nc)
			close.ncdf(nc)
		}
		else callNextMethod(x, ...)
	}
)



setMethod ('show' , 'Extent', 
	function(object) {
		cat('class       :' , class(object), '\n')
		cat('xmin        :' , xmin(object), '\n')
		cat('xmax        :' , xmax(object), '\n')
		cat('ymin        :' , ymin(object), '\n')
		cat('ymax        :' , ymax(object), '\n')
	}
)	
	

setMethod ('show' , 'BasicRaster', 
	function(object) {
		cat('class       :' , class(object), '\n')
		cat('nrow        :' , nrow(object), '\n')
		cat('ncol        :' , ncol(object), '\n')
		cat('ncell       :' , ncell(object), '\n')
		cat('projection  :' , projection(object, TRUE), '\n')
		cat('xmin        :' , xmin(object), '\n')
		cat('xmax        :' , xmax(object), '\n')
		cat('ymin        :' , ymin(object), '\n')
		cat('ymax        :' , ymax(object), '\n')
		cat('xres        :' , xres(object), '\n')
		cat('yres        :' , yres(object), '\n')
	}
)	
	
setMethod ('show' , 'RasterLayer', 
	function(object) {
		cat('class       :' , class(object), '\n')
		cat('filename    :' , filename(object), '\n')
		if (nbands(object) > 1) { cat('band        :' , band(object), '\n')	}	
		cat('nrow        :' , nrow(object), '\n')
		cat('ncol        :' , ncol(object), '\n')
		cat('ncell       :' , ncell(object), '\n')
#		cat('data type   :' , object@file@datanotation, '\n')
#		if (dataContent(object) == 'nodata') { cat('vals in mem : none', '\n')
#		} else { cat('vals in mem :', dataContent(object) , '\n') }
						
		if (is.factor(object)) {
			cat('levels      :' , paste(object@data@levels, collapse=', '), '\n')
			cat('labels      :' , paste(object@data@labels, collapse=', '), '\n')
		} else {
			if (object@data@haveminmax) {
				cat('min value   :' , minValue(object), '\n')
				cat('max value   :' , maxValue(object), '\n')
			} else { 
				if (object@data@fromdisk) {
					cat('min value   : ? \n')
					cat('max value   : ? \n')
				} else {
					cat('min value   :  \n')
					cat('max value   :  \n')		
				}
			}
		}
		cat('projection  :' , projection(object, TRUE), '\n')
		cat('xmin        :' , xmin(object), '\n')
		cat('xmax        :' , xmax(object), '\n')
		cat('ymin        :' , ymin(object), '\n')
		cat('ymax        :' , ymax(object), '\n')
		cat('xres        :' , xres(object), '\n')
		cat('yres        :' , yres(object), '\n')
		cat ('\n')
	}
)


setMethod ('show' , 'RasterBrick',
	function ( object ) {
		cat ('class       :' , class ( object ) , '\n')
		cat ('filename    :' , filename(object), '\n')
		nl <- nlayers(object)
		cat ('nlayers     :' , nl, '\n')
		cat ('nrow        :' , nrow(object), '\n')
		cat ('ncol        :' , ncol(object), '\n')
		cat ('ncell       :' , ncell(object), '\n')
		cat ('projection  :' , projection(object, TRUE), '\n')
		if (nl > 0) {
			minv <- format(minValue(object), digits=2)
			maxv <- format(maxValue(object), digits=2)
			minv <- gsub('Inf', '?', minv)
			maxv <- gsub('-Inf', '?', maxv)
			if (nl > 10) {
				minv <- c(minv[1:10], '...')
				maxv <- c(maxv[1:10], '...')
			}
			cat('min value   :', paste(minv, collapse=' '), '\n')
			cat('max value   :', paste(maxv, collapse=' '), '\n')
		}
		cat ('xmin        :' , xmin(object), '\n')
		cat ('xmax        :' , xmax(object), '\n')
		cat ('ymin        :' , ymin(object), '\n')
		cat ('ymax        :' , ymax(object), '\n')
		cat ('xres        :' , xres(object), '\n')
		cat ('yres        :' , yres(object), '\n')
		cat ('\n')
	}
)



setMethod ('show' , 'RasterStack',
	function ( object ) {
		cat ('class       :' , class ( object ) , '\n')
		cat ('filename    :' , filename(object), '\n')
		nl <- nlayers(object)
		cat ('nlayers     :' , nl, '\n')
		if (nl > 0) {
			cat ('nrow        :' , nrow(object@layers[[1]]), '\n')
			cat ('ncol        :' , ncol(object@layers[[1]]), '\n')
			cat ('ncell       :' , ncell(object@layers[[1]]), '\n')
			cat ('projection  :' , projection(object@layers[[1]], TRUE), '\n')
			minv <- format(minValue(object), digits=2)
			maxv <- format(maxValue(object), digits=2)
			minv <- gsub('Inf', '?', minv)
			maxv <- gsub('-Inf', '?', maxv)
			if (nl > 10) {
				minv <- c(minv[1:10], '...')
				maxv <- c(maxv[1:10], '...')
			}
			cat('min value   :', paste(minv, collapse=' '), '\n')
			cat('max value   :', paste(maxv, collapse=' '), '\n')
		}
		cat ('xmin        :' , as.character(xmin(object)), '\n')
		cat ('xmax        :' , as.character(xmax(object)), '\n')
		cat ('ymin        :' , as.character(ymin(object)), '\n')
		cat ('ymax        :' , as.character(ymax(object)), '\n')
		cat ('xres        :' , as.character(xres(object)), '\n')
		cat ('yres        :' , as.character(yres(object)), '\n')
		cat ('\n')
	}
)


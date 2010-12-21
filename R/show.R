# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


setMethod ('print' , 'Raster', 
	function(x, ...) {
		if (inherits(x, 'RasterStack')) {
			show(x)
		} else {
			if (x@file@driver == 'netcdf') {
				nc <- open.ncdf(x@file@name)
				print(nc)
				close.ncdf(nc)
			} else if (is.factor(x)) {
				cat('factor levels (value attributes)\n')
				f <- x@data@attributes[[1]]
				if (nrow(f) > 15) { 
					f <- f[1:15,]
				}
				print(f)
			# cat('levels      :' , paste(object@data@levels, collapse=', '), '\n')
			# cat('labels      :' , paste(object@data@labels, collapse=', '), '\n')
			} else callNextMethod(x, ...)
		}
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
		cat('extent      : ' , object@extent@xmin, ', ', object@extent@xmax, ', ', object@extent@ymin, ', ', object@extent@ymax, '  (xmin, xmax, ymin, ymax)\n', sep="")
		cat('resolution  : ' , xres(object), ', ', yres(object), '  (x, y)\n', sep="")
	}
)	
	
setMethod ('show' , 'RasterLayer', 
	function(object) {
		cat('class       :' , class(object), '\n')
		cat('filename    :' , filename(object), '\n')
		if (nbands(object) > 1) { cat('band        :' , band(object), '\n')	}	
		cat('dimensions  : ', nrow(object), ', ', ncol(object), ', 1  (nrow, ncol, nlayers)\n', sep="" ) 
		cat('ncell       :' , ncell(object), '\n')

		if (object@data@haveminmax) {
			cat('min value   :' , minValue(object), '\n')
			cat('max value   :' , maxValue(object), '\n')
		} else { 
			if (object@data@fromdisk) {
				cat('min value   : ? \n')
				cat('max value   : ? \n')
			} else {
				cat('min value   : NA \n')
				cat('max value   : NA \n')		
			}
		}
		cat('projection  :' , projection(object, TRUE), '\n')
		cat('extent      : ' , object@extent@xmin, ', ', object@extent@xmax, ', ', object@extent@ymin, ', ', object@extent@ymax, '  (xmin, xmax, ymin, ymax)\n', sep="")
		cat('resolution  : ' , xres(object), ', ', yres(object), '  (x, y)\n', sep="")
		cat ('\n')
	}
)


setMethod ('show' , 'RasterBrick',
	function ( object ) {
		cat ('class       :' , class ( object ) , '\n')
		cat ('filename    :' , filename(object), '\n')
		nl <- nlayers(object)
		cat ('dimensions  : ', nrow(object), ', ', ncol(object), ', ', nl, '  (nrow, ncol, nlayers)\n', sep="" ) 
		cat ('ncell       :' , ncell(object), '\n')
		cat ('projection  :' , projection(object, TRUE), '\n')
		if (nl > 0) {
			if (object@data@haveminmax) {
				minv <- format(minValue(object), digits=2)
				maxv <- format(maxValue(object), digits=2)
				minv <- gsub('Inf', '?', minv)
				maxv <- gsub('-Inf', '?', maxv)
				if (nl > 10) {
					minv <- c(minv[1:10], '...')
					maxv <- c(maxv[1:10], '...')
				}
			} else {
				minv <- rep('NA', min(nl, 10))
				maxv <- rep('NA', min(nl, 10))
			}
			cat('min values  :', paste(minv, collapse=' '), '\n')
			cat('max values  :', paste(maxv, collapse=' '), '\n')
		}
		cat('extent      : ' , object@extent@xmin, ', ', object@extent@xmax, ', ', object@extent@ymin, ', ', object@extent@ymax, '  (xmin, xmax, ymin, ymax)\n', sep="")
		cat('resolution  : ' , xres(object), ', ', yres(object), '  (x, y)\n', sep="")
		cat ('\n')
	}
)



setMethod ('show' , 'RasterStack',
	function ( object ) {
		cat ('class       :' , class ( object ) , '\n')
		if (filename(object) != '') {
			cat ('filename    :' , filename(object), '\n')
		}
		nl <- nlayers(object)
		if (nl == 0) {
			cat ('nlayers     :' , nl, '\n')
		} else {
			cat ('dimensions  : ', nrow(object), ', ', ncol(object), ', ', nl, '  (nrow, ncol, nlayers)\n', sep="" ) 
			cat ('ncell       :' , ncell(object), '\n')
			cat ('projection  :' , projection(object, TRUE), '\n')
			minv <- format(minValue(object), digits=2)
			maxv <- format(maxValue(object), digits=2)
			minv <- gsub('Inf', '?', minv)
			maxv <- gsub('-Inf', '?', maxv)
			if (nl > 10) {
				minv <- c(minv[1:10], '...')
				maxv <- c(maxv[1:10], '...')
			}
			cat('min values  :', paste(minv, collapse=' '), '\n')
			cat('max values  :', paste(maxv, collapse=' '), '\n')
		}
		cat('extent      : ' , object@extent@xmin, ', ', object@extent@xmax, ', ', object@extent@ymin, ', ', object@extent@ymax, '  (xmin, xmax, ymin, ymax)\n', sep="")
		cat('resolution  : ' , xres(object), ', ', yres(object), '  (x, y)\n', sep="")
		cat ('\n')
	}
)


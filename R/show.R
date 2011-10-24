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
		cat('dimensions  : ', nrow(object), ', ', ncol(object), ', ', ncell(object),'  (nrow, ncol, ncell)\n', sep="" ) 
		cat('resolution  : ' , xres(object), ', ', yres(object), '  (x, y)\n', sep="")
		cat('extent      : ' , object@extent@xmin, ', ', object@extent@xmax, ', ', object@extent@ymin, ', ', object@extent@ymax, '  (xmin, xmax, ymin, ymax)\n', sep="")
		cat('coord. ref. :' , projection(object, TRUE), '\n')
	}
)	
	
setMethod ('show' , 'RasterLayer', 
	function(object) {
		cat('class       :' , class(object), '\n')
		if (rotated(object)) {
			cat('rotated     : TRUE\n')
		}
		if (nbands(object) > 1) { cat('band        :' , bandnr(object), '\n')	}	
		cat('dimensions  : ', nrow(object), ', ', ncol(object), ', ', ncell(object),'  (nrow, ncol, ncell)\n', sep="" ) 
		cat('resolution  : ' , xres(object), ', ', yres(object), '  (x, y)\n', sep="")
		cat('extent      : ' , object@extent@xmin, ', ', object@extent@xmax, ', ', object@extent@ymin, ', ', object@extent@ymax, '  (xmin, xmax, ymin, ymax)\n', sep="")
		cat('coord. ref. :' , projection(object, TRUE), '\n')

		if (hasValues(object)) {
			fd <- object@data@fromdisk
			if (fd) {
				cat('values      :', filename(object), '\n')
			} else {
				cat('values      : in memory\n')			
			}
			
			if (object@data@haveminmax) {
				cat('min value   :' , minValue(object), '\n')
				cat('max value   :' , maxValue(object), '\n')
			#} else { 
			#	if (fd) {
			#		cat('min         : ? \n')
			#		cat('max         : ? \n')
			#	} 
			}
		} else {
			cat('values      : none\n')			
		}

		z <- getZ(object)
		if (length(z) > 0) {
			name <- object@zname
			if (name == '') name <- 'z-value'
			name <- paste(sprintf("%-12s", name), ':', sep='')
			cat(name, z[1], '\n')
		}

		if (object@file@driver == 'netcdf') {
			z <- attr(x@data, 'zvar')
			if (!is.null(z)) { cat('zvar        :', z, '\n') } 
			z <- attr(x@data, 'level')
			if (!is.null(z)) { cat('level       :', z, '\n') } 
		}
		
		cat ('\n')
	}
)


setMethod ('show' , 'RasterBrick',
	function ( object ) {
		cat ('class       :' , class ( object ) , '\n')
		if (rotated(object)) {
			cat('rotated     : TRUE\n')
		}
		
		nl <- nlayers(object)
		cat ('dimensions  : ', nrow(object), ', ', ncol(object), ', ', ncell(object), ', ', nl, '  (nrow, ncol, ncell, nlayers)\n', sep="" ) 
		#cat ('ncell       :' , ncell(object), '\n')
		cat ('resolution  : ' , xres(object), ', ', yres(object), '  (x, y)\n', sep="")
		cat ('extent      : ' , object@extent@xmin, ', ', object@extent@xmax, ', ', object@extent@ymin, ', ', object@extent@ymax, '  (xmin, xmax, ymin, ymax)\n', sep="")
		cat ('coord. ref. :' , projection(object, TRUE), '\n')
		if (hasValues(object)) {
			fd <- object@data@fromdisk
			if (fd) {
				cat('values      :', filename(object), '\n')
			} else {
				cat('values      : in memory\n')			
			}
			if (object@data@haveminmax) {
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
#			} else {
#				minv <- rep('?', min(nl, 10))
#				maxv <- rep('?', min(nl, 10))
			}
		} else {
			cat('values      : none\n')			
		}
		z <- getZ(object)
		if (length(z) > 0) {
			name <- object@zname
			if (name == '') name <- 'z-value'
			name <- paste(sprintf("%-12s", name), ':', sep='')
			if (length(z) < 10) {
				cat(name, paste(z, collapse=', '), '\n')
			} else {
				z <- summary(z)
				cat(name, paste(z, collapse=', ..., '), '(summary)\n')
			}
		}
		
		if (object@file@driver == 'netcdf') {
			z <- attr(x@data, 'zvar')
			if (!is.null(z)) { cat('zvar        :', z, '\n') } 
			z <- attr(x@data, 'level')
			if (!is.null(z)) { cat('level       :', z, '\n') } 
		}
		
		cat ('\n')
	}
)




setMethod ('show' , 'RasterStack',
	function ( object ) {
		cat ('class       :' , class ( object ) , '\n')
		if (rotated(object)) {
			cat('rotated     : TRUE\n')
		}
		
		if (filename(object) != '') {
			cat ('filename    :' , filename(object), '\n')
		}
		nl <- nlayers(object)
		if (nl == 0) {
			cat ('nlayers     :' , nl, '\n')
		} else {
			cat ('dimensions  : ', nrow(object), ', ', ncol(object), ', ', ncell(object), ', ', nl, '  (nrow, ncol, ncell, nlayers)\n', sep="" ) 
			#cat ('ncell       :' , ncell(object), '\n')
			cat ('resolution  : ' , xres(object), ', ', yres(object), '  (x, y)\n', sep="")
			cat ('extent      : ' , object@extent@xmin, ', ', object@extent@xmax, ', ', object@extent@ymin, ', ', object@extent@ymax, '  (xmin, xmax, ymin, ymax)\n', sep="")
			cat ('coord. ref. :' , projection(object, TRUE), '\n')
			
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
		
		z <- getZ(object)
		if (length(z) > 0) {
			name <- object@zname
			if (name == '') name <- 'z-value'
			name <- paste(sprintf("%-12s", name), ':', sep='')
			if (length(z) < 10) {
				cat(name, paste(z, collapse=', '), '\n')
			} else {
				z <- summary(z)
				cat(name, paste(z, collapse=' ... '), '(summary)\n')
			}
		}
		
		cat ('\n')
	}
)


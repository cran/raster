# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3



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

		cat('layer name  :', layerNames(object), '\n')
		
		z <- getZ(object)
		if (length(z) > 0) {
			name <- object@zname
			if (name == '') name <- 'z-value'
			name <- paste(sprintf("%-12s", name), ':', sep='')
			cat(name, z[1], '\n')
		}

		if (object@file@driver == 'netcdf') {
			z <- attr(object@data, 'zvar')
			if (!is.null(z)) { cat('zvar        :', z, '\n') } 
			z <- attr(object@data, 'level')
			if (!is.null(z)) { 
				if (z>0) { 
					cat('level       :', z, '\n')  
				}
			}
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
		
		mnr <- 15
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
				if (nl > mnr) {
					minv <- c(minv[1:mnr], '...')
					maxv <- c(maxv[1:mnr], '...')
				}
				cat('min values  :', paste(trim(minv), collapse=', '), '\n')
				cat('max values  :', paste(trim(maxv), collapse=', '), '\n')

#			} else {
#				minv <- rep('?', min(nl, 10))
#				maxv <- rep('?', min(nl, 10))
			}
		} else {
			cat('values      : none\n')			
		}
		ln <- layerNames(object)
		if (nl > mnr) {
			ln <- c(ln[1:mnr], '...')
		}
		cat('layer names :', paste(ln, collapse=', '), '\n')

		z <- getZ(object)
		if (length(z) > 0) {
			name <- object@zname
			if (name == '') name <- 'z-value'
			name <- paste(sprintf("%-12s", name), ':', sep='')
			if (length(z) < mnr) {
				cat(name, paste(z, collapse=', '), '\n')
			} else {
				z <- summary(z)
				cat(name, paste(z, collapse=' - '), '(range)\n')
			}
		}
		
		if (object@file@driver == 'netcdf') {
			z <- attr(object@data, 'zvar')
			if (!is.null(z)) { cat('zvar        :', z, '\n') } 
			z <- attr(object@data, 'level')
			if (!is.null(z)) { 
				if (z>0) { 
					cat('level       :', z, '\n')  
				}
			}
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
		
		mnr <- 15		
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
			if (nl > mnr) {
				minv <- c(minv[1:mnr], '...')
				maxv <- c(maxv[1:mnr], '...')
			}
			cat('min values  :', paste(trim(minv), collapse=', '), '\n')
			cat('max values  :', paste(trim(maxv), collapse=', '), '\n')
			
		}
		ln <- layerNames(object)
		if (nl > mnr) {
			ln <- c(ln[1:mnr], '...')
		}
		cat('layer names :', paste(ln, collapse=', '), '\n')
		
		
		z <- getZ(object)
		if (length(z) > 0) {
			name <- object@zname
			if (name == '') name <- 'z-value'
			name <- paste(sprintf("%-12s", name), ':', sep='')
			if (length(z) < mnr) {
				cat(name, paste(z, collapse=', '), '\n')
			} else {
				z <- range(z)
				cat(name, paste(z, collapse=' - '), '(range)\n')
			}
		}
		
		cat ('\n')
	}
)


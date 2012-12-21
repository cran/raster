# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  April 2012
# Version 1.0
# Licence GPL v3



setMethod ('print', 'Raster', 
	function(x, ...) {
		if (inherits(x, 'RasterStack')) {
			show(x)
		} else {
			if (x@file@driver == 'netcdf') {
				if (getOption('rasterNCDF4')) {
					nc <- ncdf4::nc_open(x@file@name)
					print(nc)
					ncdf4::nc_close(nc)
				} else {
					nc <- open.ncdf(x@file@name)
					print(nc)
					close.ncdf(nc)
				}
			} else if (any(is.factor(x))) {
				cat('factor levels (value attributes)\n')
				f <- x@data@attributes
				for (i in 1:length(f)) {
					ff <- f[[i]]
					if (!is.null(ff)) {
						if (nrow(ff) > 15) { 
							ff <- ff[1:15,]
						}
						print(ff)
					}
				}
			# cat('levels      :' , paste(object@data@levels, collapse=', '), '\n')
			# cat('labels      :' , paste(object@data@labels, collapse=', '), '\n')
			} else callNextMethod(x, ...)
		}
	}
)



setMethod ('show' , 'Spatial', 
	function(object) {
		print (object)
	}
)


setMethod ('print' , 'Spatial', 
	function(x, ...) {
	
		cat('class       :' , class(x), '\n')
		isRaster <- hasData <- FALSE
		nc <- 0
		if (.hasSlot(x, 'data')) {
			hasData <- TRUE
			nc <- ncol(x@data)
		}
		
		if (inherits(x, 'SpatialPixels')) {
			isRaster <- TRUE
			cr <- x@grid@cells.dim
			cat ('dimensions  : ', cr[2], ', ', cr[1], ', ', nrow(x@coords), ', ', nc, '  (nrow, ncol, npixels, nlayers)\n', sep="" ) 
			cs <- x@grid@cellsize
			cat ('resolution  : ', cs[1], ', ', cs[2], '  (x, y)\n', sep="")		

		} else if (inherits(x, 'SpatialGrid')) {
			isRaster <- TRUE
			cr <- x@grid@cells.dim
			cat ('dimensions  : ', cr[2], ', ', cr[1], ', ', prod(cr), ', ', nc, '  (nrow, ncol, ncell, nlayers)\n', sep="" ) 
			cs <- x@grid@cellsize
			cat ('resolution  : ', cs[1], ', ', cs[2], '  (x, y)\n', sep="")		
			
		} else {		
			cat('nfeatures   :' , length(x), '\n')
		}
		
		e <- bbox(x)
		cat('extent      : ' , e[1,1], ', ', e[1,2], ', ', e[2,1], ', ', e[2,2], '  (xmin, xmax, ymin, ymax)\n', sep="")
		cat('coord. ref. :' , projection(x, TRUE), '\n')
		
		
		if (hasData) {
			x <- x@data
			
			maxnl <- 15
			
			if (! isRaster) {
				cat('nvariables  : ', nc, '\n', sep="" ) 
			}
			if (nc > maxnl) {
				x <- x[, 1:maxnl]
			}
			ln <- colnames(x)
			if (nc > maxnl) {
				ln <- c(ln[1:maxnl], '...')
				x <- x[, 1:maxnl]
			}
			wrn <- getOption('warn')
			on.exit(options('warn' = wrn))
			options('warn'=-1) 
			r <- apply(x, 2, range, na.rm=TRUE)
			minv <- as.vector(r[1, ])
			maxv <- as.vector(r[2, ])
			if (nc > maxnl) {
				minv <- c(minv, '...')
				maxv <- c(maxv, '...')
			}

			w <- pmax(nchar(ln), nchar(minv), nchar(maxv))
			m <- rbind(ln, minv, maxv)
				# a loop because 'width' is not recycled by format
			for (i in 1:ncol(m)) {
				m[,i]   <- format(m[,i], width=w[i], justify="right")
			}

			cat('names       :', paste(m[1,], collapse=', '), '\n')
			cat('min values  :', paste(m[2,], collapse=', '), '\n')
			cat('max values  :', paste(m[3,], collapse=', '), '\n')
			
		}
	}
)	
	

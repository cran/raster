# R function for the raster package
# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.9
# Licence GPL v3

'extent<-' <- function(x, value) {
	return(setExtent(x, value))
}


setExtent <- function(x, ext, keepres=FALSE, snap=FALSE) {
	
	oldbb <- extent(x)
	bb <- extent(ext)
	newobj <- clearValues(x)
	if (snap) {
		bb <- alignExtent(bb, newobj)
	}
	newobj@extent <- bb

	if (inherits(x, 'RasterStack')) {
		if (keepres) {
			stop('you cannot use keepres=TRUE with a RasterStack')
		}
		x@extent <- bb
		if (nlayers(x) > 0) {
			for (i in 1:nlayers(x)) {
				x@layers[[i]]@extent <- bb
			}
		} 
		return(x)
	}

	
	if (keepres) {
		xrs <- xres(x)
		yrs <- yres(x)
		nc <- as.integer(round( (xmax(newobj) - xmin(newobj)) / xrs ))
		if (nc < 1) { stop( "xmin and xmax are less than one cell apart" ) 
		} else { newobj@ncols <- nc }
		nr <- as.integer(round( (ymax(newobj) - ymin(newobj)) / yrs ) )
		if (nr < 1) { stop( "ymin and ymax are less than one cell apart" )
		} else { newobj@nrows <- nr }
		newobj@extent@xmax <- newobj@extent@xmin + ncol(newobj) * xrs
		newobj@extent@ymax <- newobj@extent@ymin + nrow(newobj) * yrs
		
		if ( inMemory(x) ) {
			if (ncol(x) == ncol(newobj) & nrow(x) == nrow(newobj)) {
				newobj <- setValues(newobj, x@data@values)
			} else {
				newobj@data@fromdisk <- FALSE
				z <- cellsFromExtent(x, bb, expand=TRUE)
				if (inherits(x, 'RasterBrick')) {
					z[!is.na(z), ] <- getValues(x)[!is.na(z), ]
					newobj <- setValues(newobj, z)
				} else {
					z[!is.na(z)] <- getValues(x)[!is.na(z)]
					newobj <- setValues(newobj, z)
				}
			}
		}
		
	} else if (class(x) != "BasicRaster") {
		if (ncol(x)==ncol(newobj) & nrow(x)==nrow(newobj))  {
			if ( inMemory(x) ) {
				newobj <- setValues(newobj, x@data@values)
			}	
		}
	}
	return(newobj)
}


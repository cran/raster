# R function for the raster package
# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.9
# Licence GPL v3

'extent<-' <- function(x, value) {
	return(setExtent(x, value))
}


setExtent <- function(x, bndbox, keepres=FALSE, snap=FALSE) {
	if (class(x) == 'RasterStack' ) {
		stop('you can not change the extent of a RasterStack that has one or more layers')
	}

	oldbb <- extent(x)
	bb <- extent(bndbox)
	newobj <- clearValues(x)
	
	if (snap) {
		bb <- alignExtent(bb, newobj)
	}

	newobj@extent <- bb
	
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

				indices <- cellsFromExtent(x, bb, expand=TRUE)
				v <- vector(length=length(indices))
				v[] <- NA
				v[!is.na(indices)] <- x@data@values[!is.na(indices)]
				newobj <- setValues(newobj, v)
			}
		}
		
	} else if (class(x) != "BasicRaster" & class(x) != "RasterStack") {
		if (ncol(x)==ncol(newobj) & nrow(x)==nrow(newobj))  {
			if ( inMemory(x) ) {
				newobj <- setValues(newobj, x@data@values)
			}	
		}
	}
	return(newobj)
}


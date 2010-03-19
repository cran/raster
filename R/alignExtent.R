# R function for the raster package
# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.9
# Licence GPL v3


alignExtent <- function(extent, object) {
	object <- raster(object)
	oldext <- extent(object)
	e <- extent(extent)
	e@xmin <- max(e@xmin, oldext@xmin)
	e@xmax <- min(e@xmax, oldext@xmax)
	e@ymin <- max(e@ymin, oldext@ymin)
	e@ymax <- min(e@ymax, oldext@ymax)
	col <- colFromX(object, e@xmin)
	mn <- xFromCol(object, col) - 0.5 * xres(object)
	mx <- xFromCol(object, col) + 0.5 * xres(object)
	if (abs(e@xmin - mn) > abs(e@xmin - mx)) { 
		e@xmin <- mx 
	} else { 
		e@xmin <- mn 
	}
	col <- colFromX(object, e@xmax)
	mn <- xFromCol(object, col) - 0.5 * xres(object)
	mx <- xFromCol(object, col) + 0.5 * xres(object)
	if (abs(e@xmax - mn) > abs(e@xmax - mx)) { 
		e@xmax <- mx 
	} else { 
		e@xmax <- mn 
	}
	row <- rowFromY(object, e@ymin)
	mn <- yFromRow(object, row) - 0.5 * yres(object)
	mx <- yFromRow(object, row) + 0.5 * yres(object)
	if (abs(e@ymin - mn) > abs(e@ymin - mx)) {
		e@ymin <- mx
	} else { 
		e@ymin <- mn 
	}
	row <- rowFromY(object, e@ymax)
	mn <- yFromRow(object, row) - 0.5 * yres(object)
	mx <- yFromRow(object, row) + 0.5 * yres(object)
	if (abs(e@ymax - mn) > abs(e@ymax - mx)) { 
		e@ymax <- mx 
	} else {
		e@ymax <- mn 
	}
	
	if ( e@ymin == e@ymax ) {
		if (oldext@ymin > e@ymin) {
			e@ymax = e@ymax + yres(object)
		} else {
			e@ymin = e@ymin - yres(object)		
		}
	}
	if ( e@xmin == e@xmax ) {
		if (oldext@xmin > e@xmin) {
			e@xmax = e@xmax + xres(object)
		} else {
			e@xmin = e@xmin - xres(object)		
		}
	}
	
	
	
	return(e)
}



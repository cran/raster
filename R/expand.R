# Author: Robert J. Hijmans
# Date : October 2008
# Licence GPL v3
# revised November 2011
# version 1.0


if (!isGeneric("expand")) {
	setGeneric("expand", function(x, y, ...)
		standardGeneric("expand"))
}	


setMethod('expand', signature(x='Raster', y='ANY'), 
function(x, y, filename='', value=NA, datatype, ...) {

	if (is.vector(y)) {
		if (length(y) <= 2) {
			adj <- abs(y) * rev(res(x))
			y <- extent(x)
			y@ymin <- y@ymin - adj[1]
			y@ymax <- y@ymax + adj[1]
			y@xmin <- y@xmin - adj[2]
			y@xmax <- y@xmax + adj[2]
		}
	}
	
	test <- try ( y <- extent(y), silent=TRUE )
	if (class(test) == "try-error") {
		stop('Cannot get an Extent object from argument y')
	}

	filename <- trim(filename)
	
	y  <- alignExtent(y, x)
# only expanding here, not cropping
	y <- unionExtent(y, extent(x))

	
	if (nlayers(x) <= 1) {
		out <- raster(x)
		leg <- x@legend
	} else {
		out <- brick(x, values=FALSE)	
		leg <- new('.RasterLegend')
	}
	out@layernames <- layerNames(x)
	out <- setExtent(out, y, keepres=TRUE)
	
	if (nrow(x) == nrow(out) & ncol(x) == ncol(out)) {
		# nothing to do.
		return(x)
	}

	if (! hasValues(x) ) {
		return(out)
	}
	
	if (missing(datatype)) { 
		datatype <- unique(dataType(x))
		if (length(datatype) > 1) {
			datatype <- .commonDataType(datatype)
		}
	}

	
	if (canProcessInMemory(out)) {
	
		d <- matrix(value, nrow=ncell(out), ncol=nlayers(x))
		d[cellsFromExtent(out, extent(x)), ] <- getValues(x)
		x <- setValues(out, d)	
		if (filename != '') {
			x <- writeRaster(x, filename=filename, datatype=datatype, ...)
		}
		return(x)
		
	} else { 
	
		startrow <- rowFromY(out, yFromRow(x, 1))
		endrow <- rowFromY(out, yFromRow(x, nrow(x)))
		startcol <- colFromX(out, xFromCol(x, 1))
		endcol <- colFromX(out, xFromCol(x, ncol(x)))
		
		tr <- blockSize(out)
		tr$row <- sort(unique(c(tr$row, startrow, endrow)))
		tr$nrows <- c(tr$row[-1], nrow(out)+1) - tr$row
		tr$n <- length(tr$row)
			
		pb <- pbCreate(tr$n, ...)
		out <- writeStart(out, filename=filename, datatype=datatype, ... )
		for (i in 1:tr$n) {
			d <- matrix(value, nrow=tr$nrows[i] * ncol(out), ncol=nlayers(out))
			if (tr$row[i] <= endrow & (tr$row[i]+tr$nrows[i]-1) >= startrow) {
				cells <- startcol:endcol + rep((0:(tr$nrows[i]-1)) * ncol(out), each=endcol-startcol+1)
				d[cells, ] <- getValues(x, (tr$row[i]-startrow+1), tr$nrows[i])
			}
			out <- writeValues(out, d, tr$row[i])
			pbStep(pb, i) 			
		}
		
		pbClose(pb)
		return(  writeStop(out) )
	} 
}
)



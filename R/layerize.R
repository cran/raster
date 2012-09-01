# Author: Robert J. Hijmans
# Date : August 2012
# Version 1.0
# Licence GPL v3


if (!isGeneric("layerize")) {
	setGeneric("layerize", function(x, y, ...)
		standardGeneric("layerize"))
}


setMethod('layerize', signature(x='RasterLayer', y='missing'), 
	function(x, classes=NULL, digits=0, falseNA=FALSE, filename='', ...) {
		
		if (is.null(classes)) {
			classes <- round( sort(unique(x)), digits )
		}
				
		if (falseNA) {
			lyrs <- calc(x, function(x) {
					v <- round(x, digits) == classes
					v[v==0] <- NA
					v
				}
				, forceapply=TRUE, filename=filename, ...)
		} else {
			lyrs <- calc(x, function(x) round(x, digits) == classes, forceapply=TRUE, filename=filename, ...)
		}
		names(lyrs) <- as.character(classes)
		return(lyrs)
	}
)


setMethod('layerize', signature(x='RasterLayer', y='RasterLayer'), 
function(x, y, classes=NULL, digits=0, filename='', ...) { 

	resx <- res(x)
	resy <- res(y)
	if (! all( resy > resx) ) {
		stop("x and y resolution of object y should be (much) larger than that of object x")
	}
	
	int <- intersect(extent(x), extent(y))
	if (is.null(int)) {
		return(raster(y))
	}

		
	if (canProcessInMemory(x, 25)) {
		b <- crop(x, int)
		xy <- xyFromCell(b, 1:ncell(b))
		mc <- cellFromXY(y, xy)
		v <- table(mc, round(getValues(b), digits))
		cells <- as.integer(rownames(v))
		m <- match(cells, 1:ncell(y))
		cn <- as.integer(colnames(v))
		res <- matrix(NA, nrow=ncell(y), ncol=length(cn))
		for (i in 1:length(cn)) {
			 res[m,i] <- v[,i]
		}
		
		y <- brick(y, nl=length(cn))
		names(y) <- paste('count_', as.character(cn), sep='')
		y <- setValues(y, res)
		
		if (filename != '') {
			y <- writeRaster(y, filename, ...)
		}
		return(y)
	} 
	#  else 

	if (is.null(classes)) {
		classes <- round( sort(unique(x)), digits )
	}	
	
	out  <- brick(y, values=FALSE, nl=length(classes))
	names(out) <- paste('count_', as.character(classes), sep='')
	out <- writeStart(out, filename=filename, ...)
	
	tr <- blockSize(out)
	pb <- pbCreate(tr$n, ...)
	for(i in 1:tr$n) {		
		e <- extent(xmin(y), xmax(y), yFromRow(y, tr$row[i]+tr$nrows[i]-1)  - 0.5 * yres(y), yFromRow(y, tr$row[i])+0.5 * yres(y))
		int <- intersect(e, extent(x)) 

		res <- matrix(NA, nrow=tr$nrows[i] * ncol(y), ncol=length(classes))
		if (!is.null(int)) {
			b <- crop(x, int)
			xy <- xyFromCell(b, 1:ncell(b))
			mc <- cellFromXY(y, xy)
			v <- table(mc, round(getValues(b), digits=digits))
			cells <- as.integer(rownames(v))
			modcells <- cellFromRowCol(y, tr$row[i], 1) : cellFromRowCol(y, tr$row[i]+ tr$nrows[i]-1, ncol(y))
			m <- match(cells, modcells)
			cn <- as.integer(colnames(v))
			mm <- match(cn, classes)
			for (j in 1:length(cn)) {
				res[, mm[j]] <- v[, j]
			}
		}	
		out <- writeValues(out, res, tr$row[i])
	}		
	out <- writeStop(out)
	pbClose(pb)
	out	
}
)




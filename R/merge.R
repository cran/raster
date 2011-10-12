# Authors: Robert J. Hijmans 
# contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3


setMethod('merge', signature(x='Raster', y='Raster'), 
function(x, y, ..., tolerance=0.05, filename="", format, overwrite, progress) { 
	
	x <- c(x, y, list(...))
	if (missing(format)) { format <- .filetype(format=format, filename=filename) } 
	if (missing(overwrite)) { overwrite <- .overwrite()	}
	if (missing(progress)) { progress <- .progress() }
	
	merge(x, tolerance=tolerance, filename=filename, format=format, overwrite=overwrite, progress=progress, test=FALSE)
	
} )



setMethod('merge', signature(x='list', y='missing'), 
function(x, y,..., tolerance=0.05, filename="", format, overwrite, progress, test=TRUE){ 

	s <- sapply(x, function(x) inherits(x, 'Raster'))
	x <- x[s]

	if (length(x) < 2) {
		stop('merge needs at least 2 Raster* objects')
	}
	
	nl <- unique(sapply(x, nlayers))
	if (length(nl) != 1) {
		if (length(nl) == 2 & min(nl) == 1) {
			nl <- max(nl)
		} else {
			stop( 'different number of layers' )
		}
	}
	compare(x, extent=FALSE, rowcol=FALSE, orig=TRUE, res=TRUE, tolerance=tolerance)
	
	filename <- trim(filename)
	if (missing(format)) { format <- .filetype(format=format, filename=filename) } 
	if (missing(overwrite)) { overwrite <- .overwrite()	}
	if (missing(progress)) { progress <- .progress() }


	bb <- unionExtent(x)
	
	if (nl > 1) {
		outRaster <- brick(x[[1]], values=FALSE)
	} else {
		outRaster <- raster(x[[1]])
	}
	outRaster <- setExtent(outRaster, bb, keepres=TRUE, snap=FALSE)

	datatype <- 'INT4S'
	for (i in 1:length(x)) {
		dtype <- .shortDataType(dataType(x[[i]]))
		if (any(dtype == 'FLT')) {
			datatype <- 'FLT4S'
		}	
	}

	if ( canProcessInMemory(outRaster, 3) ) {
		if (nl > 1) {
			v = matrix(NA, nrow=ncell(outRaster), ncol=nl)
			for (i in 1:length(x)) {
				cells <- cellsFromExtent( outRaster, extent(x[[i]]) )
				vv <- v[cells, ]
				na <- as.logical( apply(vv, 1, FUN=function(x) sum(is.na(x))==nl) )
				dat <- getValues(x[[i]])
				if (!is.matrix(dat)) {
					dat <- matrix(dat, ncol=1)
				}
				vv[na, ] <- dat[na, ]
				v[cells, ] <- vv
			}
		} else {
			v = rep(NA, ncell(outRaster))
			for (i in 1:length(x)) {
				cells = cellsFromExtent( outRaster, extent(x[[i]]) )
				vv = v[cells]
				vv[is.na(vv)] = getValues(x[[i]])[is.na(vv)]
				v[cells] = vv
			}
		}
		rm(vv)
		outRaster <- setValues(outRaster, v)
		if (filename != '') {
			outRaster <- writeRaster(outRaster, filename=filename, format=format, datatype=datatype, overwrite=overwrite)
		}
		return(outRaster)
	}
	
	
	rowcol <- matrix(0, ncol=3, nrow=length(x))
	for (i in 1:length(x)) {
		xy1 <- xyFromCell(x[[i]], 1) # first row/col on old raster[[i]]
		xy2 <- xyFromCell(x[[i]], ncell(x[[i]]) ) #last row/col on old raster[[i]]
		rowcol[i,1] <- rowFromY(outRaster, xy1[2]) #start row on new raster
		rowcol[i,2] <- rowFromY(outRaster, xy2[2]) #end row
		rowcol[i,3] <- colFromX(outRaster, xy1[1]) #start col
	}

	if (filename == "") {
		filename <- rasterTmpFile()
	} 

	outRaster <- writeStart(outRaster, filename=filename, format=format, datatype=datatype, overwrite=overwrite)
	pb <- pbCreate(nrow(outRaster), type=progress)
	

	if (nl == 1) {
		rd <- rep(NA, outRaster@ncols) 
		for (r in 1:nrow(outRaster)) {
			rd[] <- NA
			for (i in length(x):1) {  #reverse order so that the first raster covers the second etc.
				if (r >= rowcol[i,1] & r <= rowcol[i,2]) { 
					d <- getValues(x[[i]], r + 1 - rowcol[i,1]) 
					id2 <- seq(1:ncol(x[[i]])) + rowcol[i,3] - 1
					d <- cbind(id2, d)
					d <- na.omit(d)
					rd[d[,1]] <- d[,2]
				}		
			}
			outRaster <- writeValues(outRaster, rd, r)
			pbStep(pb, r)
		}
		pbClose(pb)
		outRaster <- writeStop(outRaster)
	} else {
		rd <- matrix(nrow=ncol(outRaster), ncol=nl) 
		for (r in 1:nrow(outRaster)) {
			rd[] <- NA
			for (i in length(x):1) {  #reverse order so that the first raster covers the second etc.
				if (r >= rowcol[i,1] & r <= rowcol[i,2]) { 
					d <- getValues(x[[i]], r + 1 - rowcol[i,1]) 
					id2 <- seq(1:ncol(x[[i]])) + rowcol[i,3] - 1
					d <- cbind(id2, d)
					d <- na.omit(d)
					rd[d[,1], ] <- d[ , -1]
				}		
			}
			outRaster <- writeValues(outRaster, rd, r)
			pbStep(pb, r)
		}
		pbClose(pb)
		outRaster <- writeStop(outRaster)
	}
	layerNames(outRaster) <- paste(layerNames(x[[1]]), '(merged)')
	return(outRaster)
}
)



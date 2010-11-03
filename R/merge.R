# Authors: Robert J. Hijmans 
# contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3




setMethod('merge', signature(x='list', y='missing'), 
function(x, y, ..., tolerance=0.05, filename="", format, overwrite, progress) { 
	
	filename <- trim(filename)
	if (missing(format)) { format <- .filetype(format=format, filename=filename) } 
	if (missing(overwrite)) {	overwrite <- .overwrite() }
	if (missing(progress)) { progress <- .progress() }
	
	
	if (! all(sapply(x, function(x) inherits(x, 'Raster')))) {
		stop('elements of "x" should all inherit from RasterLayer')
	}
	
	do.call(merge, x, tolerance=tolerance, filename=filename, format=format, overwrite=overwrite, progress=progress)
} )



setMethod('merge', signature(x='Raster', y='Raster'), 
function(x,y,..., tolerance=0.05, filename="", format, overwrite, progress){ 

	filename <- trim(filename)
	if (missing(format)) { format <- .filetype(format=format, filename=filename) } 
	if (missing(overwrite)) { overwrite <- .overwrite()	}
	if (missing(progress)) { progress <- .progress() }

	nl <- nlayers(x)
	if (nlayers(x) != nlayers(y)) {
		# should be OK of one of the two has a single layer
		stop( 'nlayers(x) != nlayers(y)' )
	}

	rasters <- c(x, y)
	dots <- list(...)
	dots <- unlist(dots) # make simple list
	if (length(dots) > 0) {
		for (i in 1:length(dots)) {
			if ( inherits( dots[[i]], 'Raster' ) ) {
				if ( nl != nlayers(dots[[i]]) ) {
					# should be OK of one of the two has a single layer
					stop( 'nlayers of layers not equal' )
				}
				rasters <- c(rasters, dots[[i]])
			}
		}
	}

	compare(rasters, extent=FALSE, rowcol=FALSE, orig=TRUE, res=TRUE, tolerance=tolerance)
	bb <- unionExtent(rasters)
	
	if (nlayers(x) > 1) {
		outRaster <- brick(rasters[[1]], values=FALSE)
	} else {
		outRaster <- raster(rasters[[1]])
	}
	outRaster <- setExtent(outRaster, bb, keepres=TRUE, snap=FALSE)

	datatype <- 'INT4S'
	for (i in 1:length(rasters)) {
		dtype <- .shortDataType(dataType(rasters[[i]]))
		if (any(dtype == 'FLT')) {
			datatype <- 'FLT4S'
		}	
	}

	if ( canProcessInMemory(outRaster, 3) ) {
		if (nl > 1) {
			v = matrix(NA, nrow=ncell(outRaster), ncol=nlayers(x))
			for (i in 1:length(rasters)) {
				cells <- cellsFromExtent( outRaster, extent(rasters[[i]]) )
				vv <- v[cells, ]
				na = as.logical( apply(vv, 1, FUN=function(x) sum(is.na(x))==nl) )
				vv[na, ] <- getValues(rasters[[i]])[na, ]
				v[cells, ] <- vv
			}
		} else {
			v = rep(NA, ncell(outRaster))
			for (i in 1:length(rasters)) {
				cells = cellsFromExtent( outRaster, extent(rasters[[i]]) )
				vv = v[cells]
				vv[is.na(vv)] = getValues(rasters[[i]])[is.na(vv)]
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
	
	
	rowcol <- matrix(0, ncol=3, nrow=length(rasters))
	for (i in 1:length(rasters)) {
		xy1 <- xyFromCell(rasters[[i]], 1) # first row/col on old raster[[i]]
		xy2 <- xyFromCell(rasters[[i]], ncell(rasters[[i]]) ) #last row/col on old raster[[i]]
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
			for (i in length(rasters):1) {  #reverse order so that the first raster covers the second etc.
				if (r >= rowcol[i,1] & r <= rowcol[i,2]) { 
					d <- getValues(rasters[[i]], r + 1 - rowcol[i,1]) 
					id2 <- seq(1:ncol(rasters[[i]])) + rowcol[i,3] - 1
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
			for (i in length(rasters):1) {  #reverse order so that the first raster covers the second etc.
				if (r >= rowcol[i,1] & r <= rowcol[i,2]) { 
					d <- getValues(rasters[[i]], r + 1 - rowcol[i,1]) 
					id2 <- seq(1:ncol(rasters[[i]])) + rowcol[i,3] - 1
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
	layerNames(outRaster) <- paste(layerNames(x), '(merged)')
	return(outRaster)
}
)



# Authors: Robert J. Hijmans 
# contact: r.hijmans@gmail.com
# Date : March 2010
# Version 1.0
# Licence GPL v3

	
if (!isGeneric("mosaic")) {
	setGeneric("mosaic", function(x, y, ...)
		standardGeneric("mosaic"))
}	


setMethod('mosaic', signature(x='list', y='missing'), 
function(x, y, ..., fun, na.rm=TRUE, tolerance=0.05, filename="", format, overwrite, progress) { 
	
	if (missing(fun)) {	stop('you need to supply a function with a fun=   argument') } 
	if (missing(format)) {	format <- .filetype() } 
	if (missing(overwrite)) {	overwrite <- .overwrite() }
	if (missing(progress)) { progress <- .progress() }
	
	if (! all(sapply(x, function(x) inherits(x, 'RasterLayer')))) {
		stop('elements of "x" should all inherit from RasterLayer')
	}

	do.call(mosaic, x, tolerance=tolerance, filename=filename, format=format, overwrite=overwrite, progress=progress)
	
} )


setMethod('mosaic', signature(x='RasterLayer', y='RasterLayer'), 
function(x, y, ..., fun, na.rm=TRUE, tolerance=0.05, filename="", format, overwrite, progress) { 
	
	if (missing(fun)) {	stop('you need to supply a function with a fun=   argument') } 

	filename <- trim(filename)
	if (missing(format)) { format <- .filetype(format=format, filename=filename) } 
	if (missing(overwrite)) {	overwrite <- .overwrite() }
	if (missing(progress)) { progress <- .progress() }

	dots <- list(...)
	dots <- unlist(dots) #  make simple list
	rasters <- c(x, y)
	if (length(dots) > 0) {
		for (i in 1:length(dots)) {
			if (class(dots[[i]]) == 'RasterLayer') {
				rasters <- c(rasters, dots[[i]])
			}
		}
	}

	compare(rasters, extent=FALSE, rowcol=FALSE, orig=TRUE, res=TRUE, tolerance=tolerance)
	e <- unionExtent(rasters)
	outraster <- raster(rasters[[1]])
	outraster <- setExtent(outraster, e, keepres=TRUE, snap=FALSE)

	hasvalues = sapply(rasters, fromDisk) | sapply(rasters, inMemory)
	if (! all(hasvalues)) {
		rasters = rasters[hasvalues]
		if (length(rasters) == 0 ) {
			return(outraster)
		}
	}

	
	isInt <- TRUE
	for (i in 1:length(rasters)) {
		dtype <- .shortDataType(rasters[[i]]@file@datanotation)
		if (dtype != 'INT') {
			isInt <- FALSE
		}
	}
	if (isInt) { datatype <- 'INT4S'
	} else { datatype <- 'FLT4S'
	}

	rowcol <- matrix(0, ncol=3, nrow=length(rasters))
	for (i in 1:length(rasters)) {
		xy1 <- xyFromCell(rasters[[i]], 1) # first row/col on old raster[[i]]
		xy2 <- xyFromCell(rasters[[i]], ncell(rasters[[i]]) ) #last row/col on old raster[[i]]
		rowcol[i,1] <- rowFromY(outraster, xy1[2]) #start row on new raster
		rowcol[i,2] <- rowFromY(outraster, xy2[2]) #end row
		rowcol[i,3] <- colFromX(outraster, xy1[1]) #start col
	}

	todisk = FALSE	
	if (! canProcessInMemory(outraster) ) {
		todisk = TRUE
		if (filename == "") {
			filename <- rasterTmpFile()
		} 
	}
	if (filename != '') {
		if (file.exists(filename) & ! overwrite) {
			stop('File exists, use overwrite = TRUE if you want to overwrite it')
		}
	}


	if (todisk) {
		outraster <- writeStart(outraster, filename=filename, format=format, datatype=datatype, overwrite=overwrite)
	} else {
		v = matrix(ncol=nrow(outraster), nrow=ncol(outraster))
	}
	
	pb <- pbCreate(nrow(outraster), type=progress)
	
	rd <- matrix(nrow=ncol(outraster), ncol=length(rasters)) 
	ids = rd
	ids[] = FALSE
	for (i in 1:length(rasters)) {
		rr <- seq(1:ncol(rasters[[i]])) + rowcol[i,3] - 1
		ids[rr, i] <- TRUE
	}
	emptyrow <- rep(NA, ncol(outraster))
	
	fun <- .makeTextFun(fun)
	if (class(fun) == 'character') { 
		rowcalc <- TRUE 
		fun <- .getRowFun(fun)
	} else { rowcalc <- FALSE }
	
	
	w <- getOption('warn')
	on.exit( options('warn'= w) )
	
	if (rowcalc) {
		for (r in 1:nrow(outraster)) {
			rdd <- rd
			for (i in 1:length(rasters)) { 
				if (r >= rowcol[i,1] & r <= rowcol[i,2]) { 
					rdd[ids[,i], i] <- getValues(rasters[[i]], r+1-rowcol[i,1])
				}	
			}
		
			options('warn'=-1) 
			res <- fun(rdd, na.rm=na.rm ) 
			options('warn'=w) 
		
			if (todisk) {
				outraster <- writeValues(outraster, res, r)
			} else {
				v[,r] = res
			}
			pbStep(pb, r)
		}
		pbClose(pb)
	
	} else {
		for (r in 1:nrow(outraster)) {
			rdd <- rd
			for (i in 1:length(rasters)) { 
				if (r >= rowcol[i,1] & r <= rowcol[i,2]) { 
					rdd[ids[,i], i] <- getValues(rasters[[i]], r+1-rowcol[i,1])
				}	
			}
			options('warn'=-1) 
			res <- apply(rdd, 1, FUN=fun, na.rm=na.rm)
			options('warn'=w) 
		
			if (todisk) {
				outraster <- writeValues(outraster, res, r)
			} else {
				v[,r] = res
			}
			pbStep(pb, r)
		}
		pbClose(pb)
	}
	
	if (todisk) {
		outraster <- writeStop(outraster)
	} else {
		outraster <- setValues(outraster, as.vector(v))
		if (filename != '') {
			outraster <- writeRaster(outraster, filename, format=format, overwrite=overwrite)
		}
	}
	return(outraster)
}
)



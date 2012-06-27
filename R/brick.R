# Author: Robert J. Hijmans
# Date :  September 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric("brick")) {
	setGeneric("brick", function(x, ...)
		standardGeneric("brick"))
}	



setMethod('brick', signature(x='missing'), 
	function(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90, nl=1, crs) {
		e <- extent(xmn, xmx, ymn, ymx)
		if (missing(crs)) {
			if (e@xmin > -400 & e@xmax < 400 & e@ymin > -90.1 & e@ymax < 90.1) { 
				crs ="+proj=longlat +datum=WGS84"
			} else {
				crs=NA
			}
		}
		b <- brick(e, nrows=nrows, ncols=ncols, crs=crs, nl=nl)
		return(b)
	}
)



setMethod('brick', signature(x='character'), 
	function(x, values=FALSE, crs=NULL, ...) {
		b <- .rasterObjectFromFile(x, objecttype='RasterBrick', ...)
		if (values) {
			b <- readAll(b)
		}
		if (!is.null(crs)) {
			projection(b) <- crs
		}
		return(b)
	}
)


setMethod('brick', signature(x='RasterLayer'), 
	function(x, ..., values=TRUE, filename='', format,  datatype, overwrite, progress, nl=1) {
		nl <- max(round(nl), 0)
		if (nl != 1 | !hasValues(x)) {
			values <- FALSE
		}
		if (!values) {
			b <- brick(x@extent, nrows=nrow(x), ncols=ncol(x), crs=projection(x), nl=nl)
			if (rotated(x)) {
				b@rotated <- TRUE
				b@rotation <- x@rotation
			}
			return(b)
		}
		x <- stack(x, ...)
		filename <- trim(filename)
		if (missing(format)) { format <- .filetype(filename=filename) } 
		if (missing(datatype)) { datatype <- .datatype() }
		if (missing(overwrite)) { overwrite <- .overwrite() }
		if (missing(progress)) { progress <- .progress() }

		brick(x, values=values, filename=filename, format=format, datatype=datatype, overwrite=overwrite, progress=progress)
	}
)


setMethod('brick', signature(x='RasterStack'), 
	function(x, values=TRUE, nl, filename='', ...){
	
		e <- x@extent
		b <- brick(xmn=e@xmin, xmx=e@xmax, ymn=e@ymin, ymx=e@ymax, nrows=x@nrows, ncols=x@ncols, crs=projection(x))
		if (rotated(x)) {
			b@rotated <- TRUE
			b@rotation <- x@rotation
		}

		if (missing(nl)) {
			nl <- nlayers(x) 
			if (nl < 1) {
				values <- FALSE
			}
		} else {
			nl <- max(round(nl), 0)
			values <- FALSE
		}
		
		b@data@nlayers <- as.integer(nl)
		
		filename <- trim(filename)
		
		if (values) {
			
			b@layernames <- x@layernames[1:nl]
			if (canProcessInMemory(b, nl*2)) {
				b <- setValues( b, getValues(x)[,1:nl]) 
				if (any(is.factor(x))) {
					b@data@isfactor <- is.factor(x)
					b@data@attributes <- levels(x)
				}
				if (filename != '') {
					b <- writeRaster(b, filename, ...)
				}
				return(b)
				
			} else {

				b <- writeStart(b, filename=filename, ...)
				tr <- blockSize(b)
				pb <- pbCreate(tr$n, ...)			
				for (i in 1:tr$n) {
					vv <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
					b <- writeValues(b, vv, tr$row[i])
					pbStep(pb, i)
				}
				pbClose(pb)
				b <- writeStop(b)
				return(b)
			}
			
		} else {
			b@data@min <- rep(Inf, b@data@nlayers)
			b@data@max <- rep(-Inf, b@data@nlayers)
			return(b)
		}
	}
)

setMethod('brick', signature(x='RasterBrick'), 
	function(x, nl, ...){
		if (missing(nl)) { nl <- nlayers(x) }
		e <- x@extent
		b <- brick(xmn=e@xmin, xmx=e@xmax, ymn=e@ymin, ymx=e@ymax, nrows=x@nrows, ncols=x@ncols, crs=projection(x))
		b@data@nlayers <- as.integer(nl)
		b@data@min <- rep(Inf, nl)
		b@data@max <- rep(-Inf, nl)
		if (rotated(x)) {
			b@rotated <- TRUE
			b@rotation <- x@rotation
		}
		return(b)
	}
)



setMethod('brick', signature(x='Extent'), 
	function(x, nrows=10, ncols=10, crs=NA, nl=1) {
		bb <- extent(x)
		nr = as.integer(round(nrows))
		nc = as.integer(round(ncols))
		if (nc < 1) { stop("ncols should be > 0") }
		if (nr < 1) { stop("nrows should be > 0") }
		b <- new("RasterBrick", extent=bb, ncols=nc, nrows=nr)
		projection(b) <- crs
		nl <- max(round(nl), 0)
		b@data@nlayers <- as.integer(nl)
		b@data@isfactor <- rep(FALSE, nl)
		return(b) 
	}
)


setMethod('brick', signature(x='SpatialGrid'), 
	function(x){
		b <- brick()
		extent(b) <- extent(x)
		projection(b) <- x@proj4string
		dim(b) <- c(x@grid@cells.dim[2], x@grid@cells.dim[1])	
				
		if (class(x) == 'SpatialGridDataFrame') {
			
			x <- x@data
			
			b@data@isfactor <- rep(FALSE, ncol(x))
			
			isfact <- sapply(x, function(i) is.factor(i) | is.character(i))
			b@data@isfactor <- isfact
			if (any(isfact)) {
				for (i in which(isfact)) {
					rat <- data.frame(table(x[[i]]))
					rat <- data.frame(1:nrow(rat), rat[,2], rat[,1])
					colnames(rat) <- c("ID", "COUNT", colnames(x)[i])
					b@data@attributes[[i]] <- rat
					x[,i] <- as.integer(x[,i])
				}
			}
			
			b <- setValues(b, as.matrix(x))
			b@layernames <- colnames(x)
		}
		return(b)
	}	
)


setMethod('brick', signature(x='SpatialPixels'), 
	function(x) {
		if (inherits( x, 'SpatialPixelsDataFrame')) {
			x <- as(x, 'SpatialGridDataFrame')
		} else {	
			x <- as(x, 'SpatialGrid')
		}
		return(brick(x))
	}
)

	
setMethod('brick', signature(x='array'), 
	function(x, xmn=0, xmx=1, ymn=0, ymx=1, crs=NA, transpose=FALSE) {
		dm <- dim(x)
		if (is.matrix(x)) {
			stop('cannot coerce a matrix to a RasterBrick')
		}
		if (length(dm) != 3) {
			stop('array has wrong number of dimensions (needs to be 3)')
		}
		b <- brick(xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx, crs=crs, nl=dm[3])
		names(b) <- dimnames(x)[[3]]
		
		if (transpose) {
			dim(b) <- c(dm[2], dm[1], dm[3])
		} else {
			dim(b) <- dm
			# aperm etc suggested by Justin McGrath
			# https://r-forge.r-project.org/forum/message.php?msg_id=4312
			x = aperm(x, perm=c(2,1,3))
		}
		attributes(x) <- list()
		dim(x) <- c(dm[1] * dm[2], dm[3])
		setValues(b, x)
	}
)
	


setMethod('brick', signature(x='kasc'), 
	function(x) {
		as(x, 'RasterBrick')
	}
)




setMethod('brick', signature(x='grf'), 
	function(x) {
		as(x, 'RasterBrick')
	}
)




setMethod('brick', signature(x='list'), 
	function(x) {
		x <- stack(x)
		brick(x)
	}
)


# Author: Robert J. Hijmans
# Date :  June 2008
# Version 0.9
# Licence GPL v3



layerNames <- function(x) {
	if (.depracatedwarnings()) {
		warning('the layerNames function is obsolete. Use "names" instead')
	}
	names(x)
}



'layerNames<-' <- function(x, value) {
	if (.depracatedwarnings()) {
		warning('the layerNames function is obsolete. Use "names" instead')
	}
	
	nl <- nlayers(x)
	if (is.null(value)) {
		value <- rep('', nl)
	} else if (length(value) != nl) {
		stop('incorrect number of layer names')
	}
	value <- .goodNames(value)
	
	if (inherits(x, 'RasterStack')){
		
		x@layers <- sapply(1:nl, function(i){ 
			r <- x@layers[[i]]
			r@data@names <- value[i]
			r
		})
		
	} else {
		if (.hasSlot(x@data, 'names')) {
			x@data@names <- value
		} else {
			x@layernames <- value		
		}
	}

	return(x)
}

compare <- function(x, ..., extent=TRUE, rowcol=TRUE, crs=TRUE, res=FALSE, orig=FALSE, rotation=TRUE, tolerance, stopiffalse=TRUE, showwarning=FALSE) {
	if (.depracatedwarnings()) {
		warning("raster function 'compare' is obsolete. It has been replaced by 'compareRaster'")
	}	
	compareRaster(x, ..., extent=extent, rowcol=rowcol, crs=crs, res=res, orig=orig, rotation=rotation, tolerance=tolerance, stopiffalse=stopiffalse, showwarning=showwarning)
}



if (!isGeneric("expand")) {
	setGeneric("expand", function(x, y, ...)
		standardGeneric("expand"))
}	


setMethod('expand', signature(x='Extent'), 
function(x, y, ...) {
	if (.depracatedwarnings()) {
		warning("function 'expand' is obsolete. It has been replaced by 'extend'")
	}
	extend(x, y, ...)
}
)


setMethod('expand', signature(x='Raster'), 
function(x, y, value=NA, filename='', ...) {
	if (.depracatedwarnings()) {
		warning("function 'expand' is obsolete. It has been replaced by 'extend'")
	}
	extend(x, y, value=value, filename=filename, ...)
} )



unionExtent <- function(...) {
	stop('this function has been depracated. Use "union"')
}


intersectExtent <- function(...) {
	stop('this function has been depracated. Use "intersect"')
}


setOptions <- function(...) {
	if (.depracatedwarnings()) {
		warning('this function is depracated. Use "rasterOptions" instead')
	}
	rasterOptions(...)
}

showOptions <- function() {
	if (.depracatedwarnings()) {
		warning('This function is depracated. Use "rasterOptions()" in stead')
	}
	rasterOptions()
}

clearOptions <- function() {
	if (.depracatedwarnings()) {
		warning('This function is depracated. Use "rasterOptions(default=TRUE)" in stead')
	}
	rasterOptions(default=TRUE)
}


saveOptions <- function() {
	if (.depracatedwarnings()) {
		warning('This function is depracated. Use "rasterOptions(save=TRUE)" in stead')
	}
	rasterOptions(save=TRUE)
}


	
.focalValues <- function(x, row, ngb=3, fun=NULL, na.rm=FALSE, layer, nl, ...) {

	if (.depracatedwarnings()) {
		warning('this function is depracated. Use "getValuesFocal" instead.')
	}
	dots <- list(...)
	if (! is.null(dots$buffer) ) {
		warning('argument "buffer" is ignored')
	}

	if (missing(row)) stop('You must provide a row number "row=" argument')

	if (!(validRow(x, row))) {	stop(paste(row, 'is not a valid rownumber')) }

	ngb <- .checkngb(ngb)
	
	r1 = row - floor(ngb[1]/2)
	r2 = r1 + ngb[1] - 1
	r1 = max(1, r1)
	r2 = min(nrow(x), r2)
	nrows = r2 - r1 + 1
	
	col1 = max(1, floor(ngb[2]/2))
	col2 = max(1, ngb[2]-(col1+1))

	cols=x@ncols
	nrs = matrix(ncol=cols, nrow=nrows)
	nrs[] = 1:length(nrs)

	if (.isGlobalLonLat(x)) {
		add1 <- nrs[,(ncol(nrs)-col1+1):ncol(nrs),drop=FALSE]
		add2 <- nrs[,1:col2,drop=FALSE]
	} else {
		add1 = matrix(0, ncol=col1, nrow=nrows)
		add2 = matrix(0, ncol=col2, nrow=nrows)
	}

	nrs = cbind(add1, nrs, add2)
	
	idx = matrix(ncol=cols, nrow=ngb[2]*nrows)
	cc = 1:ngb[2]
	for (c in 1:cols) {
		idx[,c] = nrs[, (cc+c-1)] 
	}
	id = as.vector(idx)
	id = cbind(rep(1:cols, each=nrow(idx)), id)
	id <- id[id[,2]>0, ,drop=FALSE]

	nls <- nlayers(x)
	
	if (nls == 1) {
		ngbdata = matrix(getValuesBlock(x, r1, nrows), ncol=ncol(x), byrow=TRUE)
		v = cbind( id[,1], as.vector(ngbdata)[id[,2]] )
		colnames(v) <- c('col', 'value')
		
		if (!is.null(fun)) {
			v <- as.vector( tapply(v[,2], v[,1], FUN=fun, na.rm=na.rm) 	)
		}
		
	} else {
	
		nlyrs <- nls
		if (missing(layer)) { layer <- 1 } 
		if (missing(nl)) { nl <- nlyrs } 
		layer <- min(max(1, round(layer)), nlyrs)
		nl <- min( max(1, round(nl)), nlyrs-layer+1 )
		lyrs <- layer:(layer+nl-1)
		
		alldata <- getValuesBlock(x, r1, nrows)
		v <- matrix(nrow=nrow(id), ncol=nl+1)
		colnames(v) <- c('col', names(x)[lyrs])
		v[,1] <- id[,1]
		for (i in 1:nl) {
			j <- lyrs[i]
			ngbdata <- matrix( alldata[,j], ncol=ncol(x) )
			v[,i+1] <- as.vector(ngbdata)[id[,2]] 
		}
		
		if (!is.null(fun)) {
			v <- aggregate(v[,-1,drop=FALSE], list(v[,1]), FUN=fun, na.rm=na.rm)
			v <- as.matrix(v)[,-1]
			colnames(v) <- names(x)[lyrs]
		} 
	}
		
	return(v)
}


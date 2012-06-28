# Author: Robert J. Hijmans
# Date : November 2008
# Version 1.0
# Licence GPL v3


	
.xyValues <- function(object, xy, method='simple', cellnumbers=FALSE, buffer=NULL, fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE, ...) { 

	nlyrs <- nlayers(object)
	if (nlyrs > 1) {
		if (missing(layer)) { layer <- 1 } 
		if (missing(nl)) { nl <- nlyrs } 
		layer <- min(max(1, round(layer)), nlyrs)
		nl <- min(max(1, round(nl)), nlyrs-layer+1)
	} else {
		layer <- 1
		nl <- 1
	}
	
	if (dim(xy)[2] != 2) {
		stop('xy should have 2 columns only.\nFound these dimensions: ', paste(dim(xy), collapse=', ') )
	}
		
	if (! is.null(buffer)) {
		if (method != 'simple') { 
			warning('method argument is ignored when a buffer is used') 
		}
		res <- .xyvBuf(object, xy, buffer, fun, na.rm, layer=layer, nl=nl, cellnumbers=cellnumbers) 		
		
	} else if (method == 'bilinear') {
		res <- .bilinearValue(object, xy, layer=layer, n=nl) 
		if (cellnumbers) {
			warning("'cellnumbers' does not apply for bilinear values")
		}

	} else if (method=='simple') {
		cells <- cellFromXY(object, xy)
		res <-  .cellValues(object, cells, layer=layer, nl=nl) 
		if (cellnumbers) {			
			res <- cbind(cells, res)
			if (ncol(res) == 2) {
				colnames(res)[2] <- names(object)[layer]
			} 
		}
			
	} else {
		stop('invalid "method" argument. Should be simple or bilinear.')
	}
	
	if (df) {
		if (is.list(res)) {
			res <- lapply(1:length(res), function(x) if (length(res[[x]]) > 0) cbind(ID=x, res[[x]]))
			res <- do.call(rbind, res)
			rownames(res) <- NULL
		} else {
			res <- data.frame(cbind(ID=1:NROW(res), res))
		}
		lyrs <- layer:(layer-1+nl)
		colnames(res) <- c('ID', names(object)[lyrs])

		if (any(is.factor(object)) & factors) {
			v <- res[, -1, drop=FALSE]
			if (ncol(v) == 1) {
				v <- data.frame(factorValues(object, v[,1], layer))
			} else {
				v <- .insertFacts(object, v, lyrs)
			}
			res <- data.frame(res[,1,drop=FALSE], v)
		}
	}
	
	res
}



# Authors: Robert J. Hijmans
# Date :  January 2009
# Version 0.9
# Licence GPL v3

.addArgs <- function(...) {
	lst <- list(...)
	add <- list()
	if (length(lst) > 0 ) {
		cnt <- 0
		for (i in 1:length(lst)) {
		# is.atomic ?
			if (class(lst[[i]]) %in% c('logical', 'integer', 'numeric')) {
				cnt <- cnt + 1
				add[[cnt]] <- lst[[i]]
			}
		}
	}
	return(unlist(add))
}



setMethod("Summary", signature(x='Raster'),
	function(x, ..., na.rm=FALSE){

		fun <- as.character(sys.call()[[1L]])

		x <- .makeRasterList(x, ...) 
		if (length(x) > 1) {
			x <- stack(x)
		} else {
			x <- x[[1]]
		}

		add <- .addArgs(...)

		if (nlayers(x)==1 & length(add)==0) {
			warning('Nothing to summarize if you provide a single RasterLayer; see cellStats')
			return(x[[1]])
		}	
		
		if (length(add)==0) {
			if (fun[1] == 'sum') {
				return(.sum( x, add))
			} else if (fun[1] == 'min') {
				return(.min( x, add ))
			} else if (fun[1] == 'max') {
				return(.max( x, add))
			}
		}

		out <- raster(x)
	
		if (canProcessInMemory(x)) {
			
			x <- cbind(getValues(x), add)
			if (na.rm) {
				x <- apply(x, 1, FUN=fun, na.rm=TRUE)
			} else {
				x <- apply(x, 1, FUN=fun)
			}
			if (! is.null(dim(x))) {  # range, perhaps others?
				x <- t(x)
				nl <- ncol(x)
				out <- brick(out, nl=nl)
			}
			out <- setValues(out, x)
			return(out)
		}
		
		layerNames(out) <- fun
		if (fun[1] == 'range') {
			out <- brick(out, nl=2)
		}
	
		tr <- blockSize(x)
		out <- writeStart(out, filename="")
		pb <- pbCreate(tr$n)
		for (i in 1:tr$n) {
			v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
			v <- cbind(v, add)
			if (na.rm) {
				v <- apply(v, 1, FUN=fun, na.rm=TRUE)
			} else {
				v <- apply(v, 1, FUN=fun)
			}
			if (class(v) == 'matrix')  { # range
				v <- t(v)
			}
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb, i) 
		} 
		pbClose(pb)			
		writeStop(out)
	}
)



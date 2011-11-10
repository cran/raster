# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  November 2009
# Version 0.9
# Licence GPL v3



.embed <- function(x, dimension) {
    n <- length(x)
    m <- n - dimension + 1
    data <- x[1:m + rep.int(1:dimension, rep.int(m, dimension)) - 1]
	dim(data) <- c(m, dimension)
	return(data)
}


.calcNGB <- function(rows, colnrs, res, fun, keepdata) {
	res[] <- NA
    for (i in 1:dim(rows)[2]) {
		d <- as.vector(rows[, colnrs[i, ]])
		if (keepdata) {
			d <- na.omit(d)
			if (length(d) > 0) {
				res[i] <- fun(d)
			} else {  # for sum because sum(NULL) = 0
				res[i] <- NA 
			}	
		} else {
			res[i] <- fun(d)
		}
	}	
	return(res)
}



.focalNA <- function(x, ngb=3, fun=mean, recursive=FALSE, maxrec=0, filename="", ...) {

	warning('this function is depracated. Please use "focal"')

	filename <- trim(filename)
	if (recursive) {
		ovwr <- .overwrite(...)
		if (filename != '' & file.exists(filename) & !ovwr) {
			stop('file exists, use overwrite=TRUE to overwrite it')
		}
		iterator <- 0
		keepGoing <-  TRUE
		x <- list(x)
		while (keepGoing) {
			iterator <- 1 + iterator
			cat('iteration', iterator , '\n')
			flush.console()
			x <- .focNA(x[[1]], fun=fun, ngb=ngb, recursive=TRUE, filename='') 
			if (x[[2]]) {
				stop('all values are NA')
			}
			keepGoing <- x[[3]]	
			if (iterator == maxrec) { keepGoing <- FALSE }
		} 
		if (filename != '') {
			x[[1]] <- writeRaster(x[[1]], filename=filename, ...)
		}
		return(x[[1]])
	} else {
		return( .focNA(x, fun=fun, ngb=ngb, recursive=FALSE, filename=filename, ...) )
	}
}


.focNA <- function(raster, fun=mean, ngb=3, recursive=FALSE, filename="", ...) {
	ngb <- .checkngb(ngb)
	ngbgrid <- raster(raster)

	if (!canProcessInMemory(ngbgrid, 4) && filename == '') {
		filename <- rasterTmpFile()
								
	}

	if (filename == '') {
		v <- matrix(NA, ncol=nrow(ngbgrid), nrow=ncol(ngbgrid))
	} else {
		v <- vector(length=0)
	}
	
	allNA <- TRUE

	# first create an empty matrix with nrows = ngb and ncols = raster@ncols
	res <- vector(length=length(ncol(ngbgrid)))
	limcol <- floor(ngb[2] / 2)
	colnrs <- (-limcol+1):(ncol(ngbgrid)+limcol)
	colnrs <- .embed(colnrs, ngb[2])
	colnrs[colnrs > ncol(ngbgrid) | colnrs < 0] <- 0

	limrow <- floor(ngb[1] / 2)
	midrow <- ceiling(ngb[1] / 2)
	ngbdata <- matrix(NA, nrow=0, ncol=ncol(ngbgrid))
# add all rows needed for first ngb, minus 1 that will be read in first loop	

	for (r in 1:limrow) {
		ngbdata <- rbind(ngbdata, getValues(raster, r))
	}

	res <- vector(length=ncol(ngbdata))

	keepGoing <- FALSE
	
	pb <- pbCreate(nrow(ngbgrid), ...)
	if (filename != '') {
		ngbgrid <- writeStart(ngbgrid, filename=filename, ...)			
	}
	
	for (r in 1:nrow(ngbgrid)) {		
		rr <- r + limrow
		if (rr <= nrow(ngbgrid)) {
			rowdata <- getValues(raster, rr)
			if (dim(ngbdata)[1] == ngb[1]) {
				ngbdata <- rbind(ngbdata[2:ngb[1],], rowdata)
			} else {
				ngbdata <- rbind(ngbdata, rowdata)			
			}
		} else {
			ngbdata <- ngbdata[-1, ,drop=FALSE]
		}
		
		mrow <- min(r, midrow)
		vals <- ngbdata[mrow,]
		if (sum(is.na(vals)) > 0) {
			ngbvals <- .calcNGB(ngbdata, colnrs, res, fun, keepdata=TRUE)
			vals[is.na(vals)] <- ngbvals[is.na(vals)]
			nas <- sum(is.na(vals))
			if (nas > 0) { keepGoing <- TRUE }
			if (allNA) {
				if (nas < ncol(ngbgrid)) {
					allNA <- FALSE
				}
			}
		} else { 
			allNA <- FALSE
		}
		
		if (filename != "") {
			ngbgrid <- writeValues(ngbgrid, vals, r)
		} else {
			v[,r] <- vals
		}
		pbStep(pb, r)
	}
	pbClose(pb)

	if (filename != "") { 
		ngbgrid <- writeStop(ngbgrid)
	} else {
		ngbgrid <- setValues(ngbgrid, as.vector(v)) 
	}
	
	if (recursive) {
		return(list(ngbgrid, allNA, keepGoing))
	}
	return(ngbgrid)
}


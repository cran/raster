# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3



if (!isGeneric("reclass")) {
	setGeneric("reclass", function(x, rcl, ...)
		standardGeneric("reclass"))
}	


setMethod('reclass', signature(x='Raster', rcl='ANY'), 

function(x, rcl, update=FALSE, filename='', ...)  {
	
	filename <- trim(filename)

	if ( is.null(dim(rcl)) ) { 
		rcl <- matrix(rcl, ncol=3, byrow=TRUE) 
	} else if ( dim(rcl)[2] == 1 ) { 
		rcl <- matrix(rcl, ncol=3, byrow=TRUE) 
	}
	if ( dim(rcl)[2] != 3 ) { stop('rcl must have 3 columns') }
	colnames(rcl) <- c("From", "To", "Becomes")	
	
	if (getOption('verbose')) { print(rcl)  }
	
	if (nlayers(x) == 1) { out <- raster(x)
	} else { out <- brick(x, values=FALSE) }	

	
	if (canProcessInMemory(out, 2)) {

		x <- getValues(x)
		if (update) {
			for (i in 1:nrow(rcl)) {
				if (is.na(rcl[i,1]) | is.na(rcl[i,2])) {
					x[ is.na(x) ] <- rcl[i, 3] 
				} else { 
					x[ (x >= rcl[i,1]) & (x <= rcl[i,2]) ] <- rcl[i , 3] 
				}
			}
			out <- ( setValues(out, x) )
		} else {
			res <- x
			for (i in 1:nrow(rcl)) {
				if (is.na(rcl[i,1]) | is.na(rcl[i,2])) {
					res[ is.na(x) ] <- rcl[i, 3] 
				} else { 
					res[ (x >= rcl[i,1]) & ( x <= rcl[i,2]) ] <- rcl[i , 3] 
				}
			}
			out <- ( setValues(out, res) )
		}
		if ( filename != "" ) { out <- writeRaster(out, filename=filename, ...) }
		return(out)
				
	} else {

		if (filename == '') { filename <- rasterTmpFile() }

		hasNA <- FALSE
		for (i in 1:nrow(rcl)) {
			if (is.na(rcl[i,1]) | is.na(rcl[i,2])) {
				naVAL <- rcl[i,3]
				rcl <- rcl[-i, ]
				hasNA <- TRUE
			}
		}

		onlyNA <- FALSE
		if (dim(rcl)[1] == 0) {
			if (hasNA) {
				onlyNA <- TRUE
			} else {
				stop('I do not understand this reclass matrix')
			}
		}
		
		
		out <- writeStart(out, filename=filename, ...)
		tr <- blockSize(out)
		pb <- pbCreate(tr$n, type=.progress(...))

		if (onlyNA) {
			for (i in 1:tr$n) {
				res <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
				res[ is.na(res) ] <- naVAL	
				out <- writeValues(out, res, tr$row[i])
				pbStep(pb, i)
			}
		} else if (update) {
			for (i in 1:tr$n) {
				res <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
				for (i in 1:nrow(rcl)) {
					res[ (res >= rcl[i,1]) & (res <= rcl[i,2]) ] <- rcl[i,3] 
				}
				if (hasNA) {
					res[ is.na(res) ] <- naVAL
				}	
				out <- writeValues(out, res, tr$row[i])
				pbStep(pb, i)
			}
		} else {
			for (i in 1:tr$n) {
				res <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
				vals <- res
				for (i in 1:nrow(rcl)) {
					res[ (vals >= rcl[i,1]) & ( vals <= rcl[i,2]) ] <- rcl[i , 3] 
				}
				if (hasNA) {
					res[ is.na(vals) ] <- naVAL
				}	
				out <- writeValues(out, res, tr$row[i])
				pbStep(pb, i)
			}
			out <- writeStop(out)
			pbClose(pb)
		}
		return(out)
	}
}
)


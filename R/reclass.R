# Author: Robert J. Hijmans
# Date :  June 2008
# Version 1.0
# Licence GPL v3


if (!isGeneric("reclass")) {
	setGeneric("reclass", function(x, rcl, ...)
		standardGeneric("reclass"))
}	


setMethod('reclass', signature(x='Raster', rcl='ANY'), 

function(x, rcl, filename='', include.lowest=FALSE, right=TRUE, ...) {
	
	filename <- trim(filename)

	if ( is.null(dim(rcl)) ) { 
		rcl <- matrix(rcl, ncol=3, byrow=TRUE) 
	} else if ( dim(rcl)[2] == 1 ) { 
		rcl <- matrix(rcl, ncol=3, byrow=TRUE) 
	}
	
	nc <- ncol(rcl)
	if ( nc != 3 ) { 
		if (nc == 2) {
			colnames(rcl) <- c("Is", "Becomes")	
			if (getOption('verbose')) { print(rcl)  }
			rcl <- cbind(rcl[,1], rcl)
			right <- NA
		} else {
			stop('rcl must have 2 or 3 columns') 
		}
	} else {
		colnames(rcl) <- c("From", "To", "Becomes")	
		if (getOption('verbose')) { print(rcl)  }
	}

	
	hasNA <- FALSE
	onlyNA <- FALSE
	valNA <- NA
	for (i in 1:nrow(rcl)) {
		if (is.na(rcl[i,1]) | is.na(rcl[i,2])) {
			if (!hasNA) {
				valNA <- rcl[i,3]
				hasNA <- TRUE
			}
			rcl <- rcl[-i, ,drop=FALSE]
		}
	}
	if (hasNA) {
		if (dim(rcl)[1] == 0) {
			onlyNA <- TRUE
		} else {
			print(rcl)
			stop('I do not understand this reclass matrix')
		}
	}
	stopifnot(all(rcl[,2] >= rcl[,1]))

	
	if (nlayers(x) == 1) { 
		out <- raster(x)
	} else { 
		out <- brick(x, values=FALSE) 
	}

	include.lowest <- as.integer(include.lowest)
	right <- as.integer(right)
	#hasNA <- as.integer(hasNA)
	onlyNA <- as.integer(onlyNA)
	valNA <- as.double(valNA)
	
	if (nc == 2) {
		rcl <- rcl[ , 2:3, drop=FALSE]
	}
	
	if (canProcessInMemory(out)) {
		out <- setValues(out, .Call('reclass', values(x), rcl, include.lowest, right, onlyNA, valNA, NAOK=TRUE, PACKAGE='raster'))
		if ( filename != "" ) { 
			out <- writeRaster(out, filename=filename, ...) 
		}
		return(out)
				
	} else {
		
		tr <- blockSize(out)
		pb <- pbCreate(tr$n, type=.progress(...))
		out <- writeStart(out, filename=filename, ...)

		for (i in 1:tr$n) {
			vals <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
			vals <- .Call('reclass', vals, rcl, include.lowest, right, onlyNA, valNA, NAOK=TRUE, PACKAGE='raster')
			out <- writeValues(out, vals, tr$row[i])
			pbStep(pb, i)
		}
		out <- writeStop(out)
		pbClose(pb)
		
		return(out)
	}
}
)


		

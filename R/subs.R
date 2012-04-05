# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# Date :  February 2010
# Version 0.9
# Licence GPL v3

	
if (!isGeneric("subs")) {
	setGeneric("subs", function(x, y, ...)
		standardGeneric("subs"))
}


.localmerge <- function(x, y, subNA) {
	nc <- NCOL(x)
	nr <- NROW(x)
	x <- cbind(1:length(x), as.vector(x))
	if (! subNA ) {
		y <- merge(x, y, by.x=2, by.y=1)
		x[y[,2], 2] <- y[,3]
		x <- x[,2]
	} else {
		x <- as.matrix(merge(x, y, by.x=2, by.y=1, all.x=TRUE))
		x <- x[order(x[,2]), -c(1:2)]
	}	
	if (nc > 1) {
		x <- matrix(as.vector(x), nrow=nr)
	}
	return(x)
}


setMethod('subs', signature(x='Raster', y='data.frame'), 
	function(x, y, by=1, which=2, subsWithNA=TRUE, filename='', ...) { 

		if (!subsWithNA & length(which) > 1) {
			stop('you cannot use subsWithNA=FALSE if length(which) > 1')
		}
		
		if (is.character(by)) {
			by <- which(by == colnames(y))[1]
			if (is.na(by)) {stop("'by' is not a valid column name")}
		}
		if (is.character(which)) {
			which <- which(which == colnames(y))[1]
			if (is.na(which)) {stop("'which' is not valid column name")}
		}
		
		y <- y[ , c(by, which)]

		tt <- table(y[,1])
		tt <- tt[ which(tt > 1) ]
		if (length(tt) > 0) {
			stop('duplicate "by" values not allowed')
		}

		r <- raster(x)
		nlx <- nlayers(x)
		if (nlx == 1) {
			ln <- colnames(y)[which]
			if (length(which) > 1) {
				r <- brick(r, nl=length(which))
			}
		} else {
			r <- brick(r, nl=nlx * length(which))
			ln <- rep(layerNames(x), length(which))
			if (length(which) > 1) {
				ln2 <- rep(colnames(y)[which], each=nlx)
				ln <- paste(ln, paste('_', ln2, sep=''), sep='')
			}
		} 
		layerNames(r) <- ln

		filename <- trim(filename)
		
		if (canProcessInMemory(x, 3)) {
			v <- .localmerge( getValues(x), y, subsWithNA )
			r <- setValues(r, v)
			if (filename != '') {
				r <- writeRaster(r, filename=filename, ...)
			}
			return(r)
			
		} else {
			if (filename == '') {
				filename <- rasterTmpFile()
			}
			tr <- blockSize(r)
			pb <- pbCreate(tr$n, ...)
			r <- writeStart(r, filename=filename, ...)
			for (i in 1:tr$n) {
				v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
				r <- writeValues(r, .localmerge(v, y, subsWithNA), tr$row[i])
				pbStep(pb) 
			}
			pbClose(pb)			
			r <- writeStop(r)
			return(r)
		}
	}
)



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
	function(x, y, by=1, which=2, subsWithNA=TRUE, filename='', ...)  { 

		if (!subsWithNA & length(which) > 1) {
			stop('you cannot use subsWithNA=FALSE if length(which) > 1')
		}
		
		if (is.character(by)) {
			by <- which(by == colnames(y))[1]
			if (is.na(by)) { stop("'by' is not a valid column name") }
		}
		if (is.character(which)) {
			which <- which(which == colnames(y))[1]
			if (is.na(which)) { stop("'which' is not valid column name") }
		}
		
		y <- y[ , c(by, which)]

		tt <- table(y[,1])
		tt <- tt[ which(tt > 1) ]
		if (length(tt) > 0) {
			stop('duplicate "by" values not allowed')
		}

		r <- raster(x)
		nlx <- nlayers(x)
		
		cls <- sapply(y, class)
		hasfactor <- rep(FALSE, length(cls)-1)
		levs <- list()
		for (i in 2:length(cls)) {
			if (cls[i] == 'character') {
				w <- getOption('warn')
				options('warn'=-1) 
				tmp <- as.numeric(y[,i])
				options('warn'= w)
				if (all(is.na(tmp) == is.na(y[,i]))) {
					y[,i] <- tmp
					cls[i] <- 'numeric'				
				} else {
					y[,i] <- factor(y[,i])
					cls[i] <- 'factor'
				}
			}
			if (cls[i] == 'factor') {
				uny <- unique(y[,i])
				lv <- data.frame(ID=1:length(uny), COUNT=NA, uny)
				colnames(lv)[3] <- colnames(y)[i]
				levs[[i-1]] <- lv
				hasfactor[i-1] <- TRUE
				m <- match(y[,i], uny)
				y[,i] <- as.numeric(uny[m])
			}
		}
		
		if (nlx == 1) {
			ln <- colnames(y)[which]
			if (length(which) > 1) {
				r <- brick(r, nl=length(which))
			}
		} else {
			r <- brick(r, nl=nlx * length(which))
			ln <- rep(names(x), length(which))
			if (length(which) > 1) {
				ln2 <- rep(colnames(y)[which], each=nlx)
				ln <- paste(ln, paste('_', ln2, sep=''), sep='')
			}
		} 
		names(r) <- ln

		filename <- trim(filename)
		
		if (canProcessInMemory(x, 3)) {
			if (any(hasfactor)) {
				r@data@isfactor <- hasfactor
				r@data@attributes <- levs
			}
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
			
			if (any(hasfactor)) {
				r@data@isfactor <- TRUE
				r@data@attributes <- levs
			}		
			r <- writeStop(r)
			return(r)
		}
	}
)




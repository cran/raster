# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  August 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric("predict")) {
	setGeneric("predict", function(object, ...)
		standardGeneric("predict"))
}	

setMethod('predict', signature(object='Raster'), 
	function(object, model, filename="", fun=predict, ext=NULL, const=NULL, index=1, se.fit=FALSE, na.rm=TRUE, ...) {
	

		if ( class(model)[1] %in% c('Bioclim', 'Domain', 'Mahalanobis', 'MaxEnt', 'ConvexHull') ) { 
			return ( predict(model, object, filename=filename, ext=ext, ...) ) 
		}
	
		if (se.fit) {
			predrast <- brick(object, values=FALSE)
			predrast@data@nlayers <- as.integer(2)
		} else if (length(index) > 1) {	
			predrast <- brick(object, values=FALSE)
			predrast@data@nlayers <- length(index)
		} else {
			predrast <- raster(object)
		}
				
		if (!is.null(ext)) {
			predrast <- crop(predrast, extent(ext))
			firstrow <- rowFromY(object, yFromRow(predrast, 1))
			firstcol <- colFromX(object, xFromCol(predrast, 1))
		} else {
			firstrow <- 1
			firstcol <- 1
		}
		ncols <- ncol(predrast)
			
		dataclasses <- attr(model$terms, "dataClasses")[-1]	
			
		lyrnames <- layerNames(object)
		
		varnames <- names(dataclasses)

		if ( length( unique(lyrnames[(lyrnames %in% varnames)] )) != length(lyrnames[(lyrnames %in% varnames)] )) {
			stop('duplicate names in Raster* object: ', lyrnames)
		}
			
		
		f <- names( which(dataclasses == 'factor') )
		if (length(f) > 0) { 
			haveFactor <- TRUE 
		} else {
			haveFactor <- FALSE
		}
		
		filename <- trim(filename)
		if (!canProcessInMemory(predrast) && filename == '') {
			filename <- rasterTmpFile()
		} 

		if (filename == '') {
			v <- matrix(NA, ncol=nlayers(predrast), nrow=ncell(predrast))
		} 

		tr <- blockSize(predrast, n=nlayers(object)+3)

		napred <- rep(NA, ncol(predrast)*tr$size )

		if (inherits(object, 'RasterStackBrick')) {
				if (nlayers(object)==0) { stop('empty Raster object') }
		} else {
			if ( !  fromDisk(object) ) {
				if (! inMemory(object) ) {
					{ stop('No values associated with this Raster object') }
				}
			}				
		}
		
		
		pb <- pbCreate(tr$n,  type=.progress(...) )			
		
		if (filename != '') {
			predrast <- writeStart(predrast, filename=filename, ... )
		}

		for (i in 1:tr$n) {
			if (i==tr$n) { 
				ablock <- 1:(ncol(object) * tr$nrows[i])
				napred <- rep(NA, ncol(predrast) * tr$nrows[i])
			}

			rr <- firstrow + tr$row[i] - 1
		
			blockvals <- as.data.frame(getValuesBlock(object, row=rr, nrows=tr$nrows[i], firstcol, ncols))
			if (haveFactor) {
				for (i in 1:length(f)) {
					blockvals[,f[i]] <- as.factor(blockvals[,f[i]])
				}
			}
			if (! is.null(const)) {
				blockvals = cbind(blockvals, const)
			} 

			if (se.fit) {
			
				predv <- predict(model, blockvals, se.fit=TRUE, ...)
				predv <- cbind(as.vector(predv$fit), as.vector(predv$se.fit))
			
			}  else {
			
				bvr <- nrow(blockvals)
				if (na.rm) {  
					blockvals <- na.omit(blockvals)		
				}
				
				if (nrow(blockvals) == 0 ) {
					predv <- napred
				} else {
					predv <- fun(model, blockvals, ...)	
					if (class(predv)[1] == 'list') {
						predv = unlist(predv)
						if (length(predv) != nrow(blockvals)) {
							predv = matrix(predv, nrow=nrow(blockvals))
						}					
					}
					if (isTRUE(dim(predv)[2] > 1)) {
						predv = predv[,index]
					}
				}
						
				if (na.rm) {  
					naind <- as.vector(attr(blockvals, "na.action"))
					if (!is.null(naind)) {
						p <- napred
						p[-naind] <- predv
						predv <- p
						rm(p)
					}
				}
			}
				
			# to change factor to numeric; should keep track of this to return a factor type RasterLayer
			predv[] = as.numeric(predv)
		
			if (filename == '') {
				cells = cellFromRowCol(predrast, tr$row[i], 1):cellFromRowCol(predrast, tr$row[i]+tr$nrows[i]-1, ncol(predrast))
				a = v[cells, ] <- predv 
			} else {
				predrast <- writeValues(predrast, predv, tr$row[i])
			}
			pbStep(pb, i) 
		}
		pbClose(pb)
		
		if (filename == '') {
			predrast <- setValues(predrast, v)  # or as.vector
		} else {
			predrast <- writeStop(predrast)
		}
		
		return(predrast)
	}
)



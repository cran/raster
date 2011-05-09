# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  August 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric("predict")) {
	setGeneric("predict", function(object, ...)
		standardGeneric("predict"))
}	

setMethod('predict', signature(object='Raster'), 
	function(object, model, filename="", fun=predict, ext=NULL, const=NULL, index=1, se.fit=FALSE, na.rm=TRUE, progress='', ...) {
	
		if (inherits(model, 'DistModel')) {
			# models defined in package 'dismo'
			return ( predict(model, object, filename=filename, ext=ext, progress=progress, ...) ) 
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
			
		lyrnames <- layerNames(object)
		
		haveFactor <- FALSE
		dataclasses <- try (attr(model$terms, "dataClasses")[-1], silent=TRUE)
		if (class(dataclasses) != "try-error") {
			varnames <- names(dataclasses)
			if ( length( unique(lyrnames[(lyrnames %in% varnames)] )) != length(lyrnames[(lyrnames %in% varnames)] )) {
				stop('duplicate names in Raster* object: ', lyrnames)
			}

			f <- names( which(dataclasses == 'factor') )
			if (length(f) > 0) { 
				haveFactor <- TRUE 
				factlevels <- list()
				for (i in 1:length(f)) {
					if (inherits(model, "randomForest")) {
						factlevels[[i]] <- model$forest$xlevels[[f]]
					} else {
						factlevels[[i]] <- levels( model$data[f][,1] )
					}
				}
			}
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
		
		
		pb <- pbCreate(tr$n,  type=progress )			
		
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
			colnames(blockvals) <- lyrnames
			if (haveFactor) {
				for (j in 1:length(f)) {
					fl <- NULL
					try(fl <- factlevels[[j]], silent=TRUE)
					fv <- blockvals[,f[j]]
					if (!is.null(fl)) {
						fv[! fv %in% factlevels[[j]] ] <- NA 
						blockvals[,f[j]] <- factor(fv, levels=fl)
					} else {					
						blockvals[,f[j]] <- factor(fv)
					}
				}
			}
			
			if (! is.null(const)) {
				blockvals = cbind(blockvals, const)
			} 

			if (se.fit) {
			
				predv <- fun(model, blockvals, se.fit=TRUE, ...)
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
			}

			if (is.list(predv)) {
				predv <- unlist(predv)
			}
			if (is.factor(predv)) {
				# should keep track of this to return a factor type RasterLayer
				predv <- as.integer(as.character(predv))
			}
		
			if (filename == '') {
				cells = cellFromRowCol(predrast, tr$row[i], 1):cellFromRowCol(predrast, tr$row[i]+tr$nrows[i]-1, ncol(predrast))
				v[cells, ] <- predv 
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



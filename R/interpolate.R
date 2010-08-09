
if (!isGeneric("interpolate")) {
	setGeneric("interpolate", function(object, ...)
		standardGeneric("interpolate"))
}	


setMethod('interpolate', signature(object='Raster'), 
	
	function(object, model, filename="", fun=predict, xyOnly=TRUE, ext=NULL, const=NULL, index=1, na.rm=TRUE, debug.level=1, ...) {
		
		predrast <- raster(object)
				
		if (!is.null(ext)) {
			predrast <- crop(predrast, extent(ext))
			firstrow <- rowFromY(object, yFromRow(out, 1))
			firstcol <- colFromX(object, xFromCol(out, 1))
		} else {
			firstrow <- 1
			firstcol <- 1
		}
		ncols <- ncol(predrast)
			
		dataclasses <- attr(model$terms, "dataClasses")[-1]	
			
		lyrnames <- layerNames(object)
		
		xylyrnames <- c('x', 'y', lyrnames)
		
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
			v <- matrix(NA, ncol=nrow(predrast), nrow=ncol(predrast))
		} 

		if (! xyOnly) {
			if (inherits(object, 'RasterStack')) {
				if (nlayers(object)==0) { 
					warning('"object" has no data, xyOnly set to TRUE')
					xyOnly <- TRUE 
				}
			} else {
				if ( !  fromDisk(object) ) {
					if (! inMemory(object) ) {
						warning('"object" has no data, xyOnly set to TRUE')
						xyOnly <- TRUE 
					}
				}				
			}
		}
		if (xyOnly) na.rm <- FALSE
		
		
		if (inherits(model, "gstat")) { 
			gstatmod <- TRUE 
			if (!is.null(model$locations) && inherits(model$locations, "formula"))  {
				# should be ~x + y  ; need to check if it is ~lon + lat; or worse ~y+x
				sp <- FALSE
			} else {
				sp <- TRUE
			}
		} else { 
			gstatmod <- FALSE 
		}
		

		tr <- blockSize(predrast, n=nlayers(object)+3)
		ablock <- 1:(ncol(object) * tr$size)
		napred <- rep(NA, ncol(predrast)*tr$size )
				
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
		
			if (xyOnly) {
				p <- xyFromCell(predrast, ablock + (tr$row[i]-1) * ncol(predrast)) 
				p <- na.omit(p)
				blockvals <- data.frame(x=p[,1], y=p[,2])
			} else {
				blockvals <- as.data.frame(getValuesBlock(object, row=rr, nrows=tr$nrows[i], firstcol, ncols))
				#colnames(blockvals) <- lyrnames
				if (haveFactor) {
					for (i in 1:length(f)) {
						blockvals[,f[i]] <- as.factor(blockvals[,f[i]])
					}
				}

				p <- xyFromCell(predrast, ablock + (tr$row[i]-1) * ncol(predrast)) 
				blockvals <- cbind(data.frame( x=p[,1], y=p[,2]), v=blockvals) 

				if (! is.null(const)) {
					blockvals = cbind(blockvals, const)
				}
			} 
			

			if (gstatmod) { 
				if (sp) { 
					row.names(p) <- 1:nrow(p)
					blockvals <- SpatialPointsDataFrame(coords=p, data = blockvals, proj4string = projection(predrast, asText = FALSE))
				}
				if (i == 1) { 
					predv <- predict(model, blockvals, debug.level=debug.level, ...) 
				} else { 
					predv <- predict(model, blockvals, debug.level=0, ...) 
				}
				if (sp) { 
					predv <- predv@data[,index] 
				} else { 
					predv <- predv[,index+2] 
				}
					
			} else {  
			
				if (na.rm) {  
					blockvals <- na.omit(blockvals)		
				}
				if (nrow(blockvals) == 0 ) {
					predv <- napred
				} else {
					predv <- fun(model, blockvals, ...)	
				}

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
				
				# to change factor to numeric; should keep track of this to return a factor type RasterLayer
				predv = as.numeric(predv)
							
			}
			
			if (filename == '') {
				predv = matrix(predv, nrow=ncol(predrast))
				cols = tr$row[i]:(tr$row[i]+dim(predv)[2]-1)
				a = try( v[,cols] <- predv )
				if (class(a) == 'try-error') {
					print(cols)
					print(dim(v))
				}
			} else {
				predrast <- writeValues(predrast, predv, tr$row[i])
			}
			pbStep(pb, i) 
		}
		pbClose(pb)
		
		if (filename == '') {
			predrast <- setValues(predrast, as.numeric(v))  # or as.vector
		} else {
			predrast <- writeStop(predrast)
		}
		
		return(predrast)
	}
)

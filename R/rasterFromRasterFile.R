# Author: Robert J. Hijmans
# Date : June 2008
# Version 0.9
# Licence GPL v3


.rasterFromRasterFile <- function(filename, band=1, type='RasterLayer') {
	valuesfile <- raster:::.setFileExtensionValues(filename, "raster")
	if (!file.exists( valuesfile )){
		stop( paste(valuesfile,  "does not exist"))
	}	
	
	filename <- raster:::.setFileExtensionHeader(filename, "raster")
	
	ini <- readIniFile(filename)
	ini[,2] = toupper(ini[,2]) 

	byteorder <- .Platform$endian
	nbands <- as.integer(1)
	band <- as.integer(band)
	bandorder <- "BIL"
	projstring <- ""
	minval <- NA
	maxval <- NA
	nodataval <- -Inf
	layernames <- ''
	zvalues <- ''
	
	isCat <- FALSE
	ratnames <- rattypes <- ratvalues <- NULL
	catlevels = matrix(NA)

	for (i in 1:length(ini[,1])) {
		if (ini[i,2] == "MINX") { xn <- as.numeric(ini[i,3]) } 
		else if (ini[i,2] == "MAXX") { xx <- as.numeric(ini[i,3]) } 
		else if (ini[i,2] == "MINY") { yn <- as.numeric(ini[i,3]) } 
		else if (ini[i,2] == "MAXY") { yx <- as.numeric(ini[i,3]) } 
		else if (ini[i,2] == "XMIN") { xn <- as.numeric(ini[i,3]) } 
		else if (ini[i,2] == "XMAX") { xx <- as.numeric(ini[i,3]) } 
		else if (ini[i,2] == "YMIN") { yn <- as.numeric(ini[i,3]) } 
		else if (ini[i,2] == "YMAX") { yx <- as.numeric(ini[i,3]) } 
		else if (ini[i,2] == "ROWS") { nr <- as.integer(ini[i,3]) }  
		else if (ini[i,2] == "COLUMNS") { nc <- as.integer(ini[i,3]) } 
		else if (ini[i,2] == "NROWS") { nr <- as.integer(ini[i,3]) } 
		else if (ini[i,2] == "NCOLS") { nc <- as.integer(ini[i,3]) } 
		
		else if (ini[i,2] == "MINVALUE") { try ( minval <-  as.numeric(unlist(strsplit(ini[i,3], ':'))), silent = TRUE ) }
		else if (ini[i,2] == "MAXVALUE") { try ( maxval <-  as.numeric(unlist(strsplit(ini[i,3], ':'))), silent = TRUE ) }
		else if (ini[i,2] == "VALUEUNIT") { try ( maxval <-  as.numeric(unlist(strsplit(ini[i,3], ':'))), silent = TRUE ) }

		else if (ini[i,2] == "CATEGORICAL") { try ( isCat <-  as.logical(unlist(strsplit(ini[i,3], ':'))), silent = TRUE ) }
				
		#else if (ini[i,2] == "RATROWS") { ratrows <- as.integer(ini[i,3]) }
		else if (ini[i,2] == "RATNAMES") { ratnames <- unlist(strsplit(ini[i,3], ':')) }
		else if (ini[i,2] == "RATTYPES") { rattypes <- unlist(strsplit(ini[i,3], ':')) }
		else if (ini[i,2] == "RATVALUES") { ratvalues <- unlist(strsplit(ini[i,3], ':')) }
		
		else if (ini[i,2] == "LEVELS") { try ( catlevels <-  unlist(strsplit(ini[i,3], ':')), silent = TRUE ) }
		
		else if (ini[i,2] == "NODATAVALUE") { nodataval <- as.numeric(ini[i,3]) } 
		else if (ini[i,2] == "DATATYPE") { inidatatype <- ini[i,3] } 
		else if (ini[i,2] == "BYTEORDER") { byteorder <- ini[i,3] } 
		else if (ini[i,2] == "NBANDS") { nbands <- as.integer(ini[i,3]) } 
		else if (ini[i,2] == "BANDORDER") { bandorder <- ini[i,3] }  
		else if (ini[i,2] == "PROJECTION") { projstring <- ini[i,3] } 
		else if (ini[i,2] == "LAYERNAME") { layernames <- ini[i,3] } 
		else if (ini[i,2] == "ZVALUES") { zvalues <- ini[i,3] } 
    }  
	
	if (projstring == 'GEOGRAPHIC') { projstring <- "+proj=longlat" }
	if (projstring == 'UNKNOWN') { projstring <- "NA" }

	
	if (band < 1) {
		band <- 1
		warning('band set to 1')
	} else if  (band > nbands) {
		band <- nbands
		warning('band set to ', nbands)
	}
	
	minval <- minval[1:nbands]
	maxval <- maxval[1:nbands]
	minval[is.na(minval)] <- Inf
	maxval[is.na(maxval)] <- -Inf
	
	if (type == 'RasterBrick') {
		x <- brick(ncols=nc, nrows=nr, xmn=xn, ymn=yn, xmx=xx, ymx=yx, crs=projstring)
		x@data@nlayers <-  as.integer(nbands)
		x@data@min <- minval
		x@data@max <- maxval
	} else {
		x <- raster(ncols=nc, nrows=nr, xmn=xn, ymn=yn, xmx=xx, ymx=yx, crs=projstring)
		x@data@band <- as.integer(band)
		x@data@min <- minval[band]
		x@data@max <- maxval[band]
	}
	
	if (isTRUE(any(isCat))) {
		x@data@isfactor = isCat
	
	# currently only for a single layer!
	
		rat <- data.frame(matrix(ratvalues, nrow=length(ratvalues) / length(ratnames)), stringsAsFactors=FALSE)
		colnames(rat) <- ratnames
		for (i in 1:ncol(rat)) {
			if (rattypes[i] == 'integer') {
				rat[, i] <- as.integer(rat[,i])
			} else if (rattypes[i] == 'numeric') {
				rat[, i] <- as.numeric(rat[,i])
			} else if (rattypes[i] == 'factor') {
				rat[, i] <- as.factor(rat[,i])
			}
		}
		x@data@isfactor <- TRUE
		x@data@attributes <- list(rat)
		
	}

	x@file@nbands <- as.integer(nbands)

	if (bandorder %in% c("BSQ", "BIP", "BIL")) {
		x@file@bandorder <- bandorder 
	}

	if (nchar(layernames) > 0) {
		lnames <- as.vector(unlist(strsplit(layernames, ':')))
		if (length(lnames) != nbands) {
			lnames <- rep( gsub(" ", "_", extension(basename(filename), "")), nbands)
		}
	} else {
		lnames <- gsub(" ", "_", extension(basename(filename), ""))
		if (nbands < 0) {
			lnames <- paste(lnames , 1:nbands, sep='_')
		}
	}

	if (zvalues != '') {
		names(zvalues) <- NULL
		zvalues <- unlist(strsplit(zvalues, ':'))
		zname <- zvalues[1]
		zvalues <- zvalues[-1]
		if (type == 'RasterBrick') {
			zvalues <- list(zvalues)
		} else {
			zvalues <- list(zvalues[band])
		}
		names(zvalues) <- zname
		x@z <- zvalues
	} 
	
	if (type == 'RasterBrick') {
		names(x) <- lnames
	} else {
		names(x) <- lnames[band]
	}
	
	dataType(x) <- inidatatype

	x@file@name <- filename
	x@data@haveminmax <- TRUE  # should check?
	x@file@nodatavalue <- nodataval

	if ((byteorder == "little") | (byteorder == "big")) { 
		x@file@byteorder <- byteorder 
	} 	
	x@data@fromdisk <- TRUE
	
	x@file@driver <- "raster"

#	if( dataSize(x) * (ncell(x) * nbands(x) + x@file@offset) !=  file.info(valuesfile)$size ) {
	if( (dataSize(x) * ncell(x) * nbands(x))  !=  file.info(valuesfile)$size ) {
		warning('size of values file does not match the number of cells (given the data type)')
	}
	
    return(x)
}

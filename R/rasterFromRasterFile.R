# Author: Robert J. Hijmans
# Date : June 2008
# Version 1.0
# Licence GPL v3


.getRat <- function(x, ratvalues, ratnames, rattypes) {

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
	x
	
}


.rasterFromRasterFile <- function(filename, band=1, type='RasterLayer', driver='raster', RAT=TRUE) {

	valuesfile <- .setFileExtensionValues(filename, driver)
	if (!file.exists( valuesfile )){
		stop( paste(valuesfile,  "does not exist"))
	}	
	
	filename <- .setFileExtensionHeader(filename, driver)
	
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
	zclass <- NULL
	
	isCat <- FALSE
	ratnames <- rattypes <- ratvalues <- NULL
	catlevels = matrix(NA)
	w <- getOption('warn')
	on.exit(options('warn' = w))
	
	#match(c("MINX", "MAXX", "MINY", "MAXY", "XMIN", "XMAX", "YMIN", "YMAX", "ROWS", "COLUMNS", "NROWS", "NCOLS"), toupper(ini[,2]))
	
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
		
	
		else if (ini[i,2] == "MINVALUE") { 
			options('warn'=-1) 
			try ( minval <-  as.numeric(unlist(strsplit(ini[i,3], ':'))), silent = TRUE ) 
			options('warn' = w)
		}
		else if (ini[i,2] == "MAXVALUE") { 
			options('warn'=-1) 
			try ( maxval <-  as.numeric(unlist(strsplit(ini[i,3], ':'))), silent = TRUE ) 
			options('warn' = w)
		}
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
		else if (ini[i,2] == "ZCLASS") { zclass <- ini[i,3] } 
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
		if (RAT) {
			if (isTRUE(isCat[band])) {
		# currently only for a single layer!
				try( x <- .getRat(ratvalues, ratnames, rattypes) )
			}
		}
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

		if (!is.null(zclass)) {
			if (zclass == 'Date') {
				try( zvalues <- as.Date(zvalues), silent=TRUE )
			} else {
				try( zvalues <- as(zvalues, zclass), silent=TRUE )
			}
		}
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

	x@data@haveminmax <- TRUE  # should check?
	x@file@nodatavalue <- nodataval

	if ((byteorder == "little") | (byteorder == "big")) { 
		x@file@byteorder <- byteorder 
	} 	
	x@data@fromdisk <- TRUE
	x@file@driver <- driver

#	if( dataSize(x) * (ncell(x) * nbands(x) + x@file@offset) !=  file.info(valuesfile)$size ) {
	
	if (driver == 'big.matrix') {
		require(bigmemory)
		x@file@name <- valuesfile
		dscfile <- extension(valuesfile, 'big.dsc')
		attr(x@file, 'big.matrix') <- attach.big.matrix(dscfile)
		
	} else {
		x@file@name <- filename
		if( (dataSize(x) * ncell(x) * nbands(x))  !=  file.info(valuesfile)$size ) {
			warning('size of values file does not match the number of cells (given the data type)')
		}
	}
	
    return(x)
}



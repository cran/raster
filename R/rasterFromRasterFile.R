# Author: Robert J. Hijmans, r.hijmans@gmail.com
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
	
	iscat = FALSE
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

		else if (ini[i,2] == "CATEGORICAL") { try ( iscat <-  as.logical(unlist(strsplit(ini[i,3], ':'))), silent = TRUE ) }
		else if (ini[i,2] == "LEVELS") { try ( catlevels <-  as.logical(unlist(strsplit(ini[i,3], ':'))), silent = TRUE ) }
		
		else if (ini[i,2] == "NODATAVALUE") { nodataval <- as.numeric(ini[i,3]) } 
		else if (ini[i,2] == "DATATYPE") { inidatatype <- ini[i,3] } 
		else if (ini[i,2] == "BYTEORDER") { byteorder <- ini[i,3] } 
		else if (ini[i,2] == "NBANDS") { nbands <- as.integer(ini[i,3]) } 
		else if (ini[i,2] == "BANDORDER") { bandorder <- ini[i,3] }  
		else if (ini[i,2] == "PROJECTION") { projstring <- ini[i,3] } 
		else if (ini[i,2] == "LAYERNAME") { layernames <- ini[i,3] } 
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
		x@data@isfactor = iscat
		if (iscat) { 
		#	x@data@levels = catlevels 
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
	if (type == 'RasterBrick') {
		layerNames(x) <- lnames
	} else {
		layerNames(x) <- lnames[band]
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

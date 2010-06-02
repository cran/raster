# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : October 2009
# Version 0.9
# Licence GPL v3


.rasterFromSAGAFile <- function(filename) {
	valuesfile <- .setFileExtensionValues(filename, "SAGA")
	if (!file.exists(valuesfile )){
		stop( paste(valuesfile,  "does not exist"))
	}	
	filename <- .setFileExtensionHeader(filename, "SAGA")
	
	ini <- readIniFile(filename)

	ini[,2] = toupper(ini[,2]) 

	byteorder <- .Platform$endian
	ncellvals <- -9
	projstring <- ""
	nodataval <- -Inf
	layernames <- ''
	toptobottom <- FALSE
	dfoffset <- as.integer(0)
	
	for (i in 1:length(ini[,1])) {
		if (ini[i,2] == "POSITION_XMIN") {xn <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "POSITION_YMIN") {yn <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "CELLCOUNT_Y") {nr <- as.integer(ini[i,3])} 
		else if (ini[i,2] == "CELLCOUNT_X") {nc <- as.integer(ini[i,3])} 
		else if (ini[i,2] == "CELLSIZE") {cellsize <- as.integer(ini[i,3])} 
		else if (ini[i,2] == "NODATA_VALUE") {nodataval <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "DATAFORMAT") {inidatatype <- ini[i,3]} 
		else if (ini[i,2] == "BYTEORDER_BIG") {byteorder <- as.logical(ini[i,3])} 
#		else if (ini[i,2] == "NCELLVALS") {ncellvals <- ini[i,3]} 
		else if (ini[i,2] == "NAME") {layernames <- ini[i,3]} 
		else if (ini[i,2] == "TOPTOBOTTOM") { toptobottom <-  as.logical(ini[i,3])}
		else if (ini[i,2] == "DATAFILE_OFFSET") { dfoffset <-  as.integer(ini[i,3])}
    }  
	

	xx <- xn + nc * cellsize - (0.5 * cellsize)
	xn <- xn - (0.5 * cellsize)
	yx <- yn + nr * cellsize - (0.5 * cellsize)
	yn <- yn - (0.5 * cellsize)
	
	x <- raster(ncols=nc, nrows=nr, xmn=xn, ymn=yn, xmx=xx, ymx=yx, crs=projstring)

	x@file@offset <- dfoffset
	x@file@toptobottom <- toptobottom

	if (nchar(layernames) > 1) {
		layernames <- unlist(strsplit(layernames, ':'))
	}
	x@layernames <- layernames
	shortname <- gsub(" ", "_", ext(basename(filename), ""))
	x <- .enforceGoodLayerNames(x, shortname)
	
	x@file@name <- .fullFilename(filename)
	x@data@haveminmax <- FALSE
	x@file@nodatavalue <- nodataval

	if (inidatatype == 'FLOAT') {
		dataType(x) <- 'FLT4S'
	}
	
	if (byteorder) { 
		x@file@byteorder <- 'big'
	} else  {
		x@file@byteorder <- 'little'
	}
	x@data@source <- 'disk'
	x@file@driver <- 'SAGA'
    return(x)
}




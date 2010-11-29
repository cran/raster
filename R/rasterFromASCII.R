# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : October 2009
# Version 0.9
# Licence GPL v3


.rasterFromASCIIFile <- function(filename) {
	
	splitasc <- function(s) {
		s <- trim(s)
		spl <- unlist(strsplit(s, ''))
		pos <- which(spl==' ')[1]
		first <- substr(s, 1, (pos-1))
		second <- substr(s, (pos+1), nchar(s))
		return(trim(c(first, second)))
	}
	
	filename <- trim(filename)
    if (!file.exists(filename)) { stop(paste(filename, " does not exist")) }
	con <- file(filename, "rt")
	lines <- readLines(con, n=6)
	close(con)
	ini <- lapply(lines, splitasc) 
	ini <- matrix(unlist(ini), ncol=2, byrow=TRUE)
	
	ini[,1] = toupper(ini[,1]) 
	for (i in 1:length(ini[,1])) {
		if (ini[i,1] == "NCOLS") {nc <- as.integer(ini[i,2])
		} else if (ini[i,1] == "NROWS") {nr <- as.integer(ini[i,2])
		} else if (ini[i,1] == "XLLCORNER") {xn <- as.numeric(ini[i,2])
		} else if (ini[i,1] == "YLLCORNER") {yn <- as.numeric(ini[i,2])
		} else if (ini[i,1] == "CELLSIZE") {d <- as.numeric(ini[i,2])
		} else if (ini[i,1] == "NODATA_VALUE") { try (nodataval <- as.numeric(ini[i,2]), silent=TRUE)
		}
    }  
	xx <- xn + nc * d
	yx <- yn + nr * d
	x <- raster(ncols=nc, nrows=nr, xmn=xn, ymn=yn, xmx=xx, ymx=yx, crs='')	
	x@data@fromdisk <- TRUE
	
	x@file@driver <- 'ascii'
	x@file@nodatavalue <- nodataval
    x@file@name <- filename
	return(x)
}



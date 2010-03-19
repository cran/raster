# Download geographic data and return as R object
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# License GPL3
# Version 0.9
# October 2008


getData <- function(name='GADM', download=TRUE, path='', ...) {
	path <- .getDataPath(path)
	if (name=='GADM') {
		.GADM(..., download=download, path=path)
	} else if (name=='SRTM') {
		.SRTM(..., download=download, path=path, ...)
	} else if (name=='alt') {
		.raster(..., name=name, download=download, path=path)
	} else if (name=='worldclim') {
		.worldclim(..., download=download, path=path)
	} else if (name=='ISO3') {
		.countries()[,c(2,1)]
	} else {
		stop(name, 'not recognized as a valid name')
	}
}

.countries <- function() {
	path <- paste(system.file(package="raster"), "/external", sep='')
	d <- read.csv(paste(path, "/countries.csv", sep=""), header=T, quote = "!@!")
	return(as.matrix(d))
}


.getCountry <- function(country='') {
#	country <- toupper(trim(country[1]))
	if (country == '') {
		stop('provide a 3 letter ISO country code')
	}
	cs <- .countries()
	try (cs <- toupper(cs))
	if (! country %in% cs[,2]) {
		if (country %in% cs[,3]) {
			i <- which(country==cs[,3])
			country <- cs[i,2]
		} else if (country %in% cs[,1]) {
			i <- which(country==cs[,1])
			country <- cs[i,2]
		} else {
			stop('provide a valid 3 letter ISO country code; you can get a list with: getData("ISO3")')
		}
	}
	return(country)
}


.getDataPath <- function(path) {
	path <- trim(path)
	if (path=='') {
		path <- .dataloc()
	} else {
		if (substr(path, nchar(path)-1, nchar(path)) == '//' ) {
			p <- substr(path, 1, nchar(path)-2)		
		} else if (substr(path, nchar(path), nchar(path)) == '/'  | substr(path, nchar(path), nchar(path)) == '\\') {
			p <- substr(path, 1, nchar(path)-1)
		} else {
			p <- path
		}
		if (!file.exists(p)) {
			stop('path does not exist: ', p)
		}
	}
	if (substr(path, nchar(path), nchar(path)) != '/' & substr(path, nchar(path), nchar(path)) != '\\') {
		path <- paste(path, "/", sep="")
	}
	return(path)
}


.GADM <- function(country, level, download, path) {
#	if (!file.exists(path)) {  dir.create(path, recursive=T)  }

	country <- .getCountry(country)
	
	if (missing(level)) {
		stop('provide a "level=" argument; levels can be 0, 1, or 2 for most countries, and higer for some')
	}

	
	filename <- paste(path, country, '_adm', level, ".RData", sep="")
#	theurl <- paste("http://www.r-gis.org/rgis/data/adm/", country, '_adm', level, ".RData", sep="")
	if (!file.exists(filename)) {
		if (download) {
			theurl <- paste("http://gadm.org/data/rda/", country, '_adm', level, ".RData", sep="")
			download.file(url=theurl, destfile=filename, method="auto", quiet = FALSE, mode = "wb", cacheOK = TRUE)
			if (!file.exists(filename))
				{ cat("\nCould not download file -- perhaps it does not exist \n") }
		} else {
			cat("\nFile not available locally. Use 'download = TRUE'\n")
		}
	}	
	if (file.exists(filename)) {
		thisenvir = new.env()
		data <- get(load(filename, thisenvir), thisenvir)
		return(data)
	} 
}


.worldclim <- function(var, res, lon, lat, path, download=TRUE) {
	if (!res %in% c(0.5, 2.5, 5, 10)) {
		stop('resolution should be one of: 0.5, 2.5, 5, 10')
	}
	if (res==2.5) { res <- '2-5' }
	if (!var %in% c('tmin', 'tmax', 'prec', 'bio')) {
		stop('var should be one of: tmin, tmax, prec, bio')
	}
	path <- paste(path, 'wc', res, '/', sep='')
	dir.create(path, showWarnings=FALSE)

	if (res==0.5) {
		lon <- min(180, max(-180, lon))
		lat <- min(90, max(-60, lat))
		rs <- raster(nrows=5, ncols=12, xmn=-180, xmx=180, ymn=-60, ymx=90 )
		row <- rowFromY(rs, lat) - 1
		col <- colFromX(rs, lon) - 1
		rc <- paste(row, col, sep='') 
		zip <- paste(var, '_', rc, '.zip', sep='')
		zipfile <- paste(path, zip, sep='')
		if (var  != 'bio') {
			bilfiles <- paste(var, 1:12, '_', rc, '.bil', sep='')
			hdrfiles <- paste(var, 1:12, '_', rc, '.hdr', sep='')
		} else {
			bilfiles <- paste(var, 1:19, '_', rc, '.bil', sep='')
			hdrfiles <- paste(var, 1:19, '_', rc, '.hdr', sep='')		
		}
		theurl <- paste('http://biogeo.berkeley.edu/worldclim1_4/tiles/cur/', zip, sep='')
	} else {
		zip <- paste(var, '_', res, 'm_bil.zip', sep='')
		zipfile <- paste(path, zip, sep='')
		if (var  != 'bio') {
			bilfiles <- paste(var, 1:12, '.bil', sep='')
			hdrfiles <- paste(var, 1:12, '.hdr', sep='')
		} else {
			bilfiles <- paste(var, 1:19, '.bil', sep='')
			hdrfiles <- paste(var, 1:19, '.hdr', sep='')	
		}
		theurl <- paste('http://biogeo.berkeley.edu/worldclim1_4/grid/cur/', zip, sep='')
	}
	files <- c(paste(path, bilfiles, sep=''), paste(path, hdrfiles, sep=''))
	fc <- sum(file.exists(files))
	if (fc < 24) {
		if (!file.exists(zipfile)) {
			if (download) {
				download.file(url=theurl, destfile=zipfile, method="auto", quiet = FALSE, mode = "wb", cacheOK = TRUE)
				if (!file.exists(zipfile))	{ 
					cat("\n Could not download file -- perhaps it does not exist \n") 
				}
			} else {
				cat("\nFile not available locally. Use 'download = TRUE'\n")
			}
		}	
		unzip(zipfile, exdir=dirname(zipfile))
		for (h in paste(path, hdrfiles, sep='')) {
			x <- readLines(h)
			x <- c(x[1:14], 'PIXELTYPE     SIGNEDINT', x[15:length(x)])
			writeLines(x, h)
		}
	}
	st <- stack(paste(path, bilfiles, sep=''))
	return(st)
}


.raster <- function(country, name, mask=TRUE, path, download, ...) {

	country <- .getCountry(country)

	
	path <- .getDataPath(path)
	if (mask) {
		mskname <- '_msk_'
		mskpath <- 'msk_'
	} else {
		mskname<-'_'
		mskpath <- ''		
	}
	filename <- paste(path, country, mskname, name, ".grd", sep="")
#	theurl <- paste("http://www.r-gis.org/rgis/data/adm/", country, '_adm', level, ".RData", sep="")

	#http://diva-gis.org/data/msk_alt/MEX_msk_alt.zip
	if (!file.exists(filename)) {
		zipfilename <- filename
		ext(zipfilename) <- '.zip'
		if (!file.exists(zipfilename)) {
			if (download) {
				theurl <- paste("http://diva-gis.org/data/", mskpath, name, "/", country, mskname, name, ".zip", sep="")
				download.file(url=theurl, destfile=zipfilename, method="auto", quiet = FALSE, mode = "wb", cacheOK = TRUE)
				if (!file.exists(zipfilename))	{ cat("\nCould not download file -- perhaps it does not exist \n") }
			} else {
				cat("\nFile not available locally. Use 'download = TRUE'\n")
			}
		}
		unzip(zipfilename, exdir=dirname(zipfilename))
		file.remove(zipfilename)
	}	
	if (file.exists(filename)) { 
		rs <- raster(filename)
		projection(rs) <- "+proj=longlat +datum=WGS84"
		return(rs)
	}	
}


.SRTM <- function(lon, lat, download, path) {
	lon <- min(180, max(-180, lon))
	lat <- min(60, max(-60, lat))
	rs <- raster(nrows=24, ncols=72, xmn=-180, xmx=180, ymn=-60, ymx=60 )
	row <- rowFromY(rs, lat)
	col <- colFromX(rs, lon)
	if (row < 10) { row <- paste('0', row, sep='') }
	if (col < 10) { col <- paste('0', col, sep='') }
	
	f <- paste('srtm_', col, '_', row, sep="")
	zipfilename <- paste(path, "/", f, ".ZIP", sep="")
	tiffilename <- paste(path, "/", f, ".TIF", sep="")
	
	if (!file.exists(tiffilename)) {
		if (!file.exists(zipfilename)) {
			if (download) { 
				theurl <- paste("http://hypersphere.telascience.org/elevation/cgiar_srtm_v4/tiff/zip/", f, ".ZIP", sep="")
				download.file(url=theurl, destfile=zipfilename, method="auto", quiet = FALSE, mode = 'wb', cacheOK = TRUE)
			} else {cat('file not available locally, use download=TRUE\n') }	
		}
		if (file.exists(zipfilename)) { 
			unzip(zipfilename, exdir=dirname(zipfilename))
			file.remove(zipfilename)
		}	
	}
	if (file.exists(tiffilename)) { 
		rs <- raster(tiffilename)
		projection(rs) <- "+proj=longlat +datum=WGS84"
		return(rs)
	}	
}


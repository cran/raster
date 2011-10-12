
.onLoad <- function(lib, pkg)  {
	pkg.info <- drop(read.dcf(file=system.file("DESCRIPTION", package=pkg), fields=c("Version","Date")))
	cat(paste(pkg, " version ", pkg.info["Version"], " (", pkg.info["Date"], ")\n", sep=""))
	tst <- try( removeTmpFiles(), silent=TRUE )
#	if (.usecluster()) {
#		beginCluster()
#	} else {
		options( rasterCluster=FALSE )
#	}

	library.dynam("raster", pkg, .libPaths())	

	return(invisible(0))
}


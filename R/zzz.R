
.onLoad <- function(lib, pkg)  {
	pkg.info <- drop(read.dcf(file=system.file("DESCRIPTION", package=pkg), fields=c("Version","Date")))
	packageStartupMessage(paste(pkg, " version ", pkg.info["Version"], " (", pkg.info["Date"], ")", sep=""))

	#library.dynam("raster", pkg, lib)	
	
	tst <- try( removeTmpFiles(), silent=TRUE )

	return(invisible(0))
}


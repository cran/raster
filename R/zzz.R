
.onLoad <- function(lib, pkg)  {
	
	pkg.info <- utils::packageDescription('raster') 
	packageStartupMessage(paste("raster ", pkg.info[["Version"]], " (", pkg.info["Date"], ")", sep=""))

	tst <- try( removeTmpFiles(), silent=TRUE )
	return(invisible(0))
}



.onLoad <- function(lib, pkg)  {
	pkg.info <- drop(read.dcf(file=system.file("DESCRIPTION", package=pkg), fields=c("Version","Date")))
	cat(paste("\n",pkg, " version ", pkg.info["Version"], " (", pkg.info["Date"], ")\n", sep=""))
	tst <- try( removeTmpFiles(), silent=TRUE )
	return(invisible(0))
}


# Author: Shaun Walbridge
# prevent directory colisions on multiuser machines by generating a unique dir

.tmppath <- function() {
# when Sys.info is NULL, use this default
	extension <- 'user'
	s <- Sys.info()
	if (!is.null(s)) {
		# get userid from system, to generate temporary directory name
		user <- s[["user"]]
		if (!is.null(user)) {
			extension <- user
		}
	}
#	d <- paste(dirname(tempdir()), '/R_raster_tmp/', extension, '/', sep="")
    d <- paste(dirname(tempdir()), '/R_raster_', extension, '/', sep="")	
	return(d)
}


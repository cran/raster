# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.9
# Licence GPL v3



'NAvalue<-' <- function(x, value) {
	x@file@nodatavalue <- value
	return(x)
}

'NAvalue' <- function(x, value) {
	return(x@file@nodatavalue)
}

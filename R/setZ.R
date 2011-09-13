# Robert J. Hijmans
# June 2011
# Version 1.0
# Licence GPL v3



setZ <- function(x, z, name='time') {
	stopifnot(length(z) == nlayers(x))
	x@z[[1]] <- z
	x@zname <- trim(name)
	x
}


getZ <- function(x) {
	z <- try(slot(x, 'z'), silent=TRUE)
	if (class(z) == 'try-error') return(NULL)
	if (length(z) < 1) return(NULL)
	return(z[[1]])
}



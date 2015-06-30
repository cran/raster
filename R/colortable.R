

colortable <- function(x) {
	x@legend@colortable
}


'colortable<-' <- function(x, value) {
	# for now assuming values are between 0 and 255!!
	x@legend@colortable <- value
	return(x)
}

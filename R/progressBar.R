# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: Sept 2008
# Version 0.9
# Licence GPL v3


pbCreate <- function(nsteps, type='text', style=3) {
	if (type=='text') {
		pb <- txtProgressBar(min=0, max=nsteps, style=style)
	} else if (type %in% c('window', 'tcltk', 'windows')) {
		tit <- paste(' Progress (', nsteps, ' steps)', sep='')
		#if (.Platform$OS.type == "windows" ) {
		#	pb <- winProgressBar(title=tit, min=0 , max=nsteps, width = 300, label='starting')
		#} else {
		
		require(tcltk)
		pb <- tkProgressBar(title=tit, min=0, max=nsteps, width = 300, label='starting')
		
		#}
	} else {
		pb <- 'none'
	}
	attr(pb, "starttime") <- proc.time()
	return(pb)
}



pbStep <- function(pb, step=NULL, label='') {
	pbclass <- class(pb)
	if (pbclass=="txtProgressBar") {
		if (is.null(step)) { step = pb$getVal() + 1 }
		setTxtProgressBar(pb, step)
	} else if (pbclass=="tkProgressBar") {
		if (is.null(step)) { step = pb$getVal() + 1 }
		setTkProgressBar(pb, step, label=paste(label, step))	
	#} else if (pbclass=="winProgressBar") {
	#	if (is.null(step)) { step <- getWinProgressBar(pb)+1  }
	#	setWinProgressBar(pb, step, label=paste(label, step))	
	} 
}

pbClose <- function(pb, timer) {
	pbclass <- class(pb)
	if (pbclass=="txtProgressBar") {
		cat("\n")
		close(pb)
	} else if (pbclass=="tkProgressBar") {
		close(pb)
	} else if (pbclass=="winProgressBar") {
		close(pb)
	} 
	if (missing(timer)) {
		timer <- .timer()		
	}
	if (timer) {
		elapsed <- (proc.time() - 	attr(pb, "starttime"))[3]
		cat(elapsed, 'seconds\n')
	}
}

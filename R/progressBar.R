# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: Sept 2008
# Version 0.9
# Licence GPL v3


pbCreate <- function(nsteps, type, style=3) {
	if (type=='text') {
		pb <- txtProgressBar(min=0, max=nsteps, style=style)
	} else if (type == 'tcltk') {
		require(tcltk)
		pb <- tkProgressBar(title="progress bar", min=0, max=nsteps, width = 300)
	} else if (type == 'windows') {
		if (substr( R.Version()$platform, 1, 7) == "i386-pc" ) {
			pb <- winProgressBar(title="progress bar", min=0 , max=nsteps, width = 300)
		} else {
			pb <- tkProgressBar(title="progress bar", min=0, max=nsteps, width = 300)
		}
	} else {
		pb <- 'none'
	}
	attr(pb, "starttime") <- proc.time()
	return(pb)
}


pbStep <- function(pb, step, label='step') {
	pbclass <- class(pb)
	if (pbclass=="txtProgressBar") {
		setTxtProgressBar(pb, step)
	} else if (pbclass=="tkProgressBar") {
		setTkProgressBar(pb, step, label=paste(label, step))	
	} else if (pbclass=="winProgressBar") {
		setWinProgressBar(pb, step, title=paste(label, step))	
	} 
}

pbClose <- function(pb, time=FALSE) {
	pbclass <- class(pb)
	if (pbclass=="txtProgressBar") {
		cat("\n")
		close(pb)
	} else if (pbclass=="tkProgressBar") {
		close(pb)
	} else if (pbclass=="winProgressBar") {
		close(pb)
	} 
	if (time) {
		elapsed <- (proc.time() - 	attr(pb, "starttime"))[3]
		cat('Finished in ', elapsed, ' seconds\n')
	}
}

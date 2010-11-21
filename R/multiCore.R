# Author: Matteo Mattiuzzi and Robert J. Hijmans, r.hijmans@gmail.com
# Date : November 2010
# Version 1.0
# Licence GPL v3

beginCluster <- function(n, type) {
	if (! require(snow) ) {
		stop('you need to install the "snow" package')
	}

	if (exists('raster_Cluster_raster_Cluster', envir=.GlobalEnv)) {
		endCluster()
	}

	if (missing(n)) {
		n <- .detectCores()
		cat(n, 'cores detected\n')
	}
	if (n > 1) {
		if (missing(type)) {
			type <- getClusterOption("type")
			cat('cluster type:', type, '\n')
		}
		cl <- makeCluster(n, type) 
		cl <- .addPackages(cl)
		assign('raster_Cluster_raster_Cluster', cl, envir = .GlobalEnv)
		options(rasterCluster = TRUE)
	} else {
		stop('only 1 core detected. No cluster made')	
		options(rasterCluster = FALSE)
	}
}


endCluster <- function() {
	options(rasterCluster = FALSE)
	if (exists('raster_Cluster_raster_Cluster', envir=.GlobalEnv)) {
		stopCluster( get('raster_Cluster_raster_Cluster', envir=.GlobalEnv) )
		rm('raster_Cluster_raster_Cluster', envir=.GlobalEnv)
	}
}


.doCluster <- function() {
	if ( isTRUE( options('rasterCluster')[[1]] ) ) {
		if (exists('raster_Cluster_raster_Cluster', envir=.GlobalEnv)) {
			return(TRUE)
		}
	} 
	return(FALSE)
}


.getCluster <- function() {
	cl <- get('raster_Cluster_raster_Cluster', envir=.GlobalEnv)
	cl <- .addPackages(cl, exclude=c('raster', 'sp'))
	rm('raster_Cluster_raster_Cluster', envir=.GlobalEnv)
	options( rasterCluster = FALSE )
	return(cl)
}


.returnCluster <- function(cl) {
	if (missing(cl)) { 
		warning('no cluster returned' )
	} else {
		assign('raster_Cluster_raster_Cluster', cl, envir = .GlobalEnv)
		options(rasterCluster = TRUE)
	}
}


.addPackages <- function(cl, exclude=NULL) {
	pkgs <- .packages()
	i <- which( pkgs %in% c(exclude, "stats", "graphics", "grDevices", "utils", "datasets", "methods", "base") )
	pkgs <- rev( pkgs[-i] )
	for ( pk in pkgs ) {
		clusterCall(cl, library, pk, character.only=TRUE )
	}
	return(cl)
}


.detectCores <- function(all.tests = FALSE) {

	multicoreDetectCores <- function(all.tests = FALSE) {
	# taken from pkg multicore:
	# detect the number of [virtual] CPUs (cores)
	
	# feel free to add tests - those are the only ones I could test [SU]
		systems <- list(darwin  = "/usr/sbin/sysctl -n hw.ncpu 2>/dev/null",
					linux   = "grep processor /proc/cpuinfo 2>/dev/null|wc -l",
					irix    = c("hinv |grep Processors|sed 's: .*::'", "hinv|grep '^Processor '|wc -l"),
					solaris = "/usr/sbin/psrinfo -v|grep 'Status of.*processor'|wc -l")
					
		for (i in seq(systems)) {
			if(all.tests || length(grep(paste("^", names(systems)[i], sep=''), R.version$os))) {
				for (cmd in systems[i]) {
					a <- gsub("^ +", "", system(cmd, TRUE)[1])
					if (length(grep("^[1-9]", a))) {
						return(as.integer(a))
					}
				}
			}	
		}		
		return(1)
	}
	
	if (.Platform$OS.type == 'windows') {
		nn <- length(readRegistry("HARDWARE\\DESCRIPTION\\System\\CentralProcessor", maxdepth=1))
	} else {
		nn <- multicoreDetectCores(all.tests)
	}
	return(nn)
}

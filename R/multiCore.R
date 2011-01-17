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

	if (missing(type)) {
		type <- getClusterOption("type")
		cat('cluster type:', type, '\n')
	}

	cl <- makeCluster(n, type) 
	cl <- .addPackages(cl)
	options(rasterClusterObject = cl)
	options(rasterClusterCores = length(cl))
	options(rasterCluster = TRUE)
}


endCluster <- function() {
	options(rasterCluster = FALSE)
	cl <- options('rasterClusterObject')[[1]]
	if (! is.null(cl)) {
		stopCluster( cl )
		options(rasterClusterObject = NULL)
	}
}


.doCluster <- function() {
	if ( isTRUE( getOption('rasterCluster')) ) {
		return(TRUE)
	} 
	return(FALSE)
}


getCluster <- function() {
	cl <- getOption('rasterClusterObject')
	if (is.null(cl)) { stop('no cluster available, first use "beginCluster"') }
	cl <- .addPackages(cl, exclude=c('raster', 'sp'))
	options( rasterClusterObject = cl )
	options( rasterCluster = FALSE )
	return(cl)
}


returnCluster <- function() {
	cl <- getOption('rasterClusterObject')
	if (is.null(cl)) { stop('no cluster available') }
	options( rasterCluster = TRUE )
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
		if (!exists('readRegistry')) { readRegistry <- function(...)(1) } #This is a hack to stop the check NOTE: .detectCores: no visible global function definition for ‘readRegistry’
		nn <- length(readRegistry("HARDWARE\\DESCRIPTION\\System\\CentralProcessor", maxdepth=1))
	} else {
		nn <- multicoreDetectCores(all.tests)
	}
	return(nn)
}

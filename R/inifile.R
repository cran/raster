# Authors: Robert J. Hijmans 
# contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3

readIniFile <- function(filename, token='=', commenttoken=';', aslist=FALSE, case) {

	strSplitOnFirstToken <- function(s, token="=") {
		pos <- which(strsplit(s, '')[[1]]==token)[1]
		if (is.na(pos)) {
			return(c(trim(s), NA)) 
		} else {
			first <- substr(s, 1, (pos-1))
			second <- substr(s, (pos+1), nchar(s))
			return(trim(c(first, second)))
		}
	}

	strsp <- function(s){ strSplitOnFirstToken(s, token=token) }
	
	strSplitComment <- function(s,  token=";") { 
		# ";" is the start of a comment .
		strSplitOnFirstToken(s, token=";") 
	}
	strspcom <- function(s){ strSplitComment(s, token=commenttoken) }
	
	
    if (!file.exists(filename)) { stop(paste(filename, " does not exist")) }
	
	Lines <- readLines(filename,  warn = FALSE)
	Lines <- trim(Lines)
	
	ini <- lapply(Lines, strspcom) 
	
	Lines <- matrix(unlist(ini), ncol=2, byrow=TRUE)[,1]
	ini <- lapply(Lines, strsp) 
	
 	ini <- matrix(unlist(ini), ncol=2, byrow=TRUE)
	ini <- subset(ini, ini[,1] != "")

	ns <- length(which(is.na(ini[,2])))
	if (ns > 0) {
		sections <- c(which(is.na(ini[,2])), length(ini[,2]))

# here I should check whether the section text is enclused in [ ]. If not, it is junk text that should be removed, rather than used as a section
		ini <- cbind("", ini)
		for (i in 1:(length(sections)-1)) {
			ini[sections[i]:(sections[i+1]), 1] <- ini[sections[i],2]
		}	
		ini[,1] <- gsub("\\[", "", ini[,1])
		ini[,1] <- gsub("\\]", "", ini[,1])
		sections <- sections[1:(length(sections)-1)]
		ini <- ini[-sections,]
	} else {
		ini <- cbind("", ini)	
	}
		
	if (!missing(case)) {
		ini <- case(ini)
	}	
		
	colnames(ini) <- c("section", "name", "value")
	
	if (aslist) {
		ini <- .iniToList(ini)
	}
	
	return(ini)
}


.iniToList <- function(ini) {
	un <- unique(ini[,1])
	LST <- list()
	for (i in 1:length(un)) {
		sel <- ini[ini[,1] == un[i], 2:3]
		lst <- as.list(sel[,2])
		names(lst) <- sel[,1]
		LST[[i]] <- lst
	}
	names(LST) <- un
	return(LST)
}




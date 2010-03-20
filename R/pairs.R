 
 if (!isGeneric("pairs")) {
	setGeneric("pairs", function(x, ...)
		standardGeneric("pairs"))
}
 

setMethod('pairs', signature(x='RasterStackBrick'), 
	function(x, hist=TRUE, cor=TRUE, maxpixels=100000, cex=0.5, main='') {
	
		panelhist <- function(x,...)	{
			usr <- par("usr"); on.exit(par(usr))
			par(usr = c(usr[1:2], 0, 1.5) )
			h <- hist(x, plot = FALSE)
			breaks <- h$breaks
			nB <- length(breaks)
			y <- h$counts
			y <- y/max(y)
			rect(breaks[-nB], 0, breaks[-1], y, col="green")
		}
		
		panelcor <- function(x, y,...) {
			usr <- par("usr")
			on.exit(par(usr))
			par(usr = c(0, 1, 0, 1))
			r <- abs(cor(x, y))
			txt <- format(c(r, 0.123456789), digits=2)[1]
			text(0.5, 0.5, txt, cex = max(0.5, r * 2))
		}
	
		if (hist) {dp <- panelhist} else {dp <- NULL}
		if (cor) {up <- panelcor} else {up <- NULL}
	
	
		d = sampleRegular(x, maxpixels)
		pairs(d, main=main, cex=cex, upper.panel=up, diag.panel=dp)
	}
)


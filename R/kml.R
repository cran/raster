# Derived, with only minor changes, from functions GE_SpatialGrid and kml Overlay 
# in the maptools package. These were written by Duncan Golicher, David Forrest and Roger Bivand 
# Adaptation for the raster packcage by Robert J. Hijmans, r.hijmans@gmail.com
# Date : March 2009
# Version 0.9
# Licence GPL v3

KML <- function (raster, filename, col=rainbow(255), maxpixels=100000) {
    if (! .couldBeLonLat(raster)) { 
        stop("raster must be in geographical coordinates")
	}
	raster <- sampleRegular(raster, size=maxpixels, asRaster = TRUE, corners=TRUE)

	imagefile <- filename
	ext(imagefile) <- '.png'
	kmlfile <- filename
	ext(kmlfile) <- '.kml'

	png(file = imagefile, width=ncol(raster), height=nrow(raster), bg="transparent")
	par(mar=c(0,0,0,0), xaxs="i", yaxs="i")
	image(raster, col=col)
	dev.off()

	name <- layerNames(raster)[1]
	if (name == "") { name <- 'raster' }
    bb <- extent(raster)
    W <- xmin(bb)
    E <- xmax(bb)
    S <- ymin(bb)
    N <- ymax(bb)
	
    kmlheader <- c("<?xml version='1.0' encoding='UTF-8'?>", 
        "<kml xmlns='http://earth.google.com/kml/2.0'>", "<GroundOverlay>")
    kmname <- paste("<name>", name, "</name>", sep = "")
	
    icon <- paste("<Icon><href>", basename(imagefile), "</href><viewBoundScale>0.75</viewBoundScale></Icon>", 
        sep = "")
    latlonbox <- paste("<LatLonBox><north>", N, "</north><south>", 
        S, "</south><east>", E, "</east><west>", W, "</west></LatLonBox>", 
        sep = "")
    footer <- "</GroundOverlay></kml>"
    x <- (kmlheader)
    x <- append(x, kmname)
    x <- append(x, icon)
    x <- append(x, latlonbox)
    x <- append(x, footer)
    cat(paste(x, sep = "", collapse = "\n"), file = kmlfile, sep = "")
	
	kmz <- kmlfile
	ext(kmz) <- '.kmz'
	rzipcmd <- Sys.getenv('R_ZIPCMD')
	if (rzipcmd != "") {
		com1 <- paste(rzipcmd, '-a', kmz, kmlfile, imagefile)
	} else {
		com1 <- paste("7za a -tzip", kmz, kmlfile, imagefile)
		try(	sss <- system(com1, intern=TRUE) , silent = TRUE)
	}
	
}



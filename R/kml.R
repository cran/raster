# Derived, with only minor changes, from functions GE_SpatialGrid and kml Overlay 
# in the maptools package. These were written by Duncan Golicher, David Forrest and Roger Bivand 
# Adaptation for the raster packcage by Robert J. Hijmans, r.hijmans@gmail.com
# Date : March 2009
# Version 0.9
# Licence GPL v3


if (!isGeneric("KML")) {
	setGeneric("KML", function(x, ...)
		standardGeneric("KML"))
}	


setMethod('KML', signature(x='RasterLayer'), 

function (x, filename, col=rev(terrain.colors(255)), maxpixels=100000, zip='', ...) {

    if (! .couldBeLonLat(x)) { 
        stop("CRS of x must be longitude / latitude")
	}
	
	if (nlayers(x) > 1) {
		x <- x[[1]]
	}
	stopifnot(hasValues(x))

	if (missing(filename)) { 
		filename <- extension(basename(rasterTmpFile('G_')), '.kml')
	}
		
	x <- sampleRegular(x, size=maxpixels, asRaster = TRUE, useGDAL=TRUE)

	imagefile <- filename
	extension(imagefile) <- '.png'
	kmlfile <- filename
	extension(kmlfile) <- '.kml'

	png(filename = imagefile, width=max(480, ncol(x)), height=max(480, nrow(x)), bg="transparent")
	par(mar=c(0,0,0,0))
	if (R.Version()$minor >= 13) {
		image(x, col=col, axes=FALSE, useRaster=TRUE, ...)
	} else {
		image(x, col=col, axes=FALSE, ...)	
	}
	dev.off()

	name <- layerNames(x)[1]
	if (name == "") { name <- 'x' }
    kml <- c('<?xml version="1.0" encoding="UTF-8"?>', '<kml xmlns="http://www.opengis.net/kml/2.2">', "<GroundOverlay>")
    kmname <- paste("<name>", name, "</name>", sep = "")
    icon <- paste("<Icon><href>", basename(imagefile), "</href><viewBoundScale>0.75</viewBoundScale></Icon>", sep = "")
    e <- extent(x)
    latlonbox <- c("\t<LatLonBox>", paste("\t\t<north>", e@ymax, "</north><south>",  e@ymin, "</south><east>", e@xmax, "</east><west>", e@xmin, "</west>", sep = ""), "\t</LatLonBox>")
    footer <- "</GroundOverlay></kml>"
	
    kml <- c(kml, kmname, icon, latlonbox, footer)
	
    cat(paste(kml, sep="", collapse="\n"), file=kmlfile, sep="")
	
	.zipKML(kmlfile, imagefile, zip)
}
)




\name{KML}

\alias{KML}
\alias{KML,RasterLayer-method}
\alias{KML,RasterStackBrick-method}

\title{Write a KML or KMZ file}

\description{
Export raster data to a KML file and an accompanying PNG image file. Multi-layer objects can be used to create an animation. 
The function attempts to combine these into a single (and hence more convenient) KMZ file (a zip file containing the KML and PNG files). 
}

\usage{
KML(x, ...) 
}

\arguments{
\item{x}{Raster* object}
\item{...}{Additional arguments (see Details)}
}


\details{
You can use the following additional argumetns: 

\tabular{rll}{
\tab filename \tab output filename \cr
\tab time \tab character vector with time lables for multilayer objects. The lenght of this vector should be nlayers(x) to indicate "when" or nlayers(x)+1 to indicate "begin-end"\cr
\tab col \tab color scheme to be used (see \link[graphics]{image} \cr
\tab maxpixels \tab  maximum number of pixels. If ncell(raster) > maxpixels, sampleRegular is used to reduce the number of pixels \cr
\tab zip \tab If there is no zip program on your path (on windows), you can supply the full path to a zip.exe here, in order to make a KMZ file \cr
\tab ... \tab Additional arguments that can be passed to \link[graphics]{image} \cr
}
}

\value{
None. Used for the side-effect files written to disk. 
}

\author{
This function was adapted for the raster package by Robert J. Hijmans, with ideas from Tony Fischbach, and based on functions in the maptools package by Duncan Golicher, David Forrest and Roger Bivand.
}


\examples{
\dontrun{
# Meuse data from the sp package
data(meuse.grid)
b <- rasterFromXYZ(meuse.grid)
projection(b) <- "+init=epsg:28992 +towgs84=565.237,50.0087,465.658,-0.406857,0.350733,-1.87035,4.0812
                  +proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000
				  +y_0=463000 +ellps=bessel +units=m +no_defs"
				  
# transform to longitude/latitude
p <- projectRaster(b, crs="+proj=longlat +datum=WGS84", method='ngb')
KML(p, file='meuse.kml')
}
}

\keyword{spatial}

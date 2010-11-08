\name{KML}

\alias{KML}

\title{Write a KML file}

\description{
Export raster data to a KML file
}

\usage{
KML(raster, filename, col=rainbow(255), maxpixels=100000) 
}

\arguments{
\item{raster}{RasterLayer}
\item{filename}{output filename}
\item{col}{color scheme to be used}
\item{maxpixels}{maximum number of pixels. If ncell(raster) > maxpixels, sampleRegular is used to reduce the number of pixels}
}


\value{
None. But there is the side-effect of two files written to disk. 
}

\author{
adapted for the raster package by Robert J. Hijmans; based on functions in the maptools package by Duncan Golicher, David Forrest and Roger Bivand 
}


\keyword{spatial}

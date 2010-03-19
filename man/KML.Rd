\name{KML}

\alias{KML}

\title{KML}

\description{
Export raster to a KML file
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
None. But two files are written to disk. 
}

\author{adapted for the raster package by Robert J. Hijmans; based on functions in the maptools package by Duncan Golicher, David Forrest and Roger Bivand }


\keyword{spatial}

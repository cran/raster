\name{KML}

\alias{KML}

\title{Write a KML or KMZ file}

\description{
Export raster data to a KML file and an accompanying PNG image file, or to a single KMZ file (a zip file containing the KML and PNG files). 
}

\usage{
KML(x, filename, col=rainbow(255), maxpixels=100000, zip='') 
}

\arguments{
\item{x}{RasterLayer}
\item{filename}{output filename}
\item{col}{color scheme to be used}
\item{maxpixels}{maximum number of pixels. If ncell(raster) > maxpixels, sampleRegular is used to reduce the number of pixels}
\item{zip}{If there is no zip program on your path (on windows), you can supply the full path to a zip.exe here, in order to make a KMZ file}
}

\value{
None. But there is the side-effect of two files written to disk. 
}

\author{
Adapted for the raster package by Robert J. Hijmans; based on functions in the maptools package by Duncan Golicher, David Forrest and Roger Bivand 
}

\keyword{spatial}

\name{copyRasterFile}

\alias{copyRasterFile}
\alias{moveRasterFile}
\alias{removeRasterFile}

\title{Copy or move a raster file}

\description{
Move, copy or remove 'raster' format files that are associated with the RasterLayer object.
}

\usage{
moveRasterFile(raster, filename, overwrite=FALSE)
copyRasterFile(raster, filename, overwrite=FALSE)
removeRasterFile(raster)
}

\arguments{
\item{raster}{RasterLayer object that is associated with a file on disk}
\item{filename}{New filename}
\item{overwrite}{Logical. If \code{TRUE} existing files are overwritten}
}

\value{
a RasterLayer
}

\details{
\code{removeRasterFile} can also take a filename as argument in stead of a RasterLayer. 
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{saveAs}} }  

\examples{ 
r <- raster(system.file("external/test.grd", package="raster"))
r <- saveAs(r, filename='test.grd', format='raster', overwrite=TRUE)
rc <- copyRasterFile(r, filename='test2.grd', overwrite=TRUE)
rm <- moveRasterFile(r, filename='test3.grd', overwrite=TRUE)
}

\keyword{ spatial }

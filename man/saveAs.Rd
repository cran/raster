\name{saveAs}

\alias{saveAs}
\alias{saveAs,RasterLayer,character-method}
\alias{saveAs,RasterStackBrick,character-method}

\title{Save a Raster* object to a new file}

\description{
Save a RasterLayer or RasterBrick to a new file; for example a file of another format (format). You can also use this function to save a
RasterStack to a single (muti-band) file.
}

\usage{
saveAs(x, filename, ...)
}

\arguments{
  \item{x}{RasterLayer or RasterBrick object}
  \item{filename}{Character. Output filename}  
  \item{...}{additional arguments. See Details.}  
}

\details{
The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
 }
}

\value{
A RasterLayer or RasterBrick object. This primary use of this function is its side-effect of writing a file to disk.
}

\seealso{ \code{\link[raster]{writeFormats}},  \code{\link[raster]{writeRaster}} }

\author{Robert J. Hijmans}

\examples{ 
r1 <- raster(system.file("external/test.grd", package="raster"))
r2 <- saveAs(r1, 'test.tif', format='GTiff')
}

\keyword{ spatial }

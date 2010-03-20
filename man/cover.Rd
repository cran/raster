\name{cover}
\docType{methods}

\alias{cover}
\alias{cover,RasterLayer,RasterLayer-method}

\title{ Cover }
\description{
 Replace \code{NA} values in the first RasterLayer (x) with the values of the second RasterLayer (y), and so forth for additional RasterLayers.
}


\section{Methods}{
\describe{
\code{cover(x, y, ..., filename="") }
  \item{\code{x}}{ RasterLayer object }
  \item{\code{y}}{ RasterLayer object }
  \item{\code{...}}{ Additional RasterLayer objects or other arguments (see Details) }
  \item{\code{filename}}{ filename for the output RasterLayer}
}}

\details{
The function returns a RasterLayer with the values of the second RasterLayer (\code{y}) where the first RasterLayer (\code{x}) has \code{NA} values, and the values of the first RasterLayer elsewhere.

\tabular{rll}{
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
 }
}

\value{
A new RasterLayer object, and in some cases the side effect of a new file on disk.
}

\author{Robert J. Hijmans}

\examples{
r1 <- raster(ncols=36, nrows=18)
r1[] <- 1:ncell(r1)
r2 <- setValues(r1, runif(ncell(r1)))
r2[r2<0.5] <- NA
r3 <- cover(r2, r1)
}	

\keyword{methods}
\keyword{spatial}

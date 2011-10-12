\name{direction}

\alias{direction}
\alias{direction,RasterLayer-method}

\title{Direction}

\description{
The direction (azimuth) to or from the nearest cell that is not \code{NA}. The direction unit is in radians, unless you use argument \code{degrees=TRUE}. 
}

\usage{
direction(x, ...) 
}

\arguments{
\item{x}{a RasterLayer object}
\item{...}{additional arguments. See Details.}  
}


\details{
The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{filename}  \tab Filename for the output RasterLayer \cr
  \tab \code{from} \tab Logical. Default is \code{FALSE}. If \code{TRUE}, the direction from (instead of to)  the nearest cell that is not \code{NA} is returned \cr
  \tab \code{degrees}  \tab Logical. If \code{FALSE} (the default) the unit of direction is radians. \cr

  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{progress} \tab Character. "text", "window", or "" (the default, no progress bar)  \cr
 }
}

\author{Robert J. Hijmans }

\value{A RasterLayer object}

\seealso{
\code{\link[raster]{distance}},  \code{\link[raster]{gridDistance}}

For the direction between points, see the \code{azimuth} function in the \code{geosphere} package 
}


\examples{
r <- raster(ncol=36,nrow=18)
r[] <- NA
r[306] <- 1
b <- direction(r) 
#plot(b)
}

\keyword{spatial}

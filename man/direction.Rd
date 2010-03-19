\name{direction}

\alias{direction}
\alias{direction,RasterLayer-method}

\title{Direction}

\description{
The direction (azimuth) to the nearest cell that is not \code{NA}. The direction unit is in degrees (not radians). If you want radians, use \code{fun=function(x){x}}.
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
  \tab \code{fun} \tab A function that takes the direction (in radians!) as argument. E.g. \code{sin} \cr
  \tab \code{from} \tab Logical. Default is \code{FALSE}. If \code{TRUE}, the direction from (in stead of to) the nearest cell that is not \code{NA} is returned \cr
  \tab \code{filename}  \tab Filename for the output RasterLayer \cr
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
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
b <- direction(r, fun=sin, from=TRUE) 
#plot(b)
}

\keyword{spatial}

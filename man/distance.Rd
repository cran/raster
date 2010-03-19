\name{distance}

\alias{distance}
\alias{distance,RasterLayer-method}

\title{Distance}

\description{
The function calculates the distance for all cells that are \code{NA}, to the nearest cell that is not \code{NA}.
 
The distance unit is in meters if the RasterLayer is not projected (\code{+proj=longlat}) and in map units (typically also meters) when it is projected.
}

\usage{
distance(x, ...) 
}

\arguments{
\item{x}{a RasterLayer object}
\item{...}{additional arguments. See Details.}  
}


\details{
The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
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
\code{\link[raster]{distanceFromPoints}},  \code{\link[raster]{gridDistance}},  \code{\link[raster]{pointDistance}}  

For more advanced distances see the \code{gdistance} package.

For great-circle distances (and more) between (unprojected) points, see the \code{geosphere} package.
}


\examples{
r <- raster(ncol=36,nrow=18)
r[] <- NA
r[500] <- 1
dist <- distance(r) 
#plot(dist)
}

\keyword{spatial}

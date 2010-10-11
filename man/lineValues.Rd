\name{lineValues}

\docType{methods}

\alias{lineValues}
\alias{lineValues,SpatialLines,Raster-method}

\title{ Extract values from a RasterLayer by lines }

\description{
This method returns the values of the cells of a Raster* object that are crossed by a line. .
}

\usage{
lineValues(lns, x, ...)
}

\arguments{
\item{lns}{SpatialLines* object}
\item{x}{Raster* object}
\item{...}{additional arguments (none implemented)}
}


\value{
A list as long as the number of lines in \code{lns}
}

\seealso{ \code{\link[raster]{polygonValues}}, \code{\link[raster]{cellValues}}, \code{\link[raster]{xyValues}}, \code{\link[raster]{getValues}}, \code{\link[sp]{SpatialLines}}  }

\author{Robert J. Hijmans }

\examples{
r <- raster(nrow=18, ncol=9)
r[] <- 1:ncell(r)

cds1 <- rbind(c(-50,0), c(0,60), c(40,5), c(15,-45), c(-10,-25))
cds2 <- rbind(c(80,20), c(140,60), c(160,0), c(140,-55))
cds3 <- rbind(c(-180,-20), c(-140,-60), c(-60,-20), c(-140,55))
lines <- SpatialLines(list(Lines(list(Line(cds1)), "1"), Lines(list(Line(cds2)), "2"), Lines(list(Line(cds3)), "3") ))

lineValues(lines, r)
}

\keyword{methods}
\keyword{spatial}
